library(tidyverse)  # Includes dplyr, readr, tibble, tidyr, and stringr
library(lubridate)
library(xts)
library(imputeTS)
library(lares)


library(timeDate)


# data prep
eikon <-
  read_csv("./data/input/eikon_data.csv", show_col_types = FALSE) %>% dplyr::select(-c(VOLUME, COUNT))
rb_old <- read_csv("./data/input/ASV5HU.csv", show_col_types = FALSE)
rb_new <- read_csv("./data/input/ASV5XB.csv", show_col_types = FALSE)

# prep events
events <- read.csv("./data/input/events.csv", sep = ",") %>%
  as.data.frame() %>%
  dplyr::select(Date, Title, one_word) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  dplyr::filter(Date >= start.Date)
events <- events %>% mutate(rank = 1:nrow(events))


# get list of valid dates
# make a list of all valid dates
start.Date <- as.Date("1987-09-01")
end.Date <- as.Date("2022-12-15")
years_holidays <- unique(year(eikon$Date))
holidays <- unique(do.call(c, lapply(years_holidays, function(y) holidayNYSE(year = y))))
holidays <- as.Date(holidays@Data)


date.range <-
  seq.Date(start.Date, end.Date, by = "day") %>% data.frame()
colnames(date.range) <- c("Date")

date.range$Month <- month(date.range$Date)
date.range$Day <- day(date.range$Date)
date.range$Weekday <- weekdays(date.range$Date)

date.range <- date.range %>%
  dplyr::filter(!(Day %in% c(24, 25, 26, 31) &
                    Month == 12)) %>% # remove christmas
  dplyr::filter(!(Day %in% c(1, 2) &
                    Month == 1)) %>% # remove new years
  dplyr::filter(!Weekday %in% c("Sunday", "Saturday")) %>% # remove weekends
  dplyr::filter(!Date %in% holidays) %>%
  dplyr::select(-c(Day, Month, Weekday))


# set up type of category
map_df <- data.frame(
  up = c(
    "geopolitical",
    "geopolitical",
    "economic",
    "geopolitical",
    "geopolitical",
    "economic",
    "economic",
    "geopolitical",
    "economic",
    "natural",
    "economic",
    "economic",
    "natural",
    "economic",
    "economic",
    "geopolitical",
    "economic",
    "natural"
  ),
  down = c(
    "war",
    "missile",
    "boost",
    "threat",
    "political",
    "sanctions",
    "market",
    "peace",
    "cut",
    "spill",
    "maintain",
    "speculation",
    "natural",
    "inventory",
    "develop",
    "strike",
    "merge",
    "pandemic"
  ),
  stringsAsFactors = FALSE
)

events$cat <- map_df$up[match(events$one_word, map_df$down)]


# move dates if it was on weekend on holiday
find_next_date <- function(date_orig) {
  next_date <- date.range$Date[date.range$Date >= date_orig][1]
  return(next_date)
}

events$moved_date <- sapply(events$Date, find_next_date)
events$moved_date <- as.Date(events$moved_date)
events$move_by <- as.integer(events$moved_date - events$Date)
events$cat <-
  factor(events$cat, # Change ordering manually
    levels = c("natural", "economic", "geopolitical")
  )

# summary table
summary_events <- events %>%
  group_by(cat, one_word) %>%
  summarise("count" = n())
summary_events[is.na(summary_events)] <- 0
# print(xtable(summary_events), include.rownames=FALSE)


# prep tickdata RB
# Join old and new series
rb_old$Date <-
  as.POSIXct(paste(rb_old$Date, rb_old$Time), format = "%m/%d/%Y %H:%M:%S")
rb_new$Date <-
  as.POSIXct(paste(rb_new$Date, rb_new$Time), format = "%m/%d/%Y %H:%M:%S")
print(min(rb_new$Date))
print(max(rb_old$Date))
rb_old <- rb_old %>% filter(Date < min(rb_new$Date))
rb <- rbind(rb_old, rb_new)
rb <- rb %>%
  arrange(Date) %>%
  dplyr::select(-Time)
# remove 4 occasions with only one reported value on a given date (out of 5)
z <- rb %>%
  group_by("date" = as.Date(Date)) %>%
  summarize(n = n())
one_dates <- z %>%
  filter(n < 20) %>%
  dplyr::select(date)
rb <- rb %>% dplyr::filter(!as.Date(Date) %in% one_dates$date)
# make returns
rb_xts <-
  xts(
    x = rb$Close,
    order.by = strptime(rb$Date, "%Y-%m-%d %H:%M")
  )
# Add OLHC from HF data into Eikon dataset
rb_cutoff_date <- "2008-08-29"
# add RB_HF as another series
rb_hf_ohlc <- to.daily(rb_xts)
daily_ohlc_df <- as_tibble(rb_hf_ohlc)
colnames(daily_ohlc_df) <- c("OPEN", "HIGH", "LOW", "CLOSE")
daily_ohlc_df$Date <- index(rb_hf_ohlc)
daily_ohlc_df$series <- "rb"

# subset until date we want to use HF based OLHC
daily_ohlc_df <-
  daily_ohlc_df[daily_ohlc_df$Date < rb_cutoff_date, c("Date", "HIGH", "LOW", "OPEN", "CLOSE", "series")]

eikon <-
  eikon %>% filter(!((series %in% c("rb")) &
    (Date <= rb_cutoff_date)))
# remove rows of RB prior to the date
eikon <- bind_rows(daily_ohlc_df, eikon)


# eikon data cleaning
eikon <- date.range %>% left_join(eikon, by = "Date")
# Remove illogical values
eikon %>%
  dplyr::filter(LOW > HIGH |
    LOW > OPEN |
    LOW > CLOSE | HIGH < OPEN | HIGH < CLOSE) %>%
  dplyr::select(Date) %>%
  unique() %>%
  nrow()
eikon <- eikon %>%
  mutate(LOW = ifelse(LOW > CLOSE, CLOSE, LOW)) %>%
  mutate(LOW = ifelse(LOW > OPEN, OPEN, LOW)) %>%
  mutate(HIGH = ifelse(HIGH < CLOSE, CLOSE, HIGH)) %>%
  mutate(HIGH = ifelse(HIGH < OPEN, OPEN, HIGH))

# compute RV
eikon$RV <-
  0.511 * (log(eikon$HIGH) - log(eikon$LOW))^2 -
  0.019 * ((log(eikon$CLOSE) - log(eikon$OPEN)) * (log(eikon$HIGH) + log(eikon$LOW) - 2 * log(eikon$OPEN))
    - 2 * (log(eikon$HIGH) - log(eikon$OPEN)) * (log(eikon$LOW) - log(eikon$OPEN))) - 0.383 * (log(eikon$CLOSE) - log(eikon$OPEN))^
    2

eikon <- eikon %>%
  dplyr::select(Date, RV, series) %>%
  pivot_wider(names_from = series, values_from = RV) %>%
  dplyr::select(-"NA") %>%
  unnest(cols = c(rb, ho, lgo, ng, oil))


all <- eikon
oil_only <- T # FALSE TO INCLUDE ALL COMMODITIES
# choose oil or all
date_ng <- "1990-04-03"

if (oil_only) {
  all <- all %>% dplyr::select(-ng)
  iter_name <- "oil"
} else {
  all <- all %>% dplyr::filter(Date >= date_ng)
  iter_name <- "all"
}
# impute the rest with rolling mean of 5 days (163 obs)
print(all[rowSums(is.na(all)) > 0, ] %>% dim())
all <- imputeTS::na_ma(all, k = 5, weighting = "simple")


# SAVING
saveRDS(all, file = "./data/interim/vol_oil.Rdata")
saveRDS(events, file = "./data/interim//events.Rdata")