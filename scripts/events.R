library(tidyverse)
library(frequencyConnectedness)
library(xts)
library(parallel)
library(reshape2)
library(plotly)
source("R/functions.R")

all <- readRDS("./data/interim/vol_oil.Rdata")
events <- readRDS("./data/interim/events.Rdata")


# OIL SPILLOVER ANALYSIS
H <- 100 # n ahead
w <- 100 # rolling window
p <- 1 # lag order
B <- 1000 # bootstrap
iter_name<-"oil"
file_name <- paste("bootstrap", iter_name, "B", B, "w", w, sep = "_")
full_pth <- paste("./data/interim/", file_name, ".RData", sep = "")
data <- as.xts(all[-1], order.by = as.Date(all[, 1] %>% pull()))


## STATIC SPILLOVER
var_l1 <- vars::VAR(data, p = p, type = "const")
dy12 <- spilloverDY12(var_l1, n.ahead = H, no.corr = F)
bk12 <-
  spilloverBK12(
    var_l1,
    n.ahead = H,
    no.corr = F,
    partition = c(pi + 0.00001, pi / 5, pi / 20, pi / 200, 0)
  )

## ROLLING SPILLOVER
cl <- makeCluster(4)
invisible(clusterExport(cl = cl, varlist = c("H", "w", "p")))
dy12_roll_all <- spilloverRollingDY12(
  data,
  n.ahead = H,
  no.corr = F,
  func_est = "VAR",
  params_est = list(p = p, type = "const"),
  window = w,
  cluster = cl
)
bk12_roll_all <- spilloverRollingBK12(data,
  n.ahead = H, no.corr = F, func_est = "VAR",
  params_est = list(p = p, type = "const"), window = w,
  partition = c(pi + 0.00001, pi / 5, pi / 20, pi / 200, 0), cluster = cl
)
stopCluster(cl)


## BOOTSTRAP
# recalc bootstrap
if (T) {
  n <- dim(all)[1] - w
  ncols <- dim(all)[2] - 1
  cl <- makeCluster(4)
  invisible(clusterExport(
    cl = cl,
    varlist = c("H", "w", "p", "B", "ncols")
  ))
  invisible(clusterEvalQ(cl, library("vars")))
  invisible(clusterEvalQ(cl, library("frequencyConnectedness")))

  final <- vector("list", n)
  startTime <- Sys.time()

  for (i in 1:n) {
    res <- bootstrap_window(i, H = H, w = w, B = B)
    final[[i]] <- res
    if (i %% 100 == 1) {
      print(i)
      saveRDS(final, file = full_pth)
    }
  }
  saveRDS(final, file = full_pth)
  endTime <- Sys.time()
  endTime - startTime
  stopCluster(cl)
}


## COMPUTE PROBABILITIES
# compute probabilities
b_data <- readRDS(full_pth)
indices <- sapply(b_data, `[[`, 1)
dates <- as.Date(unlist(sapply(b_data, `[[`, 2)))
index_df <- data.frame(indices, dates)

j <- 1
res <- lapply(2:(length(b_data) - j), eval_date, j = j)
Date <- as.Date(sapply(res, `[[`, 1))
value <- sapply(res, `[[`, 2)
bias <- sapply(b_data, `[[`, 5)
prior <- sapply(b_data, `[[`, 3)
posterior <- sapply(b_data, `[[`, 6)

test_res <- data.frame(Date, value)
above_X <- test_res %>%
  filter(value > 0.95) %>%
  dplyr::select(Date) %>%
  pull()

for (j in 0:4) {
  events <- add_j(events, j = j, perc = 0.95)
}
hits <- events %>% dplyr::filter(j_1 > 0, j_2 > 0, j_3 > 0, j_4 > 0)


### MAIN PLOT
overall_sp<-to_long(overall(dy12_roll_all)[[1]])
overall_sp["variable"] <- "spillover"
overall_plt<-ggplot(overall_sp,aes(x=Date,y=value)) + 
  geom_line() + 
  geom_vline(xintercept = as.numeric(unique(hits$Date)),color = "red",linetype="solid",linewidth=0.3) + 
  theme_bw() + labs(x="Year",y="Spillover")
ggplotly(overall_plt)

# NET PLT
net_sp <- to_long(net(dy12_roll_all)[[1]])
ncol <- length(unique(net_sp$variable))
net_plt <- ggplot(net_sp, aes(x = Date, y = value)) +
  # geom_line(col="black")+
  geom_area(fill = "black") +
  facet_wrap(~variable, nrow = ncol / 2) +
  theme_bw() +
  labs(x = "Year", y = "") +
  scale_y_continuous(limits = c(-10, 10))
net_plt



# FREQUENCY
ovs <- overall(bk12_roll_all)
ovs_freq <- data.frame(matrix(NA, 0, 3))
colnames(ovs_freq) <- c("series", "date", "val")
mains <- c("1-5", "5-20", "20-200", "200-inf")
for (j in 1:length(ovs)) {
  i <- ovs[[j]] %>% as.data.frame()
  ov_sp <- cbind(rep(mains[j], nrow(i)), rownames(i), i)
  colnames(ov_sp) <- c("series", "date", "val")
  ovs_freq <- rbind(ovs_freq, ov_sp)
}
ovs_freq$date <- as.POSIXct(ovs_freq$date)
row.names(ovs_freq) <- NULL
ovs_freq <- ovs_freq %>% filter(series != "200-inf")
ovs_freq$series <- factor(ovs_freq$series, levels = mains)

ovs_freq <- ovs_freq %>% mutate(series = case_when(
  (series == "1-5") ~ "Short (1-5)",
  (series == "5-20") ~ "Medium (5-20)",
  (series == "20-200") ~ "Long (20-200)"
))
freq_plt <- ggplot(ovs_freq, aes(x = as.Date(date), y = val, color = series, size = series)) +
  geom_line() +
  scale_size_manual(values = c(0.5, 0.2, 0.5)) +
  scale_color_manual(values = c("#ABABAB", "#000000", "#000000")) +
  labs(y = "Spillover", x = "", colour = "", linetype = "", size = "") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 10))
ggplotly(freq_plt)


############################ 
# AVG MEANS AUX PLOT
b_data <- readRDS(full_pth)
indices <- sapply(b_data, `[[`, 1)
dates <- as.Date(unlist(sapply(b_data, `[[`, 2)))
index_df <- data.frame(indices, dates)
j <- 1
res <- lapply(2:(length(b_data) - j), eval_date, j = j)
Date_full <- as.Date(sapply(b_data, `[[`, 2))
Date <- as.Date(sapply(res, `[[`, 1))
value <- sapply(res, `[[`, 2)
bias <- sapply(b_data, `[[`, 5)
prior <- sapply(b_data, `[[`, 3)
posterior <- sapply(b_data, `[[`, 6)
df_compare <- data.frame("Date" = Date_full, prior, posterior)
test_res <- data.frame(Date, value)
above_X <- test_res %>%
  filter(value > 0.95) %>%
  dplyr::select(Date) %>%
  pull()

df_compare <- data.frame("Date" = Date_full, prior, posterior) %>%
  left_join(test_res, by = "Date") %>%
  mutate(passed = replace_na(value > 0.95, F))
prior_mean <- lapply(2:(length(b_data) - j), get_pre_mean)
Date <- as.Date(sapply(prior_mean, `[[`, 1))
prior_mean <- sapply(prior_mean, `[[`, 2)
prior_mean <- data.frame(Date, prior_mean)

post_mean <- lapply(2:(length(b_data)), get_post_mean)
Date <- as.Date(sapply(post_mean, `[[`, 1))
post_mean <- sapply(post_mean, `[[`, 2)
post_mean_main <- data.frame(Date, post_mean)
for (j in 1:4) {
  colnm <- paste(c("post_mean_", j), collapse = "")
  post_mean <- lapply(2:(length(b_data) - j), get_post_mean, j = j)
  Date <- as.Date(sapply(post_mean, `[[`, 1))
  value <- sapply(post_mean, `[[`, 2)
  post_mean <- data.frame(Date, value)
  colnames(post_mean) <- c("Date", colnm)
  post_mean_main <- post_mean_main %>% dplyr::left_join(post_mean, by = "Date")
}

means <- prior_mean %>% inner_join(post_mean_main, by = "Date")
means <- events %>%
  dplyr::select(Date, Title, cat) %>%
  inner_join(means, by = "Date") %>%
  mutate(passed_test = Date %in% hits$Date)
means <- means %>%
  mutate(post_mean_all = rowMeans(means[, c(5, 6, 7, 8, 9)])) %>%
  mutate(diff = post_mean_all - prior_mean)
means <- means %>%
  group_by(year = year(Date), cat, passed_test) %>%
  summarise(mean_diff = mean(diff))
change_plot <- ggplot(means, aes(x = year, y = mean_diff)) +
  geom_bar(stat = "identity", aes(fill = passed_test), show.legend = FALSE) +
  scale_fill_manual(values = c("black", "grey")) +
  facet_wrap(~cat, ncol = 3) +
  theme_bw() +
  labs(x = "", y = "Spillover Change", fill = "") +
  theme(
    legend.text = element_blank(), legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 10), panel.grid.major = element_blank()
  )
################
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

events_per_year<-events%>% ggplot( aes(fill=cat,colour=cat, x=year(Date))) + 
  geom_bar(position="stack", stat="count",color="black") + theme_bw() + 
  scale_fill_manual(values=c("black","grey","white"),name="Category") +
  scale_x_continuous(breaks = seq(min(year(events$Date)),max(year(events$Date)),by=2))+
  labs(y = "", x = "") + 
  theme(legend.text=element_text(size=15),legend.title=element_text(size=15),
        axis.text.x = element_text(angle=90, vjust=.5, hjust=1,size=10))


ggsave("./data/output/plots/net_plt.png",net_plt, width = 20, height = 10, units = "cm")
ggsave("./data/output/plots/overall_plt_w200.png",overall_plt, width = 20, height = 10, units = "cm")
ggsave("./data/output/plots/events_per_year.png",events_per_year, width = 20, height = 10, units = "cm")
ggsave("./data/output/plots/freq_plt.png",freq_plt, width = 20, height = 10, units = "cm")
ggsave("./data/output/plots/change_plot.png",change_plot, width = 20, height = 10, units = "cm")
