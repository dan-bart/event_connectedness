library(tidyverse)
library(frequencyConnectedness)
library(ConnectednessApproach)
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

# plot
long <-all %>% pivot_longer(cols = -c(Date),names_to = "series",values_to = "RV")
rv_plot<-ggplot(long,aes(x=Date,y=RV,color=series)) + ylim(c(0,0.015)) + geom_line()
ggplotly(rv_plot)

# BARUNIK LIBRARY
data <- as.xts(all[-1], order.by = as.Date(all[, 1] %>% pull()))
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
stopCluster(cl)

overall_sp <- to_long(overall(dy12_roll_all)[[1]])
overall_sp["variable"] <- "spillover"
overall_plt <- ggplot(overall_sp, aes(x = Date, y = value)) +geom_line(color = "black") +theme_bw()

ggplotly(overall_plt)


# CONNECTEDNESS APPROACH

# TOTAL CONNECTEDNESS

data <- as.xts(all[-1], order.by = as.Date(all[, 1] %>% pull()))
dc_orig <- ConnectednessApproach(data,
                                 nlag = p,
                                 nfore = H,
                                 window.size = w,
                                 model = "VAR",
                                 connectedness = "Time",
                                 Connectedness_config = list(TimeConnectedness = list(generalized = TRUE))
)
# TVP-VAR
dc_tvp <- ConnectednessApproach(data,
                                nlag = p,
                                nfore = H,
                                window.size = w,
                                model = "TVP-VAR",
                                connectedness = "Time",
                                VAR_config = list(TVPVAR = list(kappa1 = 0.99, kappa2 = 0.96, prior = "BayesPrior"))
)

dc_joint <- ConnectednessApproach(data,
                                  nlag = p,
                                  window.size = w,
                                  nfore = H,
                                  model = "VAR",
                                  connectedness = "Joint"
)

dc_q5 <- ConnectednessApproach(data,
                               model = "QVAR",
                               connectedness = "Time",
                               nlag = p,
                               nfore = H,
                               window.size = w,
                               VAR_config = list(QVAR = list(tau = 0.05))
)

dc_q95 <- ConnectednessApproach(data,
                                model = "QVAR",
                                connectedness = "Time",
                                nlag = p,
                                nfore = H,
                                window.size = w,
                                VAR_config = list(QVAR = list(tau = 0.95))
)



df_dc_orig <- TCI_to_df(dc_orig$TCI, 'dy12')
df_dc_tvp <-TCI_to_df(dc_tvp$TCI, 'tvp')
df_dc_joint <-TCI_to_df(dc_joint$TCI, 'joint')
df_dc_q05 <-TCI_to_df(dc_q5$TCI, 'q05')
df_dc_q95 <-TCI_to_df(dc_q95$TCI, 'q95')



df <- bind_rows(df_dc_orig, df_dc_tvp,df_dc_joint,df_dc_q05)
ggplot(df,aes(x=date,y=TCI,color=dca_type)) +geom_line()
