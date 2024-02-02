# Event Connectedness

This repository holds code for analysis related to **event conenectedness** and **frequency connectedness**.

To use, clone the repository in RStudio and run:

```R
install.packages('renv')
renv::restore() # install required packages
```

## Repo Structure

|   .gitignore
|   event_connectedness.Rproj
|   README.md
|   renv.lock
|   
+---data
|   +---input
|   |       ASV5HU.csv - old gasoline intra-day prices
|   |       ASV5XB.csv - new gasoline intra-day prices
|   |       eikon_data.csv - OLHC oil commodity prices
|   |       events.csv - cleaned events dataset
|   |       oil_events_main.xlsx - main events dataset
|   |       
|   +---interim
|   |       bootstrap_1000_oil_w_200.RData
|   |       bootstrap_all_B_1000_w_100.RData
|   |       bootstrap_oil_B_1000_w_100.RData
|   |       bootstrap_oil_B_1000_w_200.RData
|   |       events.Rdata
|   |       vol_oil.Rdata
|   |       
|   \---output
|       \---plots
|               change_plot.png
|               events_per_year.png
|               freq_plt.png
|               net_plt.png
|               overall_plt_w200.png
|               
+---R
|       functions.R
|       
\---scripts
        connectedness.R - calculations of Connectedness using both frequencyConnectedness and ConnectednessApproach packages
        eikon_data.R - script for loading OLHC and intraday data from Refinitive Eikon (includes API key)
        events.R - bootstrap-after-bootstrap code for estimating the effect of events on connectedness
        ingest_data.R - data processing, creates interim/events.Rdata and interim/vol_oil.Rdata
