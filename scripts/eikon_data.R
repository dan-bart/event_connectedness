library(Refinitiv)

# run if first time
# Refinitiv::install_eikon()

api_key <- "21528198141e4d6c9926a55900f5bb0c75001f50"
Eikon <- EikonConnect(Eikonapplication_id = api_key, PythonModule = "RD")
RD <- RDConnect(application_id = api_key, PythonModule = "RD")


commodities <- c(
  "heating_oil"="HOc1", 
  "diesel"="LGOc1", 
  "gasoline"="RBc1",
  "natural_gas"="NGc1",
  "crude_oil"="CLc1"
)



timeseries1 <- rd_GetHistory(RD = RD,
                             universe = commodities,
                             interval = "daily",
                             fields = c("OPEN_PRC", "HIGH_1", "LOW_1", "TRDPRC_1"),
                             start = "2023-12-01")


timeseries1
