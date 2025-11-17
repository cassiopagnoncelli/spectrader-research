# ChartSmith
renv::install("/Users/cassio/spec/chartsmith", prompt = FALSE)
detach("package:chartsmith", unload = TRUE)
library("chartsmith")

# Spectrader
renv::install("/Users/cassio/spec/spectrader", prompt = FALSE)
detach("package:spectrader", unload = TRUE)
library("spectrader")

# Financial Engineering Time Series
renv::remove("fets")
renv::install(paste0("packages/", dir("packages")[1]))

detach("package:fets", unload = TRUE)
library("fets")
