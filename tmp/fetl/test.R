library("devtools")

load_all()

library("tidyquant")
library("dplyr")
library("lubridate")
library("xts")
library("zoo")
library("rugarch")
library("ggplot2")
library("ggfortify")
library("tibble")

# Get data
res <- get_fsg() %>% tibble
res
