
# setwd("C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/7.0 Data/03 Analysis Projects/bod_api/bus_ping_analysis")
### 1 ### import bods - e.g.
if (!exists("bod_day")) {
  bod_rds <- readRDS("rds/bod_20250414.Rds") # obj was called bod_day
}

## or 
bod_csv <- read.csv("csv/bus_loc_20260603.csv", header = TRUE)

## To Do - store as parquet and use arrow to read data
## https://r4ds.hadley.nz/arrow.html