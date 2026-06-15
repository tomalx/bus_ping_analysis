
# setwd("C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/7.0 Data/03 Analysis Projects/bod_api/bus_ping_analysis")
### 1 ### import bods - e.g.
# if (!exists("bod_day")) {
#   bod_rds <- readRDS("rds/bod_20250414.Rds") # obj was called bod_day
# }
# 
# ## or 
# bod_csv <- read.csv("csv/bus_loc_20260603.csv", header = TRUE)

## To Do - store as parquet and use arrow to read data
## https://r4ds.hadley.nz/arrow.html

## method for importing multiple days

## parquet store group_by - should not use originAimedDepartureTime because this
## field is missing from some records.




library(arrow)
library(dplyr)
library(glue)
setwd("bus_ping_analysis")
parquet_month_dir <- "20266" # YYYYM / YYYYMM

parquet_path <- "parquet"


bus_loc_parquet <- open_dataset(glue("parquet/{parquet_month_dir}"))


bod_loc_query <- bus_loc_parquet %>%
  filter(year_month_day %in% c("202669","2026610")) %>%
  #filter(year_month_day %in% c("202662","202663","202664","202665","202668","202669","2026610")) %>% 
 # filter(year_month_day == "202669") %>% 
  filter(lineRef == "1") #%>% 
 # group_by(time,destination,journeyCode) %>% 
  #summarise(count = n())
#group_by(lineRef, journeyCode, destination) %>% 
#summarise(count = n())

bod_loc_query <- bod_loc_query %>% collect()


