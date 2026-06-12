## to_parquet_store

library(arrow)
library(tidyverse)
library(purrr)

## *TODO* - need to find way of writing parquet in batch without overwriting
##          records  from previous batch


# *WARNING* - make sure you're in the right directory
#  there are several csv folders
#  you need 03 Analysis Projects/bod_api/bus_ping_analysis
bus_loc_csv <- dir(path = "csv/", pattern = ".csv", full.names = TRUE)
bus_loc_csv
bus_loc_csv <- map(bus_loc_csv[16:17],read.csv)

#convert to date-time using lubridate
# mutate(time = ymd_hms(time)) %>% 
bus_loc_csv <- map(bus_loc_csv, filter, !lineRef == "lineRef")
bus_loc_csv <- map(bus_loc_csv, ~ .x %>% 
            mutate(time = ymd_hms(time)))



# add day week or month attribute
bus_loc_csv <- map(bus_loc_csv, ~ .x %>% 
                     mutate(day = as.character(day(time))))
bus_loc_csv <- map(bus_loc_csv, ~ .x %>% 
                     mutate(month = as.character(month(time))))
bus_loc_csv <- map(bus_loc_csv, ~ .x %>% 
                     mutate(year = as.character(year(time))))
bus_loc_csv <- map(bus_loc_csv, ~ .x %>% 
                     mutate(year_month_day = paste0(year,month,day)))


# write to parquet
bus_loc_csv <- do.call(rbind, bus_loc_csv)
  
# write to parquet
parquet_month_dir <- as.character(paste0(year(Sys.Date()),month(Sys.Date())))


bus_loc_csv %>% 
  group_by(year_month_day) %>% 
  write_dataset(path = paste0("parquet/",parquet_month_dir), format = "parquet")

tibble(
  files = list.files(path = paste0("parquet/",parquet_month_dir), recursive = TRUE),
  size = file.size(file.path(path = paste0("parquet/",parquet_month_dir), files)),
  size_MB = file.size(file.path(path = paste0("parquet/",parquet_month_dir), files)) / 1024^2
)


