## to_parquet_store

bus_loc_csv <- dir(path = "csv/", pattern = ".csv", full.names = TRUE)
bus_loc_csv <- map(bus_loc_csv[9:10],read.csv)
library(purrr)

#convert to date-time using lubridate
# mutate(time = ymd_hms(time)) %>% 
bus_loc_csv <- map(bus_loc_csv, ~ .x %>% 
            mutate(trip_start = ymd_hms(originAimedDepatureTime)))



# add day week or month attribute
bus_loc_csv <- map(bus_loc_csv, ~ .x %>% 
                     mutate(month = as.character(month(trip_start))))
bus_loc_csv <- map(bus_loc_csv, ~ .x %>% 
                     mutate(year = as.character(year(trip_start))))
bus_loc_csv <- map(bus_loc_csv, ~ .x %>% 
                     mutate(year_month = paste0(year,month)))


# write to parquet
bus_loc_csv <- do.call(rbind, bus_loc_csv)
  
# write to parquet

bus_loc_csv %>% 
  group_by(year_month) %>% 
  write_dataset(path = "parquet", format = "parquet")

tibble(
  files = list.files("parquet", recursive = TRUE),
  size_MB = file.size(file.path("parquet", files)) / 1024^2
)
