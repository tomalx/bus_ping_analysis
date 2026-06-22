### 6 get ticketer data

# NEXT STEP download start of June Ticketer data (csv) - so that I can join with
# start of June BOD data. 

library(glue)
library(purrr)

# tik_path <- here::here()
user_name <- Sys.info()["user"]
tik_path <- glue("C:/Users/{user_name}/OneDrive - West Of England Combined Authority/Transport/7.0 Data/03 Analysis Projects/tdh_databricks/bus_ticket_sales_reporting")
tik_path <- glue("{tik_path}/csv/bronze_month/")

month_csv <- dir(tik_path, pattern = "all_ops_ticketer_", full.names = FALSE)
cat(" ","listing files in ",tik_path, " ............. \n")
for(i in 1:length(month_csv)){
  cat("   ",i," ",fs::path_file(month_csv[i]), "\n")
}

month_csv <- dir(tik_path, pattern = "all_ops_ticketer_", full.names = TRUE)
# month_csv <- dir(glue("{proj_path}/csv/gold/"), pattern = "gold_ticketer_", full.names = TRUE) # could use gold_production tables

month_csv <- month_csv[6] %>%   ## !!! need to make sure ticketer dates match gtfs quarter !!!
  #month_csv <- month_csv[1] %>%  
  map(read.csv) %>%
  map(as_tibble)

rm(i, tik_path)
# aggregate passeneger boardings by route



# bod_eg$ticketMachineServiceCode %>% unique()
# unique_service_code <- bod_eg$ticketMachineServiceCode %>% unique() 
# unique_service_code <- unique_service_code[1] 

month_csv <- map(month_csv, dplyr::filter, Service %in% unique_service_code)  # unique_service_code comes from 2_tidy_bod_gps

month_csv <- map(month_csv, dplyr::select, c(-Shadow_Fare, -Latitude, -Longitude, -From_Stage, -From_Stage_ID,
                                             -To_Stage, -To_Stage_ID, -Price, -Journey_Code, -Passenger_Count,
                                             -Currency, -External_Service_Code, -ETM_ID, - Ticket_Type_ID))
month_csv <- map(month_csv, mutate, datetime = as_datetime(IssuedAt))
month_csv <- do.call("rbind", month_csv)

pax <- month_csv %>% 
  left_join(stop_seq %>% 
              ungroup() %>% 
              select(-shape_id, 
                     -direction_id, 
                     -stop_sequence ) %>% 
              distinct(),
            by = c("Service" = "route_short_name", "Bus_Stop_Atco" = "stop_code"))










