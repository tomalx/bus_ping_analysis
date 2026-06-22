### 2 ### shape and tidy bod_day
#### filter to remove extra heading rows
#### time in jny var
#### (filter for just one route/direction)

# ticket_machine_service_code <- "7"  ## for testing only - set this var in run_gps_snapper.R

library(stringr)
library(tictoc)
library(sf)
library(lubridate)

tic(msg="filter to remove headings rows and mutate route_destination")
bod_eg <- bod_loc_query %>% 
  filter(!lineRef == "lineRef") %>%
  mutate(route_destination = paste0(lineRef,"-",destination)) %>%
  mutate(route_destination = word(route_destination,1, sep = "__"))
toc()

rm(bod_loc_query) # don't need this anymore?

# filter services that have same route number (6 in Bath, 6 in Bristol etc)
bod_eg$ticketMachineServiceCode %>% unique()
unique_service_code <- bod_eg$ticketMachineServiceCode %>% unique() 
unique_service_code <- unique_service_code[1] 

# filter for e.g. m1 inbound
bod_eg <- bod_eg %>%
  filter(ticketMachineServiceCode %in% unique_service_code) %>% 
  #filter(ticketMachineServiceCode == "U1" & directionRef == direction) %>%
  #filter(journeyCode == "0630") %>% 
  st_as_sf(coords = c("lng","lat"), crs = 4326) %>% 
  mutate(journeyCode = as.factor(journeyCode)) %>% 
  mutate(journeyCodeUnq = paste0(journeyCode,"-",vehicleId
  ))

bod_eg_am <- bod_eg %>% 
  filter(hour(time) %in% c(8,9))

bod_eg_pm <- bod_eg %>% 
  filter(hour(time) %in% c(16,17))

# create time in jny var, time_trip - 
# move this after dist_m has been calculated (so that layover at start of trip can be removed)
# bod_eg <- bod_eg %>% 
#   mutate(time = ymd_hms(time)) %>% 
#   group_by(journeyCodeUnq,year_month_day) %>%    ## *WARNING* possibly need to also group_by direction and/or destination???
#   # normalise time to start of journey
#   mutate(time_trip = time - min(time)) %>% 
#   ungroup()
