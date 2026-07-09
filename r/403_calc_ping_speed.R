### ping speed

# assumes dist_m and time_trip vars have already been calculated
# i.e. 402_rank_time_dist has already been run

ping_speed <- function(pings = pings){
  
  pings_new <- pings %>% 
    group_by(journeyCodeUnq, month, day) %>% 
    arrange(time) %>% 
    mutate(prev_ping_dist = dist_m - lag(dist_m)) %>% 
    mutate(prev_ping_time = time_trip - lag(time_trip)) %>% 
    mutate(ping_speed = (dist_m - lag(dist_m)) / 
             (as.numeric(time_trip) - lag(as.numeric(time_trip))) ) %>% 
    mutate(across(ping_speed, ~ replace(., is.nan(.), 0))) %>%
    mutate(across(ping_speed, ~ replace(., . < 0, 0)))
    
  
  return(pings_new)
}

p <- ping_speed(pings) %>% 
  filter(journeyCodeUnq %in% pings_unq_trip_id) %>% 
  #head(20) %>% 
  select(journeyCodeUnq, time, dist_m,
         prev_ping_dist, time_trip, prev_ping_time, 
         ping_speed)

p
