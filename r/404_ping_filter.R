### filter pings by direction, time of day, short runnings etc

ping_filter <- function(pings = pings,
                        direction = c(0,1),
                        hr_of_day = c(7,8,9),
                        sample_jnycode){
  
  pings_unq_trip_id <- pings %>% 
    filter(direction_id %in% direction) %>% 
    filter(hour(time) %in% hr_of_day) %>% 
    pull(journeyCodeUnq) %>% 
    unique()
  
  if(!missing(sample_jnycode)){
    pings_unq_trip_id <- pings_unq_trip_id %>% 
      sample(sample_jnycode)
  }else{
    pings_unq_trip_id <- pings_unq_trip_id
  }
  
  pings_filtered <- pings %>% 
    filter(journeyCodeUnq %in% pings_unq_trip_id)
  
  return(pings_filtered)
  
}


pings_plot <- pings %>% 
  ping_filter()


pings_plot <- pings %>% 
  ping_filter(
              direction = 1,
              sample_jnycode = 5)
