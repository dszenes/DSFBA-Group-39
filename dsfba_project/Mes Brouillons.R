schools_coord <- data5 %>% separate(`Location 1`, into = c( "lat", "lon"), sep = ",")

schools_coord$lat = substr(schools_coord$lat, 5, nchar(schools_coord$lat))
schools_coord$lon = substr(schools_coord$lon, 2, nchar(schools_coord$lon) - 1)

schools_coord$lat <- sapply( schools_coord$lat, as.numeric)
schools_coord$lon <- sapply( schools_coord$lon, as.numeric)

schools_coord <- schools_coord %>% 
  mutate(lat = unname(lat),
         lon = unname(lon))


#create lan and lon variables for schools dataset

government_aid <- data2 %>% group_by(`Supervisor District`) %>% count(`Program Area`)
