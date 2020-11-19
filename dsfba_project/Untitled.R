library(sf)
mymap <- st_read("data/San Francisco Neighborhood Boundaries.kml")
plot(mymap %>% select(Name)
)
mymap %>% select(Name)

View(mymap)


View(data2)
summary(data2)

crime <- data2 %>% select(`Incident Date`,`Incident Time`,`Incident Year`, `Incident Day of Week`,
                 `Incident Category`,`Incident Subcategory`, `Incident Description`,
                 `Police District`, point)
#select the variable that we're interested in


crime_clean <- crime %>%
  separate( `Incident Date`, into = c("year","mon", "day"), sep = "[-]+")
# change the incident date column in order to have year, month and the day in different column
View(crime_clean)

View(crime_clean %>% count(`Incident Category`, name = "num") %>% arrange(desc(num)))
#count the number of crime given their category
View(crime_clean %>% count(`Incident Day of Week`, name = "day") %>% arrange(desc(day)))
#count the number of crime given their day of the week

 crime_clean_asc <- crime_clean %>% arrange(`Incident Time`) %>%  arrange(day) %>%  arrange(mon) %>% arrange(year) %>%
  rowid_to_column(var = 'crime number')

timeseriesmonth <- crime_clean_asc %>% group_by(mon, year) %>% summarize( count = n()) %>% arrange(year)


ggplot(data = crime, aes(x = `Incident Date`, y = pop))+
  geom_line(color = "#00AFBB", size = 2)

library("leaflet")
library("magrittr")

SFMAP <-
  leaflet() %>% addTiles() %>% setView(-122.42, 37.78, zoom = 11) %>% addProviderTiles(providers$MtbMap) %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 1)) %>%
  addProviderTiles(providers$Stamen.TonerLabels)
