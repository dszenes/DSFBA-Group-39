crime <- data2 %>% select(`Incident Date`,`Incident Time`,`Incident Year`, `Incident Day of Week`,
                 `Incident Category`,`Incident Subcategory`, `Incident Description`,
                 `Police District`, point)
#select the variable that we're interested in

View(crime %>% count(`Incident Category`, name = "num") %>% arrange(desc(num)))
#count the number of crime given their category (time period 2018-2020)
View(crime %>% count(`Incident Day of Week`, name = "day") %>% arrange(desc(day)))
#count the number of crime given their day of the week (time period 2018-2020)


crime_number <- crime %>% arrange(`Incident Time`) %>%  arrange(`Incident Date`) %>%
  add_column(crime = 1) %>% select(crime, `Incident Date`)
#establish the number of crime from 2018 to 2020

crime_number_monhtly <- crime_number %>%
  mutate(month = month(`Incident Date`, label = TRUE),
         year  = year(`Incident Date`)) %>%
  group_by(year, month) %>%
  summarise(tcrimemonth = sum(crime)) %>% mutate(monthcrime = make_date(year, month)) %>%
  select(monthcrime, tcrimemonth)
#group the number of crime per month given the time period 2018 to 2020

ggplot(crime_number_monhtly, aes(x = monthcrime, y = tcrimemonth)) +
  geom_smooth()
#timeseries graph


crime_clean <- crime_number %>%
  separate( `Incident Date`, into = c("year","mon", "day"), sep = "[-]+")
# change the incident date column in order to have year, month and the day in different column




library("leaflet")
library("magrittr")

SFMAP <-
  leaflet() %>% addTiles() %>% setView(-122.42, 37.78, zoom = 11) %>% addProviderTiles(providers$MtbMap) %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 1)) %>%
  addProviderTiles(providers$Stamen.TonerLabels)
