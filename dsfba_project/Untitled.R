crime <- data2 %>% select(`Incident Date`,`Incident Time`, `Incident Day of Week`,
                 `Incident Category`, `Incident Description`,
                 `Police District`, point)
#select the variable that we're interested in (time horizon 2018-2020)

crime_separated <-
  crime %>% # extract from database
  separate(point, into = c("long", "lat"), sep = "[,]+")
 crime_separated$lat = substr(crime_separated$lat, 2 , nchar(crime_separated$lat) - 1)
  crime_separated$long = substr(crime_separated$long, 2 , nchar(crime_separated$long))
    
View(crime_separated)
 View(crime)

#######################################################################################
#BIND DES DEUX DATA ->2003-2018 & 2018-2020 POUR AVOIR 2003-2020
data1_colchange <- data1 %>% mutate(Date = mdy(Date)) %>% 
  rename(
    `Incident Date` = Date,
    `Incident Time` = Time,
    `Incident Day of Week` = DayOfWeek,
    `Incident Category` = Category,
    `Incident Description` = Descript,
    `Police District` = PdDistrict,
    point = location
  ) #change the column name of the data set with horizon 2003-2018 in order to bind the 2 data set

crime2 <- data1_colchange %>% select(`Incident Date`,`Incident Time`, `Incident Day of Week`,
                                     `Incident Category`, `Incident Description`,
                                     `Police District`, point)
#select the variable that we're interested in

police2003_2020 <- bind_rows(crime, crime2)
#bind both data set


police2003_2020 %>% count(`Incident Category`, name = "num") %>% arrange(desc(num))
#count the number of crime given their category (time period 2018-2020)
police2003_2020 %>% count(`Incident Day of Week`, name = "day") %>% arrange(desc(day))
#count the number of crime given their day of the week (time period 2018-2020)

#######################################################################################
#MANIPULATION SUR LA DATA SET 2018-2020

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

#######################################################################################
#MANIPULATION SUR LA DATA SET 2003-2018

library(lubridate)

data1.2 <- data1 %>% mutate(Date = mdy(Date))

crime_number2 <- data1.2  %>% arrange(Time) %>%  arrange(Date) %>%
  add_column(crime = 1) %>% select(crime, Date)
#establish the number of crime from 2003 to 2017



crime_number_monhtly2 <- crime_number2 %>%
  mutate(month = month(Date, label = TRUE),
         year  = year(Date)) %>%
  group_by(year, month) %>%
  summarise(tcrimemonth = sum(crime)) %>% mutate(monthcrime = make_date(year, month)) %>%
  select(monthcrime, tcrimemonth)
#group the number of crime per month given the time period 2003 to 2017

ggplot(crime_number_monhtly2, aes(x = monthcrime, y = tcrimemonth)) +
  geom_smooth(se = F)
#timeseries graph


#######################################################################################
#MANIPULATION SUR LA DATA SET 2003-2020

crime_number0320 <- police2003_2020 %>% arrange(`Incident Time`) %>%  arrange(`Incident Date`) %>%
  add_column(crime = 1) %>% select(crime, `Incident Date`)
#establish the number of crime from 2018 to 2020

crime_number_monhtly0320 <- crime_number0320 %>%
  mutate(month = month(`Incident Date`, label = TRUE),
         year  = year(`Incident Date`)) %>%
  group_by(year, month) %>%
  summarise(tcrimemonth = sum(crime)) %>% mutate(monthcrime = make_date(year, month)) %>%
  select(monthcrime, tcrimemonth)
#group the number of crime per month given the time period 2018 to 2020

library(ggthemes)
install.packages("ggthemes")
ggplot(crime_number_monhtly0320, aes(x = monthcrime, y = tcrimemonth)) +
  theme_economist_white() +
  scale_color_economist(name = "Seasons:") +
  geom_smooth(se = F)
#timeseries graph


crime_clean <- crime_number %>%
  separate( `Incident Date`,
            into = c("year","mon", "day"),
            sep = "[-]+")
# change the incident date column in order to have year, month and the day in different column

#######################################################################################
#######################################################################################


#VISUALISATION

library("leaflet")
library("magrittr")

SFMAP <-
  leaflet() %>% addTiles() %>% setView(-122.42, 37.78, zoom = 11) %>%
  addProviderTiles(providers$MtbMap) %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 1)) %>%
  addProviderTiles(providers$Stamen.TonerLabels)



leaflet(data = crime_separated) %>%
  addTiles() %>%
  addMarkers() %>%
  frameWidget()

