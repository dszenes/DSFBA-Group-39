data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "1"] <- "Bayview Hunters Point"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "2"] <- "Bernal Heights"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "3"] <- "Castro/Upper Market"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "4"] <- "Chinatown"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "5"] <- "Excelsior"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "6"] <- "Financial District/South Beach"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "7"] <- "Glen Park"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "8"] <- "Golden Gate Park"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "9"] <- "Haight Ashbury"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "10"] <- "Hayes Valley"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "11"] <- "Inner Richmond"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "12"] <- "Inner Sunset"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "13"] <- "Japantown"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "14"] <- "Lakeshore"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "15"] <- "Lincoln Park"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "16"] <- "Lone Mountain/USF"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "17"] <- "Marina"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "18"] <- "McLaren Park"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "19"] <- "Mission"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "20"] <- "Mission Bay"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "21"] <- "Nob Hill"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "22"] <- "Noe Valley"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "23"] <- "North Beach"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "24"] <- "Oceanview/Merced/Ingleside"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "25"] <- "Outer Mission"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "26"] <- "Outer Richmond"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "27"] <- "Pacific Heights"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "28"] <- "Portola"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "29"] <- "Potrero Hill"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "30"] <- "Presidio"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "31"] <- "Presidio Heights"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "32"] <- "Russian Hill"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "33"] <- "Seacliff"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "34"] <- "South of Market"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "35"] <- "Sunset/Parkside"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "36"] <- "Tenderloin"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "37"] <- "Treasure Island"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "38"] <- "Twin Peaks"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "39"] <- "Visitacion Valley"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "40"] <- "West of Twin Peaks"
data1$`Analysis Neighborhoods 2 2`[data1$`Analysis Neighborhoods 2 2` == "41"] <- "Western Addition"
#changing the level of the neighborhood, in order to make them coincide with the other data table and the shd file
#####


crime <- data2 %>% select(`Incident Date`,`Incident Time`, `Incident Day of Week`,
                 `Incident Category`, `Incident Description`,
                 `Police District`, point)
#select the variable that we're interested in (time horizon 2018-2020)

crime_separated <-
  crime %>% filter(!is.na(point)) %>% separate(point, into = c("lon", "lat"), sep = "[,]+")
 
crime_separated$lat = substr(crime_separated$lat, 2 , nchar(crime_separated$lat) - 1)
  crime_separated$lon = substr(crime_separated$lon, 2 , nchar(crime_separated$lon))
  options(digits = 13)
  crime_separated$lat <- sapply( crime_separated$lat, as.numeric)
  crime_separated$lon <- sapply( crime_separated$lon, as.numeric)

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
    `Police District` = PdDistrict
    
    point = location
  ) #change the column name of the data set with horizon 2003-2018 in order to bind the 2 data set

crime2 <- data1_colchange %>% select(`Incident Date`,`Incident Time`, `Incident Day of Week`,
                                     `Incident Category`, `Incident Description`,
                                     `Police District`, point)
#select the variable that we're interested in

police2003_2020 <- bind_rows(crime, crime2)
#bind both data set

data2 %>% count(`Analysis Neighborhood`, name = "num") %>% arrange(desc(num))
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
###################################################
#POUR REPRESENTATION AVEC ADD MARKER -> BIEN FILTRER LA DATA SET SINON CA PLANTE CPU PROBLEM

essai <-
  crime %>% filter(!is.na(point), `Incident Date` == "2020-08-15") %>%
  separate(point, into = c("lat", "lon"), sep = "[,]+")

essai$lon = substr(essai$lon, 2 , nchar(essai$lon) - 1)
essai$lat = substr(essai$lat, 2 , nchar(essai$lat))
options(digits = 13)
essai$lat <- sapply( essai$lat, as.numeric)
essai$lon <- sapply( essai$lon, as.numeric)


SFMAP <-
  leaflet() %>% addTiles() %>% #create a variable leaflet on which we add the map Tiles
  setView(-122.42, 37.78, zoom = 11) %>% # Precise the map target on San Francisco
  addPolylines(data = data10) %>%
  addMarkers(lng = essai$lon, lat = essai$lat, popup = essai$`Incident Category`, clusterOptions = markerClusterOptions()) %>%
  #add a marker for every point of the data set
  addProviderTiles(providers$MtbMap) %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 1)) %>%
  addProviderTiles(providers$Stamen.TonerLabels) #change the design of the map


#######################################################################################
#######################################################################################


library(sp)

neighborhoods <- st_read("data/SF Find Neighborhoods.kml") %>% select(Name, geometry)
View(neighborhoods)

head(neighborhoods)

neighborhoods <- read_csv(file = here::here("data/planning_neighborhoods.csv"))


plot(neighborhoods) +
  geom_polygon(data = essai, aes(fill = essai), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw()
plot

#######################################################################################
#######################################################################################
library(sf)

map <- st_read(dsn = here::here("data/geo_export_a105db98-2460-49d1-9618-57cf22a53772.shp"))

map %>% ggplot() + geom_sf() +
geom_point(
  aes(x = lon, y = lat)
  ,
  data = essai
  ,
  na.rm = T
  ,
  size = .5
)
