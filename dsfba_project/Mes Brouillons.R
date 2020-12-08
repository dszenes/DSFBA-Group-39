data10 <- read_csv(file = here::here("data/COVID-19_Cases_and_Deaths_Summarized_by_Geography.csv")) 
data10 <- as_tibble(data10)

covid_neighborhoods <- data10 %>% 
  filter(area_type == "Analysis Neighborhood") %>%
  select(id, count, rate)

data11 <- read_csv(file = here::here("data/COVID-19_Cases_Summarized_by_Date__Transmission_and_Case_Disposition.csv")) 
data11 <- as_tibble(data11)

covid_sf <- data11 %>% 
  group_by(`Specimen Collection Date`) %>%
  summarise(`Case Count` = sum(`Case Count`))
