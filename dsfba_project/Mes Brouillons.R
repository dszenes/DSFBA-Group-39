fit1 <- step(lm(`Total crimes`~ . -`Analysis Neighborhood` -`Violent crimes`, data = crime_demostats_2016), direction = "forward")

fit2 <- lm(crime_demostats_2016$`Total crimes`~crime_demostats_2016$`Units of Housing` + crime_demostats_2016$`High School or Less`)

fit3 <- lm(crime_demostats_2016$`Total crimes`~crime_demostats_2016$`Non-Family Households`+crime_demostats_2016$`Number of people Poverty`)


summary(fit2)

summary(fit3)
