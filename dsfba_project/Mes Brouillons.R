fit1 <- step(lm(`Total crimes`~ . -`Analysis Neighborhood` -`Violent crimes`, data = crime_demostats_2016), direction = "forward")
