library(dplyr)
library(readr)
library(ggplot2)


#data for birth and death per year
yearly_death <- read_csv("yearly_deaths_by_clinic.csv")

#data for birth and death per month
monthly_death <- read_csv("monthly_deaths.csv")

#death to birth ratio for yearly
yearly_death %>% mutate(ratio = deaths/births) %>% 
  ggplot(aes(year, ratio, color = clinic)) + geom_line()


#clinic 1 has 188% more death than clinic 2
yearly_death %>% group_by(clinic) %>% summarize(total_death = sum(deaths)) %>% ungroup() %>%
 mutate(percentage = (abs(total_death[2]-total_death[1]) / total_death[2]) * 100)


#monthly deaths to birth ratio
monthly_death %>% mutate(monthly_ratio = deaths/births) %>%
  ggplot(aes(date, monthly_ratio)) + geom_line()



