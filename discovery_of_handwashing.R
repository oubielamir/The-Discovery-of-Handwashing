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

#death percentage per year
yearly_death %>% mutate(death_percentage = (deaths/births) * 100)

#death percentage per month
monthly_death %>% mutate(death_percentage = (deaths/births) * 100)


#clinic 1 has 188% more death than clinic 2
yearly_death %>% group_by(clinic) %>% summarize(total_death = sum(deaths)) %>% ungroup() %>%
 mutate(percentage = (abs(total_death[2]-total_death[1]) / total_death[2]) * 100)
#monthly deaths to birth ratio

monthly <- monthly_death %>% mutate(monthly_ratio = deaths/births)

ggplot(monthly, aes(date, monthly_ratio)) + geom_line()


#Date when handwashing was required
monthly <- monthly %>% mutate(handwashing_required = date > "1847-06-01") 
  ggplot(monthly, aes(date, monthly_ratio, color = handwashing_required)) + geom_line()


#handwashing affect on deaths numbers
monthly%>% group_by(handwashing_required) %>% summarize(mean(monthly_ratio))

#percentage affect

monthly %>% group_by(handwashing_required) %>% summarize(sum(deaths))


#t-test 
# 95% confidence interval

t_test <- t.test(monthly_ratio ~handwashing_required, data = monthly)

t_test




