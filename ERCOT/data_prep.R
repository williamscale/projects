library(tidyverse)
library(readxl)
library(lubridate)
library(assertthat)
# library(forecast)
library(ggthemes)

# library(rstanarm)
# library(bayesplot)
# library(bayestestR)

set.seed(55)

# Data Prep ---------------------------------------------------------------

## Temperature ------------------------------------------------------------

# https://www.weather.gov/wrh/Climate?wfo=ewx
# Location: San Antonio Area
# Product: Calendar day summaries
# Options: Year range = 2024-2024,
# Variable = Min/Max Temp, Summary = Daily Minimum/Maximum 
temps <- read_excel('D:/projects/ERCOT/data/daily_temps_2024.xlsx')
temps <- temps %>%
  mutate(dayOfYear = seq(0, nrow(temps) - 1),
         operatingDay = as.Date(dayOfYear, origin = '2024-01-01')) %>%
  select(-dayOfYear)

ggplot(data = pivot_longer(temps, Min:Max, names_to = 'Temp_Extreme',
                           values_to = 'Temp'),
       aes(x = operatingDay,
           y = Temp,
           color = Temp_Extreme)) +
  geom_line() +
  scale_color_discrete(name = element_blank()) +
  ggtitle('San Antonio Daily Extreme Temps') +
  xlab('Date') +
  ylab('Temp [F]') +
  theme_solarized_2()

assert_that(
  temps %>% mutate(diff = Max - Min) %>% slice_min(diff) %>% pull(diff) >= 0
  )

## ERCOT ------------------------------------------------------------------

data.raw <- read_excel('D:/projects/ERCOT/data/CPS_demand_2024.xlsx')
# data.raw <- read.csv('D:/projects/ERCOT/data/00014836.np6-346-cd.act_sys_load_by_fzn.20250110.132511.csv')
colSums(is.na(data.raw))

fed.holidays <- as.Date(c(
  '2024-01-01',
  '2024-01-15',
  '2024-02-19',
  '2024-05-27',
  '2024-06-19',
  '2024-07-04',
  '2024-09-02',
  '2024-10-14',
  '2024-11-11',
  '2024-11-28',
  '2024-12-25'
  ))

data <- data.raw %>%
  mutate(operatingDay = mdy(operatingDay),
         dayOfWeek = weekdays(operatingDay),
         month = month(operatingDay, label = TRUE)) %>%
  arrange(operatingDay)

data <- data %>% 
  mutate(
    dayOfWeek = factor(dayOfWeek,
                       levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday',
                                  'Friday', 'Saturday', 'Sunday')),
    month = factor(month),
    weekend.flag = factor(
      case_when(dayOfWeek %in% c('Saturday', 'Sunday') ~ 'Weekend',
                TRUE ~ 'Weekday')),
    fed.holiday = case_when(operatingDay %in% fed.holidays ~ TRUE,
                            TRUE ~ FALSE)
    )

data <- data %>%
  mutate(
    weekend.or.holiday = case_when(
      weekend.flag == 'Weekend' ~ TRUE,
      fed.holiday == TRUE ~ TRUE,
      TRUE ~ FALSE
    )
  )

str(data)

data <- data %>%
  left_join(temps, by = 'operatingDay')

ggplot(data = data,
       aes(x = operatingDay,
           y = demand)) +
  geom_line() +
  ggtitle('CPS Daily Demand') +
  xlab('Date') +
  ylab('Energy [MWh]') +
  theme_solarized_2()

ggplot(data = data,
       aes(x = demand)) +
  geom_histogram(bins = 12, color = '#002b36') +
  ggtitle('CPS Daily Demand') +
  xlab('Energy [MWh]') +
  ylab('Count') +
  theme_solarized_2()

## Split Data -------------------------------------------------------------

# data.train <- data %>%
#   # filter(operatingDay < '2024-10-01')
#   filter(operatingDay < '2024-09-13')
# data.validate <- data %>%
#   # filter(operatingDay >= '2024-10-01',
#   #        operatingDay < '2024-12-01')
#   filter(operatingDay >= '2024-09-13',
#          operatingDay < '2024-11-07')
# data.test <- data %>%
#   # filter(operatingDay >= '2024-12-01')
#   filter(operatingDay >= '2024-11-07')

data.train <- data %>%
  slice_head(prop = 0.8)
data.v.te <- anti_join(data, data.train)
data.validate <- data.v.te %>%
  slice_head(prop = 0.5)
data.test <- anti_join(data.v.te, data.validate)

# data.train <- data %>%
#   slice_sample(prop = 0.7) %>%
#   mutate(set = 'train')
# data.v.te <- anti_join(data, data.train)
# data.validate <- data.v.te %>%
#   slice_sample(prop = 0.5) %>%
#   mutate(set = 'validate')
# data.test <- anti_join(data.v.te, data.validate) %>%
#   mutate(set = 'test')
# 
# data <- bind_rows(data.train, data.validate, data.test) %>%
#   arrange(operatingDay)

rm(data, data.raw, data.v.te, temps, fed.holidays)
save.image(file = 'D:/projects/ERCOT/data/datasets.RData')


