library(tidyverse)
library(readxl)
library(lubridate)
library(assertthat)
library(forecast)
library(ggthemes)

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
  ggtitle('San Antonio Daily High/Low Temps') +
  xlab('Date') +
  ylab('Temp [F]') +
  theme_solarized_2()

assert_that(
  temps %>% mutate(diff = Max - Min) %>% slice_min(diff) %>% pull(diff) >= 0
  )

## ERCOT ------------------------------------------------------------------

data.raw <- read.csv('D:/projects/ERCOT/data/00014836.np6-346-cd.act_sys_load_by_fzn.20250110.132511.csv')
colSums(is.na(data.raw))

data <- data.raw %>%
  mutate(operatingDay = ymd(operatingDay),
         hourEnding = as.integer(substr(hourEnding, start = 1, stop = 2)),
         dayOfWeek = weekdays(operatingDay),
         month = month(operatingDay, label = TRUE)) %>%
  filter(DSTFlag != TRUE) %>%
  arrange(operatingDay, hourEnding)

assert_that(max(abs(
  data$total - (data$north + data$south + data$west + data$houston))) <= 0.05)

data.daily <- data %>% 
  select(-c(north, west, houston, total, DSTFlag)) %>%
  filter(operatingDay <= '2024-12-31',
         operatingDay >= '2024-01-01') %>%
  mutate(
    dayOfWeek = factor(dayOfWeek,
                       levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday',
                                  'Friday', 'Saturday', 'Sunday')),
    month = factor(month),
    weekend.flag = factor(
      case_when(dayOfWeek %in% c('Saturday', 'Sunday') ~ 'Weekend',
                TRUE ~ 'Weekday'))
    ) %>%
  group_by(operatingDay, dayOfWeek, month, weekend.flag) %>%
  summarize(daily.load = sum(south))

data.daily <- data.daily %>%
  ungroup() %>%
  # mutate(dayOfYear = seq(1, nrow(data.daily))) %>%
  left_join(temps, by = 'operatingDay')
# %>%
#   mutate(Min.sq = Min^2,
#          Max.sq = Max^2)

ggplot(data = data.daily,
       aes(x = operatingDay,
           y = daily.load)) +
  geom_line() +
  ggtitle('System Load') +
  xlab('Date') +
  ylab('Daily Load') +
  theme_solarized_2()

ggplot(data = data.daily,
       aes(x = daily.load)) +
  geom_histogram(bins = 12) +
  xlab('Daily Load') +
  ylab('Count') +
  theme_solarized_2()

## Split Data -------------------------------------------------------------

data.train <- data.daily %>%
  filter(operatingDay < '2024-10-01')
data.validate <- data.daily %>%
  filter(operatingDay >= '2024-10-01',
         operatingDay < '2024-12-01')
data.test <- data.daily %>%
  filter(operatingDay >= '2024-12-01')

# Feature Work ------------------------------------------------------------

## Month ------------------------------------------------------------------

# Month can't be a factor if the test data is all a new month!
# ggplot(data = data.train,
#        aes(x = month,
#            y = daily.load)) +
#   geom_boxplot() +
#   xlab('Month') +
#   ylab('Load') +
#   theme_solarized_2()
# 
# ggplot(data = data.train,
#        aes(x = month,
#            y = daily.load)) +
#   geom_boxplot() +
#   geom_jitter() +
#   xlab('Month') +
#   ylab('Load') +
#   theme_solarized_2()
# 
# aov.month <- aov(daily.load ~ month,
#                  data = data.train)
# 
# summary(aov.month)
# # At least one mean is significantly different.
# 
# model.tables(x = aov.month,
#              type = 'means')
# 
# TukeyHSD(aov.month,
#          conf.level = 0.95)

## Day of Week ------------------------------------------------------------

ggplot(data = data.train,
       aes(x = dayOfWeek,
           y = daily.load)) +
  geom_boxplot() +
  xlab('Day of Week') +
  ylab('Daily Load [MW]') +
  theme_solarized_2()

ggplot(data = data.train,
       aes(x = dayOfWeek,
           y = daily.load)) +
  geom_boxplot() +
  geom_jitter() +
  xlab('Day of Week') +
  ylab('Daily Load [MW]') +
  theme_solarized_2()

dayOfWeek.cd <- cooks.distance(lm(daily.load ~ dayOfWeek,
                                  data = data.train))
dayOfWeek.cd.df <- data.frame(idx = seq(1, nrow(data.train)),
                              cd = dayOfWeek.cd)

ggplot(data = dayOfWeek.cd.df,
       aes(x = idx,
           y = cd)) +
  geom_point() +
  geom_hline(yintercept = 4 / nrow(data.train),
             color = 'red',
             linetype = 'dashed',
             size = 1) +
  xlab('Index') +
  ylab("Cook's Distance") +
  theme_solarized_2()
# Although some points are > 4/n, none are anywhere near the rule of thumb
# value of 1. They may be ignored. Perhaps perform ANOVA with and without the
# potential outliers and observe any differences.
# https://rpubs.com/DragonflyStats/Cooks-Distance

# ggplot(data = data.train,
#        aes(x = daily.load)) +
#   geom_histogram(bins = 18) +
#   xlab('Daily Load [MW]') +
#   ylab('Count') +
#   theme_solarized_2()

ggplot(data = data.train,
       aes(x = daily.load)) +
  geom_histogram(bins = 6, color = '#002b36') +
  facet_wrap(facets = vars(dayOfWeek)) +
  xlab('Daily Load [MW]') +
  ylab('Count') +
  theme_solarized_2()

ggplot(data = data.train,
       aes(sample = resid(lm(daily.load ~ dayOfWeek, data = data.train)))) +
  stat_qq() +
  stat_qq_line() +
  xlab('Theoretical') +
  ylab('Actual') +
  theme_solarized_2()
shapiro.test(residuals(lm(daily.load ~ dayOfWeek, data = data.train)))
# Not normal, so ANOVA is not appropriate. Must go with Kruskal-Wallis.

kruskal.test(daily.load ~ dayOfWeek, data = data.train)
# No significant differences.

## Weekend Flag -----------------------------------------------------------

ggplot(data = data.train,
       aes(x = weekend.flag,
           y = daily.load)) +
  geom_boxplot() +
  xlab('Day of Week') +
  ylab('Daily Load [MW]') +
  theme_solarized_2()

ggplot(data = data.train,
       aes(x = weekend.flag,
           y = daily.load)) +
  geom_boxplot() +
  geom_jitter() +
  xlab('Day of Week') +
  ylab('Daily Load [MW]') +
  theme_solarized_2()

weekend.flag.cd <- cooks.distance(lm(daily.load ~ weekend.flag,
                                     data = data.train))
weekend.flag.cd.df <- data.frame(idx = seq(1, nrow(data.train)),
                                 cd = weekend.flag.cd)

ggplot(data = weekend.flag.cd.df,
       aes(x = idx,
           y = cd)) +
  geom_point() +
  geom_hline(yintercept = 4 / nrow(data.train),
             color = 'red',
             linetype = 'dashed',
             size = 1) +
  xlab('Index') +
  ylab("Cook's Distance") +
  theme_solarized_2()
# Although some points are > 4/n, none are anywhere near the rule of thumb
# value of 1. They may be ignored. Perhaps perform ANOVA with and without the
# potential outliers and observe any differences.

ggplot(data = data.train,
       aes(x = daily.load)) +
  geom_histogram(bins = 12, color = '#002b36') +
  facet_grid(rows = vars(weekend.flag)) +
  xlab('Daily Load [MW]') +
  ylab('Count') +
  theme_solarized_2()

shapiro.test(residuals(lm(daily.load ~ weekend.flag, data = data.train)))

# shapiro.test(data.train %>% filter(weekend.flag == 'Weekday') %>% pull(daily.load))
# shapiro.test(data.train %>% filter(weekend.flag == 'Weekend') %>% pull(daily.load))
# https://www.geeksforgeeks.org/shapiro-wilk-test-in-r-programming/

ggplot(data = data.train,
       aes(sample = resid(lm(daily.load ~ weekend.flag, data = data.train)))) +
  stat_qq() +
  stat_qq_line() +
  xlab('Theoretical') +
  ylab('Actual') +
  theme_solarized_2()
# The samples are not normal, thus a t-test cannot be used.
# https://www.statology.org/t-test-assumptions/

wilcox.test(daily.load ~ weekend.flag, data = data.train)
# No significant differences.
# https://www.r-tutor.com/elementary-statistics/non-parametric-methods/mann-whitney-wilcoxon-test

## Temperature ------------------------------------------------------------

ggplot(data = data.train,
       aes(x = Min,
           y = daily.load)) +
  geom_point() +
  xlab('Min Temp [F]') +
  ylab('Daily Load [MW]') +
  theme_solarized_2()

ggplot(data = data.train,
       aes(x = Max,
           y = daily.load)) +
  geom_point() +
  xlab('Max Temp [F]') +
  ylab('Daily Load [MW]') +
  theme_solarized_2()

ggplot(data = data.train,
       aes(x = Min)) +
  geom_histogram(bins = 16) +
  xlab('Min Temp [F]') +
  ylab('Daily Load [MW]') +
  theme_solarized_2()

ggplot(data = data.train,
       aes(x = Max)) +
  geom_histogram(bins = 16) +
  xlab('Max Temp [F]') +
  ylab('Daily Load [MW]') +
  theme_solarized_2()

# Build Models ------------------------------------------------------------

## Model 1: Min Temp ------------------------------------------------------

m1 <- lm(daily.load ~ Min + I(Min^2),
         data = data.train)
summary(m1)

ggplot(data = data.train,
       aes(x = Min)) +
  geom_point(aes(y = daily.load)) +
  geom_line(aes(y = m1$fitted.values), color = '#dc322f', size = 1) +
  xlab('Min Temp [F]') +
  ylab('Daily Load [MW]') +
  theme_solarized_2()

### Check Assumptions -----------------------------------------------------

# Outliers
m1.cd <- cooks.distance(m1)
m1.cd.df <- data.frame(idx = seq(1, nrow(data.train)),
                       cd = m1.cd)

ggplot(data = m1.cd.df,
       aes(x = idx,
           y = cd)) +
  geom_point() +
  geom_hline(yintercept = 4 / nrow(data.train),
             color = 'red',
             linetype = 'dashed',
             size = 1) +
  xlab('Index') +
  ylab("Cook's Distance") +
  theme_solarized_2()

# Linearity, constant variance & independence
ggplot() +
  geom_point(aes(x = m1$fitted.values,
                 y = m1$residuals)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#dc322f') +
  xlab('Fitted Values [MW]') +
  ylab('Residuals [MW]') +
  theme_solarized_2()
# Constant variance and linearity holds. There may be deviations from independence at lower
# fitted values.

# Normality
ggplot() +
  geom_histogram(aes(x = m1$residuals), bins = 18, color = '#002b36') +
  xlab('Residuals [MW]') +
  ylab('Count') +
  theme_solarized_2()

ggplot() +
  stat_qq(aes(sample = m1$residuals)) +
  stat_qq_line(aes(sample = m1$residuals)) +
  xlab('Theoretical') +
  ylab('Actual') +
  theme_solarized_2()
# Normality holds.

## Model 2: Max Temp ------------------------------------------------------

m2 <- lm(daily.load ~ Max + I(Max^2),
         data = data.train)
summary(m2)

# data.train <- data.train %>%
#   mutate(fitted2 = m2$fitted.values)

ggplot(data = data.train,
       aes(x = Max)) +
  geom_point(aes(y = daily.load)) +
  geom_line(aes(y = m2$fitted.values), color = '#dc322f', size = 1) +
  xlab('Max Temp [F]') +
  ylab('Daily Load [MW]') +
  theme_solarized_2()

# data.test <- data.test %>%
#   mutate(pred2 = predict(m2, newdata = data.test))

### Check Assumptions -----------------------------------------------------

# Outliers
m2.cd <- cooks.distance(m2)
m2.cd.df <- data.frame(idx = seq(1, nrow(data.train)),
                       cd = m2.cd)

ggplot(data = m2.cd.df,
       aes(x = idx,
           y = cd)) +
  geom_point() +
  geom_hline(yintercept = 4 / nrow(data.train),
             color = 'red',
             linetype = 'dashed',
             size = 1) +
  xlab('Index') +
  ylab("Cook's Distance") +
  theme_solarized_2()

# Linearity, constant variance & independence
ggplot() +
  geom_point(aes(x = m2$fitted.values,
                 y = m2$residuals)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#dc322f') +
  xlab('Fitted Values [MW]') +
  ylab('Residuals [MW]') +
  theme_solarized_2()
# Constant variance and linearity holds. There may be deviations from independence at lower
# fitted values.

# Normality
ggplot() +
  geom_histogram(aes(x = m2$residuals), bins = 18, color = '#002b36') +
  xlab('Residuals [MW]') +
  ylab('Count') +
  theme_solarized_2()

ggplot() +
  stat_qq(aes(sample = m2$residuals)) +
  stat_qq_line(aes(sample = m2$residuals)) +
  xlab('Theoretical') +
  ylab('Actual') +
  theme_solarized_2()
# Normality holds.

# Select Model & Re-Train -------------------------------------------------

data.validate <- data.validate %>%
  mutate(pred1 = predict(m1, newdata = data.validate),
         pred2 = predict(m2, newdata = data.validate))

mae1 <- mean(abs(data.validate$daily.load - data.validate$pred1))
mae2 <- mean(abs(data.validate$daily.load - data.validate$pred2))
mse1 <- mean((data.validate$daily.load - data.validate$pred1)^2)
mse2 <- mean((data.validate$daily.load - data.validate$pred2)^2)
rmse1 <- sqrt(mse1)
rmse2 <- sqrt(mse2)

data.train.full <- bind_rows(data.train, data.validate) %>%
  select(-c(pred1, pred2))

m <- lm(daily.load ~ Max + I(Max^2),
        data = data.train.full)
summary(m)

ggplot(data = data.train.full,
       aes(x = Max)) +
  geom_point(aes(y = daily.load)) +
  geom_line(aes(y = m$fitted.values), color = '#dc322f', size = 1) +
  xlab('Max Temp [F]') +
  ylab('Daily Load [MW]') +
  theme_solarized_2()

min.temp.pred <- -m$coefficients[2] / (2 * m$coefficients[3])

# Predict -----------------------------------------------------------------

data.test <- data.test %>%
  mutate(pred = predict(m, newdata = data.test),
         err = pred - daily.load)

mae <- mean(abs(data.test$daily.load - data.test$pred))
mse <- mean((data.test$daily.load - data.test$pred)^2)
rmse <- sqrt(mse)
total.err <- sum(data.test$daily.load - data.test$pred)
sum(data.test$daily.load)
sum(data.test$pred)

data.test %>% summarize(count.over = sum(pred > daily.load),
                        count.under = sum(pred < daily.load))








