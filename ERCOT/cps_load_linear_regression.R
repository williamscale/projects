library(tidyverse)
library(ggthemes)
library(rstanarm)
library(bayesplot)
library(bayestestR)

set.seed(55)

load('D:/projects/ERCOT/data/datasets.RData')

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
           y = demand)) +
  geom_boxplot() +
  xlab('Day of Week') +
  ylab('Demand [MWh]') +
  theme_solarized_2()

ggplot(data = data.train,
       aes(x = dayOfWeek,
           y = demand)) +
  geom_boxplot() +
  geom_jitter() +
  xlab('Day of Week') +
  ylab('Demand [MWh]') +
  theme_solarized_2()

dayOfWeek.cd <- cooks.distance(lm(demand ~ dayOfWeek,
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

ggplot(data = data.train,
       aes(x = demand)) +
  geom_histogram(bins = 6, color = '#002b36') +
  facet_wrap(facets = vars(dayOfWeek)) +
  xlab('Demand [MWh]') +
  ylab('Count') +
  theme_solarized_2()

ggplot(data = data.train,
       aes(sample = resid(lm(demand ~ dayOfWeek, data = data.train)))) +
  stat_qq() +
  stat_qq_line() +
  xlab('Theoretical') +
  ylab('Actual') +
  theme_solarized_2()
shapiro.test(residuals(lm(demand ~ dayOfWeek, data = data.train)))
# Not normal, so ANOVA is not appropriate. Must go with Kruskal-Wallis.

kruskal.test(demand ~ dayOfWeek, data = data.train)
# No significant differences.

## Weekend Flag -----------------------------------------------------------

ggplot(data = data.train,
       aes(x = weekend.flag,
           y = demand)) +
  geom_boxplot() +
  xlab('Day of Week') +
  ylab('Demand [MWh]') +
  theme_solarized_2()

ggplot(data = data.train,
       aes(x = weekend.flag,
           y = demand)) +
  geom_boxplot() +
  geom_jitter() +
  xlab('Day of Week') +
  ylab('Demand [MWh]') +
  theme_solarized_2()

weekend.flag.cd <- cooks.distance(lm(demand ~ weekend.flag,
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
       aes(x = demand)) +
  geom_histogram(bins = 12, color = '#002b36') +
  facet_grid(rows = vars(weekend.flag)) +
  xlab('Demand [MWh]') +
  ylab('Count') +
  theme_solarized_2()

shapiro.test(residuals(lm(demand ~ weekend.flag, data = data.train)))

# shapiro.test(data.train %>% filter(weekend.flag == 'Weekday') %>% pull(daily.load))
# shapiro.test(data.train %>% filter(weekend.flag == 'Weekend') %>% pull(daily.load))
# https://www.geeksforgeeks.org/shapiro-wilk-test-in-r-programming/

ggplot(data = data.train,
       aes(sample = resid(lm(demand ~ weekend.flag, data = data.train)))) +
  stat_qq() +
  stat_qq_line() +
  xlab('Theoretical') +
  ylab('Actual') +
  theme_solarized_2()
# The samples are not normal, thus a t-test cannot be used.
# https://www.statology.org/t-test-assumptions/

wilcox.test(demand ~ weekend.flag, data = data.train)
# No significant differences.
# https://www.r-tutor.com/elementary-statistics/non-parametric-methods/mann-whitney-wilcoxon-test

## Weekend or Holiday -----------------------------------------------------

ggplot(data = data.train,
       aes(x = weekend.or.holiday,
           y = demand)) +
  geom_boxplot() +
  xlab('Weekend or Holiday') +
  ylab('Demand [MWh]') +
  theme_solarized_2()

ggplot(data = data.train,
       aes(x = weekend.or.holiday,
           y = demand)) +
  geom_boxplot() +
  geom_jitter() +
  xlab('Weekend or Holiday') +
  ylab('Demand [MWh]') +
  theme_solarized_2()

weekendholiday.cd <- cooks.distance(lm(demand ~ weekend.or.holiday,
                                       data = data.train))
weekendholiday.cd.df <- data.frame(idx = seq(1, nrow(data.train)),
                                   cd = weekendholiday.cd)

ggplot(data = weekendholiday.cd.df,
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
       aes(x = demand)) +
  geom_histogram(bins = 12, color = '#002b36') +
  facet_grid(rows = vars(weekend.or.holiday)) +
  xlab('Demand [MWh]') +
  ylab('Count') +
  theme_solarized_2()

shapiro.test(residuals(lm(demand ~ weekend.or.holiday, data = data.train)))

# shapiro.test(data.train %>% filter(weekend.flag == 'Weekday') %>% pull(daily.load))
# shapiro.test(data.train %>% filter(weekend.flag == 'Weekend') %>% pull(daily.load))
# https://www.geeksforgeeks.org/shapiro-wilk-test-in-r-programming/

ggplot(data = data.train,
       aes(sample = resid(lm(demand ~ weekend.or.holiday, data = data.train)))) +
  stat_qq() +
  stat_qq_line() +
  xlab('Theoretical') +
  ylab('Actual') +
  theme_solarized_2()
# The samples are not normal, thus a t-test cannot be used.
# https://www.statology.org/t-test-assumptions/

wilcox.test(demand ~ weekend.or.holiday, data = data.train)
# No significant differences.
# https://www.r-tutor.com/elementary-statistics/non-parametric-methods/mann-whitney-wilcoxon-test

## Temperature ------------------------------------------------------------

ggplot(data = data.train,
       aes(x = Min,
           y = demand)) +
  geom_point() +
  xlab('Min Temp [F]') +
  ylab('Demand [MWh]') +
  theme_solarized_2()

ggplot(data = data.train,
       aes(x = Min)) +
  geom_histogram(bins = 16, color = '#002b36') +
  xlab('Min Temp [F]') +
  ylab('Demand [MWh]') +
  theme_solarized_2()

ggplot(data = data.train,
       aes(x = Max,
           y = demand)) +
  geom_point() +
  xlab('Max Temp [F]') +
  ylab('Demand [MWh]') +
  theme_solarized_2()

ggplot(data = data.train,
       aes(x = Max)) +
  geom_histogram(bins = 16, color = '#002b36') +
  xlab('Max Temp [F]') +
  ylab('Demand [MWh]') +
  theme_solarized_2()

min(data.train$Min)
max(data.train$Min)
mean(data.train$Min)
median(data.train$Min)
sd(data.train$Min)

min(data.train$Max)
max(data.train$Max)
mean(data.train$Max)
median(data.train$Max)
sd(data.train$Max)

ggplot(data = data.train,
       aes(x = Min,
           y = Max)) +
  geom_point() +
  xlab('Min Temp [F]') +
  ylab('Max Temp [F]') +
  theme_solarized_2()

cor(data.train$Min, data.train$Max, method = 'pearson')

# Build Models ------------------------------------------------------------

## Model 1: Min Temp ------------------------------------------------------

m1 <- lm(demand ~ Min + I(Min^2),
         data = data.train)
summary(m1)

ggplot(data = data.train,
       aes(x = Min)) +
  geom_point(aes(y = demand)) +
  geom_line(aes(y = m1$fitted.values), color = '#dc322f', size = 1) +
  xlab('Min Temp [F]') +
  ylab('Demand [MWh]') +
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
  xlab('Fitted Values [MWh]') +
  ylab('Residuals [MWh]') +
  theme_solarized_2()
# Constant variance and linearity holds. There may be deviations from independence at lower
# fitted values.

# Normality
ggplot() +
  geom_histogram(aes(x = m1$residuals), bins = 18, color = '#002b36') +
  xlab('Residuals [MWh]') +
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

m2 <- lm(demand ~ Max + I(Max^2),
         data = data.train)
summary(m2)

ggplot(data = data.train,
       aes(x = Max)) +
  geom_point(aes(y = demand)) +
  geom_line(aes(y = m2$fitted.values), color = '#dc322f', size = 1) +
  xlab('Max Temp [F]') +
  ylab('Demand [MWh]') +
  theme_solarized_2()

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
  xlab('Fitted Values [MWh]') +
  ylab('Residuals [MWh]') +
  theme_solarized_2()
# Constant variance and linearity holds. There may be deviations from independence at lower
# fitted values.

# Normality
ggplot() +
  geom_histogram(aes(x = m2$residuals), bins = 18, color = '#002b36') +
  xlab('Residuals [MWh]') +
  ylab('Count') +
  theme_solarized_2()

ggplot() +
  stat_qq(aes(sample = m2$residuals)) +
  stat_qq_line(aes(sample = m2$residuals)) +
  xlab('Theoretical') +
  ylab('Actual') +
  theme_solarized_2()
# Normality holds.

## Model 3: Min Temp - Bayesian -------------------------------------------

m3 <- stan_glm(demand ~ Min + I(Min^2),
               data = data.train)
m3

mcmc_dens(m3)
describe_posterior(m3)
hdi(m3)

## Model 4: Max Temp - Bayesian -------------------------------------------

m4 <- stan_glm(demand ~ Max + I(Max^2),
               data = data.train)
m4

mcmc_dens(m4)
describe_posterior(m4)
hdi(m4)

# Select Model & Re-Train -------------------------------------------------

data.validate <- data.validate %>%
  mutate(pred1 = predict(m1, newdata = data.validate),
         pred2 = predict(m2, newdata = data.validate),
         # pred3 = predict(m3, newdata = data.validate),
         # pred4 = predict(m4, newdata = data.validate)
         )

saveRDS(
  data.validate,
  file = 'D:/projects/ERCOT/data/predictions_val_linear_regression.RDS'
)



mae1 <- mean(abs(data.validate$demand - data.validate$pred1))
mae2 <- mean(abs(data.validate$demand - data.validate$pred2))
mae3 <- mean(abs(data.validate$demand - data.validate$pred3))
mae4 <- mean(abs(data.validate$demand - data.validate$pred4))

rmse1 <- sqrt(mean((data.validate$demand - data.validate$pred1)^2))
rmse2 <- sqrt(mean((data.validate$demand - data.validate$pred2)^2))
rmse3 <- sqrt(mean((data.validate$demand - data.validate$pred3)^2))
rmse4 <- sqrt(mean((data.validate$demand - data.validate$pred4)^2))

mape1 <- sum(abs((data.validate$demand - data.validate$pred1) / data.validate$demand)) / nrow(data.validate)
mape2 <- sum(abs((data.validate$demand - data.validate$pred2) / data.validate$demand)) / nrow(data.validate)
mape3 <- sum(abs((data.validate$demand - data.validate$pred3) / data.validate$demand)) / nrow(data.validate)
mape4 <- sum(abs((data.validate$demand - data.validate$pred4) / data.validate$demand)) / nrow(data.validate)

data.train.full <- bind_rows(data.train, data.validate) %>%
  select(-c(pred1, pred2, pred3, pred4))

# m <- stan_glm(demand ~ Max + I(Max^2),
#               data = data.train.full)
# m
m <- lm(demand ~ Max + I(Max^2),
        data = data.train.full)
summary(m)

# mcmc_dens(m)
# describe_posterior(m)
# hdi(m)

ggplot(data = data.train.full,
       aes(x = Max)) +
  geom_point(aes(y = demand)) +
  geom_line(aes(y = m$fitted.values), color = '#dc322f', size = 1) +
  xlab('Max Temp [F]') +
  ylab('Demand [MW]') +
  theme_solarized_2()
# 
# min.temp.pred <- -m$coefficients[2] / (2 * m$coefficients[3])

# Predict -----------------------------------------------------------------

data.test <- data.test %>%
  mutate(pred = predict(m, newdata = data.test),
         err = pred - demand,
         err.pct = err / demand)

mae <- mean(abs(data.test$demand - data.test$pred))
mse <- sqrt(mean((data.test$demand - data.test$pred)^2))
total.err <- sum(data.test$demand - data.test$pred)
mape <- sum(abs(data.test$err.pct)) / nrow(data.test)
sum(data.test$demand)
sum(data.test$pred)

data.test %>% summarize(count.over = sum(pred > demand),
                        count.under = sum(pred < demand))

ggplot(data = data.test,
       aes(x = err.pct)) +
  geom_histogram(bins = 8, color = '#002b36') +
  xlab('Error %') +
  ylab('Count') +
  theme_solarized_2()

ggplot(data = data.test,
       aes(x = operatingDay)) +
  geom_line(aes(y = demand), color = '#268bd2') +
  geom_line(aes(y = pred), color = '#cb4b16') +
  xlab('Day of Year') +
  ylab('Energy') +
  theme_solarized_2()

saveRDS(
  data.test,
  file = 'D:/projects/ERCOT/data/predictions_linear_regression.RDS'
  )
