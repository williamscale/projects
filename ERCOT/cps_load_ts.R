# library(dynlm)
library(tidyverse)
library(ggthemes)
library(tseries)
library(forecast)

set.seed(55)

load('D:/projects/ERCOT/data/datasets.RData')

ggplot(data = data.train,
       aes(x = operatingDay,
           y = demand)) +
  geom_line() +
  ggtitle('CPS Daily Demand: Training Set') +
  xlab('Date') +
  ylab('Energy [MWh]') +
  theme_solarized_2()

# data.train <- bind_rows(data.train, data.validate)

acf(data.train$demand)
# acf.df <- data.frame(
#   lag = acf(data.train$demand, plot = FALSE)$lag,
#   acf = acf(data.train$demand, plot = FALSE)$acf
# )
# ggplot(data = acf.df,
#        aes(x = lag,
#            y = acf)) +
#   geom_segment(aes(xend = lag,
#                    yend = 0)) +
#   geom_hline(yintercept = 0) +
#   xlab('Lag') +
#   ylab('ACF') +
#   theme_solarized_2()
adf.test(data.train$demand, k = 0)
# Non-stationary!

acf(diff(data.train$demand, lag = 1, differences = 1))
adf.test(diff(data.train$demand, lag = 1, differences = 1))
adf.test(diff(data.train$demand, lag = 1, differences = 1), k = 0)
adf.test(data.train$demand, k = 1)
# Stationary!
# d = 1

data.train$set <- 'train'
data.validate$set <- 'validate'
data.test$set <- 'test'

data.full <- rbind(data.train, data.validate, data.test) %>%
  arrange(operatingDay)

data.full <- data.full %>%
  mutate(demand.diff.1 = c(NA, diff(demand, lag = 1, differences = 1)))

# data.full <- data.full %>%
#   mutate(
#     demand.lag.1 = lag(demand, 1),
#     demand.lag.2 = lag(demand, 2),
#     demand.lag.3 = lag(demand, 3),
#     demand.lag.4 = lag(demand, 4),
#     demand.lag.5 = lag(demand, 5),
#     demand.lag.6 = lag(demand, 6),
#     demand.lag.7 = lag(demand, 7),
#     Min.lag.1 = lag(Min, 1),
#     Min.lag.2 = lag(Min, 2),
#     Min.lag.3 = lag(Min, 3),
#     Min.lag.4 = lag(Min, 4),
#     Min.lag.5 = lag(Min, 5),
#     Min.lag.6 = lag(Min, 6),
#     Min.lag.7 = lag(Min, 7),
#     Max.lag.1 = lag(Max, 1),
#     Max.lag.2 = lag(Max, 2),
#     Max.lag.3 = lag(Max, 3),
#     Max.lag.4 = lag(Max, 4),
#     Max.lag.5 = lag(Max, 5),
#     Max.lag.6 = lag(Max, 6),
#     Max.lag.7 = lag(Max, 7)
#     )

data.train <- data.full %>%
  filter(set == 'train') %>%
  select(-set)
data.validate <- data.full %>%
  filter(set == 'validate') %>%
  select(-set)
data.test <- data.full %>%
  filter(set == 'test') %>%
  select(-set)

# data.train.decomp <- decompose(ts(data.train$demand, frequency = 7))
# plot(data.train.decomp)
# 
# data.train.decomp <- stl(ts(data.train$demand, frequency = 7),
#                          s.window = 'periodic')
# plot(data.train.decomp)
# 
# data.train.decomp.adj <- seasadj(data.train.decomp)
# plot(data.train.decomp.adj)
# acf(data.train.decomp.adj)

ggplot(data = data.train,
       aes(x = operatingDay,
           y = demand.diff.1)) +
  geom_line() +
  ggtitle('CPS Daily Demand Lag') +
  xlab('Date') +
  ylab('Energy [MWh]') +
  theme_solarized_2()

acf(data.train$demand.diff.1[-1])
pacf(data.train$demand.diff.1[-1])
# p = 0, d = 1, q = 4
# See 8.5 in Forecasting textbook.

# Build Models ------------------------------------------------------------

## Model 1: ARIMA(0, 1, 4) ------------------------------------------------

m1 <- Arima(data.train$demand, order = c(0, 1, 4))
m1

## Model 2: ARIMA(0, 1, 5) ------------------------------------------------

m2 <- Arima(data.train$demand, order = c(0, 1, 5))
m2

## Model 3: ARIMA(0, 1, 3) ------------------------------------------------

m3 <- Arima(data.train$demand, order = c(0, 1, 3))
m3

## Model 4: ARIMA(1, 1, 4) ------------------------------------------------

m4 <- Arima(data.train$demand, order = c(1, 1, 4))
m4

## Model 5: auto.arima() ---------------------------------------------------

m5 <- auto.arima(data.train$demand)
m5
m5 <- auto.arima(data.train$demand, stepwise = FALSE, approximation = FALSE)
m5
# Note a different model is selected.
# Same as model 1 so model 5 is not necessary.

## Model 6: ARIMAX(0, 1, 4) w/ Min Temp -----------------------------------

m6 <- Arima(data.train$demand, order = c(0, 1, 4), xreg = data.train$Min)
m6

## Model 7: ARIMAX(0, 1, 4) w/ Max Temp -----------------------------------

m7 <- Arima(data.train$demand, order = c(0, 1, 4), xreg = data.train$Max)
m7

m1$aicc
m2$aicc
m3$aicc
m4$aicc
m6$aicc
m7$aicc

checkresiduals(m1)
checkresiduals(m2)
checkresiduals(m3)
checkresiduals(m4)
checkresiduals(m6)
checkresiduals(m7)

autoplot(forecast(m1))
autoplot(forecast(m2))
autoplot(forecast(m3))
autoplot(forecast(m4))
autoplot(forecast(m6, xreg = data.validate$Min))
autoplot(forecast(m7, xreg = data.validate$Max))

# Select Model & Re-Train -------------------------------------------------

data.validate <- data.validate %>%
  mutate(pred1 = forecast(m1, h = nrow(data.validate))$mean,
         pred2 = forecast(m2, h = nrow(data.validate))$mean,
         pred3 = forecast(m3, h = nrow(data.validate))$mean,
         pred4 = forecast(m4, h = nrow(data.validate))$mean,
         pred6 = forecast(m6, xreg = data.validate$Min, h = nrow(data.validate))$mean,
         pred7 = forecast(m7, xreg = data.validate$Max, h = nrow(data.validate))$mean
         )

saveRDS(
  data.validate,
  file = 'D:/projects/ERCOT/data/predictions_val_ts.RDS'
)




mae1 <- mean(abs(data.validate$demand - data.validate$pred1))
mae2 <- mean(abs(data.validate$demand - data.validate$pred2))
mae3 <- mean(abs(data.validate$demand - data.validate$pred3))
mae4 <- mean(abs(data.validate$demand - data.validate$pred4))
mae6 <- mean(abs(data.validate$demand - data.validate$pred6))
mae7 <- mean(abs(data.validate$demand - data.validate$pred7))

rmse1 <- sqrt(mean((data.validate$demand - data.validate$pred1)^2))
rmse2 <- sqrt(mean((data.validate$demand - data.validate$pred2)^2))
rmse3 <- sqrt(mean((data.validate$demand - data.validate$pred3)^2))
rmse4 <- sqrt(mean((data.validate$demand - data.validate$pred4)^2))
rmse6 <- sqrt(mean((data.validate$demand - data.validate$pred6)^2))
rmse7 <- sqrt(mean((data.validate$demand - data.validate$pred7)^2))

mape1 <- sum(abs((data.validate$demand - data.validate$pred1) / data.validate$demand)) / nrow(data.validate)
mape2 <- sum(abs((data.validate$demand - data.validate$pred2) / data.validate$demand)) / nrow(data.validate)
mape3 <- sum(abs((data.validate$demand - data.validate$pred3) / data.validate$demand)) / nrow(data.validate)
mape4 <- sum(abs((data.validate$demand - data.validate$pred4) / data.validate$demand)) / nrow(data.validate)
mape6 <- sum(abs((data.validate$demand - data.validate$pred6) / data.validate$demand)) / nrow(data.validate)
mape7 <- sum(abs((data.validate$demand - data.validate$pred7) / data.validate$demand)) / nrow(data.validate)

data.train.full <- bind_rows(data.train, data.validate) %>%
  select(-c(pred1, pred2, pred3, pred4, pred6, pred7))

m.arima <- Arima(data.train.full$demand,
                 order = c(0, 1, 3))
m.arimax <- Arima(data.train.full$demand,
                  order = c(0, 1, 4),
                  xreg = data.train.full$Min)
m.arima
m.arimax

checkresiduals(m.arima)
# p-value suggests residuals are white noise
checkresiduals(m.arimax)
# p-value suggests residuals are white noise

# Predict -----------------------------------------------------------------

data.test <- data.test %>%
  mutate(pred.arima = forecast(m.arima, h = nrow(data.test))$mean,
         pred.arimax = forecast(m.arimax, xreg = data.test$Min, h = nrow(data.test))$mean,
         err.arima = pred.arima - demand,
         err.arimax = pred.arimax - demand,
         err.pct.arima = err.arima / demand,
         err.pct.arimax = err.arimax / demand)

mae <- mean(abs(data.test$demand - data.test$pred))
rmse <- sqrt(mean((data.test$demand - data.test$pred)^2))
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
  file = 'D:/projects/ERCOT/data/predictions_ts.RDS'
)

