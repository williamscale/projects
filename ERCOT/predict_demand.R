library(tidyverse)
library(ggthemes)

load('D:/projects/ERCOT/data/datasets.RData')

data.folder <- 'D:/projects/ERCOT/data/'
pred.reg.raw <- readRDS(paste0(data.folder, 'predictions_val_linear_regression.RDS'))
pred.ts.raw <- readRDS(paste0(data.folder, 'predictions_val_ts.RDS'))

data.validate <- data.validate %>%
  mutate(pred11 = pred.reg.raw$pred1,
         pred12 = pred.reg.raw$pred2,
         pred21 = pred.ts.raw$pred1,
         pred22 = pred.ts.raw$pred2,
         pred23 = pred.ts.raw$pred3,
         pred24 = pred.ts.raw$pred4,
         pred31 = pred.ts.raw$pred6,
         pred32 = pred.ts.raw$pred7)

rmse11 <- sqrt(mean((data.validate$demand - data.validate$pred11)^2))
rmse12 <- sqrt(mean((data.validate$demand - data.validate$pred12)^2))
rmse21 <- sqrt(mean((data.validate$demand - data.validate$pred21)^2))
rmse22 <- sqrt(mean((data.validate$demand - data.validate$pred22)^2))
rmse23 <- sqrt(mean((data.validate$demand - data.validate$pred23)^2))
rmse24 <- sqrt(mean((data.validate$demand - data.validate$pred24)^2))
rmse31 <- sqrt(mean((data.validate$demand - data.validate$pred31)^2))
rmse32 <- sqrt(mean((data.validate$demand - data.validate$pred32)^2))

data.train.full <- bind_rows(data.train, data.validate) %>%
  select(-c(pred11, pred12, pred21, pred22, pred23, pred24, pred31, pred32))

m <- lm(demand ~ Max + I(Max^2),
        data = data.train.full)
summary(m)

ggplot(data = data.train.full,
       aes(x = Max)) +
  geom_point(aes(y = demand)) +
  geom_line(aes(y = m$fitted.values), color = '#dc322f', size = 1) +
  xlab('Max Temp [F]') +
  ylab('Demand [MWh]') +
  theme_solarized_2()

data.test <- data.test %>%
  mutate(pred = predict(m, newdata = data.test),
         err = pred - demand,
         err.pct = err / demand)

mae <- mean(abs(data.test$demand - data.test$pred))
rmse <- sqrt(mean((data.test$demand - data.test$pred)^2))
total.err <- sum(data.test$demand - data.test$pred)
mape <- sum(abs(data.test$err.pct)) / nrow(data.test)
sum(data.test$demand)
sum(data.test$pred)
sum(data.test$pred) - sum(data.test$demand)

data.test %>% summarize(count.over = sum(pred > demand),
                        count.under = sum(pred < demand))

ggplot(data = data.test,
       aes(x = err.pct * 100)) +
  geom_histogram(bins = 10, color = '#002b36') +
  xlab('Error %') +
  ylab('Count') +
  theme_solarized_2()

ggplot(data = data.test,
       aes(x = operatingDay,
           y = err.pct * 100)) +
  geom_col() +
  xlab('Date') +
  ylab('Error %') +
  theme_solarized_2()

ggplot(data = data.test %>%
         select(operatingDay, demand, pred) %>%
         pivot_longer(cols = demand:pred, names_to = 'type', values_to = 'energy'),
       aes(x = operatingDay,
           y = energy,
           color = type)) +
  geom_line(size = 1) +
  scale_color_discrete(labels = c('Demand', 'Prediction'),
                       name = NULL) +
  xlab('Date') +
  ylab('Energy [MWh]') +
  theme_solarized_2()

saveRDS(
  data.test,
  file = 'D:/projects/ERCOT/data/predictions.RDS'
)
