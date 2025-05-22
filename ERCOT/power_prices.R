library(tidyverse)
library(lubridate)
library(EnvStats)
library(ggthemes)
library(forcats)

set.seed(55)

data.folder <- 'D:/projects/ERCOT/data/'
pred.raw <- readRDS(paste0(data.folder, 'predictions.RDS'))

data <- pred.raw %>%
  select(operatingDay, demand, pred)

# data <- pred.reg %>%
#   rename('pred1' = 'pred') %>%
#   left_join(pred.ts, by = c('operatingDay', 'demand')) %>%
#   rename('pred2' = 'pred.arima',
#          'pred3' = 'pred.arimax') 
# %>%
#   mutate(err.reg = pred.reg - demand,
#          err.ts = pred.ts - demand,
#          err.pct.reg = err.reg / demand,
#          err.pct.ts = err.ts / demand)

rm(pred.raw, data.folder)



# ggplot(data = data %>% pivot_longer(cols = err.reg:err.ts,
#                                     names_to = 'type', values_to = 'err'),
#        aes(x = operatingDay,
#            y = err,
#            fill = type)) +
#   geom_col(position = 'dodge') +
#   scale_fill_discrete(name = NULL, labels = c('Regression', 'ARIMAX')) +
#   xlab('Day') +
#   ylab('Error') +
#   theme_solarized_2()

# cumulative err plot

# https://www.cpsenergy.com/content/dam/corporate/en/Documents/RAC/RAC%20Generation%20Utilization%20Update%20V8.pdf
reserve.margin <- 0.1375

data <- data %>%
  mutate(generate = pred * (1 + reserve.margin),
         # generate2 = pred2 * (1 + reserve.margin),
         # generate3 = pred3 * (1 + reserve.margin),
         residential.prop = rnorm(n = n(), mean = 0.5, sd = 0.02),
         commercial.prop = 1 - residential.prop)

# ggplot(data = data,
#        aes(x = operatingDay,
#            y = ))

# non-peak prices
# per kWh
# https://www.cpsenergy.com/content/dam/corporate/en/Documents/2024_Rate_GeneralService.pdf
# https://www.cpsenergy.com/content/dam/corporate/en/Documents/2024_Rate_ResidentialElectric.pdf

cps.residential.rate <- 0.07503 * 1000
cps.commercial.rate <- 0.07817 * 1000
non.cps.rate.mean <- 100

# need to adjust operation costs to be on same magnitude as rates
# operation.cost [$/MWh]
cps.plants <- list(
  baseload = data.frame(
    operatingDay = data$operatingDay,
    operating.cost = 50,
    capacity = 35000,
    priority = 1
    ),
  intermediate = data.frame(
    operatingDay = data$operatingDay,
    operating.cost = 56,
    capacity = 20000,
    priority = 2
    ),
  peaking = data.frame(
    operatingDay = data$operatingDay,
    operating.cost = 70,
    capacity = 10000,
    priority = 5
    ),
  wind = data.frame(
    operatingDay = data$operatingDay,
    operating.cost = 45,
    capacity = rnormTrunc(n = nrow(data), mean = 8000, sd = 1000, min = 0,
                          max = 10000),
    priority = 3
    ),
  solar = data.frame(
    operatingDay = data$operatingDay,
    operating.cost = 45,
    capacity = rnormTrunc(n = nrow(data), mean = 8000, sd = 1000, max = 10000),
    priority = 3
    )
  )
cps.plants <- bind_rows(cps.plants, .id = 'type')

cps.plants <- cps.plants %>%
  arrange(operatingDay, priority, desc(capacity)) %>%
  left_join(data %>% select(operatingDay, generate),
            by = 'operatingDay') %>%
  group_by(operatingDay) %>%
  mutate(cumsum.capacity = cumsum(capacity)) %>%
  mutate(
    source = case_when(
      cumsum.capacity <= generate ~ capacity,
      (generate <= cumsum.capacity & generate >= lag(cumsum.capacity, n = 1)) ~ generate - lag(cumsum.capacity, n = 1),
      TRUE ~ 0)
    ) %>%
  # ungroup() %>%
  mutate(operating.cost = -operating.cost * source,
         type = factor(type,
                       levels = c('baseload', 'intermediate', 'wind', 'solar', 'peaking')))

ggplot(data = cps.plants,
       aes(x = operatingDay,
           y = capacity,
           fill = fct_rev(type))) +
  geom_area() +
  xlab('Date') +
  ylab('Capacity [MWh]') +
  scale_fill_discrete(name = 'Source') +
  theme_solarized_2()

cps.plants.daily.cost <- cps.plants %>%
  # group_by(operatingDay) %>%
  summarize(operating.cost = sum(operating.cost)) %>%
  left_join(data %>% select(-pred), by = 'operatingDay') %>%
  mutate(
    cps.cust.rev = case_when(demand <= generate ~ demand * (residential.prop * cps.residential.rate + commercial.prop * cps.commercial.rate),
                              TRUE ~ generate * (residential.prop * cps.residential.rate + commercial.prop * cps.commercial.rate)),
    surplus = generate - demand,
    non.cps.demand.flag = rbinom(n = n(), size = 1, prob = 0.5),
    non.cps.demand = case_when(non.cps.demand.flag == 1 ~ rnorm(n = n(), mean = 10000, sd = 2000),
                               TRUE ~ 0),
    non.cps.sell = case_when(surplus > 0 ~ pmin(surplus, non.cps.demand),
                              TRUE ~ 0),
    non.cps.rate = rnormTrunc(n = n(), mean = non.cps.rate.mean,
                              sd = non.cps.rate.mean / 4, min = 0.01,
                              max = 1.5 * non.cps.rate.mean),
    non.cps.rev = non.cps.sell * non.cps.rate,
    non.cps.exp = case_when(surplus < 0 ~ surplus * non.cps.rate,
                             TRUE ~ 0),
    profit = operating.cost + cps.cust.rev + non.cps.rev + non.cps.exp,
    )
  
sum(cps.plants.daily.cost$cps.cust.rev)
sum(cps.plants.daily.cost$non.cps.rev)
sum(cps.plants.daily.cost$operating.cost)
sum(cps.plants.daily.cost$non.cps.exp)
sum(cps.plants.daily.cost$profit)







# 
#   
# # # https://data.ercot.com/data-product-archive/NP4-190-CD
# # folder <- './data/DAM_settlementpoint/'
# # dam.settlement.120124.raw <- read.csv(
# #   paste0(data.folder, 'DAM_settlementpoint/DAM_settlementpoint_120124.csv'))
# # # dam.settlement.120224.raw <- read.csv(
# # #   paste0(data.folder, 'DAM_settlementpoint/DAM_settlementpoint_120224.csv'))
# # 
# # dam.settlement.dec2024.raw <- bind_rows(
# #   dam.settlement.120124.raw,
# #   # dam.settlement.120224.raw
# #   )
# # 
# # str(dam.settlement.dec2024.raw)
# # unique(dam.settlement.dec2024.raw$DSTFlag)
# # unique(dam.settlement.dec2024.raw$SettlementPoint)
# # 
# # prices.dec2024 <- dam.settlement.dec2024.raw %>%
# #   select(-DSTFlag) %>%
# #   rename('operatingDay' = DeliveryDate,
# #          'hourEnding' = HourEnding,
# #          'site' = SettlementPoint,
# #          'DAM.price' = SettlementPointPrice) %>%
# #   mutate(operatingDay = mdy(operatingDay),
# #          hourEnding = hour(hm(hourEnding)))
# # 
# # sites <- data.frame(
# #   site = c('CALAVER_JKS1', 'BRAUNIG_VHB1', 'X443ESRN', 'RN_OCI_ALAMO',
# #            'SOLARA_UNIT1', 'ZIER_SLR_ALL'),
# #   type = c('Baseload', 'Intermediate', 'Peaking', 'Wind', 'Solar', 'Non.CPS')
# #   # priority = c(1, 2, 5, 3, 4, 6)
# # )
# 
# capacity <- data.frame(
#   hourEnding = seq(1, 24),
#   ZIER_SLR_ALL = 10000,
#   CALAVER_JKS1 = 5000,
#   BRAUNIG_VHB1 = 2000,
#   X443ESRN = 5000
#   ) %>%
#   mutate(RN_OCI_ALAMO = case_when((hourEnding > 6 & hourEnding < 21) ~ 800,
#                                   TRUE ~ 1200),
#          SOLARA_UNIT1 = case_when((hourEnding > 8 & hourEnding < 17) ~ 1500,
#                                   TRUE ~ 500)
#          )
# # wind.jitter <- rnorm(n = 24, mean = 1, sd = 0.1)
# # solar.jitter <- rnorm(n = 24, mean = 1, sd = 0.1)
# # capacity <- capacity %>%
# #   mutate(RN_OCI_ALAMO = case_when((hourEnding > 6 & hourEnding < 21) ~ 800,
# #                                   TRUE ~ 1200),
# #          SOLARA_UNIT1 = case_when((hourEnding > 8 & hourEnding < 17) ~ 1500,
# #                                   TRUE ~ 500)
# #   )
# # capacity <- capacity %>%
# #   mutate(wind.jitter = wind.jitter,
# #          solar.jitter = solar.jitter,
# #          RN_OCI_ALAMO = RN_OCI_ALAMO * wind.jitter,
# #          SOLARA_UNIT1 = SOLARA_UNIT1 * solar.jitter) %>%
# #   select(-c(wind.jitter, solar.jitter))
# 
# prices.dec2024 <- prices.dec2024 %>%
#   filter(site %in% sites$site)
#   
# rm(pred.raw, dam.settlement.120124.raw, dam.settlement.120224.raw,
#    dam.settlement.dec2024.raw, folder, data.folder, solar.jitter, wind.jitter)
# 
# RTM.upcharge <- rnormTrunc(n = nrow(prices.dec2024), mean = 0.1, sd = 0.05,
#                            min = 0)
# 
# prices.dec2024 <- prices.dec2024 %>%
#   left_join(sites, by = 'site') %>%
#   mutate(RTM.upcharge = 1 + RTM.upcharge)
# 
# prices.dec2024 <- prices.dec2024 %>%
#   # mutate(RTM.upcharge = case_when(!type %in% c('Solar', 'Wind') ~ RTM.upcharge,
#   #                                 TRUE ~ NA))
#   mutate(RTM.upcharge = RTM.upcharge)
# prices.dec2024 <- prices.dec2024 %>%
#   mutate(RTM.price = DAM.price * RTM.upcharge)
# 
# ggplot(data = prices.dec2024,
#        aes(x = hourEnding,
#            y = DAM.price,
#            color = type)) +
#   geom_line(size = 1) +
#   scale_color_discrete(name = 'Resource') +
#   xlab('Hour of Day') +
#   ylab('DAM Price') +
#   theme_solarized_2()
# 
# capacity.long <- capacity %>%
#   pivot_longer(cols = -hourEnding, names_to = 'site', values_to = 'capacity') %>%
#   left_join(sites, by = 'site') %>%
#   mutate(priority = case_when(type == 'Baseload' ~ 1,
#                               type == 'Intermediate' ~ 2,
#                               type == 'Peaking' ~ 5,
#                               type == 'Non.CPS' ~ 6,
#                               TRUE ~ 3)) %>%
#   arrange(hourEnding, priority, desc(capacity)) %>%
#   mutate(priority = rep(seq(1, nrow(sites)), times = nrow(pred)),
#          type = factor(type,
#                        levels = c('Non.CPS', 'Peaking', 'Solar', 'Wind',
#                                   'Intermediate', 'Baseload')))
# 
# # prices.dec2024 <- prices.dec2024 %>%
# #   left_join(capacity.long %>% select(-type), by = c('hourEnding', 'site'))
# 
# capacity.long <- capacity.long %>%
#   group_by(hourEnding) %>%
#   mutate(cumsum.capacity = cumsum(capacity)) %>%
#   ungroup()
# 
# planner <- capacity.long %>%
#   left_join(pred %>% select(hourEnding, pred), by = 'hourEnding') %>%
#   mutate(need = case_when(
#     pred >= cumsum.capacity ~ capacity,
#     (pred <= cumsum.capacity & pred >= lag(cumsum.capacity, n = 1)) ~ pred - lag(cumsum.capacity, n = 1),
#     TRUE ~ 0))
# 
# planner.wide <- planner %>%
#   select(hourEnding, type, need) %>%
#   pivot_wider(names_from = type, values_from = need) %>%
#   left_join(pred %>% select(hourEnding, load, pred), by = 'hourEnding')
# 
# ggplot(data = planner,
#        aes(x = hourEnding,
#            y = need,
#            fill = type)) +
#   geom_area() +
#   xlab('Hour of Day') +
#   ylab('Predicted Demand') +
#   theme_solarized_2()
# 
# wind.jitter <- rnorm(n = 24, mean = 1, sd = 0.1)
# solar.jitter <- rnorm(n = 24, mean = 1, sd = 0.1)
# 
# prices.dec2024.wide <- prices.dec2024 %>%
#   select(hourEnding, type, DAM.price, RTM.price) %>%
#   pivot_wider(names_from = type, values_from = c(DAM.price, RTM.price),
#               names_sep = '.')
# 
# actual <- planner.wide %>%
#   mutate(Wind.actual = Wind * wind.jitter,
#          Solar.actual = Solar * solar.jitter,
#          actual.generated = Baseload + Intermediate + Wind.actual + Solar.actual
#          + Peaking + Non.CPS,
#          err = actual.generated - load) %>%
#   left_join(prices.dec2024.wide, by = 'hourEnding') %>%
#   mutate(DAM.expenses.Non.CPS = -Non.CPS * DAM.price.Non.CPS,
#          RTM.expenses.Non.CPS = if_else(err < 0, err * RTM.price.Non.CPS, 0))
# 
# # assumption is that operation/generation costs are the same for CPS
# off.sys.sales <- actual %>%
#   select(hourEnding, Baseload, Intermediate, Wind.actual, Solar.actual, Peaking,
#          RTM.price.Baseload, RTM.price.Intermediate, RTM.price.Wind,
#          RTM.price.Solar, RTM.price.Peaking, err) %>%
#   pivot_longer(cols = Baseload:Peaking, names_to = 'type',
#                values_to = 'generated') %>%
#   pivot_longer(cols = RTM.price.Baseload:RTM.price.Peaking, names_to = 'type.price',
#                values_to = 'price.per') %>%
#   mutate(type.price = str_remove(type.price, pattern = 'RTM.price.'),
#          type = str_remove(type, pattern = '.actual')) %>%
#   filter(type == type.price,
#          err > 0) %>%
#   arrange(hourEnding, price.per) %>%
#   group_by(hourEnding) %>%
#   mutate(cumsum.generated = cumsum(generated)) %>%
#   mutate(sell = case_when(
#     err >= cumsum.generated ~ generated,
#     (err <= cumsum.generated & err >= lag(cumsum.generated, n = 1)) ~ err - lag(cumsum.generated, n = 1),
#     (cumsum.generated == min(cumsum.generated) & generated != 0) ~ err,
#     TRUE ~ 0)) %>%
#   filter(sell != 0) %>%
#   mutate(revenue = sell * price.per)
#   
# expenses <- sum(actual$DAM.expenses.Non.CPS) + sum(actual$RTM.expenses.Non.CPS)
# revenue <- sum(off.sys.sales$revenue)
# profit <- expenses + revenue


