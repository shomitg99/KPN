library(ggplot2)
library(plotly)
library(lubridate)
library(caret)

load("KPN.RData")


# Unknown province frequency
nrow(full[full$customer_province == "-",]) # only 2, drop
full <- full[full$customer_province != "-",]

# dHLV by province
dhlv_provSum <- aggregate(full, delta_hlv ~ customer_province, FUN = sum)
ggplotly(ggplot(data = dhlv_provSum, aes(x = customer_province, y = delta_hlv)) +
  geom_bar(stat = "identity") +
  theme_bw())

dhlv_provMean <- aggregate(full, delta_hlv ~ customer_province, FUN = mean)
ggplotly(ggplot(data = dhlv_provMean, aes(x = customer_province, y = delta_hlv)) +
           geom_bar(stat = "identity") +
           theme_bw())

# Week variable
full$week_decision <- week(full$decision_date)
full$week_reward <- week(full$reward_date)

# dHLV OT by province
dHLV_provOT_mean_decision <- aggregate(full, delta_hlv ~ customer_province + week_decision, FUN = mean)
ggplotly(ggplot(data = dHLV_provOT_mean_decision, aes(x = week_decision, y = delta_hlv, color = customer_province)) +
  geom_line() +
  theme_bw())

dHLV_provOT_mean_reward <- aggregate(full, delta_hlv ~ customer_province + week_reward, FUN = mean)
ggplotly(ggplot(data = dHLV_provOT_mean_reward, aes(x = week_reward, y = delta_hlv, color = customer_province)) +
           geom_line() +
           theme_bw())

dHLV_provOT_sum_decision <- aggregate(full, delta_hlv ~ customer_province + week_decision, FUN = sum)
ggplotly(ggplot(data = dHLV_provOT_sum_decision, aes(x = week_decision, y = delta_hlv, color = customer_province)) +
           geom_line() +
           theme_bw())

dHLV_provOT_sum_reward <- aggregate(full, delta_hlv ~ customer_province + week_reward, FUN = sum)
ggplotly(ggplot(data = dHLV_provOT_sum_reward, aes(x = week_reward, y = delta_hlv, color = customer_province)) +
           geom_line() +
           theme_bw())

# preliminary analysis test
subTest <- full[, c("delta_hlv", "mob_avgUsage_sub", "customer_helpLine_3months", "customer_age", "customer_income")]
subTest <- subTest[subTest$mob_avgUsage_sub != -1,]
subTest$customer_age <- as.factor(subTest$customer_age)
subTest$customer_income<- as.factor(subTest$customer_income)

trainRows <- createDataPartition(subTest$delta_hlv, p = 0.8, list = FALSE)
train <- subTest[trainRows,]
test <- subTest[-trainRows,]

linear <- lm(data = subTest, delta_hlv ~.)
coef(linear)
varImp(linear)

preds <- predict(linear, newdata = test)
plot(preds ~ test$delta_hlv)
