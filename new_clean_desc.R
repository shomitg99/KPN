library(ggplot2)
library(plotly)
library(lubridate)
library(caret)
library(writexl)

load("KPN.RData")
full <- data.frame(full)

# as.factor
for (i in 1:length(full)) {
  if (is.character(full[,i])) {
    full[,i] <- as.factor(full[,i])
  }
}

# Unknown province frequency
nrow(full[full$customer_province == "-",]) # only 2, drop
full <- full[full$customer_province != "-",]

# Not business park or Business Line (ZM)
full <- full[full$customer_BizPark != 1,]
full <- full[full$BusinessLine != "CM_ZM",]

# validity check reward date
check <- full[,c("decision_date","nba_name.x", "delta_hlv", "reward_date")]

# unique NBAs
NBA_names <- data.frame(levels(full$nba_name.x))
#write_xlsx(NBA_names, "NBA_unique.xlsx")

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

