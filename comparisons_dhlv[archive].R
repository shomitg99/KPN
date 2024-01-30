# comparing raw vs cleaned dhlv

library(ggplot2)
library(caret)
library(data.table)
library(lubridate)

load("final.RData")
clean_dhlv <- full12[,c("week_decision","delta_hlv")]
nba_served <- fread("campaigns_served.csv")
nba_served$week_decision <- week(nba_served$decision_date)
raw_dhlv <- nba_served[,c("week_decision", "delta_hlv")]
raw_dhlv <- na.omit(raw_dhlv)

raw_week <- aggregate(raw_dhlv, delta_hlv ~ week_decision, FUN = sum)
clean_week <- aggregate(clean_dhlv, delta_hlv ~ week_decision, FUN = sum)

plot(raw_week$delta_hlv ~ raw_week$week_decision, type = "l",
     col = "#002ab5",
     xlab = "Week",
     ylab = "Weekly dHLV sum")
points(clean_week$delta_hlv ~ clean_week$week_decision, 
       type = "l",
       col = "#01a9fe")
legend("topright", legend = c("Raw Data", "Cleaned Data"), 
       col = c("#002ab5", "#01a9fe"), lty = 1, cex = 0.8)

