library(ggplot2)
library(plotly)
library(lubridate)
library(caret)
library(writexl)
library(dplyr)

load("full2.RData")
full2 <- full
rm(full)
full2$reaction <- ifelse(full2$delta_hlv == 0, 0, ifelse(full2$delta_hlv > 0, 1,2))
full2$reaction <- as.factor(full2$reaction)
levels(full2$reaction) <- c("No reaction", "Positive", "Negative")

# Customer
## Customer_province
ggplot(data = full2, aes(x = customer_province)) +
  geom_histogram(stat = "count",
                 fill = "#002ab5") +
  coord_flip() +
  labs(x = "Province",
       y = "Frequency") +
  theme_bw()

## Customer_income (and median by province)
ggplot(data = full2, aes(x = customer_income)) +
  geom_histogram(stat = "count",
                 fill ="#002ab5") +
  coord_flip() +
  labs(x = "Income",
       y = "Frequency") +
  theme_bw()

# !!!Median
inc_data <- full2[, c("customer_income", "customer_province")]
inc_levels <- levels(inc_data$customer_income)

inc_data$customer_income <- as.numeric(inc_data$customer_income)
inc_data <- inc_data[,c("Var1", "inc_data.customer_province")]
colnames(inc_data) <- c("Median", "customer_province")
median_inc <- inc_data %>%
  group_by(customer_province) %>%
  summarise(Median_Income = median(Median))

for (i in 1:nrow(median_inc)) {
   x <- as.numeric(median_inc[i,2])
   median_inc[i,2] <- inc_levels[[x]]
}

median_inc$IncLvl <- factor(inc_levels[median_inc$Median_Income])
median_inc <- median_inc[, c("customer_province", "IncLvl")]

## Customer_education (and median by province)
ggplot(data = full2, aes(x = customer_education)) +
  geom_histogram(stat = "count",
                 fill ="#002ab5") +
  coord_flip() +
  labs(x = "Education level",
       y = "Frequency") +
  theme_bw()

# !!!Median
edu_data <- full2[, c("customer_education", "customer_province")]
edu_levels <- levels(edu_data$customer_education)

edu_data$customer_education<- as.numeric(edu_data$customer_education)
median_edu <- edu_data %>%
  group_by(customer_province) %>%
  summarise(Median_Education = median(customer_education))

median_edu$EduLvl <- factor(edu_levels[median_edu$Median_Education])
median_edu <- median_edu[, c("customer_province", "EduLvl")]

## Historic service provider (top 10) (and mode by province)
prov_data <- data.frame(table(full2$historic_serviceProvider))
prov_data <- prov_data[order(-prov_data$Freq),]
prov_data <- head(prov_data, n = 10)

ggplot(data = prov_data , aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity",
                 fill ="#002ab5") +
  coord_polar("y", start=0) +
  coord_flip() +
  labs(x = "Provider",
       y = "Frequency") +
  theme_bw()

# !!! Mode
historic_data <- full2[, c("historic_serviceProvider", "customer_province")]
mode_historic <- historic_data %>%
  group_by(customer_province, historic_serviceProvider) %>%
  summarise(count = n()) %>%
  slice(which.max(count))

mode_historic <- mode_historic[,1:2]

## Outcome
## NBA reaction (positive, zero, or negative dHLV - by province)
ggplot(data = full2, aes(x = reaction)) +
  geom_histogram(stat = "count",
                 fill ="#002ab5") +
  coord_flip() +
  labs(x = "Campaign Reaction",
       y = "Frequency") +
  theme_bw()

reaction_province <- data.frame(table(full2$reaction, full2$customer_province))
reaction_province <- reaction_province[reaction_province$Freq != 0,]
reaction_province <- na.omit(reaction_province)

ggplot(data = reaction_province, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() +
  labs(x = "Province",
       y = "Frequency",
       fill = "Reaction") +
  scale_fill_manual(values = c("#dcff00","#01a9fe", "#002ab5"))

# NBA features
## Strategic goal (and by province)
ggplot(data = full2, aes(x = StrategicGoal)) +
  geom_histogram(stat = "count",
                 fill ="#002ab5") +
  coord_flip() +
  labs(x = "Strategic Goal",
       y = "Frequency") +
  theme_bw()

strat_province <- data.frame(table(full2$StrategicGoal, full2$customer_province))
strat_province <- strat_province[strat_province$Var2 != "-",]

pop_strat <- strat_province %>%
  group_by(Var2) %>%
  arrange(desc(Freq)) %>%
  slice_head(n = 3) %>%
  ungroup()

ggplot(data = pop_strat, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  labs(x = "Province",
       y = "Frequency",
       fill = "Strategic Goal") +
  scale_fill_manual(values = c("#dcff00","#01a9fe", "#002ab5"))

# dHLV by province
dhlv_provSum <- aggregate(full2, delta_hlv ~ customer_province, FUN = sum)
dhlv_provSum$month.x <- as.factor(dhlv_provSum$month.x)
levels(dhlv_provSum$month.x) <- c("June", "July","August","September","October","November")

ggplot(data = dhlv_provSum, aes(x = customer_province, y = delta_hlv)) +
  geom_bar(stat = "identity",
           fill = "#002ab5") +
  theme_bw() +
  labs(x = "Province",
       y = "Total Delta HLV",
       fill = "Month")

# Week variable
full2$week_decision <- week(full2$decision_date)
full2$week_reward <- week(full2$reward_date)

# dHLV OT by province
dHLV_provOT_sum_decision <- aggregate(full2, delta_hlv ~ customer_province + week_decision, FUN = sum)
ggplot(data = dHLV_provOT_sum_decision, aes(x = week_decision, y = delta_hlv, color = customer_province)) +
           geom_line() +
           theme_bw() +
  labs(x = "Week",
       y = "Total Delta HLV",
       fill = "Province")
