library(dplyr)
library(ggplot2)
library(caret)

load("final.RData")
final <- full12
rm(full12)

# Summary statistics
summary(final$delta_hlv)
summary(final$edu_diff)
summary(final$inc_diff)
summary(final$age_diff)
summary(final$mktSize_proxy)
summary(final$BB_revenueFrom_BB_lastMonth)
summary(final$social_dens)

# Reaction
summary(final$reaction) # overall
by(final$reaction, final$customer_province, summary) # by province


ggplot(data = final, aes(x = reaction)) +
  geom_histogram(stat = "count",
                 fill ="#002ab5") +
  coord_flip() +
  labs(x = "Campaign Reaction",
       y = "Frequency") +
  theme_bw()

reaction_province <- data.frame(table(final$reaction, final$customer_province))

ggplot(data = reaction_province, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() +
  coord_flip() +
  labs(x = "Province",
       y = "Frequency",
       fill = "Reaction",
       title = "Reaction to campaigns by province") +
  scale_fill_manual(values = c("#dcff00","#01a9fe", "#002ab5"))

# Customer/ features - age, income, education
ggplot(data = final, aes(x = customer_province)) +
  geom_histogram(stat = "count",
                 fill = "#002ab5") +
  coord_flip() +
  labs(x = "Province",
       y = "Frequency",
       title = "Customer base by province") +
  theme_bw()

ggplot(data = final, aes(x = customer_income)) +
  geom_histogram(stat = "count",
                 fill ="#002ab5") +
  coord_flip() +
  labs(x = "Income",
       y = "Frequency",
       title = "Customer income levels by province") +
  theme_bw()

ggplot(data = final, aes(x = customer_education)) +
  geom_histogram(stat = "count",
                 fill ="#002ab5") +
  coord_flip() +
  labs(x = "Education level",
       y = "Frequency",
       title = "Customer education levels by province") +
  theme_bw()

# Median income by province
inc_data <- final[, c("customer_income", "customer_province")]
inc_levels <- levels(inc_data$customer_income)

inc_data$customer_income <- as.numeric(inc_data$customer_income)
colnames(inc_data) <- c("Median", "customer_province")
median_inc <- inc_data %>%
  group_by(customer_province) %>%
  summarise(Median_Income = median(Median))

median_inc$IncLvl <- factor(inc_levels[median_inc$Median_Income])
median_inc <- median_inc[, c("customer_province", "IncLvl")]

# Context - income, age, edu diff
summary(final$edu_diff)
by(final$edu_diff, final$customer_province, summary)

summary(final$inc_diff)
by(final$inc_diff, final$customer_province, summary)

summary(final$age_diff)
by(final$age_diff, final$customer_province, summary)

# Market size
summary(final$mktSize_proxy)

# Strategic goal
ggplot(data = final, aes(x = StrategicGoal)) +
  geom_histogram(stat = "count",
                 fill ="#002ab5") +
  coord_flip() +
  labs(x = "Strategic Goal",
       y = "Frequency") +
  theme_bw()

strat_province <- data.frame(table(final$StrategicGoal, final$customer_province))
strat_province <- strat_province[strat_province$Var2 != "-",]

pop_strat <- strat_province %>%
  group_by(Var2) %>%
  arrange(desc(Freq)) %>%
  slice_head(n = 3) %>%
  ungroup()

ggplot(data = pop_strat, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  coord_flip() +
  labs(x = "Province",
       y = "Frequency",
       fill = "Strategic Goal",
       title = "Top 3 strategic goals by province") +
  scale_fill_manual(values = c("#dcff00","#01a9fe", "#002ab5"))

# dHLV
summary(final$delta_hlv)
by(final$delta_hlv, final$customer_province, summary)

dhlv_provSum <- aggregate(final, delta_hlv ~ customer_province, FUN = sum)

ggplot(data = dhlv_provSum, aes(x = customer_province, y = delta_hlv)) +
  geom_bar(stat = "identity",
           fill = "#002ab5") +
  theme_bw() +
  coord_flip() +
  labs(x = "Province",
       y = "Total Delta HLV",
       fill = "Month",
       title = "Total delta HLV by province")


# simple analysis - 5 relationships

# simple analyses
final2 <- final[, -which(names(final) == "reaction")]

set.seed(100)
trainRows <- createDataPartition(final2$historic_serviceProvider, p = 0.8, list = FALSE)
train <- final2[trainRows,]
test <- final2[-trainRows,]

initial <- lm(data = train, delta_hlv ~ (mktSize_proxy + edu_diff + inc_diff + age_diff + social_dens + customer_province)^2)


devel <- lm(data = train,
            delta_hlv ~ (mktSize_proxy + edu_diff + inc_diff + age_diff + social_dens + customer_province)^2 + CampagneDuration + BB_revenueFrom_BB_lastMonth)

p_initial <- predict(initial, newdata = test)
p_devel <- predict(devel, newdata = test)

par(mfrow = c(1, 2))
plot(p_initial ~ test$delta_hlv,
     xlab = "Actual Delta HLV",
     ylab = "Predicted Delta HLV",
     main = "Model 1")

plot(p_devel ~ test$delta_hlv,
     xlab = "Actual Delta HLV",
     ylab = "Predicted Delta HLV",
     main = "Model 2")

