library(dplyr)
library(caret)
library(ggplot2)
library(readxl)

load("full3.RData")
load("Income_and_Density.RData")
load("EducationTable2.RData")
load("AgeTable2.RData")
load("Market_Size_Proxy.RData")

inc_dens <- data3
prov_age <- result
prov_edu <- result1

full3 <- full2
rm(full2)
rm(data3)
rm(result)
rm(result1)

# Select columns
fin_cols <- read_excel("variable_names.xlsx")
fin_cols <- fin_cols$colnames.full.

full4 <- full3[, fin_cols]

# VAS recoding
full4$VAS_ziggo <- ifelse(full4$VAS_ziggo != "-",1,0)
full4$VAS_kids <- ifelse(full4$VAS_kids != "-",1,0)
full4$VAS_viaplay <- ifelse(full4$VAS_viaplay != "-",1,0)
full4$VAS_espn <- ifelse(full4$VAS_espn != "-",1,0)
full4$VAS_disney <- ifelse(full4$VAS_disney != "-", 1, 0)
full4$VAS_hbo <- ifelse(full4$VAS_hbo != "-", 1,0)
full4$VAS_prime <- ifelse(full4$VAS_prime != "-",1,0)
full4$VAS_netflix <- ifelse(full4$VAS_netflix != "-",1,0)
full4$VAS_kpnpay <- ifelse(full4$VAS_kpnpay != "-",1,0)
full4$VAS_spotify <- ifelse(full4$VAS_spotify != "-",1,0)
full4$VAS_videoland <- ifelse(full4$VAS_videoland != "-",1,0)
full4$VAS_pluspakket <- ifelse(full4$VAS_pluspakket != "-",1,0)

# Missing values
na_check <- colMeans(is.na(full4)) # no NAs, but still need to check for -1s and "-"

# "-"
full4 <- full4 %>%
  mutate_if(is.factor, as.character)

for (i in 1:length(full4)) {
  for (j in 1:nrow(full4)) {
    if (is.character(full4[,i])) {
      if (full4[j,i] == "-" | full4[j,i] == "") {
        full4[j,i] <- NA
      }
    }
  }
}

full4 <- full4 %>%
  mutate_if(is.character, as.factor)

full5 <- na.omit(full4)

# -1
for (i in 1:length(full5)) {
  for (j in 1:nrow(full5)) {
    if (is.numeric(full5[,i])) {
      if (full5[j,i] == -1) {
        full5[j,i] <- NA
      }
    }
  }
}

full6 <- na.omit(full5)
full6 <- full6[,-c(1,3,4)]

# merge external - age
prov_age <- data.frame(
  "customer_province" = levels(full6$customer_province)
)

prov_age$median_age <- c(2,2,2,1,1,2,1,2,1,2,2,2)



numeric_part <- as.numeric(sub("^\\D*(\\d+).*", "\\1", levels(full6$customer_age)))
ordLev <- levels(full6$customer_age)[order(numeric_part)]
full6$customer_age <- factor(full6$customer_age, levels = ordLev)
full6$age_2545 <- ifelse(as.numeric(full6$customer_age) >= 2 & as.numeric(full6$customer_age) <= 5, 1, 0)
full6$age_4565 <- ifelse(as.numeric(full6$customer_age) >= 6 & as.numeric(full6$customer_age) <= 9, 1, 0)
full6_1 <- merge(full6, prov_age, by = "customer_province")

# merge external - income and population density
colnames(inc_dens) <- c("customer_province", "social_dens","mean_income")
inc_dens$mean_income <- ifelse(inc_dens$mean_income < 50 & inc_dens$mean_income > 35, "4 - 35.000 - 50.000 euro", inc_dens$mean_income)
inc_dens$mean_income <- ifelse(inc_dens$mean_income < 70 & inc_dens$mean_income > 50, "5 - 50.000 - 75.000 euro", inc_dens$mean_income)

full7 <- merge(full6_1, inc_dens, by = "customer_province")

# merge external - education (+ adjust main education levels - drop 2,3,1)
full8 <- full7[full7$customer_education != "1 - Basisonderwijs",]
full9 <- full8[full8$customer_education != "2 - LBO / VMBO (kader of beroep) / MBO1",]
full10 <- full9[full9$customer_education != "3 - MAVO / MULO / VMBO (theoretisch of gemengd)",]

colnames(prov_edu) <- c("customer_province", "irrel","median_education","irrel2")
prov_edu <- prov_edu[,c(1,3)]
prov_edu$median_education <- as.factor(prov_edu$median_education)

prov_edu2 <- prov_edu %>%
  mutate(median_education = recode(median_education, "MBO Education" = 4, "VO Education" = 5))


full11 <- merge(full10, prov_edu2, by = "customer_province")

# external - diff
full11$edu_diff <- ifelse(as.numeric(full11$customer_education) != full11$median_education, 1, 0)
full11$inc_diff <- ifelse(full11$customer_income != full11$mean_income, 1, 0)
full11$age_diff <- ifelse(full11$age_2545 == 1 & full11$median_age == 1, 0, ifelse(full11$age_4565 == 1 & full11$median_age == 2,0,1))

# market size addition
colnames(MarketSizeData) <- c("customer_province", "mktSize_proxy")
full12 <- merge(full11, MarketSizeData, by = "customer_province")


# FINAL DATASET
save(full12, file = "final.RData")
