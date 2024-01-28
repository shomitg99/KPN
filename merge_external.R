library(dplyr)
library(caret)
library(ggplot2)
library(readxl)

load("full3.RData")
load("Income_and_Density.RData")
load("EducationTable.RData")
load("AgeTable.RData")

full3 <- full2
inc_dens <- data3
prov_edu <- data2
prov_age <- data

rm(full2)
rm(data3)
rm(data2)
rm(data)

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
