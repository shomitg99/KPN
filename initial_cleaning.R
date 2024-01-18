# Load packages
library(ggplot2)
library(caret)
library(readxl)
library(lubridate)
library(data.table)
library(progress)

# Load raw data
nba_served <- fread("campaigns_served.csv")
nba_meta <- fread("campaign_metadata.csv")
customers <- fread("customer_features.csv")

# Rename customer feature variables - needs to be figured out later (check descriptions)
new_vars <- read_excel("new_vars.xlsx", col_names = FALSE)
new_vars <- t(new_vars)
colnames(customers) <- new_vars

# Create 'month' in customer features and campaigns served
colnames(customers)[colnames(customers) == "datum"] <- "date"
customers$date <- as.Date(customers$date)
table(customers$date)
customers$month <- month(customers$date)

nba_served$month <- month(nba_served$decision_date)

# Merging datasets (by 'month' and 'nba_name')
full <- nba_served
col_nba_meta <- names(nba_meta)
full[, col_nba_meta] <- NA

pb <- progress_bar$new(total = nrow(full))

for (i in 1:nrow(full)) {
  pb$tick()
  full[i, col_nba_meta] <- nba_meta[i,]
}

pb$close()
