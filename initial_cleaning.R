# Load packages
library(ggplot2)
library(caret)
library(readxl)
library(writexl)
library(lubridate)
library(data.table)
library(progress)

# Load raw data
nba_served <- fread("campaigns_served.csv")
nba_meta <- fread("campaign_metadata.csv")
customers <- fread("customer_features.csv")

# Rename customer feature variables - needs to be figured out later (check descriptions)
current_var_names <- data.frame(names(customers))
# write_xlsx(current_var_names, "current_var_names2.xlsx") <- one-time. to create list of new variables manually
new_var_names <- read_xlsx("current_var_names.xlsx")[,2]
colnames(customers) <- new_var_names$new
customers$date <- as.Date(customers$date)
# save(customers, file ="customers.RData") # !! Use this for other files

# Create 'month' in customer features and campaigns served
table(customers$date)
customers$month <- month(customers$date)
nba_served$month <- month(nba_served$decision_date)

# Merging datasets - NBA served + customer
batch <- nba_served
batch$location_id <- ifelse(batch$location_id == "", NA, batch$location_id)
sum(is.na(batch$location_id))
batch <- na.omit(batch)

batch$idMonth <- paste(batch$location_id, batch$month,sep = "")
customers$idMonth <- paste(customers$customer_id, customers$month,sep = "")

nba_cus <- merge(batch, customers, by = "idMonth")

# Merging datasets - (NBA served + customer) + NBA metadata
nba_meta$extract_date <- format(as.Date(nba_meta$extract_date, format = "%d/%m/%Y"), "%Y-%m-%d")
nba_meta$date_name <- paste(nba_meta$extract_date, nba_meta$nba_name, sep = "")
nba_cus$date_chr <- as.character(nba_cus$decision_date)
nba_cus$date_name <- paste(nba_cus$decision_date, nba_cus$nba_name, sep = "")

full <- merge(nba_cus, nba_meta, by = "date_name")

save(full, file = "KPN.RData")