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
# write_xlsx(current_var_names, "current_var_names.xlsx") <- one-time. to create list of new variables manually
new_var_names <- fread("current_var_names.xlsx")
new_var_names <- read_xlsx("current_var_names.xlsx")[,2]
colnames(customers) <- new_var_names$new
customers$date <- as.Date(customers$date)
save(customers, file ="customers.RData") # !! Use this for other files

# Create 'month' in customer features and campaigns served
table(customers$date)
customers$month <- month(customers$date)

nba_served$month <- month(nba_served$decision_date)

# Merging datasets
batch <- nba_served[1:100,]
batch$location_id <- ifelse(batch$location_id == "", NA, batch$location_id)

sum(is.na(batch$location_id))
batch <- na.omit(batch)

batch$idMonth <- paste(batch$location_id, batch$month,sep = "")
customers$idMonth <- paste(customers$location_id, customers$month,sep = "")

full <- merge(batch, customers, by = "idMonth")
