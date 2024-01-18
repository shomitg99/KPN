# Load packages
library(ggplot2)
library(caret)

# Load raw data
nba_served <- read.csv("campaigns_served.csv", header = TRUE, stringsAsFactors = TRUE)
nba_meta <- read.csv("campaign_metadata.csv", sep=";", stringsAsFactors=TRUE)
customers <- read.csv("customer_features.csv", header = TRUE, stringsAsFactors = TRUE)