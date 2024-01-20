# Load packages
library(data.table)
library(caret)
library(ggplot2)
library(plotly)
library(lubridate)
library(leaflet)
library(sp)
library(sf)
library(geojsonio)
library(dplyr)
library(rmapshaper)

NLD <- geojson_read("NLD1.geojson", what = "sp")
NLD_sf <- st_as_sf(NLD)
idx <- which(NLD_sf$prov_name == "Fryslân")
NLD_sf$prov_name[idx] <- "Friesland"
NLD_sf <- na.omit(NLD_sf)

# Simplify geometries within each province
simplified_sf <- NLD_sf %>%
  group_by(prov_name) %>%
  summarize(geometry = st_simplify(geometry))

# Unionize the simplified geometries
agg_sf <- simplified_sf %>%
  summarize(geometry = st_union(geometry))

# Convert back to SpatialPolygonsDataFrame
agg_spdf <- st_as_sf(agg_sf)

# Check the result
plot(agg_spdf)


# Load data
load("customers.RData")
customers$customer_id <- as.character(customers$customer_id)

# Demographics - Age
table(customers$customer_age)

# Known age
age <- customers$customer_age[customers$customer_age != "-" & customers$customer_age != "0 - Onbekend"]
age <- droplevels(age)
age <- data.frame(age)

numeric_part <- as.numeric(sub("^\\D*(\\d+).*", "\\1", levels(age$age)))
ordLev <- levels(age$age)[order(numeric_part)]
age$age <- factor(age$age, levels = ordLev)

age_frq <- table(age$age)
age_df <- data.frame(age = names(age_frq), frequency = as.numeric(age_frq))

ggplot(age_df, aes(x = age, y = frequency)) +
  geom_bar(stat = "identity", ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Demographics - Income
income <- customers$customer_income
levels(income)
levels(income) <- c("NA", "< 18K", "18K-26K", "26K-35K", "35K-50K", "50K-75K","75K-100K", "> 100K")
income_frq <- table(income)
income_df <- data.frame(income = names(income_frq), frequency = as.numeric(income_frq))
income_df$income <- factor(income_df$income, levels = income_df$income)

ggplot(income_df, aes(x = income, y = frequency)) +
  geom_bar(stat = "identity", ) +
  theme_bw()

# Demographics - Province
province <- customers$customer_province
prov_frq <- table(province)
prov_df <- data.frame(province = names(prov_frq), frequency = as.numeric(prov_frq))
prov_df$province <- factor(prov_df$province, levels = prov_df$province)

ggplot(prov_df, aes(x = province, y = frequency)) +
  geom_bar(stat = "identity", ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# 
