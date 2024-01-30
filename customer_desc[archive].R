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
library(ggstream)
library(streamgraph)

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
customers$month <- month(customers$date)

# Demographics - Age
table(customers$customer_age)

# Known age
age <- customers[customers$customer_age != "-" & customers$customer_age != "0 - Onbekend", c("month", "customer_age")]
age <- droplevels(age)
colnames(age) <- c("month", "age")

numeric_part <- as.numeric(sub("^\\D*(\\d+).*", "\\1", levels(age$age)))
ordLev <- levels(age$age)[order(numeric_part)]
age$age <- factor(age$age, levels = ordLev)

age_frq <- table(age)
age_frq <- data.frame(age_frq)
age_frq$month <- as.numeric(age_frq$month)

ggplot(data = age_frq, aes(x = month, y = Freq, fill = age)) +
              geom_stream(type = "proportional") +
              theme_bw()

age_mean <- age_frq %>%
  group_by(age) %>%
  summarize(avg_frq = mean(Freq))

ggplot(age_mean, aes(x = age, y = avg_frq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Demographics - Income
income <- customers[,c("customer_income", "month")]
colnames(income) <- c("income", "month")
levels(income$income)
levels(income$income) <- c("NA", "< 18K", "18K-26K", "26K-35K", "35K-50K", "50K-75K","75K-100K", "> 100K")
income_frq <- table(income)
income_frq <- data.frame(income_frq)
income_frq$month <- as.numeric(income_frq$month)


ggplot(data = income_frq, aes(x = month, y = Freq, fill = income)) +
  geom_stream(type = "proportional") +
  theme_bw()

income_mean <- income_frq %>%
  group_by(income) %>%
  summarize(avg_frq = mean(Freq))

ggplot(income_mean, aes(x = income, y = avg_frq)) +
  geom_bar(stat = "identity") +
  theme_bw()

# Demographics - Province
province <- customers[,c("customer_province", "month")]
colnames(province) <- c("province", "month")
prov_frq <- table(province)
prov_frq <- data.frame(prov_frq)
prov_frq$month <- as.numeric(prov_frq$month)

ggplot(data = prov_frq, aes(x = month, y = Freq, fill = province)) +
  geom_stream(type = "proportional")

prov_mean <- prov_frq %>%
  group_by(province) %>%
  summarize(avg_frq = mean(Freq))

ggplot(data = prov_mean, aes(x = province, y = avg_frq)) +
  geom_bar(stat = "identity") +
  theme_bw()

# Customer - Segment Type
segType <- customers[,c("customer_SegmentType", "month")]
colnames(segType) <- c("segment", "month")
seg_frq <- table(segType)
seg_frq <- data.frame(seg_frq)
seg_frq$month <- as.numeric(seg_frq$month)

ggplot(data = seg_frq, aes(x = month, y = Freq, fill = segment)) +
  geom_stream(type = "proportional")

seg_mean <- seg_frq %>%
  group_by(segment) %>%
  summarize(avg_frq = mean(Freq))

ggplot(data = seg_mean, aes(x = segment, y = avg_frq)) +
  geom_bar(stat = "identity") +
  theme_bw()
