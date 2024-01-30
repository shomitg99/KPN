library(ggplot2)
library(data.table)
library(plotly)

nba_served <- fread("campaigns_served.csv")
nba_meta <- fread("campaign_metadata.csv", stringsAsFactors = TRUE)

# business line
bizLine <- nba_meta$BusinessLine
biz_frq <- table(bizLine)
biz_frq <- data.frame(biz_frq)

ggplotly(ggplot(data = biz_frq, aes(x = bizLine, y = Freq)) +
  geom_bar(stat = "identity", width = 1) +
  theme_bw())
