library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(fpp3)
library(lubridate)
library(gtrendsR)

StockInfo <- gtrends(keyword = "Lindsey Stirling", time = "all")
g_trends <- as.data.frame(StockInfo$interest_over_time)
g_trends$keyword <- NULL
g_trends$geo <- NULL
g_trends$time <- NULL
g_trends$gprop <- NULL
g_trends$category <- NULL
names(g_trends) <- c("Month", "Interest")
g_trends$Month <- yearmonth(g_trends$Month)
g_trends <- tsibble(g_trends)
g_trends$Interest <- as.numeric(
  ifelse(g_trends$Interest == "<1", 0, g_trends$Interest)
)

