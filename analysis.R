library(tidyverse)
library(httr)
library(jsonlite)
library(data.table)
library(dplyr)
library(ggplot2)

# reading in station lat/lon/subscriber proportion from file created by group_bikedata.py
data = read.csv("business.csv", header=TRUE, sep=",")

row_data = data[235,]
row_data = data[156,]
row_data = data[334,]
selected_columns = row_data[c("Food", "Healthcare", "Other", "N_A", "Work", "Education", "Retail", "Recreation")]
barplot(selected_columns, 
        main = "Business Frequencies within 0.5 miles of ", 
        col = "skyblue", 
        ylab = "Frequency", 
        las = 2)