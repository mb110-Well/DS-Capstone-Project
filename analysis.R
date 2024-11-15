library(tidyverse)
library(httr)
library(jsonlite)
library(data.table)
library(dplyr)
library(ggplot2)

# reading in station lat/lon/subscriber proportion from file created by group_bikedata.py
data = read.csv("business.csv", header=TRUE, sep=",")

# creating bar charts for 4 randomly selected stations (using a random number generator and the index)

data[116,]
data[101,]
data[63,]
data[398,]

selected_rows = data[398, c("Food", "Healthcare", "Other", "N_A", "Work", "Education", "Retail", "Recreation")]

long_data = data.frame(
  Category = colnames(selected_rows),
  Count = as.numeric(selected_rows)
)

ggplot(long_data, aes(x = Category, y = Count)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Business Frequencies within 0.5 miles of Everett Square (Broadway at Chelsea St) Station", x = "Business Type", y = "Frequency") +
  coord_flip() + 
  theme_minimal()

# creating scatterplots of the different types vs. subscriber proportion

# FOOD
ggplot(data, aes(x = Food, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Food Business Frequency vs. Subscriber Proportion by Station",
    x = "Food Business Frequency",
    y = "Subscriber Proportion"
  )

# HEALTHCARE
ggplot(data, aes(x = Healthcare, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Healthcare Business Frequency vs. Subscriber Proportion by Station",
    x = "Healthcare Business Frequency",
    y = "Subscriber Proportion"
  )

# OTHER
ggplot(data, aes(x = Other, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Other Business Frequency vs. Subscriber Proportion by Station",
    x = "Other Business Frequency",
    y = "Subscriber Proportion"
  )

# NA
ggplot(data, aes(x = N_A, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "NA Business Frequency vs. Subscriber Proportion by Station",
    x = "NA Business Frequency",
    y = "Subscriber Proportion"
  )

# WORK
ggplot(data, aes(x = Work, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Work Business Frequency vs. Subscriber Proportion by Station",
    x = "Work Business Frequency",
    y = "Subscriber Proportion"
  )

# EDUCATION
ggplot(data, aes(x = Education, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Education Business Frequency vs. Subscriber Proportion by Station",
    x = "Education Business Frequency",
    y = "Subscriber Proportion"
  )

# RETAIL
ggplot(data, aes(x = Retail, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Retail Business Frequency vs. Subscriber Proportion by Station",
    x = "Retail Business Frequency",
    y = "Subscriber Proportion"
  )

# RECREATION
ggplot(data, aes(x = Recreation, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Recreation Business Frequency vs. Subscriber Proportion by Station",
    x = "Recreation Business Frequency",
    y = "Subscriber Proportion"
  )
