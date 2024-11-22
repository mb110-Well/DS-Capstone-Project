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




## LOGIT Y ##

# some of the proportions are 1 - add small value to prevent undefined
small_value = 1e-6

data <- as.data.table(data)

data[ , Logit_Prop := log((Sub_Prop + small_value) / (1 - Sub_Prop + small_value))]

## scatterplots with transnformed proportion ## 

# FOOD
ggplot(data, aes(x = Food, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Food Business Frequency vs. Subscriber Proportion by Station (Logit)",
    x = "Food Business Frequency",
    y = "Subscriber Proportion"
  )

# HEALTHCARE
ggplot(data, aes(x = Healthcare, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Healthcare Business Frequency vs. Subscriber Proportion by Station (Logit)",
    x = "Healthcare Business Frequency",
    y = "Subscriber Proportion"
  )

# OTHER
ggplot(data, aes(x = Other, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Other Business Frequency vs. Subscriber Proportion by Station (Logit)",
    x = "Other Business Frequency",
    y = "Subscriber Proportion"
  )

# NA
ggplot(data, aes(x = N_A, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "NA Business Frequency vs. Subscriber Proportion by Station (Logit)",
    x = "NA Business Frequency",
    y = "Subscriber Proportion"
  )

# WORK
ggplot(data, aes(x = Work, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Work Business Frequency vs. Subscriber Proportion by Station (Logit)",
    x = "Work Business Frequency",
    y = "Subscriber Proportion"
  )

# EDUCATION
ggplot(data, aes(x = Education, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Education Business Frequency vs. Subscriber Proportion by Station (Logit)",
    x = "Education Business Frequency",
    y = "Subscriber Proportion"
  )

# RETAIL
ggplot(data, aes(x = Retail, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Retail Business Frequency vs. Subscriber Proportion by Station (Logit)",
    x = "Retail Business Frequency",
    y = "Subscriber Proportion"
  )

# RECREATION
ggplot(data, aes(x = Recreation, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Recreation Business Frequency vs. Subscriber Proportion by Station (Logit)",
    x = "Recreation Business Frequency",
    y = "Subscriber Proportion"
  )

## LOG transformation of frequencies ##

# many frequencies are 0 - add small value to prevent undefined

data[ , Log_Food := log(Food + small_value)]
data[ , Log_Healthcare := log(Healthcare + small_value)]
data[ , Log_Other := log(Other + small_value)]
data[ , Log_N_A := log(N_A + small_value)]
data[ , Log_Work := log(Work + small_value)]
data[ , Log_Education := log(Education + small_value)]
data[ , Log_Retail := log(Retail + small_value)]
data[ , Log_Recreation := log(Recreation + small_value)]

## scatterplots with transformed frequencies ##

# FOOD
ggplot(data, aes(x = Log_Food, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Food Business Frequency vs. Subscriber Proportion by Station (Log)",
    x = "Food Business Frequency",
    y = "Subscriber Proportion"
  )

# HEALTHCARE
ggplot(data, aes(x = Log_Healthcare, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Healthcare Business Frequency vs. Subscriber Proportion by Station (Log)",
    x = "Healthcare Business Frequency",
    y = "Subscriber Proportion"
  )

# OTHER
ggplot(data, aes(x = Log_Other, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Other Business Frequency vs. Subscriber Proportion by Station (Log)",
    x = "Other Business Frequency",
    y = "Subscriber Proportion"
  )

# NA
ggplot(data, aes(x = Log_N_A, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "NA Business Frequency vs. Subscriber Proportion by Station (Log)",
    x = "NA Business Frequency",
    y = "Subscriber Proportion"
  )

# WORK
ggplot(data, aes(x = Log_Work, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Work Business Frequency vs. Subscriber Proportion by Station (Log)",
    x = "Work Business Frequency",
    y = "Subscriber Proportion"
  )

# EDUCATION
ggplot(data, aes(x = Log_Education, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Education Business Frequency vs. Subscriber Proportion by Station (Log)",
    x = "Education Business Frequency",
    y = "Subscriber Proportion"
  )

# RETAIL
ggplot(data, aes(x = Log_Retail, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Retail Business Frequency vs. Subscriber Proportion by Station (Log)",
    x = "Retail Business Frequency",
    y = "Subscriber Proportion"
  )

# RECREATION
ggplot(data, aes(x = Log_Recreation, y = Sub_Prop)) + 
  geom_point() + 
  labs(
    title = "Recreation Business Frequency vs. Subscriber Proportion by Station (Log)",
    x = "Recreation Business Frequency",
    y = "Subscriber Proportion"
  )


## scatterplots with both proportion and frequency transformed ##

# FOOD
ggplot(data, aes(x = Log_Food, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Food Business Frequency vs. Subscriber Proportion by Station (Log, Logit)",
    x = "Food Business Frequency",
    y = "Subscriber Proportion"
  )

# HEALTHCARE
ggplot(data, aes(x = Log_Healthcare, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Healthcare Business Frequency vs. Subscriber Proportion by Station (Log, Logit)",
    x = "Healthcare Business Frequency",
    y = "Subscriber Proportion"
  )

# OTHER
ggplot(data, aes(x = Log_Other, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Other Business Frequency vs. Subscriber Proportion by Station (Log, Logit)",
    x = "Other Business Frequency",
    y = "Subscriber Proportion"
  )

# NA
ggplot(data, aes(x = Log_N_A, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "NA Business Frequency vs. Subscriber Proportion by Station (Log, Logit)",
    x = "NA Business Frequency",
    y = "Subscriber Proportion"
  )

# WORK
ggplot(data, aes(x = Log_Work, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Work Business Frequency vs. Subscriber Proportion by Station (Log, Logit)",
    x = "Work Business Frequency",
    y = "Subscriber Proportion"
  )

# EDUCATION
ggplot(data, aes(x = Log_Education, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Education Business Frequency vs. Subscriber Proportion by Station (Log, Logit)",
    x = "Education Business Frequency",
    y = "Subscriber Proportion"
  )

# RETAIL
ggplot(data, aes(x = Log_Retail, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Retail Business Frequency vs. Subscriber Proportion by Station (Log, Logit)",
    x = "Retail Business Frequency",
    y = "Subscriber Proportion"
  )

# RECREATION
ggplot(data, aes(x = Log_Recreation, y = Logit_Prop)) + 
  geom_point() + 
  labs(
    title = "Recreation Business Frequency vs. Subscriber Proportion by Station (Log, Logit)",
    x = "Recreation Business Frequency",
    y = "Subscriber Proportion"
  )




