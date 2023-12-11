# comparing poverty and adults ages 18 to 24 with not 
# enough food in the past week
library(dplyr)
library(stringr)

child_poverty <- read_csv("Children in poverty.csv")
food_insecure_18to24 <- read_csv("Adults ages 18 to 24 with food insecurity .csv")

# merged by both location column
both_df <- merge(x = child_poverty, y = food_insecure_18to24, by.x = "Location", by.y = "Location", all.y = TRUE, all.x = TRUE)


