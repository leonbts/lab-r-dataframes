# Step 1
superstore <- read.csv("dataset/Sample - Superstore.csv")

# Step 2
head(superstore, n=10)
str(superstore)
summary(superstore)

# Step 3
sales_data <- superstore$Sales
subset_data <- subset(superstore[1:15, ], select = c(Order.ID, Customer.Name, Sales))
nrow(superstore)
ncol(superstore)

# Step 4
filter1 <- subset(superstore, Profit > 100)
filter2 <- subset(superstore, Category == "Furniture" & Sales > 500)
filter3 <- subset(superstore, Region == "West" & Quantity > 5)

# Step 5
superstore$"Profit Margin" <- (superstore$Profit / superstore$Sales) * 100
superstore$Sales <- round(superstore$Sales, 2)
superstore <- subset(superstore, select = -Postal.Code)

# Step 6
any(is.na(superstore)) # FALSE, so no need for the other 2 tasks

#Step 7
library(dplyr)

region_summary <- superstore %>%
  group_by(Region) %>%
  summarise(
    Total_Sales = sum(Sales, na.rm = TRUE),
    Total_Profit = sum(Profit, na.rm = TRUE)
  )

superstore <- superstore %>%
  mutate(Discount_Level = case_when(
    Discount >= 0 & Discount < 0.2 ~ "Low",
    Discount >= 0.2 & Discount < 0.5 ~ "Medium",
    Discount >= 0.5 & Discount <= 1 ~ "High",
    TRUE ~ "Unknown"  # Catch unexpected values
  ))

superstore <- superstore %>%
  arrange(desc(Sales))


head(superstore)

