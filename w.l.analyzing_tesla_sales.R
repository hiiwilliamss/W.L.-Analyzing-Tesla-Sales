#######################
# William L.
# 2024-11-29
# Analyzing Tesla Sales
#######################

# Unpacking Package

library(tidyverse)

# Unloading File

tesla_df <- read.csv("tesla_used_car_sold-2022-08.csv")

# Summary Statistics

summary(tesla_df)

# Data Cleaning

placeholders <- c("")

for (col in names(tesla_df)) {
  for (placeholder in placeholders) {
    tesla_df[[col]][tesla_df[[col]] == placeholder] <- NA
  }
}

head(tesla_df)

is.na(tesla_df)
sum(is.na(tesla_df))

tesla_df <- na.omit(tesla_df)
head(tesla_df)

dup_rows <- duplicated(tesla_df)
tesla_df[dup_rows, ]

total_dup <- sum(duplicated(tesla_df))
total_dup

# Price Variation

tesla_ca <- tesla_df |>
  filter(state == "CA")

low_high_ca <- tesla_df |>
  select(location, metro, sold_price, trim, sold_date, miles) |>
  filter(metro == "Bay Area") |>
  arrange(sold_price)
head(low_high_ca)

high_low_ca <- tesla_df |>
  select(location, metro, sold_price, trim, sold_date, miles) |>
  filter(metro == "Bay Area") |>
  arrange(desc(sold_price))
head(high_low_ca)

ca_sales <- tesla_ca |>
  summarize(total_sales = n(),
            total_revenue = sum(sold_price, na.rm = T))
ca_sales

ggplot(tesla_ca, aes(x = sold_price)) +
  geom_histogram(binwidth = 5000, fill = "darkred", color = "black") +
  labs(title = "Distribution of Sold Used Tesla Prices in CA",
       x = "Sold Price ($)",
       y = "Count")

# Mileage Analysis

average_mileage <- tesla_df |>
  group_by(model) |>
  summarize(
    count = n(),
    avg_mileage = mean(miles, na.rm = T)
  )
average_mileage

ggplot(data = average_mileage, aes(x = model, y = count, fill = model)) +
  geom_bar(stat = "identity", col = "black") +
  labs(title = "Number of Units Sold by Tesla Model",
       x = "Models",
       y = "Amount of Units Sold",
       fill = "Model Type:")

# Affect of Sold Price by Mileage

corr <- round(cor(tesla_df$miles, tesla_df$sold_price), 2)
corr

ggplot(data = tesla_df, aes(x = miles, y = sold_price)) +
  geom_point(color = "navy", size = 2) +
  geom_smooth(method = "lm", col = "red", se = F) +
  labs(title = "Correlation Between Mileage and Sold Price",
       x = "Mileage of sold Tesla Model",
       y = "Sold Price") +
  annotate("text", x = max(tesla_df$miles) * 0.7, y = max(tesla_df$sold_price) * 0.9,
           label = paste("Correlation Coefficient:", round(corr, 2)), color = "black", size = 5)

cor_matrix <- cor(tesla_df |> select_if(is.numeric))
cor_matrix

model <- lm(sold_price ~ miles, data = tesla_df)
summary(model)

# Feature Importance

library(randomForest)

rf_tesla <- randomForest(sold_price ~ ., data = tesla_df, importance = T)
importance(rf_tesla)

fi_df <- as.data.frame(importance(rf_tesla))
fi_df$Feature <- rownames(fi_df)

ggplot(fi_df, aes(x = reorder(Feature, IncNodePurity), y = IncNodePurity)) + geom_bar(stat = "identity", col = "black", fill = "lightblue") +
  coord_flip() +
  labs(title = "Tesla: FI (Feature Importance) Analysis",
       x = "Feature",
       y = "Importance (Mean Decrease in Gini)")









