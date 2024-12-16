library(readr)
library(tidyr)
library(dplyr)
cars <- read.csv("C:/Users/rajes/Downloads/automobile/imports-85.data")

# Fixing the column names since the .data file reads headers incorrectly
colnames(cars) <- c(
  "symboling",
  "normalized_losses",
  "make",
  "fuel_type",
  "aspiration",
  "num_doors",
  "body_style",
  "drive_wheels",
  "engine_location",
  "wheel_base",
  "length",
  "width",
  "height",
  "curb_weight",
  "engine_type",
  "num_cylinders",
  "engine_size",
  "fuel_system",
  "bore",
  "stroke",
  "compression_ratio",
  "horsepower",
  "peak_rpm",
  "city_mpg",
  "highway_mpg",
  "price"
)

# Removing non-numerical columns and removing missing data
cars <- cars %>% 
  select(
    symboling, wheel_base, length, width, height, curb_weight,
    engine_size, bore, stroke, compression_ratio, horsepower, 
    peak_rpm, city_mpg, highway_mpg, price
  ) %>% 
  filter(
    stroke != "?",
    bore != "?",
    horsepower != "?",
    peak_rpm != "?",
    price != "?"
  ) %>% 
  mutate(
    stroke = as.numeric(stroke),
    bore = as.numeric(bore),
    horsepower = as.numeric(horsepower),
    peak_rpm = as.numeric(peak_rpm),
    price = as.numeric(price)
  )

# Confirming that each of the columns are numeric
library(purrr)
map(cars, typeof)
library(caret)
featurePlot(cars, cars$price)
library(ggplot2)
ggplot(cars, aes(x = price)) +
  geom_histogram(color = "red") +
  labs(
    title = "Distribution of prices in cars dataset",
    x = "Price",
    y = "Frequency"
  )
library(caret)
split_indices <- createDataPartition(cars$price, p = 0.8,  list = FALSE)
train_cars <- cars[split_indices,]
test_cars <- cars[-split_indices,]
# 5-fold cross-validation 
five_fold_control <- trainControl(method = "cv", number = 5)

tuning_grid <- expand.grid(k = 1:20)
# Creating a model based on all the features
full_model <- train(price ~ .,
                    data = train_cars,
                    method = "knn",
                    trControl = five_fold_control,
                    tuneGrid = tuning_grid,
                    preProcess = c("center", "scale"))
predictions <- predict(full_model, newdata = test_cars)
postResample(pred = predictions, obs = test_cars$price)
