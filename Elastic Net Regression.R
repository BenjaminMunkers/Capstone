# this script trains and evaluates an elastic net regression 

# Load the required packages
library(caret)
library(glmnet)
library(doParallel)
library(tictoc)


# Load the dataset
# Load the dataset
data <- read.csv("C:/Users/Joseph/Desktop/EFIN 499/county_yield_drought.csv")

# Convert categorical variables to factors
data$commodityCode <- as.factor(data$commodityCode)
data$FIPS <- as.factor(data$FIPS)
data$cropYear <- as.factor(data$cropYear)

# data goes through 2016 and we want to predict 1 year ahead, so use most recent year as testing dataset
train_data <- subset(data, cropYear != 2016)
test_data <- subset(data, cropYear == 2016)

# Define the cross-validation method
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Define the model training grid
grid <- expand.grid(alpha = seq(0, 1, 0.01), lambda = seq(0, 1, 0.01))

# Define the model formula
formula <- county.yield ~ FIPS  + cropYear + commodityCode + avg.D0 + avg.D1 + avg.D2 + avg.D3 + avg.D4 +  num_d0 + num_d1 + num_d2 + num_d3 + num_d4

# Set up parallel processing
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Train the Elastic Net Regression model using the caret package
set.seed(123)
tic()
model <- train(formula,
               data = train_data,
               method = "glmnet",
               trControl = ctrl,
               tuneGrid = grid,
               allowParallel = TRUE )


# Stop the cluster
stopCluster(cl)

# Print the model performance metrics
print(model)
toc()

# Predict on test data
predictions <- predict(model, newdata = test_data)

# Calculate the RMSE
rmse <- RMSE(predictions, test_data$county.yield)
print(rmse)

plot(model)

# Get the coefficients for the model
coef(model$finalModel, s = model$bestTune$lambda)

# range of  county yield 
(range(data$county.yield))

# percent error
((rmse/(range(data$county.yield)[2]))*100)

  