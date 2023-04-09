# this script trains and evaluates an elastic net regression 

# Load the required packages
library(caret)
library(glmnet)
library(doParallel)
library(tictoc)



# Load the data
data <- read.csv("C:/Users/Joseph/Desktop/EFIN 499/county_yield_drought.csv")

# Convert categorical variables to factors
data$commodityCode <- as.factor(data$commodityCode)
data$FIPS <- as.factor(data$FIPS)
data$cropYear <- as.factor(data$cropYear)

# data goes through 2016 and we want to predict 1 year ahead, so use most recent
# year as testing dataset
train_data <- subset(data, cropYear != 2016)
test_data <- subset(data, cropYear == 2016)

# Define the cross-validation method
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Define the model training grid
grid <- expand.grid(alpha = seq(0, 1, 0.01), lambda = seq(0, 1, 0.01))

# Define the model formula
formula <- county.yield ~ FIPS   + commodityCode + avg.D0 + avg.D1 + avg.D2 +
  avg.D3 + avg.D4 + num_d0 + num_d1 + num_d2 + num_d3 + num_d4

# Set up parallel processing
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Train the Elastic Net Regression model using the caret package
set.seed(123)
tic()
enet_model <- train(formula,
               data = train_data,
               method = "glmnet",
               trControl = ctrl,
               tuneGrid = grid,
               allowParallel = TRUE )


# Stop the cluster
stopCluster(cl)

# Print the model performance metrics
print(enet_model)
toc()

# Predict on test data
predictions <- predict(enet_model, newdata = test_data)

# Calculate the RMSE
rmse <- RMSE(predictions, test_data$county.yield)
print(rmse)

plot(enet_model)

# Get the coefficients for the model
coef(enet_model$finalModel, s = enet_model$bestTune$lambda)

################################################################################

# split data into corn and soy
train_corn_yield <- subset(train_data, commodityCode == 41)
test_corn_yield <- subset(test_data, commodityCode == 41)

train_soy_yield <- subset(train_data, commodityCode == 81)
test_soy_yield <- subset(test_data, commodityCode == 81)

################################################################################

# train model for corn

# Define the model formula
formula <- county.yield ~ FIPS   + commodityCode + avg.D0 + avg.D1 + avg.D2 + avg.D3 + avg.D4 + num_d0 + num_d1 + num_d2 + num_d3 + num_d4

# Set up parallel processing
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Train the Elastic Net Regression model for corn using the caret package
set.seed(123)
tic()
enet_model_corn <- train(formula, data = train_corn_yield, method = "glmnet",  trControl = ctrl, tuneGrid = grid, allowParallel = TRUE )

# Stop the cluster
stopCluster(cl)

toc()

# test the corn model
corn_predictions <- predict(enet_model_corn, newdata = test_corn_yield)

rmse_corn <- RMSE(corn_predictions, test_corn_yield$county.yield)
print(rmse_corn)

se_corn <- 

################################################################################

# train model for soy

# Define the model formula
formula <- county.yield ~ FIPS   + commodityCode + avg.D0 + avg.D1 + avg.D2 + avg.D3 + avg.D4 + num_d0 + num_d1 + num_d2 + num_d3 + num_d4

# Set up parallel processing
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Train the Elastic Net Regression model for corn using the caret package
set.seed(123)
tic()
enet_model_soy <- train(formula, data = train_soy_yield, method = "glmnet",  trControl = ctrl, tuneGrid = grid, allowParallel = TRUE )

# Stop the cluster
stopCluster(cl)

toc()

# test the corn model
soy_predictions <- predict(enet_model_soy, newdata = test_corn_yield)

rmse_soy <- RMSE(soy_predictions, test_soy_yield$county.yield)
print(rmse_soy)
################################################################################

# for x% reported, return the rmse of the enet model and graph it

percent_reported <- seq(0.01,1,(1/length(test_data$county.yield)))
rmse = vector("numeric", length = length(percent_reported))

for (i in 1:length(percent_reported)) {
  temp = test_data |> sample_frac(percent_reported[i])
  temp_pred = predict(enet_model, newdata = temp, interval = 'confidence', level = 0.95)
  rmse[i] = RMSE(temp_pred, temp$county.yield)
}
plot(rmse)

library(ggplot2)
enet_results <- as.data.frame(cbind(percent_reported, rmse))
ggplot(data = enet_results, aes(x = percent_reported, y = rmse)) + geom_point() +
  labs(x = 'Percent Reported', y = 'RMSE',
       title = 'Root Mean Squared Error as Function of Percent Reported') 

coef(enet_model$finalModel, s = enet_model$bestTune$lambda)
enet_model$bestTune
