# Load Libraries - note that you might have to install the package first, e.g. enter "install.packages('randomForest')" in your R console
require(xgboost)
require(ggplot2)
require(readr)

# Clear work space
rm(list = ls())

# Set working directory - save the Prudential train.csv and test.csv there
setwd("C:\\Users\\si278\\Desktop\\prudential")

# Load data
train = read.csv("train.csv")
test = read.csv("test.csv")

### CLEANING DATA ##########################################################################

testId <- test$Id
Response <- train$Response
train$Id = test$Id = train$Response = NULL

### Train Classifier ##############################################################################################

# Set a random seed for reproducibility
set.seed(111)

# Train xgboost classifier
my_classifier  <- xgboost(data        = data.matrix(train),
                          label       = Response,
                          nrounds     = 100,
                          objective   = "reg:linear",
                          eval_metric = "rmse",
                          missing     = NaN)

### Understand the Model ##########################################################################################

# Compute feature importance matrix und plot (gives feature weight information)
names <- names(train)
importance_matrix <- xgb.importance(names, model = my_classifier)
xgb.plot.importance(importance_matrix[1:15,])

### Prediction #########################################################################################

submission <- data.frame(Id=testId)
submission$Response <- as.integer(round(predict(my_classifier, data.matrix(test[,1:ncol(test)]), missing = NaN)))

# I pretended this was a regression problem and some predictions may be outside the range
submission[submission$Response<1, "Response"] <- 1
submission[submission$Response>8, "Response"] <- 8

### Write Output #########################################################################################

# Writing output file
write_csv(submission, "prudential_submission_file.csv")
