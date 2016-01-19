# Load Libraries - note that you might have to install the package first, e.g. enter "install.packages('randomForest')" in your R console
require(randomForest)
require(xgboost)
require(ggplot2)
require(readr)
require(methods)
require(data.table)
require(magrittr)

# Clear work space
rm(list = ls())

# Set working directory - save the Prudential train.csv and test.csv there
setwd("C:\\Users\\si278\\Desktop\\prudential")

# Load data
train = read.csv("train.csv")
test = read.csv("test.csv")

### CLEANING DATA ##########################################################################

testId <- test$Id
train$Id = test$Id = 0

# Concatenate train and test data
train$Response <- NA   #Add in a column for Response in the test data and initialize to NA
train$Train_Flag <- 1 #Add in a flag to identify if observations fall in train data, 1 train, 0 test
test$Train_Flag <- 0 
combi <- rbind(train, test)










### Subsetting Data ################################################################################################
set.seed(1234)
train$random <- runif(nrow(train))

train_70 <- train[train$random <= 0.7,] #41,561 obs
train_30 <- train[train$random > 0.7,] #17,820 obs

### Train Classifier ##############################################################################################

# Set a random seed for reproducibility
set.seed(111)

# Train xgboost classifier
my_classifier  <- xgboost(data        = data.matrix(train[,2:(ncol(train)-1)]),
                          label       = train$Response,
                          nrounds     = 100,
                          objective   = "reg:linear",
                          eval_metric = "rmse",
                          missing     = NaN)

### Understand the Model ##########################################################################################

# Compute feature importance matrix und plot (gives feature weight information)
names <- names(train)[2:(ncol(train)-1)]
importance_matrix <- xgb.importance(names, model = my_classifier)
xgb.plot.importance(importance_matrix[1:15,])

# Plot trees to reveal interaction between features (only suitable if the model is simple)
xgb.plot.tree(feature_names = names, model = my_classifier, n_first_tree = 2)

### Prediction #########################################################################################

submission <- data.frame(Id=testId)
submission$Response <- as.integer(round(predict(my_classifier, data.matrix(test[,2:ncol(test)]), missing = NaN)))

# I pretended this was a regression problem and some predictions may be outside the range
submission[submission$Response<1, "Response"] <- 1
submission[submission$Response>8, "Response"] <- 8

### Write Output #########################################################################################

# Writing output file
write_csv(submission, "prudential_submission_file.csv")







### Ideas #########################################################################################

# Change spacing of result vector (ordinal variable!) while keeping regression or build several predictors. Also possible to combine both efforts
# Drop less important features
# Treat NA values differently - E.g. Medical History 15 is the second most important classification value but 75% are NA
# Build new features based on the most important feature values
# Split Product_Info_2 in two features (letter and number)










### Old Code Bits #########################################################################################

# Get an overview of NA values in the data (they have to be removed prior to learning a Random Forest)
for(i in 1:ncol(combi))
  cat("NA values in column i =", i, "(", colnames(combi)[i], ") :", (sum(is.na(combi[,i]))/nrow(combi)), "%", "\n")

# The following two code lines provide an idea how R classifies the columns of the data set
for(i in 1:ncol(combi))
  cat("Class of column i =", i, "(", colnames(combi)[i], ") :", class(combi[,i]), "\n")

# All continous variables are correctly classified as numeric (see https://www.kaggle.com/c/prudential-life-insurance-assessment/data)
# Other variables are classified as integers even though they are supposed to be nominal
numeric_col <- c("Product_Info_4",	"Ins_Age",	"Ht",	"Wt",	"BMI",	"Employment_Info_1",	"Employment_Info_4",	"Employment_Info_6",	"Insurance_History_5",	"Family_Hist_2",	"Family_Hist_3",	"Family_Hist_4",	"Family_Hist_5",	"Medical_History_1")
integer_col <- c("Medical_History_1", "Medical_History_10",	"Medical_History_15",	"Medical_History_24",	"Medical_History_32")
logic_col <- c(paste("Medical_Keyword_",1:48,sep=""))

#Define variables as either numeric or factor, Data_1 - Numeric Variables, Data_2 - factor variables
Data_1 <- All_Data[,names(All_Data) %in% c("Product_Info_4",	"Ins_Age",	"Ht",	"Wt",	"BMI",	"Employment_Info_1",	"Employment_Info_4",	"Employment_Info_6",	"Insurance_History_5",	"Family_Hist_2",	"Family_Hist_3",	"Family_Hist_4",	"Family_Hist_5",	"Medical_History_1",	"Medical_History_15",	"Medical_History_24",	"Medical_History_32",paste("Medical_Keyword_",1:48,sep=""))]
Data_2 <- All_Data[,!(names(All_Data) %in% c("Product_Info_4",	"Ins_Age",	"Ht",	"Wt",	"BMI",	"Employment_Info_1",	"Employment_Info_4",	"Employment_Info_6",	"Insurance_History_5",	"Family_Hist_2",	"Family_Hist_3",	"Family_Hist_4",	"Family_Hist_5",	"Medical_History_1",	"Medical_History_15",	"Medical_History_24",	"Medical_History_32",paste("Medical_Keyword_",1:48,sep="")))]
Data_2 <- data.frame(apply(Data_2, 2, as.factor))

#Explore data
table(train$Response)/nrow(train)

