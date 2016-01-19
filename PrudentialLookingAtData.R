# Load Libraries - note that you might have to install the package first, e.g. enter "install.packages('randomForest')" in your R console
require(ggplot2)
require(sm)

# Clear work space
rm(list = ls())

# Set working directory - save the Prudential train.csv and test.csv there
setwd("C:\\Users\\si278\\Desktop\\prudential")

# Load data
train = read.csv("train.csv")
test = read.csv("test.csv")

### CLEANING DATA ##########################################################################

# nrow, ncol
# Present str, head, summary
# Use Product_Info_1 as a motivation to classify the data
summary(train$Product_Info_1)

test$Response <- NA #Add in a column for Response in the test data and initialize to NA
train$Train_Flag <- 1 #Add in a flag to identify if observations fall in train data, 1 train, 0 test
test$Train_Flag <- 0 #Add in a flag to identify if observations fall in train data, 1 train, 0 test
All_Data <- rbind(train,test) #79,146 observations, 129 variables

DataN <- All_Data[,names(All_Data) %in% c("Product_Info_4",	"Ins_Age",	"Ht",	"Wt",	"BMI",	"Employment_Info_1",	"Employment_Info_4",	"Employment_Info_6",	"Insurance_History_5",	"Family_Hist_2",	"Family_Hist_3",	"Family_Hist_4",	"Family_Hist_5",	"Medical_History_1",	"Medical_History_15",	"Medical_History_24",	"Medical_History_32")]
DataF <- All_Data[,!(names(All_Data) %in% c("Product_Info_4",	"Ins_Age",	"Ht",	"Wt",	"BMI",	"Employment_Info_1",	"Employment_Info_4",	"Employment_Info_6",	"Insurance_History_5",	"Family_Hist_2",	"Family_Hist_3",	"Family_Hist_4",	"Family_Hist_5",	"Medical_History_1",	"Medical_History_15",	"Medical_History_24",	"Medical_History_32"))]
DataF <- data.frame(apply(DataF, 2, as.factor))

summary(DataF$Product_Info_1)
summary(DataF)

#Scanning Data for NA values
sum(is.na(All_Data)) / (nrow(All_Data) * ncol(All_Data))
apply(All_Data, 2, function(x) { sum(is.na(x))/nrow(All_Data) })

tt<-table(train$Response, is.na(train$Medical_History_15))
prop.table(tt, margin=1)

# Duplicate rows
nrow(train) - nrow(unique(train))

# Constant Columns
train.const <- sapply(train, function(x) { length(unique(x)) == 1 })

# Table function
tt<-table(DataF$Response, DataF$Insurance_History_4)
prop.table(tt, margin=1)

levels(DataF$Insurance_History_4)
levels(DataF$Product_Info_2)
levels(DataF$Medical_History_10)
# it seems reasonable to use the variables Medical_History_2 and Medical_History_10 as continuous.

#Is there a function that returns qunatile values?

#You can plot data, simple examples are histograms or density plots
hist(DataN$BMI)
plot(density(DataN$BMI))
# Would love to find out more about plotting data in R (lots of information online)
# Example - comparing densities

# create value labels 
Response <- factor(train$Response, levels= c(1:8), labels = c(1:8)) 

# plot densities 
sm.density.compare(train$BMI, train$Response, xlab="BMI")
title(main="BMI density for different Response Variables")

# add legend via mouse click
colfill<-c(2:(2+length(levels(Response)))) 
legend(locator(1), levels(Response), fill=colfill)

# Boxplot
boxplot(train$BMI~train$Response, data=train, main="BMI~Response", xlab="Response", ylab="BMI")




### Subsetting Data ################################################################################################
# This is required for cross validation later

set.seed(1234)
train$random <- runif(nrow(train))

train_70 <- train[train$random <= 0.7,] #41,561 obs
train_30 <- train[train$random > 0.7,] #17,820 obs
