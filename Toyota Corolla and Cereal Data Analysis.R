# Installing the necessary packages
# calibrate: Package for drawing calibrated scales with tick marks on (non-orthogonal) variable vectors in scatterplots and biplots
install.packages("calibrate", dependencies = TRUE)
library(calibrate)

# Amelia: Package for handling and visualizing missing data (missmap) 
install.packages("Amelia", dependencies = TRUE)
library(Amelia)

# corrplot: The corrplot package is a graphical display of a correlation matrix, and confidence interval 
install.packages("corrplot", dependencies = TRUE) 
library(corrplot)

# psych: Package for drawing illustrations of correlation plots (pairs.panels, cor.plot)
install.packages("psych", dependencies = TRUE)
library(psych)

# caret: Package contians functions to streamline the model training process (e.g., dimensionality reduction) 
install.packages("caret", dependencies = TRUE)
library(caret)

# readr: It is designed to flexibly parse many types of data
install.packages("readr", dependencies = TRUE)
library(readr)

# moments: Used to create a statistical data summary
install.packages("fBasics", dependencies = TRUE)
library(fBasics)

# Metrics: Used to compute accuracy metrics for model comparison
install.packages("Metrics", dependencies = TRUE)
library(Metrics)

# reshape: Provides a variety of methods for reshaping data prior to analysis
install.packages("reshape", dependencies = TRUE)
library(reshape)

# tidyr: Designed specifically for data tidying
install.packages("tidyr", dependencies = TRUE)
library(tidyr)

# dplyr: Package contains grammer of data manipulation. (e.g., ' %>% ' syntax)
install.packages("dplyr", dependencies = TRUE)
library(dplyr)

# ggplot2: Package provides plotting capabilities enhancing aesthetics in visualization
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# plotly: Package provides interactive plotting capabilities
install.packages("plotly", dependencies = TRUE) 
library(plotly)

# To make plotly work on Mac (after installing X11 for Mac, called XQuartz)
install.packages("Cairo")
library(Cairo)

#3. rpart: The corrplot package is a Recursive partitioning for classification and regression trees   
install.packages('rpart', dependencies = TRUE)   
library(rpart) 

#4. rpart.plot: Package for plotting decision trees developed using rpart package   
install.packages('rpart.plot', dependencies = TRUE)  
library(rpart.plot) 

# Set the working directory
setwd("~/Desktop/La Trobe University/Year 2 - 1st Semester/Predictive Analytics/Assignment/Assignment 1")

# Part A - Data Exploration and Cleaning
# Load the dataset
CerealData <- read.csv(file = "Cereal.csv", header = TRUE, sep = ",")

# View the data
View(CerealData)

# Verify that CerealData is a data.frame
class(CerealData)

# Check the dimensions
dim(CerealData)

# View the variable names
names(CerealData)

# View the structure of the data
str(CerealData)

# Look at the structure of the data in a clearer way
glimpse(CerealData)

# View a summary of the data
summary(CerealData)

# View the first 6 rows
head(CerealData)

# View the last 6 rows
tail(CerealData)

# Most of the numerical data are in the form of discrete values (integers),
# except for "Fat" and "cups.serv" which exhibited continuous values. 
# However, after deriving its attributes and descriptions, all of the numerical variables are 
# continuous in nature.

# Create a subset of data with only these naturally continuous variables
CerealNumbers <- select_if(CerealData, is.numeric)
ncol(CerealNumbers)

# Create a subset of data with only categorical variables
CerealCategories <- select_if(CerealData, is.factor)
ncol(CerealCategories)

# Plot a histogram of each of the numerical variables
ggplot(gather(CerealNumbers), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = "free_x") # because they are in different scales

# Create a summary statistics of the numerical variables
summary(CerealNumbers)
basicStats(CerealNumbers)

# Select relevant metrics
CerealNumbers.Stats <- data.frame(t(basicStats(CerealNumbers)[c("Minimum", "Maximum", 
  "1. Quartile", "3. Quartile", "Mean", "Median", "Variance", "Stdev", "Skewness", "Kurtosis"), ]))

CerealNumbers.Stats$Coeff_Var <- CerealNumbers.Stats$Stdev / CerealNumbers.Stats$Mean

CerealNumbers.Stats$IQR <- CerealNumbers.Stats$X3..Quartile - CerealNumbers.Stats$X1..Quartile

CerealNumbers.Stats$UpperOutlier <- CerealNumbers.Stats$Maximum > CerealNumbers.Stats$X3..Quartile + CerealNumbers.Stats$IQR * 1.5
CerealNumbers.Stats$LowerOutlier <- CerealNumbers.Stats$Minimum < CerealNumbers.Stats$X1..Quartile - CerealNumbers.Stats$IQR * 1.5

CerealNumbers.Stats

# Create a summary statistics of the categorical variables
summary(CerealCategories)

# Create a boxplot of the numerical variables
CerealNumbersMelted <- melt(CerealNumbers)
CerealNumbersBoxplot <- ggplot(CerealNumbersMelted, aes(factor(variable), value)) + 
  geom_boxplot() + facet_wrap(~variable, scale = "free")

# Handling Missing Data
# Get the number of missing values in the dataset
sum(is.na(CerealData))

# Find missing values
summary(CerealData)

# Find indices of NAs in Wt.serving
ind <- which(is.na(CerealData$Wt.serving))

# Look at the full rows for records missing in Wt.serving
CerealData[ind, ]

# Wt.serving refers to the weight in ounces of one serving

# Method 1 of Handling NAs: Replace NAs with 0
CerealData.zeros <- CerealData

is.na(CerealData.zeros) 

CerealData.zeros[is.na(CerealData.zeros)] <- 0

summary(CerealData.zeros)

# Compare the means
mean(CerealData.zeros$Wt.serving, na.rm = TRUE)
mean(CerealData$Wt.serving, na.rm = TRUE) 

# Show the difference in density
plot(density(CerealData.zeros$Wt.serving), col = "red", main = "Wt.serving Original (Blue) vs Transformed (Red)")

lines(density(CerealData$Wt.serving, na.rm = TRUE), col = "blue")

# Method 2 of Handling NAs: Delete records with NAs
CerealData.deleted <- CerealData[complete.cases(CerealData),] 

summary(CerealData.deleted)

# Compare the means
mean(CerealData.deleted$Wt.serving, na.rm = TRUE)
mean(CerealData$Wt.serving, na.rm = TRUE) 

# Show the difference in density
plot(density(CerealData.deleted$Wt.serving), col = "red", main = "Wt.serving Original (Blue) vs Transformed (Red)")

lines(density(CerealData$Wt.serving, na.rm = TRUE), col = "blue")

# Method 3 of Handling NAs: Replace NAs with mean of the column 
CerealData.clean <- CerealData

summary(CerealData.clean$Wt.serving) 

mean(CerealData.clean$Wt.serving, na.rm = TRUE) 

is.na(CerealData.clean$Wt.serving)

# Replace missing values with the mean
CerealData.clean$Wt.serving[is.na(CerealData.clean$Wt.serving)] <- mean(CerealData.clean$Wt.serving, na.rm = TRUE)

summary(CerealData.clean$Wt.serving)

# Compare the means
mean(CerealData.clean$Wt.serving, na.rm = TRUE)
mean(CerealData$Wt.serving, na.rm = TRUE) 

# Show the difference in density
plot(density(CerealData.clean$Wt.serving), col = "red", main = "Wt.serving Original (Blue) vs Transformed (Red)")

lines(density(CerealData$Wt.serving, na.rm = TRUE), col = "blue")

# Part B - Building predictive models using real world business case
# Load the dataset
ToyotaCorollaData <- read.csv(file="ToyotaCorolla.csv", header = TRUE, sep = ",")

# Explore, Clean, and Preprocess the Data
# View the data
View(ToyotaCorollaData)

# Verify that ToyotaCorollaData is a data.frame
class(ToyotaCorollaData)

# Check the dimensions
dim(ToyotaCorollaData)

# View the variable names
names(ToyotaCorollaData)

# View the structure of the data
str(ToyotaCorollaData)

# Look at the structure of the data in a clearer way
glimpse(ToyotaCorollaData)

# View a summary of the data
summary(ToyotaCorollaData)

# View the first 6 rows
head(ToyotaCorollaData)

# View the last 6 rows
tail(ToyotaCorollaData)

#Evaluate the data distribution
PriceData <- ToyotaCorollaData$Price

par(mfrow = c(1, 2))

hist(PriceData, col = "orange", main = "Histogram") 
plot(density(PriceData, na.rm = TRUE), main = "Density")

# Create a summary statistics of the numerical variables
summary(PriceData)

PriceDataStats <- data.frame(basicStats(PriceData))

options(scipen = 999)

PriceDataStats

# Check for any NA in the data frame
missmap(ToyotaCorollaData, col = c("yellow", "blue"), y.at = 1, y.labels = "", legend = TRUE)
# No NA detected

# Transform the Model into numerical value
# The first numerical (continuous) value in the string is extracted to get the model version
Model.parsed <- parse_number(ToyotaCorollaData$Model) 

ToyotaCorollaData.parsed <- cbind(Model.parsed, ToyotaCorollaData)

# Find the index in Model.parsed that has NA
ind2 <- which(is.na(ToyotaCorollaData.parsed$Model.parsed))

# Replace NA with the mean of the column
ToyotaCorollaData.parsed$Model.parsed[is.na(ToyotaCorollaData.parsed$Model.parsed)] <- mean(ToyotaCorollaData.parsed$Model.parsed, na.rm = TRUE)

# Delete the original Model variable
reject_Model <- names(ToyotaCorollaData.parsed) %in% c("Model") 
ToyotaCorollaData.parsed2 <- ToyotaCorollaData.parsed[!reject_Model]

# Transform catagorical (nominal) variable to numerical by creating dummy variables
Fuel_Indicator <- dummy.code(ToyotaCorollaData.parsed2$Fuel_Type)
ToyotaCorollaData.extended <- cbind(Fuel_Indicator, ToyotaCorollaData.parsed2)

# Delete the original Fuel_Type variable
reject_Fuel_Type <- names(ToyotaCorollaData.extended) %in% c("Fuel_Type") 
ToyotaCorollaData.extended2 <- ToyotaCorollaData.extended[!reject_Fuel_Type]

# Filter out any column with zero variance (NA mean) 
ToyotaCorollaData.extended3 <- Filter(var, ToyotaCorollaData.extended2)
ToyotaCorollaData.extended3
# The "Cyclinders" variable has been removed as it has only a single unique value 

# Obtain the missing Ids
setdiff(1:1436, ToyotaCorollaData.extended$Id)

# Remove the Id column (since it is similar to the index number of each observation)
reject_Id <- names(ToyotaCorollaData.extended3) %in% c("Id") 
ToyotaCorollaData.new <- ToyotaCorollaData.extended3[!reject_Id]

# Reduce Data Dimensions
# Analyze correlation of attributes
par(mfrow = c(1, 1))
pairs.panels(ToyotaCorollaData.new, col = "red")

corrplot(cor(ToyotaCorollaData.new))

# Get the correlation matrix
M <- data.matrix(ToyotaCorollaData.new)

corrM <- cor(M)

# Linear Regression
# 1st Model
# Find the variables with higher cross-correlation
highlyCorrM1 <- findCorrelation(corrM, cutoff = 0.5) 

names(ToyotaCorollaData.new)[highlyCorrM1]

# Remove highly correlated variables form the matrix and merge the target variable
ToyotaCorollaData.selected1 <- data.frame(M[, -highlyCorrM1]) 
ToyotaCorollaData.selected1$Price <- ToyotaCorollaData.new$Price
View(ToyotaCorollaData.selected1)

dim(ToyotaCorollaData.selected1)

# Evaluate correlation of the dimension reducted dataset
pairs.panels(ToyotaCorollaData.selected1, col = "red")

corrplot(cor(ToyotaCorollaData.selected1))

corrplot.mixed(cor(ToyotaCorollaData.selected1), tl.col = "black")

# Show the distribution of each independent variable against the target variable 
names(ToyotaCorollaData.selected1)

ToyotaCorollaData.selected1 %>%
  gather(-Price, key = "SellingPrice", value = "Values") %>%
  ggplot(aes(x = Values, y = Price)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~ SellingPrice, scales = "free")

# Dataset to be used for modelling
summary(ToyotaCorollaData.selected1)

# Partition the data
# Set up the sample configuration
smp_size <- floor(2/3 * nrow(ToyotaCorollaData.selected1)) 
set.seed(42)

# Sample the dataset
ToyotaCorollaData.selected1 <- ToyotaCorollaData.selected1[sample(nrow(ToyotaCorollaData.selected1)), ]
ToyotaCorollaData.train1 <- ToyotaCorollaData.selected1[1:smp_size, ] 
ToyotaCorollaData.test1 <- ToyotaCorollaData.selected1[(smp_size+1):nrow(ToyotaCorollaData.selected1), ]

# Build the predictive model
# Specifying target and input variables
lr_formula1 = Price ~ .

# Fit the linear regression algorithm
lr_model1 <- lm(formula = lr_formula1, data = ToyotaCorollaData.train1) 

# Display a summary of the linear regression model
summary(lr_model1)

# Make Predictions for test and training datasets
ToyotaCorollaData.train1$predicted.Price <- predict(lr_model1, ToyotaCorollaData.train1) 
ToyotaCorollaData.test1$predicted.Price <- predict(lr_model1, ToyotaCorollaData.test1)

print("Actual Values") 

head(ToyotaCorollaData.test1$Price) 

print("Predicted Values")

head(ToyotaCorollaData.test1$predicted.Price)

# Plot Predicted values vs Actual values of the target variable
pl1 <- ToyotaCorollaData.test1 %>% 
  ggplot(aes(Price, predicted.Price)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(aes(colour = "black")) + 
  xlab("Actual value of Price") + 
  ylab("Predicted value of Price") + 
  theme_bw()

ggplotly(pl1)

# Model assessment with Root Mean Square Error
lr1_error <- ToyotaCorollaData.test1$Price - ToyotaCorollaData.test1$predicted.Price 

lr1_mae <- mean(abs(lr1_error))
lr1_rmse <- sqrt(mean(lr1_error^2))

print(paste("Mean Absolute Error: ", lr1_mae))
print(paste("Root Mean Square Error: ", lr1_rmse)) 

# Another way to compute RMSE
pred1 <- predict(object = lr_model1,
                newdata = ToyotaCorollaData.test1) 

rmse(actual = ToyotaCorollaData.test1$Price, 
          predicted = pred1)

# 2nd Model
# Find the variables with higher cross-correlation
highlyCorrM2 <- findCorrelation(corrM, cutoff = 0.65) 

names(ToyotaCorollaData.new)[highlyCorrM2]

# Remove highly correlated variables form the matrix and merge the target variable
ToyotaCorollaData.selected2 <- data.frame(M[, -highlyCorrM2]) 
ToyotaCorollaData.selected2$Price <- ToyotaCorollaData.new$Price
View(ToyotaCorollaData.selected2)

dim(ToyotaCorollaData.selected2)

# Evaluate correlation of the dimension reducted dataset
pairs.panels(ToyotaCorollaData.selected2, col = "red")

corrplot(cor(ToyotaCorollaData.selected2))

corrplot.mixed(cor(ToyotaCorollaData.selected2), tl.col = "black")

# Show the distribution of each independent variable against the target variable 
names(ToyotaCorollaData.selected2)

ToyotaCorollaData.selected2 %>%
  gather(-Price, key = "SellingPrice", value = "Values") %>%
  ggplot(aes(x = Values, y = Price)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~ SellingPrice, scales = "free")

# Dataset to be used for modelling
summary(ToyotaCorollaData.selected2)

# Partition the data
# Set up the sample configuration
set.seed(42)

sample_lr2 <- sample(1:2, size = nrow(ToyotaCorollaData.selected2), prob = list(0.7, 0.3), replace = TRUE)

# Create a training and test data with 70/30 split
ToyotaCorollaData.train2 <- ToyotaCorollaData.selected2[sample_lr2 == 1, ]  
ToyotaCorollaData.test2 <- ToyotaCorollaData.selected2[sample_lr2 == 2, ] 

# Build the predictive model
# Specifying target and input variables
lr_formula2 = Price ~ .

#Fit the linear regression algorithm
lr_model2 <- lm(formula = lr_formula2, data = ToyotaCorollaData.train2) 

# Display a summary of the linear regression model
summary(lr_model2)

# Make Predictions for test, validation and training datasets
ToyotaCorollaData.train2$predicted.Price <- predict(lr_model2, ToyotaCorollaData.train2) 
ToyotaCorollaData.test2$predicted.Price <- predict(lr_model2, ToyotaCorollaData.test2)

print("Actual Values") 

head(ToyotaCorollaData.test2$Price) 

print("Predicted Values")

head(ToyotaCorollaData.test2$predicted.Price)

# Plot Predicted values vs Actual values of the target variable
pl2 <- ToyotaCorollaData.test2 %>% 
  ggplot(aes(Price, predicted.Price)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(aes(colour = "black")) + 
  xlab("Actual value of Price") + 
  ylab("Predicted value of Price") + 
  theme_bw()

ggplotly(pl2)

# Model assessment with Root Mean Square Error
lr2_error <- ToyotaCorollaData.test2$Price - ToyotaCorollaData.test2$predicted.Price 

lr2_mae <- mean(abs(lr2_error))
lr2_rmse <- sqrt(mean(lr2_error^2))

print(paste("Mean Absolute Error: ", lr2_mae))
print(paste("Root Mean Square Error: ", lr2_rmse)) 

# Another way to compute RMSE
pred2 <- predict(object = lr_model2,
                 newdata = ToyotaCorollaData.test2) 

rmse(actual = ToyotaCorollaData.test2$Price, 
     predicted = pred2)

# 3rd Model
# Find the variables with higher cross-correlation
highlyCorrM3 <- findCorrelation(corrM, cutoff = 0.8) 

names(ToyotaCorollaData.new)[highlyCorrM3]

# Remove highly correlated variables form the matrix and merge the target variable
ToyotaCorollaData.selected3 <- data.frame(M[, -highlyCorrM3]) 
ToyotaCorollaData.selected3$Price <- ToyotaCorollaData.new$Price
View(ToyotaCorollaData.selected3)

dim(ToyotaCorollaData.selected3)

# Evaluate correlation of the dimension reducted dataset
pairs.panels(ToyotaCorollaData.selected3, col = "red")

corrplot(cor(ToyotaCorollaData.selected3))

corrplot.mixed(cor(ToyotaCorollaData.selected3), tl.col = "black")

# Show the distribution of each independent variable against the target variable 
names(ToyotaCorollaData.selected3)

ToyotaCorollaData.selected3 %>%
  gather(-Price, key = "SellingPrice", value = "Values") %>%
  ggplot(aes(x = Values, y = Price)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~ SellingPrice, scales = "free")

# Dataset to be used for modelling
summary(ToyotaCorollaData.selected3)

# Partition the data
# Set up the sample configuration
set.seed(42)

sample_lr3 <- sample(1:2, size = nrow(ToyotaCorollaData.selected3), prob = list(0.7, 0.3), replace = TRUE)

# Create a training and test data with 70/30 split
ToyotaCorollaData.train3 <- ToyotaCorollaData.selected3[sample_lr3 == 1, ]  
ToyotaCorollaData.test3 <- ToyotaCorollaData.selected3[sample_lr3 == 2, ] 

# Build the predictive model
# Specifying target and input variables
lr_formula3 = Price ~ .

#Fit the linear regression algorithm
lr_model3 <- lm(formula = lr_formula3, data = ToyotaCorollaData.train3) 

# Display a summary of the linear regression model
summary(lr_model3)

# Make Predictions for test, validation and training datasets
ToyotaCorollaData.train3$predicted.Price <- predict(lr_model3, ToyotaCorollaData.train3) 
ToyotaCorollaData.test3$predicted.Price <- predict(lr_model3, ToyotaCorollaData.test3)

print("Actual Values") 

head(ToyotaCorollaData.test3$Price) 

print("Predicted Values")

head(ToyotaCorollaData.test3$predicted.Price)

# Plot Predicted values vs Actual values of the target variable
pl3 <- ToyotaCorollaData.test3 %>% 
  ggplot(aes(Price, predicted.Price)) + 
  geom_point(alpha = 0.5) + 
  stat_smooth(aes(colour = "black")) + 
  xlab("Actual value of Price") + 
  ylab("Predicted value of Price") + 
  theme_bw()

ggplotly(pl3)

# Model assessment
lr3_error <- ToyotaCorollaData.test3$Price - ToyotaCorollaData.test3$predicted.Price 

lr3_mae <- mean(abs(lr3_error))
lr3_rmse <- sqrt(mean(lr3_error^2))

print(paste("Mean Absolute Error: ", lr3_mae))
print(paste("Root Mean Square Error: ", lr3_rmse)) 

# Another way to compute RMSE
pred3 <- predict(object = lr_model3,
                 newdata = ToyotaCorollaData.test3) 

rmse(actual = ToyotaCorollaData.test3$Price, 
     predicted = pred3)

# Find the optimal model
lr1 <- c(lr1_mae, lr1_rmse, summary(lr_model1)$r.squared)
lr2 <- c(lr2_mae, lr2_rmse, summary(lr_model2)$r.squared)
lr3 <- c(lr3_mae, lr3_rmse, summary(lr_model3)$r.squared)

lr_accuracy <- data.frame(lr1, lr2, lr3, row.names = c("Mean Absolute Error", "Root Mean Squared Error", "R-squared"))
lr_accuracy

# lr3 is the best linear regression model

# Tree-based Regression Model (Decision Tree)
# 4th Model
smp_dt4 <- floor(2/3 * nrow(ToyotaCorollaData.new)) 
set.seed(42)

# Sample the dataset
ToyotaCorollaData.new4 <- ToyotaCorollaData.new[sample(nrow(ToyotaCorollaData.new)), ]

ToyotaCorollaData.train4 <- ToyotaCorollaData.new4[1:smp_dt4, ] 
ToyotaCorollaData.test4 <- ToyotaCorollaData.new4[(smp_dt4+1):nrow(ToyotaCorollaData.new4), ]

# Build the predictive model
# Specifying target and input variables
dt_formula1 = Price ~ .

# Running the decision tree algorithm 
dt1 <- rpart(dt_formula1, data = ToyotaCorollaData.train4, method = "anova") 

# Visualize the decision tree
rpart.plot(dt1, type = 4, fallen.leaves = FALSE, digits = -4) 

print(dt1) 

# Plot the cross validation error 
dev.off()

plotcp(dt1) 

# Check the importance of attributes 
dt1$variable.importance 

# Make Predictions and Assessment 
ToyotaCorollaData.test4$predicted.Price <- predict(dt1, ToyotaCorollaData.test4) 

# Model assessment
dt1_error <- ToyotaCorollaData.test4$Price - ToyotaCorollaData.test4$predicted.Price 

dt1_mae <- mean(abs(dt1_error))
dt1_rmse <- sqrt(mean(dt1_error^2))

print(paste("Mean Absolute Error: ", dt1_mae))
print(paste("Root Mean Square Error: ", dt1_rmse)) 

# Another way to compute RMSE
pred4 <- predict(object = dt1,
                 newdata = ToyotaCorollaData.test4) 

rmse(actual = ToyotaCorollaData.test4$Price, 
     predicted = pred4)

# Fine-tuning the predictive model (Pruning)
printcp(dt1)

# Convert the CP table into a data frame
dt1.cp <- dt1$cptable
dt1.cp.table <- data.frame(dt1.cp)

# 5th Model
# Choosing a different parameter
opt_index1 <- which(dt1.cp.table$nsplit == 6)
cp_opt1 <- dt1.cp.table[opt_index1, "CP"]

# Prune the model 
dt1_pruned <- prune(tree = dt1, cp = cp_opt1)

rpart.plot(dt1_pruned, type = 4, fallen.leaves = FALSE, digits = -4) 

# Checking the order of variable importance
dt1_pruned$variable.importance

# Model assessment after pruning
ToyotaCorollaData.test4$predicted_pruned.Price <- predict(dt1_pruned, ToyotaCorollaData.test4) 

dt1_pruned_error <- ToyotaCorollaData.test4$Price - ToyotaCorollaData.test4$predicted_pruned.Price 

dt1_pruned_mae <- mean(abs(dt1_pruned_error))
dt1_pruned_rmse <- sqrt(mean(dt1_pruned_error^2)) 

print(paste("Mean Absolute Error: ", dt1_pruned_mae))
print(paste("Root Mean Square Error: ", dt1_pruned_rmse)) 

# Another way to compute RMSE
pred5 <- predict(object = dt1_pruned,
                 newdata = ToyotaCorollaData.test4) 

rmse(actual = ToyotaCorollaData.test4$Price, 
     predicted = pred5)

# 6th Model
# Choosing a different parameter
opt_index2 <- which(dt1.cp.table$nsplit == 5)
cp_opt2 <- dt1.cp.table[opt_index2, "CP"]

# Prune the model 
dt1_pruned2 <- prune(tree = dt1, cp = cp_opt2)

rpart.plot(dt1_pruned2, type = 4, fallen.leaves = FALSE, digits = -4) 

# Checking the order of variable importance
dt1_pruned2$variable.importance

# Model assessment after pruning
ToyotaCorollaData.test4$predicted_pruned2.Price <- predict(dt1_pruned2, ToyotaCorollaData.test4) 

dt1_pruned2_error <- ToyotaCorollaData.test4$Price - ToyotaCorollaData.test4$predicted_pruned2.Price 

dt1_pruned2_mae <- mean(abs(dt1_pruned2_error))
dt1_pruned2_rmse <- sqrt(mean(dt1_pruned2_error^2)) 

print(paste("Mean Absolute Error: ", dt1_pruned2_mae))
print(paste("Root Mean Square Error: ", dt1_pruned2_rmse)) 

# Another way to compute RMSE
pred6 <- predict(object = dt1_pruned2,
                 newdata = ToyotaCorollaData.test4) 

rmse(actual = ToyotaCorollaData.test4$Price, 
     predicted = pred6)

# Find the optimal model
DecisionTree1 <- c(dt1_mae, dt1_rmse)
DecisionTree2 <- c(dt1_pruned_mae, dt1_pruned_rmse)
DecisionTree3 <- c(dt1_pruned2_mae, dt1_pruned2_rmse)

dt_accuracy <- data.frame(DecisionTree1, DecisionTree2, DecisionTree3, row.names = c("Mean Absolute Error", "Root Mean Squared Error"))
dt_accuracy

# dt1 is the best tree-based regression model

# Compare regression and decision tree models
LinearRegression3 <- c(lr3_mae, lr3_rmse)

best_models <- data.frame(DecisionTree1, LinearRegression3, row.names = c("Mean Absolute Error", "Root Mean Squared Error"))
best_models

best_models$Difference <- best_models$DecisionTree1 - best_models$LinearRegression3 
best_models
