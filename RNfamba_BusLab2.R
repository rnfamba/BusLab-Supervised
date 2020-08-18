getwd()
setwd("/Users/ritahnfamba/Downloads")
bus <- read_excel("bus1_47.xlsx")
library(readxl)
summary(bus)
#a Which attribute describes the "class" or predicted outcome label?
#Ans: routeTag
#b Remove any attribute that will not contribute to this classification exercise
library(dplyr)
#data cleaning
#predictor variables
bus.subset <- bus[c('lat', 'lon', 'vehicleID')]
bus.subset
#c As in the previous lab, remove any rows not corresponding to results 1 or 47
#Ans: none is applicable; all is compliant 
#2
#Split data into training data set and test data.  Use 80% of the data for training
#and 20% of the data for test 
#generate a random number
normBus <- sample(1:nrow(bus), 0.8 * nrow(bus))
#normalization 
normalize <- function(x){(x -min(x))/(max(x) - min(x))}
#normalization on the first 2 columns
busData <- as.data.frame(lapply(bus.subset [1:2], normalize))
summary(busData)
#extract training set
train <- busData[normBus,]
#extract testing set
test <- busData[-normBus,]
#k=3 will be used as the argument in KNN function
train_category <- bus[normBus, 3]
test_category <- bus[-normBus, 3]
#separate data frame 
train.label <- bus.subset[normBus, 3]
test.label <- bus.subset[-normBus, 3]
library(class)
#finding the number of observations
nrow(train)
# we have 3120 observations the square root is approx 55.85
# with this we create two models with 55 and 56 for the k values
knn_55 <- knn(train = train, test=test, cl = train.label, k=55)
knn_56 <- knn(train = train, test=test, cl = train.label, k=56)
prediction <- knn(train, test, cl = train_category, k=55)

#checking prediction
table <- table(knn_55, test.label)
table
table.1 <- table(knn_56, test.label)
table.1
#creating confusion matrix 
library(caret)
table(knn_55, test.label)
confusionMatrix(table(knn_55, bus.subset))
matrix(table(knn_55, test.label))
newPredict <- table(prediction, test_category)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(newPredict)
#data partition
library(ggplot2)
library(dplyr)
library(class)
library(gridExtra)
x <- test
y <- train
plot(x, row.names = NULL, k = NULL, max.dist = NULL, sym = FALSE, long.lat = FALSE, drop.lower = FALSE)
#SVM Implementation
bus
str(bus)
bus.subset
str(bus.subset)
library(e1071)
# splitting the dataset into training and testing set
set.seed(1010)
trainData <- createDataPartition(y=bus.subset$vehicleID, p = 0.8, list = FALSE)
library(caret)
svm_training <- bus.subset[trainData,]
svm_testing <- bus.subset[- trainData,]
#scaling
svm_testing[-3] = scale(svm_testing [-3])
head(svm_testing)
svm_training[-3] = scale(svm_training [-3])
head(svm_training)
dim(svm_training); dim(svm_testing)
#training the svm model to training set
library(e1071)
trcontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
svmlinear = svm(formula = vehicleID ~ ., data = svm_training, method = "svmLinear", tControl = trControl,
                preProcess = c("center", "scale"), tuneLength = 10)


#How is the prediction from SVM classifier? Use predictions, confusion matrix and scatter plot to understand
#how well the SMV classifier classifies the test and the train data. 
#testing the prediction on test set
newPrediction <- predict(svmLinear, newdata = svm_testing)
#partition the dataset
x <-svm_testing
y  <- svm_training
svmModel <- svm(vehicleID ~ ., data = bus.subset)
summary(svmModel)
#perform the prediction 
myPrediction <- predict(svmModel, x)

#implementing the confusion matrix - predicting the accuracy
confusionMatrix(myPrediction, y$vehicleID)

#scatterplot
plot(svm_testing$vehicleID)




