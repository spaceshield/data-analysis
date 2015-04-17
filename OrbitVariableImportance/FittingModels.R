# Clear Workspace
rm(list = ls())

# Necessary libraries
library(caret)
library(randomForest)
library(Metrics)
library(foreach)
library(gbm)
library(party)

# Set seed for reproducibility and also set working directory
set.seed(1)
setwd("C:/Users/User/Documents/R/SpaceApps1/")

load("TrainData.rda")


# Further partitioning our original training data into training and test sets
inTrain         = createDataPartition(TrainData$orbit_type, p = 0.8)[[1]]
training        <- TrainData[inTrain,]
remainder        <- TrainData[-inTrain,]


##
# RF

set.seed(415)
RFfit <- train(orbit_type~.,  
                      data = training[,!(names(training) %in% c("designation", "objectnumber"))], 
                      method = "rf",
                      importance = TRUE)
save(RFfit, file = "RFfit.rda")

load("RFfit.rda")
predictionRF <- predict(RFfit, newdata = remainder)
confusionMatrix(remainder$orbit_type, predictionRF)
save(predictionRF, file = "predictionRF.rda")

# Plot variable importance
png(filename = "RFimp.png", width = 1920, height = 1080)
varImpPlot(RFfit$finalModel)
dev.off()


#### 
####


# Conditional inference trees

# Parameters for caret's train
set.seed(415)
fitControl <- trainControl(method = "repeatedcv",        # do repeated Cross Validation
                           number = 5,                   # 10-fold
                           repeats = 5)                  # repeat 4 times each 
# Ctree for casual
CT <- train(orbit_type~.,  
            data = training[,!(names(training) %in% c("designation", "objectnumber"))], 
            method = "ctree",
            tuneLength = 5,
            trControl = fitControl
)
save(CT, file = "ctree.rda")
load("ctree.rda")
predictionCtree <- predict(CT, newdata = remainder)
confusionMatrix(remainder$orbit_type, predictionCtree)



summary(factor(TrainData$objectnumber))
unique(TrainData$objectnumber)

