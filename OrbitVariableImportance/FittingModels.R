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
load("TestData.rda")


# Further partitioning our original training data into training and test sets
inTrain         = createDataPartition(TrainData$orbit_type, p = 0.8)[[1]]
training        <- TrainData[inTrain,]
testing        <- TrainData[-inTrain,]


##
# RF

set.seed(415)
RFfitOrbit <- train(orbit_type~.,  
                      data = training[,!(names(training) %in% c("designation", "objectnumber", "id", "number"))], 
                      method = "rf",
                      importance = TRUE)
save(RFfitOrbit, file = "RFfitOrbit.rda")

load("RFfitOrbit.rda")
predictionRF <- predict(RFfitOrbit, newdata = testing)
confusionMatrix(testing$orbit_type, predictionRF)
save(predictionRF, file = "predictionRF.rda")

UNKNOWNpredictionRF <- predict(RFfitOrbit, newdata = TestData)
png(filename = "OrbitTypesPredicted.png", width = 500, height = 500)
qplot(UNKNOWNpredictionRF, geom="histogram")
dev.off()


# Plot variable importance
png(filename = "RFimp.png", width = 1920, height = 1080)
varImpPlot(RFfitOrbit$finalModel)
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
predictionCtree <- predict(CT, newdata = testing)
confusionMatrix(testing$orbit_type, predictionCtree)



###
###
###

# Now let's try to predict orbit types based ONLY on lightcurves data:
set.seed(415)
RFfitOrbitLC <- train(training$orbit_type~.,  
                    data = training[seq(108,265)], 
                    method = "rf",
                    importance = TRUE)
save(RFfitOrbitLC, file = "RFfitOrbitLC.rda")
load("RFfitOrbitLC.rda")
predictionRFLC <- predict(RFfitOrbitLC, newdata = testing)
confusionMatrix(testing$orbit_type, predictionRFLC)

# plot variable importance
png(filename = "RfimpLC.png", width = 1000, height = 1000)
varImpPlot(RFfitOrbitLC$finalModel)
dev.off()


# Unique asteroids among our data
summary(factor(TrainData$objectnumber))
unique(TrainData$objectnumber)

