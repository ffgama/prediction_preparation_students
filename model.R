# load the dataset
load("data/open_data.RData")

source("unbalanced_target.R")

# 70% training
size_sample <-  floor(0.7 * nrow(open_data))

set.seed(123)
# random partitioning
train_ind <- sample(seq_len(nrow(open_data)), size_sample)

# training x test
data_train <- open_data[train_ind, ]
data_test <- open_data[-train_ind, ]

#install.packages("mlr")
library(mlr)

# create tasks
trainTask <- makeClassifTask(data = data_train, target = "TestPreparationCourse")
testTask <- makeClassifTask(data = data_test, target = "TestPreparationCourse")

# normalize the variables
trainTask <- normalizeFeatures(trainTask, method = "standardize")
testTask <- normalizeFeatures(testTask, method = "standardize")

#install.packages("randomForest")
library(randomForest)

model_rf <- randomForest(TestPreparationCourse ~ ., data = data_train, importance = TRUE, ntree = 200)
model_rf

# importance variables
varImpPlot(model_rf)

# remove the features 
trainTask <- dropFeatures(task = trainTask,features = c("Lunch"))

rf <- makeLearner("classif.randomForest", predict.type = "response", par.vals = list(mtry = 3, ntree = 200))


# set 10 fold cross validation
# set.seed(123)
# cv <- makeResampleDesc("CV",iters = 10L)
# r <- resample(learner = rf, task = trainTask, resampling = cv, measures = list(acc), show.info = T)


# gridsearch (tuning parameters)
parameters <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 6),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)

ctrl <- makeTuneControlRandom(maxit = 50L)

# set 10 fold cross validation
cv <- makeResampleDesc("CV",iters = 10L)

# cv accuracy
rf_tune <- tuneParams(learner = rf, resampling = cv, task = trainTask, par.set = parameters, control = ctrl, show.info = TRUE)

# using hyperparameters
rf_tuning <- setHyperPars(rf, par.vals = rf_tune$x)

# train a model with hyperparameters
new_rf <- mlr::train(rf_tuning, trainTask)

# predictions
pred <- predict(new_rf, testTask)

df<-data.frame(pred$data)

library(caret)
cm <- confusionMatrix(df$truth,df$response, positive = "none")
cm$overall[1]

roc.curve(df$truth, df$response, plotit = T)
