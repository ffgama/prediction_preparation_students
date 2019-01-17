# load the dataset
load("data/open_data.RData")

#install.packages("caret")
library(caret)

head(open_data)

# rfe parameters
ctr_rfe <- rfeControl(functions=rfFuncs, method="cv", number=10)

set.seed(123)
# rfe algorithm
rfe_algorithm <- rfe(open_data[,1:(ncol(open_data)-1)], open_data[,c(ncol(open_data))], sizes=c(1:(ncol(open_data)-1)), 
                     rfeControl=ctr_rfe)

# show summarized results
print(rfe_algorithm)

# list the chosen features
predictors(rfe_algorithm)

# plot the results
plot(rfe_algorithm, type=c("g", "o"))