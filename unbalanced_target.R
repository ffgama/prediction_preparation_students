# load the dataset
load("data/open_data.RData")

#install.packages("ROSE")
library(ROSE)

# unbalanced target variable (proportion)
prop.table(table(open_data$TestPreparationCourse))

# over and undersampling
balanced_target <- ovun.sample(TestPreparationCourse ~ ., data = open_data, method = "both",N = 1350, seed = 1)$data

open_data <- balanced_target

prop.table(table(open_data$TestPreparationCourse))