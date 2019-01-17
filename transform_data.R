rm(list=ls())
# load the data
load("data/open_data.RData")

head(open_data)

# structure the dataset
str(open_data)

# changing column names
colnames(open_data) <- c("Gender", "RaceEthnicity", "ParentalLevelEducation","Lunch","TestPreparationCourse", 
                         "MathScore","ReadingScore","WritingScore")

# reorder 
open_data <- as.data.frame(cbind(open_data[,-c(5)], TestPreparationCourse = open_data$TestPreparationCourse))

# statistical informations summary
summary(open_data)

# detect missing values
sapply(open_data, function(x) all(is.na(x)))

save(open_data, file = "data/open_data.RData")