# dataset: students performance in Exams
# open dataset
open_data <- read.csv("data/StudentsPerformance.csv", header=TRUE, sep=",")
head(open_data)

# save 
save(open_data, file="data/open_data.RData")