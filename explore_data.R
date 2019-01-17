#rm(list=ls())
load("data/open_data.RData")

# install.packages("ggplot2")
# install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

head(open_data)

# 1) General informations about all variables

# Gender
df_plot <- as.data.frame(table(open_data$Gender))
colnames(df_plot) <- c("Gender","Total")

plotGender<-ggplot(df_plot, aes(Gender, Total)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=Total), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_classic()

# RaceEthnicity
df_plot <- as.data.frame(table(open_data$RaceEthnicity))
colnames(df_plot) <- c("RaceEthnicity","Total")

plotRace<-ggplot(df_plot, aes(RaceEthnicity, Total)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=Total), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_classic()
  
# ParentalLevelEducation
df_plot <- as.data.frame(table(open_data$ParentalLevelEducation))
colnames(df_plot) <- c("ParentalLevelEducation","Total")

#  ordering
plotParent<-ggplot(df_plot, aes(x=reorder(ParentalLevelEducation, -Total), y = Total)) + 
  scale_x_discrete(name = "ParentLevelEducation") + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=Total), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_classic()

# Lunch
df_plot <- as.data.frame(table(open_data$Lunch))
colnames(df_plot) <- c("Lunch","Total")

plotLunch<-ggplot(df_plot, aes(Lunch, Total)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=Total), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_classic()

# show plots
grid.arrange(plotGender,plotRace, plotParent, plotLunch, nrow=2)

# TestPreparationCourse
df_plot <- as.data.frame(table(open_data$TestPreparationCourse))
colnames(df_plot) <- c("TestPreparationCourse","Total")

ggplot(df_plot, aes(TestPreparationCourse, Total)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=Total), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_classic()

# Plot scores (histograms)
plotMath <- ggplot(open_data, aes(MathScore)) + 
  geom_histogram() +
  theme_classic()

plotReading <- ggplot(open_data, aes(ReadingScore)) + 
  geom_histogram() +
  theme_classic()

plotWriting <- ggplot(open_data, aes(WritingScore)) + 
  geom_histogram() +
  theme_classic()

# show plots
grid.arrange(plotMath, plotReading, plotWriting, nrow=3)

# 2) Checking other variables

# MathScore
boxMath <- ggplot(open_data, aes(Gender, MathScore)) + geom_boxplot(fill="#aaaaaa") + theme_classic()

# ReadingScore
boxReading <- ggplot(open_data, aes(Gender, ReadingScore)) + geom_boxplot(fill="#cccccc") + theme_classic()

# WritingScore
boxWriting <- ggplot(open_data, aes(Gender, WritingScore)) + geom_boxplot(fill="#eeeeee") + theme_classic()

# show plots
grid.arrange(boxMath, boxReading, boxWriting, nrow=1)


######### ######### ######### CORRELATION ANALYSIS ######### ######### ######### ######### 

# transforming data
open_data$Gender <- as.integer(open_data$Gender)
open_data$RaceEthnicity <- as.integer(open_data$RaceEthnicity)
open_data$ParentalLevelEducation <- as.integer(open_data$ParentalLevelEducation)
open_data$Lunch <- as.integer(open_data$Lunch)
open_data$TestPreparationCourse <- as.integer(open_data$TestPreparationCourse)


#install.packages("corrplot")
library(corrplot)

# default: pearson method
corr_data <- cor(open_data)
corrplot(corr_data, method="number")


# variables correlation higher: MathScore, ReadingScore, WritingScore  
# correlation analysis

##### ##### ##### Math Score x Reading Score ##### ##### ##### 
# (5% significant)
pvalue <- 0.05

# normal distribution data? Yes <- NULL Hypothesis
# test for both variables
st_MathScore <- shapiro.test(open_data$MathScore)
st_MathScore
st_ReadingScore <- shapiro.test(open_data$ReadingScore) 
st_ReadingScore

# (defining using pearson or spearman)
# if true reject null hypothesis 
st_MathScore$p.value < pvalue
st_ReadingScore$p.value < pvalue

# spearmman method for correlation test
cor.test_Math_Reading <- cor.test(open_data$MathScore, open_data$ReadingScore, method = "spearman", exact = FALSE)
cor.test_Math_Reading

# if true correlation between X and Y is significant
cor.test_Math_Reading$p.value < 0.05

##### ##### #####  Math Score x Writing Score ##### ##### ##### ##### 

# normal distribution data? Yes <- NULL Hypothesis
# test for both variables
st_MathScore <- shapiro.test(open_data$MathScore)
st_MathScore
st_WritingScore <- shapiro.test(open_data$WritingScore) 
st_WritingScore

# (defining using pearson or spearman)
# if true reject null hypothesis 
st_MathScore$p.value > pvalue
st_WritingScore$p.value > pvalue

# spearmman method for correlation test
cor.test_Math_Writing <- cor.test(open_data$MathScore, open_data$WritingScore, method = "spearman",  exact = FALSE)
cor.test_Math_Writing


# if true correlation between X and Y is significant
cor.test_Math_Writing$p.value < 0.05

##### Reading Score x Writing Score

# normal distribution data? Yes <- NULL Hypothesis
# test for both variables
st_ReadingScore <- shapiro.test(open_data$ReadingScore)
st_ReadingScore
st_WritingScore <- shapiro.test(open_data$WritingScore) 
st_WritingScore

# (defining using pearson or spearman)
# if true reject null hypothesis
st_ReadingScore$p.value < pvalue
st_WritingScore$p.value < pvalue

# spearmman method for correlation test
cor.test_Read_Writing <- cor.test(open_data$ReadingScore, open_data$WritingScore, method = "spearman", exact = FALSE)
# biggest correlation
cor.test_Read_Writing

# if true correlation between X and Y is significant
cor.test_Read_Writing$p.value < 0.05