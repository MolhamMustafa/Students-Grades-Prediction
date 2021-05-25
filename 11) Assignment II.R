# mat data = Mathematics subjects while pot = Portuguese language
rm(list = ls())
setwd('D:/University of Huddersfield/Modules/4) Data Mining/Data Sets')

# reading the data
#----------------------------------------
dataMat <- read.csv('student-mat.csv', sep = ';')
dataPor <- read.csv('student-por.csv', sep = ';')

# add subject column then combine the 2 data sets
dataMat$subject = 'Math'
dataPor$subject = 'Port'

# check if headers of both data are equal or not
colnames(dataPor) == colnames(dataMat)

# Combine data sets
data <- rbind(dataMat,dataPor)

summary(data)
numeric_features <- c(3,7,8,13,14,15,24:33)

# get the data dimensions
#------------------------------------
print(paste('number of Math data columns: ',dim(dataMat)[2]))
print(paste('number of Math data rows', dim(dataMat)[1]))

print(paste('number of Portuguese data columns', dim(dataPor)[2]))
print(paste('number of Portuguese data rows', dim(dataPor)[1]))
# both data have the same number of columns while different number of rows

# Data exploration
#------------------------------------

# Missing data
#---------------------------------------
# check missing data in Math data set
for(i in colnames(data)){
  if(NA %in% data[,i]){
    print(paste('column',i,'contains missing values'))
  } else{
    print(paste('no missing data in column',i))
  }
}
# check ? in math data set
for(i in colnames(data)){
  if('?' %in% dataMat[,i]){
    print(paste('column',i,'contains ? values'))
  } else{
    print(paste('no ? values in column',i))
  }
}


# Unique data for each feature
#-----------------------------------------------
for(i in colnames(data)){
  print(paste('column', i, 'unique values are:'))
  print(unique(dataMat[,i]))
}
# conclusion: all data makes sense


# co-variance
cov(data[,numeric_features])
# Correlation
cor_table <- as.data.frame(cor(data[,numeric_features],
                               use="complete.obs", method="kendall"))[length(numeric_features)]

summary(data[,numeric_features])
main_features <- c(33,32,31,7,8,14,15)
for(i in main_features){print(paste(colnames(data)[i],':',sd(data[,i])))}

# frequency tables
table(data$G1)
table(data$G2)
table(data$G3)

# Histograms
#--------------------------------------------
library(ggplot2)


hisG3 <- ggplot(dataMat, aes(x = G3))
hisG3 + geom_histogram(binwidth = 1, fill = 'green4', color = 'Black') + 
  ggtitle('Final Period Grade (G3) Histogram') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15),
        plot.title = element_text(color='DarkBlue',
                                  size = 20,
                                  family = 'Courier'))

his_G2 <- ggplot(dataMat, aes(x = G2))
his_G2 + geom_histogram(binwidth = 1, fill = 'green4', color = 'Black') + 
  ggtitle('Second Period Grade (G2) Histogram') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15),
        plot.title = element_text(color='DarkBlue',
                                  size = 20,
                                  family = 'Courier'))

his_G1 <- ggplot(dataMat, aes(x = G1))
his_G1 + geom_histogram(binwidth = 1, fill = 'green4', color = 'Black') + 
  ggtitle('First Period Grade (G1) Histogram') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15),
        plot.title = element_text(color='DarkBlue',
                                  size = 20,
                                  family = 'Courier'))

his_failures <- ggplot(dataMat, aes(x = failures))
his_failures + geom_histogram(binwidth = 1, fill = 'green4', color = 'Black') + 
  ggtitle('Failures Histogram') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15),
        plot.title = element_text(color='DarkBlue',
                                  size = 20,
                                  family = 'Courier'))

his_absences <- ggplot(dataMat, aes(x = absences))
his_absences + geom_histogram(binwidth = 1, fill = 'green4', color = 'Black') + 
  ggtitle('Failures Histogram') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15),
        plot.title = element_text(color='DarkBlue',
                                  size = 20,
                                  family = 'Courier'))

his_absences <- ggplot(dataMat, aes(x = absences))
his_absences + geom_histogram(binwidth = 1, fill = 'green4', color = 'Black') + 
  ggtitle('Failures Histogram') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15),
        plot.title = element_text(color='DarkBlue',
                                  size = 20,
                                  family = 'Courier'))

his_studytime <- ggplot(dataMat, aes(x = studytime))
his_studytime + geom_histogram(binwidth = 1, fill = 'green4', color = 'Black') + 
  ggtitle('Study Time Histogram') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15),
        plot.title = element_text(color='DarkBlue',
                                  size = 20,
                                  family = 'Courier'))

his_studytime <- ggplot(dataMat, aes(x = studytime))
his_studytime + geom_histogram(binwidth = 1, fill = 'green4', color = 'Black') + 
  ggtitle('Study Time Histogram') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15),
        plot.title = element_text(color='DarkBlue',
                                  size = 20,
                                  family = 'Courier'))

his_Medu <- ggplot(dataMat, aes(x = Medu))
his_Medu + geom_histogram(binwidth = 1, fill = 'green4', color = 'Black') + 
  ggtitle('Mother Education Histogram') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15),
        plot.title = element_text(color='DarkBlue',
                                  size = 20,
                                  family = 'Courier'))

his_Fedu <- ggplot(dataMat, aes(x = Fedu))
his_Fedu + geom_histogram(binwidth = 1, fill = 'green4', color = 'Black') + 
  ggtitle('Father Education Histogram') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15),
        plot.title = element_text(color='DarkBlue',
                                  size = 20,
                                  family = 'Courier'))


his_Walc <- ggplot(dataMat, aes(x = Walc))
his_Walc + geom_histogram(binwidth = 1, fill = 'green4', color = 'Black') + 
  ggtitle('Working Day Alcohol Histogram') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15),
        plot.title = element_text(color='DarkBlue',
                                  size = 20,
                                  family = 'Courier'))

his_Dalc <- ggplot(dataMat, aes(x = Dalc))
his_Dalc + geom_histogram(binwidth = 1, fill = 'green4', color = 'Black') + 
  ggtitle('Weekend Day Alcohol Histogram') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15),
        plot.title = element_text(color='DarkBlue',
                                  size = 20,
                                  family = 'Courier'))

# Plots for descriptive analysis
# Relation Ships
#-------------------------------------

# Numeric data

# G2 (Positive Relationship --needs cleansing--)
relMat <- ggplot(data = dataMat,aes(x=G2, y = G3))
relPor <- ggplot(data = dataPor,aes(x=G2, y = G3))
relAll <- ggplot(data = data,aes(x=G2, y = G3))
 
relMat + geom_point() + geom_smooth(method = 'lm') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15)) +
  ggtitle('G2, G3 Relationship')

relPor + geom_point() + geom_smooth(method = 'lm') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15)) +
  ggtitle('G2, G3 Relationship')

relAll + geom_point() + geom_smooth(method = 'lm') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15)) +
  ggtitle('G2, G3 Relationship')


# Failure (Negative Relationship --high noise--)
relMat <- ggplot(data = dataMat,aes(x=failures, y = G3))
relPor <- ggplot(data = dataPor,aes(x=failures, y = G3))

relMat + geom_point() + geom_smooth(method = 'lm')
relPor + geom_point() + geom_smooth(method = 'lm')


# Study time (Slight Positive relationship)
relMat <- ggplot(data = dataMat,aes(x=studytime, y = G3))
relPor <- ggplot(data = dataPor,aes(x=studytime, y = G3))

relMat + geom_point() + geom_smooth(method = 'lm')
relPor + geom_point() + geom_smooth(method = 'lm')


# Quality of family relationship (Almost no relationship)
relMat <- ggplot(data = dataMat,aes(x=famrel, y = G3))
relPor <- ggplot(data = dataPor,aes(x=famrel, y = G3))

relMat + geom_point() + geom_smooth(method = 'lm')
relPor + geom_point() + geom_smooth(method = 'lm')

# Free time after school (Almost no relationship)
relMat <- ggplot(data = dataMat,aes(x=freetime , y = G3))
relPor <- ggplot(data = dataPor,aes(x=freetime , y = G3))

relMat + geom_point() + geom_smooth(method = 'lm')
relPor + geom_point() + geom_smooth(method = 'lm')

# Going out with friends (Almost no relationship)
relMat <- ggplot(data = dataMat,aes(x=goout , y = G3))
relPor <- ggplot(data = dataPor,aes(x=goout , y = G3))

relMat + geom_point() + geom_smooth(method = 'lm')
relPor + geom_point() + geom_smooth(method = 'lm')

# Working days alcohol (Slight negative relationship --noise exists--)
relMat <- ggplot(data = dataMat,aes(x=Dalc , y = G3))
relPor <- ggplot(data = dataPor,aes(x=Dalc , y = G3))

relMat + geom_point() + geom_smooth(method = 'lm')
relPor + geom_point() + geom_smooth(method = 'lm')

# Week ends alcohol (Almost no relationship)
relMat <- ggplot(data = dataMat,aes(x=Walc , y = G3))
relPor <- ggplot(data = dataPor,aes(x=Walc , y = G3))

relMat + geom_point() + geom_smooth(method = 'lm')
relPor + geom_point() + geom_smooth(method = 'lm')

# Health ends alcohol (Almost no relationship)
relMat <- ggplot(data = dataMat,aes(x=health , y = G3))
relPor <- ggplot(data = dataPor,aes(x=health , y = G3))

relMat + geom_point() + geom_smooth(method = 'lm')
relPor + geom_point() + geom_smooth(method = 'lm')

# Absence (Very slight negative relationship --noise exists--)
relMat <- ggplot(data = dataMat,aes(x=absences , y = G3))
relPor <- ggplot(data = dataPor,aes(x=absences , y = G3))

relMat + geom_point() + geom_smooth(method = 'lm')
relPor + geom_point() + geom_smooth(method = 'lm')

# age (Negative relationship!!)
relMat <- ggplot(data = dataMat,aes(x=age , y = G3))
relPor <- ggplot(data = dataPor,aes(x=age , y = G3))

relMat + geom_point() + geom_smooth(method = 'lm')
relPor + geom_point() + geom_smooth(method = 'lm')


# Categorical data

# Sex (M has slight better G3 in Math only)
boxMat <- ggplot(data = dataMat, aes(x = sex, y = G3, color = sex))
boxPor <- ggplot(data = dataPor, aes(x = sex, y = G3, color = sex))

boxMat + geom_boxplot() + geom_jitter()
boxPor + geom_boxplot() + geom_jitter()

# Address (Urban students has better grades)
boxMat <- ggplot(data = dataMat, aes(x = address, y = G3, color = address))
boxPor <- ggplot(data = dataPor, aes(x = address, y = G3, color = address))

boxMat + geom_boxplot() + geom_jitter()
boxPor + geom_boxplot() + geom_jitter()


# PCA
#-------------------------
library(factoextra)

#performing principle component
pc<-prcomp(data[,c('G1','G2','G3','failures','age',
                   'absences','health','studytime',
                   'famrel','freetime')],center=TRUE, scale. = TRUE)
pc$rotation


pc$center
pc$scale

fviz_eig(pc)
fviz_pca_var(pc,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


fviz_pca_var(pc, axes=c(2,3),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



# Regression Random Forest:
#----------------------------------------
library(MASS)
library(randomForest)

# Creating the model data
data_prep <- data[,numeric_features]
colnames(data_prep)
ncol(data_prep)


# split the data into train and test
set.seed(111)
ind <- sample(2,nrow(data), replace = TRUE, prob = c(0.7,0.3))
train <- data[ind == 1,]
test <- data[ind == 2,]

train$G3 <- factor(train$G3)
test$G3 <- factor(test$G3)
# build the model
set.seed(111)
rf<-randomForest(G3 ~., data = train, importance = T, mtry = 10, ntree = 300, replace = T)
rf # the % var explained should be close to 100%
# mean of squares residuals means how far your prediction is from the results

# test error
# better confusion matrix
library(caret)
pred <- predict(rf, newdata = test[,-ncol(test)])
confusionMatrix(pred, factor(test$G3))

pred <- predict(rf, newdata = train[,-ncol(test)])
confusionMatrix(pred, factor(train$G3))
# if most of the numbers are being presented in diagonal while others are 0s so the model is good
# the Accuracy & CI should be high, P-Value should be low
# the higher sensitivity the better (above 85% is good)
# Both sensitivity and Specificity should be close to 1
importance(rf)
varImpPlot(rf)
# the top one in the graph is the most important
plot(rf)

plot(rf, uniform = T, main = 'Studens G3')


test.data<-predict(rf, newdata = test)
test.data <- as.data.frame(test.data)
Diff_df = as.numeric(test$G3) - as.numeric(unlist(test.data))
Diff_df = as.data.frame(Diff_df)

table(Diff_df)
sum(Diff_df)/sum(as.numeric(test$G3))

his_Diff <- ggplot(Diff_df, aes(x = Diff_df))
his_Diff + geom_histogram(binwidth = 1, fill = 'green4', color = 'Black') + 
  ggtitle('Differences Between Prediction and Actual Data') + 
  theme(axis.title.x = element_text(color = 'Blue', size = 15),
        axis.title.y = element_text(color = 'Blue', size = 15),
        plot.title = element_text(color='DarkBlue',
                                  size = 20,
                                  family = 'Courier')) + 
  xlab('Deviation')

pred <- predict(rf, newdata = train[,-ncol(train)])
confusionMatrix(pred, factor(train$G3))

# Classification Random Forest:
#---------------------------------------------------
set.seed(222)
rf<-randomForest(G3 ~., data = train, importance=T, ntree = 501, mtry = 4)
# the default of number of trees (ntree) is 500
# mtry set the number of variables per each tree that are been chosen randomly by the model
# estimate error

pie(table(dataMat$G3))

# grouping G1 grades into 4 groups
dataMat$G1_Rank <- 0
dataMat[dataMat$G1 <= 5,][,'G1_Rank'] <- '0-5'
dataMat[dataMat$G1 > 5 & dataMat$G1 <= 10,][,'G1_Rank'] <- '6-10'
dataMat[dataMat$G1 > 10 & dataMat$G1 <= 15,][,'G1_Rank'] <- '10-15'
dataMat[dataMat$G1 > 15,][,'G1_Rank'] <- '15-20'
pie(table(dataMat$G1_Rank))

dataMat$G2_Rank <- 0
dataMat[dataMat$G2 <= 5,][,'G2_Rank'] <- '0-5'
dataMat[dataMat$G2 > 5 & dataMat$G2 <= 10,][,'G2_Rank'] <- '6-10'
dataMat[dataMat$G2 > 10 & dataMat$G2 <= 15,][,'G2_Rank'] <- '10-15'
dataMat[dataMat$G2 > 15,][,'G2_Rank'] <- '15-20'
pie(table(dataMat$G2_Rank))


dataMat$G3_Rank <- 0
dataMat[dataMat$G3 <= 5,][,'G3_Rank'] <- '0-5'
dataMat[dataMat$G3 > 5 & dataMat$G3 <= 10,][,'G3_Rank'] <- '6-10'
dataMat[dataMat$G3 > 10 & dataMat$G3 <= 15,][,'G3_Rank'] <- '10-15'
dataMat[dataMat$G3 > 15,][,'G3_Rank'] <- '15-20'
pie(table(dataMat$G3_Rank))



# convert data into dataframe
#------------------------------------
dfMat <- as.data.frame(dataMat)
dfMat





# write data in csv file
pathMat = ('D:/University of Huddersfield/Modules/4) Data Mining/Assignments/Assignment II/Math Data.csv')
write.csv(dataMat,pathMat, row.names = FALSE)

pathPor = ('D:/University of Huddersfield/Modules/4) Data Mining/Assignments/Assignment II/Portuguese Data.csv')
write.csv(dataPor,pathPor, row.names = FALSE)



# different trials
#-------------------------------------
for(i in seq(dim(dataMat)[1])){
  if(dataMat$G1[i] >= 0 & dataMat$G1[i] <= 5){
    dataMat$G1_Rank[i] = 'from 0 to 5'
  } else {
    if(dataMat$G1[i] > 5 & dataMat$G1[i] <= 10){
      dataMat$G1_Rank[i] = 'from 6 to 10'
    } else {
      if(dataMat$G1[i] > 10 & dataMat$G1[i] <= 15){
        dataMat$G1_Rank[i] = 'from 11 to 15'
      } else {
        if(dataMat$G1[i] > 15){
          dataMat$G1_Rank = 'from 16 to 20'
        }
      }
    }
  }
}
dataMat[dataMat$G1 >6 & dataMat$G1 <9,][,c('G1','G1_Rank'),]