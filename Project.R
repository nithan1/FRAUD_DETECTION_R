# CREDIT CARD FRAUD DETECTION

install.packages("dplyr")
install.packages("caret")
install.packages("e1071")
install.packages("ggplot2")
install.packages("caTools")
install.packages("ROSE")
install.packages("smotefamily")
install.packages("rpart")
install.packages("rpart.plot")


library(dplyr)
library(caret)
library(ggplot2)
library(caTools)
library(ROSE)
library(smotefamily)
library(rpart)
library(rpart.plot)

#Loading the dataset
credit<-read.csv("C:/Users/Nitha/OneDrive/Desktop/creditcard.csv")

View(credit)
str(credit)

# Convert class to a factor variable
credit$Class <- factor(credit$Class, levels =  c(0,1))

summary(credit)
sum(is.null(credit))

table(credit$Class)

prop.table(table(credit$Class))

#Pie Chart 
labels <- c("legit","fraud")
labels <- paste(labels, round(100*prop.table(table(credit$Class)),2))
labels <- paste(labels ,"%")

pie(table(credit$Class),labels, col = c("red" , "yellow"),
    main = "Pie chart of Credit Card Transactions")


# model predictions
predictions <- rep.int(0,nrow(credit))
predictions <- factor(predictions, levels= c(0,1))
confusionMatrix(data = predictions, reference = credit$Class)


#Creating training and test sets 
set.seed(123)
data_sample = sample.split(credit$Class,SplitRatio= 0.80)
train_data = subset(credit,data_sample == TRUE)
test_data = subset(credit, data_sample == FALSE)

str(train_data)
str(test_data)

# ROS and RUS

new <- nrow(train_data)
fraction_fraud_new <-0.50

sampling_result <-ovun.sample(Class ~ .,
                              data = train_data,
                              method = "both",
                              N = new,
                              p = fraction_fraud_new,
                              seed =123)

sampled_credit <- sampling_result$data
table(sampled_credit$Class)
prop.table(table(sampled_credit$Class))

ggplot(data = sampled_credit , aes(x = V1, y = V2, col =Class))+
  geom_point(position = position_jitter(width = 0.2))+
  theme_bw()+
  scale_color_manual(values = c('orange','red'))


#Using SMOTE 

table(train_data$Class)

#Set the number of fraud and legitimate cases, and the desired percentage of legitmate cases

n0 <- 22750
n1 <- 35
r0 <- 0.6

#Calculate the values for the dup_size parameter of SMOTE

ntimes <- ((1 - r0)/ r0) *(n0 / n1) - 1
ntimes

smote_output = SMOTE(X = train_data[ , -c(1,31)],
                     target = train_data$Class,
                     K = 5,
                     dup_size = ntimes)

credit_smote <- smote_output$data

colnames(credit_smote)[30] <-"Class"

prop.table(table(credit_smote$Class))

#Class distribution for original dataset

ggplot(train_data, aes(x = V1, y = V2, color = Class))+
  geom_point()+
  scale_color_manual(values = c('orange','red'))

# Class distribution for original dataset

ggplot(credit_smote, aes(x = V1, y = V2, color = Class))+
  geom_point()+
  scale_color_manual(values = c('orange','red'))


#Decision tree
CART_model <- rpart(Class ~ ., credit_smote)
rpart.plot(CART_model, extra = 0 , type = 5, tweak = 1.2)

#Predict fraud Classes
predicted_val <-predict(CART_model, test_data, type ='class')
predicted_val


#Build Confusion Matrix
confusionMatrix(predicted_val, test_data$Class)
