# load packages
library(randomForest)
library(party)
library(pROC)
library(caret)
library(grid)
library(mvtnorm)
library(modeltools)
library(stats4)
library(strucchange)
library(zoo)
library(sandwich)
library(lattice)
library(ggplot2)
library(pROC)
library(rpart)
library(rpart.plot)
library(ipred)
library(ggplot2)

# Dataset clean up
# Overview of dataset
sapply(train,function(x) sum(is.na(x)))
sapply(train,function(x) sum(x ==""))

# Deal with missing Embarked
table(train$Embarked)

# Fix missing Embarked with mode
train$Embarked[62]<-"S"
train$Embarked[830]<-"S"

# Double check
sapply(train,function(x) sum(x ==""))

# Collect title information
train$Title <- gsub("(.*, )|(\\..*)","",train$Name)
table(train$Sex, train$Title)
# Clean-up Title dataset
rare_title <- c('Dona','the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev','Jonkheer')
train$Title[train$Title == 'Lady']        <- 'Miss'
train$Title[train$Title == 'Mlle']        <- 'Miss' 
train$Title[train$Title == 'Ms']          <- 'Miss'
train$Title[train$Title == 'Mme']         <- 'Mrs'
train$Title[train$Title == 'Sir']         <- 'Mr'
train$Title[train$Title %in% rare_title]  <- 'Rare Title'

sapply(train, function(x) sum(x==''))

# Create new factor as Familysize
train[,"Familysize"] <- train[,"Parch"]+train[,"SibSp"]+1

# Age data clean-up
# Build a model predict missing age data
age.model <- rpart(Age ~ Pclass + SibSp + Parch + Fare + Title + Familysize, data=train[!is.na(train$Age), ], method='anova')
# Input predict result of age
train$Age[is.na(train$Age)] <- predict(age.model, train[is.na(train$Age), ])
# Check
sapply(train,function(x) sum(is.na(x)))

# Cabin
summary(train$Cabin)
train$Cabin <- as.character(train$Cabin)
n <- nrow(train)
for (i in 1:n) {
  if (train$Cabin[i]=='') train$Cabin[i] <-'U'
  else if (grepl('A', train$Cabin[i])) train$Cabin[i] <- 'A'
  else if (grepl('B', train$Cabin[i])) train$Cabin[i] <- 'B'
  else if (grepl('C', train$Cabin[i])) train$Cabin[i] <- 'C'
  else if (grepl('D', train$Cabin[i])) train$Cabin[i] <- 'D'
  else if (grepl('E', train$Cabin[i])) train$Cabin[i] <- 'E'
  else if (grepl('F', train$Cabin[i])) train$Cabin[i] <- 'F'
  else train$Cabin[i] <- 'FG' 
}
train$Cabin <- factor(train$Cabin)

# Ticket
summary(train$Ticket)
ticket.count <- aggregate(train$Ticket, by = list(train$Ticket), function(x) sum(!is.na(x)))
train$TicketCount <- apply(train, 1, function(x) ticket.count[which(ticket.count[, 1] == x['Ticket']), 2])
train$TicketCount <- factor(sapply(train$TicketCount, function(x) ifelse(x > 1, 'Shared Ticket', 'Unique Ticket')))

sapply(train, function(x) sum(x==''))

# Cut out all missing data without data of Age and stored as a new dataset
cleandataset <- na.omit(train)

# Transfer categories into factors

cleandataset$Survived <- factor(cleandataset$Survived)
cleandataset$Pclass <- factor(cleandataset$Pclass)
cleandataset$Embarked <- factor(cleandataset$Embarked)
cleandataset$Title <- factor(cleandataset$Title)

# Evaluation of variables
# Correlation between Survived and Pclass

ggplot(data = cleandataset[1:nrow(cleandataset),],aes(x = Pclass, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Pclass') + 
  ylab('Passengers') + 
  ggtitle('Correlation') +
  scale_fill_manual(values = c("red","blue")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5)
# Taken

# Correlation between Survived and sex

ggplot(data = cleandataset[1:nrow(cleandataset),],aes(x = Sex, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Sex') + 
  ylab('Passengers') + 
  ggtitle('Correlation') +
  scale_fill_manual(values = c("red","blue")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5)
# Taken

# Correlation between Survived and Age

ggplot(cleandataset[!is.na(cleandataset$Survived),],aes(Age,color=Survived))+
  geom_line(aes(label=..count..), stat = 'bin', binwidth=10, size=2)  + 
  labs(title = "Age and Survived", x = "Age", y = "Passengers", fill = "Survived")
# Taken

# Correlation between Survived and SibSp

ggplot(data = cleandataset[1:nrow(cleandataset),],aes(x = SibSp, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('SibSp') + 
  ylab('Passengers') + 
  ggtitle('Correlation') +
  scale_fill_manual(values = c("red","blue")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5)
# Taken

# Correlation between Survived and Parch

ggplot(data = cleandataset[1:nrow(cleandataset),],aes(x = Parch, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Parch') + 
  ylab('Passengers') + 
  ggtitle('Correlation') +
  scale_fill_manual(values = c("red","blue")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5)
# Taken

# Correlation between Survived and Ticketcount

ggplot(data = cleandataset[1:nrow(cleandataset),],aes(x = TicketCount, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Ticket') + 
  ylab('Passengers') + 
  ggtitle('Correlation') +
  scale_fill_manual(values = c("red","blue")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5)
# Taken

# Correlation between Survived and Cabin

ggplot(data = cleandataset[1:nrow(cleandataset),],aes(x = Cabin, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Cabin') + 
  ylab('Passengers') + 
  ggtitle('Correlation') +
  scale_fill_manual(values = c("red","blue")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5)
# Taken

# Correlation between Survived and Embarked

ggplot(data = cleandataset[1:nrow(cleandataset),],aes(x = Embarked, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Embarket') + 
  ylab('Passengers') + 
  ggtitle('Correlation') +
  scale_fill_manual(values = c("red","blue")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5)
# Taken

# Correlation between Survived and Title

ggplot(data = cleandataset[1:nrow(cleandataset),],aes(x = Title, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Title') + 
  ylab('Passengers') + 
  ggtitle('Correlation') +
  scale_fill_manual(values = c("red","blue")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5)
# Taken

# Correlation between Survived and Familysize

ggplot(data = cleandataset[1:nrow(cleandataset),],aes(x = Familysize, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Familysize') + 
  ylab('Passengers') + 
  ggtitle('Correlation') +
  scale_fill_manual(values = c("red","blue")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5)
# Taken

# Train dataset and test dataset
data <- cleandataset[,c('Survived','Pclass','Age','Sex','Fare','Embarked','Parch','SibSp','Title','TicketCount','Familysize','Cabin')]
n<- nrow(data)
n_train<- round(0.8*n)

set.seed(123)
train_indices<- sample(1:n, n_train)
data.train <- data[train_indices,]
data.test <- data[-train_indices,]

# Model build

# Random forest
set.seed(123)
model <- cforest(formula = Survived~., data = data.train, controls = cforest_unbiased(ntree=2000,mtry=3))
print(model)
# Classification tree
model_class <- rpart(formula = Survived~.,data = data.train)
rpart.plot(model_class)
# bagging
model_bagging <- bagging(formula = Survived~., data = data.train)
print(model_bagging)

# Model test

# Random forest
prediction_model <- predict(object=model, newdata = data.test,OOB = TRUE,type = "response" )
print(prediction_model)
# Class tree
prediction_class <- predict(object = model_class, newdata = data.test, type = 'class')
print(prediction_class)
summary(prediction_class)
# bagging
prediction_bagging <- predict(object = model_bagging, newdata = data.test)
summary(prediction_bagging)

# confusion matrix
confusionMatrix(data = prediction_model, reference = data.test$Survived) # RF
confusionMatrix(data = prediction_class, reference = data.test$Survived) # class
confusionMatrix(data = prediction_bagging, reference = data.test$Survived) # bagging

# calculate AUC
auc(data.test$Survived,as.ordered(prediction_model), type='reponse') # RF
auc(data.test$Survived,as.ordered(prediction_class)) # Class
auc(data.test$Survived,as.ordered(prediction_bagging)) # bagging

# Test data clean up
sapply(test, function(x) sum(x==''))

# Collect title information
test$Title <- gsub("(.*, )|(\\..*)","",test$Name)
table(test$Sex, test$Title)
# Clean-up Title dataset
rare_title <- c('Dona','the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev','Jonkheer')
test$Title[test$Title == 'Lady']        <- 'Miss'
test$Title[test$Title == 'Mlle']        <- 'Miss' 
test$Title[test$Title == 'Ms']          <- 'Miss'
test$Title[test$Title == 'Mme']         <- 'Mrs'
test$Title[test$Title == 'Sir']         <- 'Mr'
test$Title[test$Title %in% rare_title]  <- 'Rare Title'

test[,'Familysize'] <- test[,'Parch'] + test[,'SibSp'] + 1

# Age data clean-up
# Build a model predict missing age data
age.model <- rpart(Age ~ Pclass + SibSp + Parch + Fare + Title + Familysize, data=test[!is.na(test$Age), ], method='anova')
# Input predict result of age
test$Age[is.na(test$Age)] <- predict(age.model, test[is.na(test$Age), ])


# Cabin
n <- nrow(test)
test$Cabin <- as.character(test$Cabin)
for (i in 1:n) {
  if (test$Cabin[i]=='') test$Cabin[i] <-'U'
  else if (grepl('A', test$Cabin[i])) test$Cabin[i] <- 'A'
  else if (grepl('B', test$Cabin[i])) test$Cabin[i] <- 'B'
  else if (grepl('C', test$Cabin[i])) test$Cabin[i] <- 'C'
  else if (grepl('D', test$Cabin[i])) test$Cabin[i] <- 'D'
  else if (grepl('E', test$Cabin[i])) test$Cabin[i] <- 'E'
  else if (grepl('F', test$Cabin[i])) test$Cabin[i] <- 'F'
  else test$Cabin[i] <- 'FG' 
}
test$Cabin <- factor(test$Cabin)

sapply(test, function(x) sum(x==''))

# Ticket
ticket.count <- aggregate(test$Ticket, by = list(test$Ticket), function(x) sum(!is.na(x)))
test$TicketCount <- apply(test, 1, function(x) ticket.count[which(ticket.count[, 1] == x['Ticket']), 2])
test$TicketCount <- factor(sapply(test$TicketCount, function(x) ifelse(x > 1, 'Shared Ticket', 'Unique Ticket')))


test$Pclass <- factor(test$Pclass)
test$Embarked <- factor(test$Embarked)
test$Title <- factor(test$Title)

test_new <- test[,c('Pclass','Age','Sex','Fare','Embarked','Parch','SibSp','Title','TicketCount','Familysize','Cabin')]
summary(test_new)
summary(data.test)

prediction_final <- predict(object = model, newdata = test_new, OOB = TRUE, type = 'response')
summary(prediction_final)
output <- data.frame(PassengerId = test$PassengerId, Survived = prediction_final)
write.csv(output, file='C:/Users/bodme/Desktop/hult/big data/assignment/all/predict3.csv',row.names = FALSE)

