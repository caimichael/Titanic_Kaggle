setwd("/Users/michaelcai/Desktop/Leada")

titanic_train <- read.csv('titanic_train.csv', header = T, stringsAsFactors=F)
titanic_test <- read.csv('titanic_test.csv', header = T, stringsAsFactors=F)

#Setting Sex: Female to 1 and Male to 0
# titanic_train$Sex = gsub(1, "female", titanic_train$Sex)
# titanic_train$Sex = gsub(0, "male", titanic_train$Sex)

#How many missing age values are in the Train dataset 
#(Or just do summary(titanic_train$Age))
# count <- 0
# for (x in titanic_train$Age)
# {
#  if (is.na(x)){
#    count <- count + 1
#  }
# }
# count

#Average age of all of the passengers in the Train dataset
#mean(titanic_train$Age, na.rm = TRUE)

#Density Plots for various variables
#plot(density(titanic_train$Age, na.rm=T))
#plot(density(titanic_train$Fare, na.rm=T)) #how do you change the x-axis scale?

#Separating out the survivors and non-survivors into two separate data frames
#titanic.sub1 <- subset(titanic_train, titanic_train$Survived==1)
#titanic.sub2 <- subset(titanic_train, titanic_train$Survived==0)

#Finding the proportions of various classes that survived
#prop.table(table(titanic_train$Pclass, titanic_train$Survived),1)

#Finding the proportions of females vs. males that survived
#prop.table(table(titanic_train$Sex, titanic_train$Survived),1)

#titanic_test$Survived <- 0
#titanic_test$Survived[titanic_test$Sex == 'female'] <- 1

#Bar plot of Female vs. Male subsetted with Survival
#counts <- table(titanic_train$Survived, titanic_train$Sex)
#barplot(counts, xlab = "Gender", ylab = "Number of People", main = "survived and deceased between male and female")

#Creating a new field called Child and filtering from Age (to give nicer categorical bins)
#titanic_train$Child <- 0
#titanic_train$Child[titanic_train$Age < 18] <- 1

#Table of whether or not a child, sex, and survived
#aggregate(Survived ~ Child + Sex, data=titanic_train, FUN=sum)
#aggregate(Survived ~ Child + Sex, data=titanic_train, FUN=length)
#aggregate(Survived ~ Child + Sex, data=titanic_train, FUN=function(x) {sum(x)/length(x)})

#titanic_train$Fare2 <- '30+'
#titanic_train$Fare2[titanic_train$Fare < 30 & titanic_train$Fare >= 20] <- '20-30'
#titanic_train$Fare2[titanic_train$Fare < 20 & titanic_train$Fare >= 10] <- '10-20'
#titanic_train$Fare2[titanic_train$Fare< 10] <- '<10'
#aggregate(Survived ~ Fare2 + Pclass + Sex, data=titanic_train, FUN=function(x) {sum(x)/length(x)})
#Females in the Pclass 3 w/ Fares above 20 did not seem to fair nearly as well

#test_submit2.csv
#titanic_test$Survived <- 0
#titanic_test$Survived[titanic_test$Sex == 'female'] <- 1
#titanic_test$Survived[titanic_test$Sex == 'female' & titanic_test$Pclass == 3 & titanic_test$Fare >=20] <- 0

#Up until now, we were just manually picking bins/observing survival rates
#But now, we're going to move on to more sophisticated obs. using Decision trees
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=titanic_train,method = "class")
fancyRpartPlot(fit)

#test_submit3_dtree.csv
Prediction <- predict(fit, titanic_test, type="class")
submit <- data.frame(PassengerId = titanic_test$PassengerId, Survived = Prediction)
#write.csv(submit, file= "test_submit3_dtree.csv", row.names=F)

#Setting my own controls, interactive snipping of branches for the user
# fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=titanic_train,
#              method="class", control=rpart.control(minsplit = 40))
# new.fit <- prp(fit,snip=TRUE)$obj
# fancyRpartPlot(new.fit)

#Feature Engineering: Trying to parse more meaning out of people's titles
titanic_test$Survived <- NA #resetting the Survived column from previous submits
combi <- rbind(titanic_train, titanic_test) 
#^Combining the train and test datasets so the feature engineering will affect both

strsplit(combi$Name[1], split='[,.]') #returns the broken up string components 
strsplit(combi$Name[1], split='[,.]')[[1]][2] #returns just the person's title

#Applying this splitting operation to all of the values in the Name column to grab all of the titles
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ','', combi$Title) #substituting out the space in the front
table(combi$Title) 

#Because there are some rare titles, we are combining them into comparable "feature" categories
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

#Creating a feature category for Family Size (and indicating FamilyID as well)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <-paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
#overwriting the unexpected small families that weren't originally categorized as 'Small'
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

#Splitting back into the respective datasets to do more predictions with newly engineered features
titanic_train <- combi[1:891,]
titanic_test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + 
            FamilyID, data = titanic_train, method = "class")
fancyRpartPlot(fit)
#Decision trees are biased to favor factors with many levels

#test_submit4_feateng.csv
Prediction <- predict(fit, titanic_test, type="class")
submit <- data.frame(PassengerId = titanic_test$PassengerId, Survived = Prediction)
#write.csv(submit, file= "test_submit4_feateng.csv", row.names=F)

#Random Forest Model
#Introduction of bagging - randomly sampling from a group to grow different decision trees

#___________
#Bagging Prep
#Filling all of the NA values
Agefit <- rpart( Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                 data = combi[!is.na(combi$Age),], method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#___________
#Factor level prep (Random Forests in R can only digest factors up to 32 levels)
#We currently have nearly double that many in FamilyID
#We're going to make the 'Small' family size indicate 3 and under instead of 2 and under
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

titanic_train <- combi[1:891,]
titanic_test <- combi[892:1309,]

library(randomForest)

titanic_train$Sex = as.factor(titanic_train$Sex) #because there was an error fitting the randomForest
titanic_test$Sex = as.factor(titanic_test$Sex) #because it needs to match the Sex factor in train to Predict
set.seed(415) #setting the random seed so you can reproduce the same results at a later date
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + 
                      Title + FamilySize + FamilyID2, data=titanic_train, importance=TRUE, ntree=2000)
varImpPlot(fit, main="Variable Importances")

#test_submit5_randfor1.csv
Prediction <- predict(fit, titanic_test)
submit <- data.frame(PassengerId = titanic_test$PassengerId, Survived = Prediction)
#write.csv(submit, file = "test_submit5_randfor1.csv", row.names=F)

#Conditional Inference Tree
#Makes decisions using statistical tests rather than purity measures
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked +
               Title + FamilySize + FamilyID, data = titanic_train, 
               controls = cforest_unbiased(ntree=2000, mtry=3))
#Conditional inference trees can handle factors with more levels than Rand Forests, so we can use 
#Original FamilyID
#mtry is for choosing the number of variables to sample at each node (default of 5 is high for our small dataset)

#test_submit6_cforest.csv
Prediction <- predict(fit, titanic_test, OOB=T, type = "response")
submit <- data.frame(PassengerId = titanic_test$PassengerId, Survived = Prediction)
#write.csv(submit, file = "test_submit6_cforest.csv", row.names=F)
