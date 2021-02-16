setwd("~/proj/ml/titanic_R/titanic")

#by default read.csv will read the data file, build a dataframe and convert
#all the strings into categories. we dont want that because we want to do manipulation hence 
#the stringsAsFactors
titanic.train <- read.csv(file="train.csv",stringsAsFactors= FALSE,header=TRUE)
titanic.test <- read.csv(file="test.csv",stringsAsFactors= FALSE,header=TRUE)
#tail(titanic.train)
#median(titanic.train$Age)  #this will return NA because of empty values
#median(titanic.train$Age, na.rm=TRUE) #this will get the median removing the null values


#because we want to clean the two datasets concurrently, we will have to join the two datasets and 
#for us to know which row was in which set, we will create a new row isTrainSet
titanic.train$isTrainSet <- TRUE
titanic.test$isTrainSet <- FALSE
#head(titanic.train$isTrainSet) #to view

#for the two to be able to be joined smoothly they need to align i.e same no. of columns and should also have similar naming. from below we see they are not the same
ncol(titanic.train)
ncol(titanic.test)

#below outputs the column names for both.
#names(titanic.train)
#names(titanic.test)

#From that we see the missing column is Survived and we add it as below
titanic.test$Survived <- NA

#now that the columns have lined up we will bind the two (similar to union operator) / vertical join
titanic.full <- rbind(titanic.train,titanic.test)

#the models dont work well with missing values. e.g the 2 missing values in the Embarked column
table(titanic.full$Embarked)
#we select the rows in the embarked column which are empty and replace them with the median i.e S
titanic.full[titanic.full$Embarked == "","Embarked"] <- "S"
#age also has some missing values seen as below
#is.na(titanic.full$Age) represents a query of TRUE or FALSE where NA is TRUE
table(is.na(titanic.full$Age))

#we will therefore replace the missing age values with the Age median
age.median <- median(titanic.full$Age,na.rm = TRUE)
titanic.full[is.na(titanic.full$Age),"Age"] <- age.median

#categorical casting(takes on a limited, and usually fixed, number of possible values )
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

#Data is clean now. we should now return it to the original train and test dataset
titanic.test <- titanic.full[titanic.full$isTrainSet==FALSE,]
titanic.train <- titanic.full[titanic.full$isTrainSet==TRUE,]

#we also categorize the survived after splitting
titanic.train$Survived <- as.factor(titanic.train$Survived)


#we build the model equation, we 1st pullout the fields we want to make a predictive model on
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked "

#we will need to pass the equation as a formula type(create a formula)
survived.formula <- as.formula(survived.equation)

#install randomForest 
install.packages("randomForest")
library(randomForest)

#create the model
titanic.model <- randomForest(formula = survived.formula, data=titanic.train, ntree=500,mtry=3,nodesize=0.01*nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked "

Survived <-predict(titanic.model,newdata = titanic.test)

PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df,file="kaggle_submission.csv", row.names = FALSE)
