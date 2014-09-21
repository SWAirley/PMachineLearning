library(caret); library(rpart); library(rpart.plot); library(rattle); library(randomForest)


if (!file.exists("./data/pml-testing.csv")){
        download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                      "./data/pml-testing.csv")
}

if (!file.exists("./data/pml-training.csv")){
        download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                      "./data/pml-testing.csv")
}

training = read.csv("./data/pml-training.csv", na.strings = c("NA", ""))
Ptesting = read.csv("./data/pml-testing.csv", na.strings = c("NA", ""))

trainingRNA <- training[ , colSums(is.na(training)) == 0]
Final.training <- trainingRNA[,-c(1:7)]

inTrain <- createDataPartition(Final.training$classe, p=0.7, list = FALSE)
train <- Final.training[inTrain,]
test <- Final.training[-inTrain,]


##              R-Part Model
set.seed(505)
model.Rpart <- train(classe ~., method ='rpart', data = train)
fancyRpartPlot(model.Rpart$finalModel)

model.Rpart

pred.Rpart <- predict(model.Rpart, test)
confusionMatrix(pred.Rpart, test$classe)


##              Random Forest Model
set.seed(506)
model.Rforest <- randomForest(classe~., mtry = 30, ntree =100, keep.forest = TRUE, importance = TRUE, proximity = TRUE, data = train)
model.Rforest
pred.Rforest <- predict(model.Rforest, test)
confusionMatrix(pred.Rforest, test$classe)
