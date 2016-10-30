library('xgboost')
library('Metrics')

train<-read.csv("train.csv",stringsAsFactors= T)
test<-read.csv("test.csv",stringsAsFactors = T)

test$Y<-0

all<-rbind(train,test)
str(all)

summary(all)


train$f<-rowSums(train[,c(1:101)])/100

test$f<-rowSums(test[,c(1:101)])/100


table(as.factor(train$Y))

outcomeName <- c('Y')
predictors <- names(train)[!names(train) %in% outcomeName]


trainPortion <- floor(nrow(train)*0.1)

trainSet <- train[ 1:floor(trainPortion/2),]
testSet <- train[(floor(trainPortion/2)+1):trainPortion,]


#Using XGBoost
smallestError <- 100
for (depth in seq(1,10,1)) {
  for (rounds in seq(1,20,1)) {
    
    # train
    bst <- xgboost(data = as.matrix(trainSet[,predictors]),
                   label = trainSet[,outcomeName],
                   max.depth=depth, nround=rounds,
                   objective = "reg:linear", verbose=0)
    gc()
    
    # predict
    predictions <- predict(bst, as.matrix(testSet[,predictors]), outputmargin=TRUE)
    err <- auc(as.numeric(testSet[,outcomeName]), as.numeric(predictions))

    
    if (err < smallestError) {
      smallestError = err
      print(paste(depth,rounds,err))
    }     
  }
}  

"4 2 0.459738163558106"

bst <- xgboost(data = as.matrix(trainSet[,predictors]),
               label = trainSet[,outcomeName],
               max.depth=4, nround=2,
               objective = "reg:linear")

predictions <- predict(bst, as.matrix(testSet[,predictors]), outputmargin=TRUE)

predictions<-ifelse(predictions>-0.01414,1,-1)

6*abs(auc(as.numeric(as.factor(testSet[,outcomeName])), as.factor(predictions))-0.5)

outcomeName <- c('Y')
no<-c('Y','Time')
predictors <- names(train)[!names(train) %in% no]


#Using GBM
library(caret)

train$Y<-ifelse(train$Y==1,1,0)

train$Y<-as.factor(train$Y)


model_gbm<-train(train[,predictors],train[,outcomeName],method="gbm")

varImp(model_gbm)

predictions<-predict(object=model_gbm,test[,predictors],type="prob")

summary(predictions)

sub<-as.data.frame(test$Time)

sub$Y<-predictions$'1'

sub$Y<-ifelse(sub$Y>0.5,1,-1)

write.csv(sub,"sub.csv",row.names = F)
