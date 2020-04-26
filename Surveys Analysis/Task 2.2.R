library(caret)
library(readr)
library(tidyverse)
library(ggplot2)
library(pscl)
library(C50)
library(rpart.plot)
library(plyr)
library(dplyr)
library(party)



CompleteResponses <- read_csv("~/Downloads/CompleteResponses.csv")
SurveyIncomplete <- read_csv("~/Downloads/SurveyIncomplete.csv")
View(SurveyIncomplete)
View(CompleteResponses)

na.omit(CompleteResponses)

multi.fun <- function(x) {
  c(mean = mean(x), median = median(x), var = var(x), sd = sd(x))
}

#CompleteResponses$brand = factor(CompleteResponses$brand, levels = c(0, 1), labels = c('Acer', 'Sony'))







#REMOVEDUPLICATE####

duplicated(CompleteResponses)

CompleteResponses[!duplicated(CompleteResponses),]

ggplot(CompleteResponses, aes(x=brand, fill=brand))+ geom_bar()





#OUTLIER CHECKS####
boxplot.stats(CompleteResponses$age)   #Complete response
boxplot.stats(CompleteResponses$credit)  #Complete response
boxplot.stats(CompleteResponses$salary)   #Complete response

attributes(CompleteResponses)
attributes(SurveyIncomplete)

#RIGHT DATATYPE ####
CompleteResponses$car<- factor(CompleteResponses$car)  #Complete response
CompleteResponses$zipcode<- factor(CompleteResponses$zipcode)  #Complete response
CompleteResponses$elevel<- factor(CompleteResponses$elevel)   #Complete response
CompleteResponses$brand<- factor(CompleteResponses$brand, labels = c("Acer","Sony"))   #Complete response

SurveyIncomplete$car<- factor(SurveyIncomplete$car)  #Complete response
SurveyIncomplete$zipcode<- factor(SurveyIncomplete$zipcode)  #Complete response
SurveyIncomplete$elevel<- factor(SurveyIncomplete$elevel)   #Complete response


#DISTRIBUTION CHECK####
ggplot(CompleteResponses, aes(x=brand, fill=brand))+ geom_bar()


hist(CompleteResponses$age, main="AGE in Complete Responses", xlab="Age")
hist(SurveyIncomplete$age, main="AGE in Survey Incomplete", xlab="Age")
multi.fun(CompleteResponses$age)



hist(CompleteResponses$salary, main="SALARY in Complete Responses", xlab="Salary")
hist(SurveyIncomplete$salary, main="SALARY in Survey Incomplete", xlab="Salary")
multi.fun(CompleteResponses$salary)
summary(CompleteResponses$salary)


plot(CompleteResponses$car, main="CAR in Complete Responses", xlab="Type of Car")
plot(SurveyIncomplete$car, main="CAR in Survey Incomplete", xlab="Type of Car")
multi.fun(CompleteResponses$salary)



plot(CompleteResponses$elevel, main="EDUCATION in Complete Responses", xlab="Education level")
plot(SurveyIncomplete$elevel, main="EDUCATION in Survey Incomplete", xlab="Education level")



plot(CompleteResponses$zipcode, main="ZIPCODE in Complete Responses", xlab="Zipcode")
plot(SurveyIncomplete$zipcode, main="ZIPCODE in Survey Incomplete", xlab="Zipcode")

ggplot(CompleteResponses, aes(x=salary ))+geom_histogram(color="darkblue", fill="lightblue", bins=20)
ggplot(CompleteResponses, aes(x=brand, fill=brand))+ geom_bar()
ggplot(CompleteResponses, aes(x=salary, fill=brand))+geom_histogram(color="black",bins=20)
ggplot(CompleteResponses, aes(x=salary )) +
  geom_histogram(color="darkblue", fill="lightblue", bins=20) +
  facet_wrap(CompleteResponses$elevel, scales ="free_x")

ggplot(CompleteResponses, aes(x=age,y=salary, col=brand)) +
  geom_point(size=0.01) + 
  geom_smooth() + 
  scale_color_discrete(name="Brand",labels=c("Acer","Sony"))

ggplot(SurveyIncomplete, aes(x=age,y=salary, col=brand)) +
  geom_point(size=0.01) + 
  geom_smooth() + 
  scale_color_discrete(name="Brand",labels=c("Acer","Sony"))

ggplot(CompleteResponses, aes(x="",fill=brand)) +
  geom_bar() + 
  coord_polar(theta="y")+
  scale_color_discrete(name="Brand",breaks=c("0","1"),labels=c("Acer","Sony"))
  

#DOWNSAMPLING####

set.seed(123)
CompleteResponsesTrue <-subset(CompleteResponses,CompleteResponses$brand=="Sony")  #a new dataset of True
CompleteResponsesFalse <-subset(CompleteResponses,CompleteResponses$brand=="Acer") #a new dataset of False
additionalTrueResponse <- nrow(CompleteResponsesTrue) - nrow(CompleteResponsesFalse ) #difference amongst raws
CompleteResponsesTrue <-CompleteResponsesTrue[-sample(1:nrow(CompleteResponsesTrue), additionalTrueResponse),]
CompleteResponses <- rbind(CompleteResponsesTrue,CompleteResponsesFalse)
 


#Correlation matrix ####
x <- round(cor(CompleteResponses[,c(1,2,6,7)]),2)
x




#correlation between level of education and Brand ### INDIPENDENT - TO EXCLUDE
xtabs(~ CompleteResponses$elevel + CompleteResponses$brand )
test <-chisq.test(CompleteResponses$elevel, CompleteResponses$brand )
c(test$statistic, test$p.value)
t<-sqrt(test$statistic/sum(table(CompleteResponses[,c(3,7)]))) ##KRAMER V
t

#correlation between car and Brand - CSQUARED #### REALLY LOW K BUT ALSO KIND OF SMALL P 
xtabs(~ CompleteResponses$zipcode + CompleteResponses$brand )
test <-chisq.test(table(CompleteResponses[,c(4,7)]))
c(test$statistic, test$p.value)
t<-sqrt(test$statistic/sum(table(CompleteResponses[,c(4,7)])))
t

#correlation between zip and Brand - CSQUARED #### REALLY LOW K BUT ALSO KIND OF SMALL P BUT WE EXLUDE
xtabs(~ CompleteResponses$zipcode + CompleteResponses$brand )
test <-chisq.test(table(CompleteResponses[,c(5,7)]))
c(test$statistic, test$p.value)
t<-sqrt(test$statistic/sum(table(CompleteResponses[,c(5,7)])))
t


#correlation between level of education and car
test <-chisq.test(table(CompleteResponses[,c(3,4)]))
c(test$statistic, test$p.value)
x<-table(CompleteResponses[,c(3,4)])
t<-sqrt(test$statistic/sum(x))
t
x

#correlation between level of education and ZIP
test <-chisq.test(table(CompleteResponses[,c(3,5)]))
c(test$statistic, test$p.value)
x<-table(CompleteResponses[,c(3,5)])
t<-sqrt(test$statistic/sum(x))
t
#correlation between level of education and car
test <-chisq.test(table(CompleteResponses[,c(3,5)]))
c(test$statistic, test$p.value)
x<-table(CompleteResponses[,c(3,5)])
t<-sqrt(test$statistic/sum(x))
t
#correlation between zip and car
test <-chisq.test(table(CompleteResponses[,c(4,5)]))
c(test$statistic, test$p.value)
x<-table(CompleteResponses[,c(4,5)])
t<-sqrt(test$statistic/sum(x))
t


#salary is good
model <- glm(brand~ salary + age + credit , data=CompleteResponses, family=binomial(link="logit"))
model
summary(model)
anova(model, test="Chisq")
pR2(model)


#age we dont know
model <- glm(brand~ salary+ age, data=CompleteResponses, family=binomial(link="logit"))
anova(model, test="Chisq")

#credit is not good
model <- glm(brand~ credit, data=CompleteResponses, family=binomial(link="logit"))
anova(model, test="Chisq")



#CompleteResponses <- CompleteResponses[,c(1,2,3,7)]

#  splitting the set ####

trainSize<-round(nrow(CompleteResponses)*0.75)
testSize<-nrow(CompleteResponses)-trainSize

training_indices <- sample(seq_len(nrow(CompleteResponses)), size=trainSize) 
trainSet <- CompleteResponses[training_indices,]
testSet <- CompleteResponses[-training_indices,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) #cross validation


# Model for varimp####

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) #cross validation

rfFit2 <- train(y=trainSet$brand, x=trainSet[-7], method = "rf", trControl=fitControl, tuneLength= 5)
rfFit2

pred <- predict(rfFit2,testSet)


res <- postResample(pred,testSet$brand)
res



varImp(rfFit2)
plot(varImp(rfFit2))


trainSet$brand <- factor(trainSet$brand)

my_tree <- train(brand ~ salary + age + elevel + car + zipcode + credit, data=trainSet, method = "rpart")
rpart.plot(my_tree$finalModel, type=0, digits=-2, under = TRUE)
my_tree$finalModel


#c - automatic grid#### 


FitC <- train(brand ~ salary + age, data=trainSet, method ="C5.0", 
             trControl = fitControl, tuneLength =3, 
             preProcess = c("center", "scale"))
pred <- predict(FitC,testSet)

FitC
wasItgood <- postResample(pred,testSet$brand)
wasItgood

summary(FitC)


#c - manual grid####

rfGrid <- expand.grid( winnow ="FALSE", trials=c(25,35,45), model="tree")
FitMC <- train(brand~salary+age,data = trainSet,method ="C5.0" , 
             trControl = fitControl, tuneGrid =rfGrid, 
             preProcess = c("center", "scale"))

summary(FitMC)
pred <- predict(FitMC,testSet)

wasItgood <- postResample(pred,testSet$brand)
FitMC
wasItgood

#rf - automatic grid#### 
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) #cross validation

rfFit2 <- train(y=trainSet$brand, x=trainSet[-7], method = "rf", trControl=fitControl, tuneLength=3,preProcess = c("center", "scale"))
pred <- predict(rfFit2,testSet)

plot(rfFit2,level=10, type=1)
res <- postResample(pred,testSet$brand)
res

#rf - manual grid#### 

rfGrid <- expand.grid( mtry=c(1,2,3))

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) #cross validation


rfMGFit2 <- train(y=trainSet$brand, x=trainSet[-7], method = "rf", trControl=fitControl, tuneGrid =rfGrid)
rfMGFit2

pred <- predict(rfMGFit2,testSet)
res <- postResample(pred,testSet$brand)
res

#Cycle of Models with different algorithm#### 
accuracyKPrediction <- c()
a <- c("knn", "svmRadial", "svmLinear")
for(i in a) {
  
  Fit <- train(brand~salary+age,data = trainSet,method = i, 
              trControl = fitControl, tuneLength = 5, 
               preProcess = c("center", "scale"))
  print(Fit)
  pred <- predict(Fit,testSet)
  res <- postResample(pred,testSet$brand)
  
  accuracyKPrediction <- cbind(accuracyKPrediction,res)
  
}

colnames(accuracyKPrediction) <- a
accuracyKPrediction


rfGrid <- expand.grid( C=4, sigma=1.23)
FitGoodModel <- train(brand~ salary+age,data =trainSet,method = "svmRadial", 
             trControl = fitControl, tuneGrid =rfGrid,
             preProcess = c("center", "scale"))

predictionTest <-predict(FitGoodModel, testSet)
postResample(predictionTest,testSet$brand)
confusionMatrix(testSet$brand,predictionTest)

predictionGoodModel <- predict(FitGoodModel, SurveyIncomplete)

SurveyIncomplete$brand<-predictionGoodModel
summary(SurveyIncomplete$brand)


TotalDataSet<- rbind(SurveyIncomplete,CompleteResponses)
ggplot(SurveyIncomplete, aes(x=brand, fill=brand))+ geom_bar()


ggplot(TotalDataSet, aes(x=brand, fill=brand))+ geom_bar()
summary(TotalDataSet$brand)


