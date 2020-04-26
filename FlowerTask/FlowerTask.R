
library("readr")
IrisDataset <- read.csv("~/Downloads/R Tutorial Data Sets/iris.csv")
View(IrisDataset)
attributes(IrisDataset)
summary(IrisDataset) 
str(IrisDataset)
names(IrisDataset)
IrisDataset$Species<- as.numeric(IrisDataset$Species) 
qqnorm()

View(IrisDataset)

IrisDataset$Colour <- cut(IrisDataset$Species, breaks = c(0,1,2,3), labels = c("black", "red", "green"))

plot(IrisDataset$Sepal.Length, IrisDataset$Sepal.Width,col=IrisDataset$Colour, xlab="Sepal Length",ylab="Sepal Width")
plot(IrisDataset$Petal.Length, IrisDataset$Petal.Width,col=IrisDataset$Colour, xlab="Petal Length",ylab="Petal Width"
    , main="Petal Lengths vs Petal Width ")
legend("topleft", legend=c("Setosa","Versicolor", "Virginic"), 
       pch=c("o","o","o"), col=c("black","red","green"))







#Full dataset####
boxplot(IrisDataset$Sepal.Length, main="Sepal Length")
boxplot(IrisDataset$Sepal.Width, main="Sepal Width")
boxplot(IrisDataset$Petal.Length, main="Petal Length")
boxplot(IrisDataset$Petal.Width, main="Petal Width")

options(3)
boxplot.stats(IrisDataset$Petal.Width)




set.seed(123)
trainSize<- round(nrow(IrisDataset)*0.8)
testSize <- nrow(IrisDataset)-trainSize
trainSize
testSize
trainIndices <- sample(seq_len(nrow(IrisDataset)),size=trainSize)
trainSet<- IrisDataset[trainIndices,]
testSet <- IrisDataset[-trainIndices,]

predictionModel <-lm(Petal.Length~Petal.Width,trainSet)
summary(predictionModel)
intercept<- predictionModel$coefficients["(Intercept)"]

slope<- predictionModel$coefficients["Petal.Width"]
prediction<- predict( predictionModel, testSet)
prediction

plot(IrisDataset$Petal.Width,IrisDataset$Petal.Length, col=IrisDataset$Colour, main="Prediction", xlab="Petal Width", ylab="Petal Length")
legend("topleft", legend=c("Setosa","Versicolor", "Virginic"), 
       pch=c("o","o","o"), col=c("black","red","green"))

abline (predictionModel,  lty=1, lwd=2, col="blue")

intercept
slope
abline
error<- (prediction-testSet$Petal.Length)
error
data.frame(testSet$Species,testSet$Petal.Length,testSet$Petal.Width,  prediction, error)

plot(testSet$Petal.Length,error, col=testSet$Colour, main="Error Plot", xlab="Petal Length")
abline(h=0)
hist(error, xlim = c(-1,1), col="blue")


rmse <- sqrt(mean(error^2))
rmse
mae <-  mean(abs(error))
mae
mape <- mean(abs((error/testSet$Petal.Length)))
mape
             

             





