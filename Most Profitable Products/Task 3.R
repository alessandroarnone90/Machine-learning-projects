pacman:: p_load(caret,party,reshape,ggplot2,dplyr,readr,rpart, rpart.plot, rattle, corrplot)


existing <- read_csv("~/Downloads/existingproductattributes2017.csv")
existing2 <- read_csv("~/Downloads/existingproductattributes2017.csv")
newproducts <- read_csv("~/Downloads/newproductattributes2017.csv")


#CHECK OUTLIERS###
str(existing) #check type 
outliersVolume <- boxplot.stats(existing$Volume)$out #record outlier info
existing<- existing[-which(existing$Volume %in% outliersVolume),] #delete outlier rows

boxplot(existing2$Volume)
plot()

#FIRST GRAPH####
ggplot(existing, aes(x=PositiveServiceReview, y=x4StarReviews, size = Volume, col=ProductType)) +
  geom_point(alpha=0.7)



#NEWCOLUMS####
existing$PhisicalVolume <- as.integer( existing$ProductDepth*existing$ProductWidth*existing$ProductHeight)
newproducts$PhisicalVolume <- as.integer(newproducts$ProductDepth*newproducts$ProductWidth*newproducts$ProductHeight)
existing$TotalNumberOfReviews <- as.integer(existing$x4StarReviews+existing$x3StarReviews+existing$x2StarReviews+existing$x1StarReviews)
existing$TotalNumberOfStars <- as.integer((existing$x4StarReviews*4+existing$x3StarReviews*3+existing$x2StarReviews*2+existing$x1StarReviews))
existing$TotalNumberOfService <- as.integer((existing$PositiveServiceReview+existing$NegativeServiceReview))
existing$happyCustomer <- (existing$PositiveServiceReview/(existing$TotalNumberOfService+0.001)+existing$x4StarReviews/(existing$TotalNumberOfReviews+0.0001))


#ELIMINATE EXTEDED WARRANTY AND COLUMN I DO NOT NEED####
existing <-  existing[-which(existing$ProductType=="ExtendedWarranty"),]
existing<- existing[  ,-c(12,13,14,15,16,17)]
newproducts <-  newproducts[-which(newproducts$ProductType=="ExtendedWarranty"),]

#ADDITIONAL GRAPH####
ggplot(data=existing, aes(x=reorder(ProductNum, -Volume), y=Volume, fill= ProductType))+
  geom_col(position='dodge') +
  theme(axis.text.x=element_blank()) +
  xlab('Product Number') +
  ylab('Volume') +
  ggtitle("Volume of sales by Product") +
  labs(fill="Product Type")


#graphs####

existing<- existing[  ,-2] #Delete ID - I do not need anymore

for(i in 1:ncol(existing)) {
  
  if(is.numeric(existing[[i]])=="TRUE"){
    
    graph <- ggplot(data=existing, aes(x=existing[[i]], y=Volume)) +
      geom_point() +
      geom_smooth() +
      labs(x=colnames(existing[i]), scales = "free_x") 
    print(graph)
  }
  
  else if(colnames(existing[i])=="ProductType") {
    graph<- ggplot(data=existing, aes(x=existing[[i]], fill=ProductType))+
      geom_bar()+
      labs(x="Product Type")
    print(graph)
  }
  
}


#DUPLICATE####
duplicated(existing)
existing <- existing[!duplicated(existing),]

#TYPE####
str(existing)
createBinaries <- dummyVars(" ~ .", data = existing)
existing <- data.frame(predict(createBinaries, newdata = existing))



#correlationMatrix####
correlation <- cor(existing)
View(correlation)
corrplot(correlation,type= "upper",tl.cex = 0.5 )


#give me the best and the worst####

correlation[lower.tri(correlation,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
correlation<-as.data.frame(as.table(correlation))  #Turn into a 3-column table
correlation <- na.omit(correlation)  #Get rid of the stuff we flagged above
correlation<- correlation[order(-abs(correlation$Freq)),]    #Sort by highest correlation (whether + or -)
correlation<-subset(correlation,correlation$Freq>0.60) #subset it as we want
correlation


#testTest####

set.seed(123)
in_training <- createDataPartition(existing$Volume, p = 0.7, list = F)
trainSet <- existing[in_training,]
testSet <- existing[-in_training,]



#MODEL CONTROL VARIABLES #### 
fitControl <- trainControl(method = "repeatedcv", number = 10) #cross validation
metrics <- c()
modelused <- c("knn","rf","svmLinear")
variables <- c( Volume ~ PositiveServiceReview + x4StarReviews + NegativeServiceReview,
               Volume ~ PositiveServiceReview + x4StarReviews,
               Volume ~ NegativeServiceReview + x4StarReviews)
salesPrediction <- c()
errorPrediction<- c()
finalPrediction <- c()


for (x in variables){
  
  for( i in modelused){
 
    model <- train((x),
                   data=trainSet, method = i, trainControl=fitControl,
                   tuneLength= 3, preProcess = c("center", "scale"))
    prediction <- predict(model,testSet)  #predict test Set
    error<- prediction - testSet$Volume #calculate punctual error (Im sure there is a better way)
    newPrediction <- predict(model,newproducts) #predict on NewProduct
    
    salesPrediction<-cbind(salesPrediction, prediction) #add the prediction in a table - not really useful but i will do it anyway
    errorPrediction<- cbind(errorPrediction,error) #add the error in a table
    finalPrediction <- cbind(finalPrediction,newPrediction) #add the prediction in a table - related to the new dataset
  
    metricCycle <-postResample(prediction,testSet$Volume)
    metrics<- rbind(metrics,metricCycle)
  }
}
View(metrics)
#make better titles for the dataset created SalesPrediction, errorPrediction, finalPrediction, metric####

#EACH PREDICTION WILL REFER TO A SPECIFIC DATASET AND A SPECIFIC ALGORITHM - I WANT TO REFER TO THEM AS "1SET + KNN" - "2SET + SVM"
x <- seq(from=1, to=length(variables)) 
x
#create a vector from 1 to number of set of variables used

variablesUsed <-c()
for( m in 1:length(variables)){         #add to each number created the word SET
  variablesUsed[m]<-paste(x[m],"SET", sep=" ")
}
variablesUsed
#past each number + SET with the names of the algorithm used
index=0
trial <-c()
for(i in 1:length(variablesUsed)){
  for(l in 1:length(modelused)){
    index=index+1
  trial[index] <- paste(variablesUsed[i],modelused[l])
  }}

trial



colnames(salesPrediction)<- trial
colnames(errorPrediction)<-trial
colnames(finalPrediction)<-trial

#create colnames as index####
errorPrediction<- as.data.frame(errorPrediction)

#errorhistogram
for(i in 1:ncol(errorPrediction)){
  p1<-ggplot(errorPrediction, aes(x=errorPrediction[[i]])) + 
    geom_histogram(aes(y=..density..), bindwidth =0.5, colour = "blue", fill = "white") + 
    geom_density( alpha =0.2, fill = "#FF6666")+
    labs(x=colnames(errorPrediction[i]), scales = "free_x") 
  
  print(p1)
}



newproducts$Volume<-as.integer(finalPrediction[,"2 SET rf"])



tree <-ctree(Volume~ x4StarReviews + PositiveServiceReview ,data=existing , control = ctree_control(maxdepth = 4))
plot(tree)



View(newproducts)
finalAnalysis<- c()
finalAnalysis<- subset(newproducts, newproducts$ProductType=="PC"| newproducts$ProductType=="Laptop" | newproducts$ProductType=="Netbook" | newproducts$ProductType=="Smartphone") 



e <- ggplot(finalAnalysis, aes(x="", y=finalAnalysis$Volume, fill=ProductType)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme(  axis.title.x = element_blank())
print(e)

write.csv(newproducts, file="~/Desktop/newproductattributes2017.csv", row.names = TRUE)
write.csv 


newproducts


#ELIMINATEDEFECTEDROW####
existing2<-existing
existing2<-existing2[-which(existing$TotalNumberOfReviews==0),]
existing2<-existing2[-which(existing$x1StarReviews==1654),]
existing2<-existing2[-which(existing$PositiveServiceReview==310),]
correlationMatrix <- cor(existing2)
corrplot(correlationMatrix)

correlationMatrix[lower.tri(correlationMatrix,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
correlationMatrix<-as.data.frame(as.table(correlationMatrix))  #Turn into a 3-column table
correlationMatrix <- na.omit(correlationMatrix)  #Get rid of the stuff we flagged above
correlationMatrix<- correlationMatrix[order(-abs(correlationMatrix$Freq)),]    #Sort by highest correlation (whether +ve or -ve)
correlationMatrix<-subset(correlationMatrix,correlationMatrix$Freq>0.5)
correlationMatrix


set.seed(123)
in_training <- createDataPartition(existing2$Volume, p = 0.7, list = F)
trainSet <- existing2[in_training,]
testSet <- existing2[-in_training,]

metrics <- c()
modelused <- c("knn","rf","svmLinear")
variables <- c(
  Volume ~ PositiveServiceReview + x4StarReviews + NegativeServiceReview,
  Volume ~ PositiveServiceReview + x4StarReviews,
  Volume ~ NegativeServiceReview + x4StarReviews,
  Volume ~ PositiveServiceReview,
  Volume ~ x4StarReviews)
salesPrediction <- c()
errorPrediction<- c()
finalPrediction <- c()
metrics <- c()


for (x in variables){
  for( i in modelused){
    
    model <- train((x),
                   data=trainSet, method = i, trainControl=fitControl,
                   tuneLength= 3, preProcess = c("center", "scale"))
    print(model$results)
    print(model$finalModel)
    prediction <- predict(model,testSet)
    error<- prediction - testSet$Volume
    salesPrediction<-cbind(salesPrediction, prediction)
    errorPrediction<- cbind(errorPrediction,error)
    newPrediction <- predict(model,newproducts)
    finalPrediction <- cbind(finalPrediction,newPrediction)
    
    metricCycle <-postResample(prediction,testSet$Volume)
    
    metrics<- rbind(metrics,metricCycle)
  }
}
metrics





