pacman:: p_load(DataCombine,zoo,caret,treemap,tidyr,party,RColorBrewer,ggplot2,dplyr,readr,rpart, rpart.plot, corrplot,arules,arulesViz,stringdist)

Prediction <- read_csv("~/Desktop/DATACON/DengAI_Predicting_Disease_Spread_-_Submission_Format.csv")
TestData <- read_csv("~/Desktop/DATACON/DengAI_Predicting_Disease_Spread_-_Test_Data_Features.csv")
TrainingData <- read_csv("~/Desktop/DATACON/DengAI_Predicting_Disease_Spread_-_Training_Data_Features.csv")
TotalCases <- read_csv("~/Desktop/DATACON/DengAI_Predicting_Disease_Spread_-_Training_Data_Labels.csv")

#split dataset ####

TrainingData_sj <- subset(TrainingData, TrainingData$city=="sj")
TrainingData_iq <- subset(TrainingData, TrainingData$city== "iq")



TrainingData_sj <- TrainingData_sj[order(TrainingData_sj$year, TrainingData_sj$weekofyear),]
TrainingData_iq <- TrainingData_iq[order(TrainingData_iq$year, TrainingData_iq$weekofyear),]

TotalCases_sj <- subset(TotalCases, TotalCases$city== "sj")
TotalCases_iq <- subset(TotalCases, TotalCases$city== "iq")

TotalCases_sj <- TotalCases_sj[order(TotalCases_sj$year, TotalCases_sj$weekofyear),]
TotalCases_iq <- TotalCases_iq[order(TotalCases_iq$year, TotalCases_iq$weekofyear),]


TrainingData_Final_sj <- cbind(TrainingData_sj,TotalCases_sj)
TrainingData_Final_iq <- cbind(TrainingData_iq,TotalCases_iq)

TrainingData_Final_iq <- TrainingData_Final_iq[, !duplicated(colnames(TrainingData_Final_iq))]
TrainingData_Final_sj <- TrainingData_Final_sj[, !duplicated(colnames(TrainingData_Final_sj))]


#splidatasetTestSet####

TestData_sj <- subset(TestData, TestData$city=="sj")
TestData_iq <- subset(TestData, TestData$city== "iq")

TestData_sj <- TestData_sj[order(TestData_sj$year, TestData_sj$weekofyear),]
TestData_iq <- TestData_iq[order(TestData_iq$year, TestData_iq$weekofyear),]





#THREATING NA####

TrainingData_Final_sj <- TrainingData_Final_sj[,c(-1,-4)]
TestData_sj <- TestData_sj[,c(-1,-4)]
TestData_sj <- TestData_sj[,-3]
TrainingData_Final_sj <- TrainingData_Final_sj[,-3]


TrainingData_Final_iq <- TrainingData_Final_iq[,c(-1,-4)]
TestData_iq <- TestData_iq[,c(-1,-4)]
TestData_iq <- TestData_iq[,-3]
TrainingData_Final_iq <- TrainingData_Final_iq[,-3]



for(i in 1:ncol(TrainingData_Final_sj)){
  for(j in 1:nrow(TrainingData_Final_sj)){
    
    if(is.na(TrainingData_Final_sj[j,i])) {
     
      TrainingData_Final_sj[j,i] <- TrainingData_Final_sj[j+52,i]
      }
      }
  
}
i=0
j=0

for(i in 1:ncol(TrainingData_Final_sj)){
  for(j in 1:nrow(TrainingData_Final_sj)){
    
    if(is.na(TrainingData_Final_sj[j,i])) {
      
      TrainingData_Final_sj[j,i] <- TrainingData_Final_sj[j-52,i]
    }
    
  }
  
}

i=0
j=0
for(i in 1:ncol(TestData_sj)){
  for(j in 1:nrow(TestData_sj)){
    
    if(is.na(TestData_sj[j,i])) {
      
      TestData_sj[j,i] <- TestData_sj[j+52,i]
    }
    
    
    
  }
  
}

i=0
j=0
for(i in 1:ncol(TestData_sj)){
  for(j in 1:nrow(TestData_sj)){
    
    if(is.na(TestData_sj[j,i])) {
      
      TestData_sj[j,i] <- TestData_sj[j-52,i]
    }
    
    
    
  }
  
}

#IQ####
for(i in 1:ncol(TrainingData_Final_iq)){
  for(j in 1:nrow(TrainingData_Final_iq)){
    
    if(is.na(TrainingData_Final_iq[j,i])) {
      
      TrainingData_Final_iq[j,i] <- TrainingData_Final_iq[j+52,i]
    }
  }
  
}
i=0
j=0

for(i in 1:ncol(TrainingData_Final_iq)){
  for(j in 1:nrow(TrainingData_Final_iq)){
    
    if(is.na(TrainingData_Final_iq[j,i])) {
      
      TrainingData_Final_iq[j,i] <- TrainingData_Final_iq[j-52,i]
    }
    
  }
  
}

i=0
j=0
for(i in 1:ncol(TestData_iq)){
  for(j in 1:nrow(TestData_iq)){
    
    if(is.na(TestData_iq[j,i])) {
      
      TestData_iq[j,i] <- TestData_iq[j+52,i]
    }
    
    
    
  }
  
}

i=0
j=0
for(i in 1:ncol(TestData_iq)){
  for(j in 1:nrow(TestData_iq)){
    
    if(is.na(TestData_iq[j,i])) {
      
      TestData_iq[j,i] <- TestData_iq[j-52,i]
    }
    
    
    
  }
  
}

i=0
j=0




#correlationMatrix####

correlationSH <- cor(TrainingData_Final_sj)
View(correlationSH)
corrplot(correlationSH,type= "upper",tl.cex = 0.5 )

correlationSH[lower.tri(correlationSH,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
correlationSH<-as.data.frame(as.table(correlationSH))  #Turn into a 3-column table
correlationSH <- na.omit(correlationSH)  #Get rid of the stuff we flagged above
correlationSH<- correlationSH[order(-abs(correlationSH$Freq)),]    #Sort by highest correlation (whether + or -)
correlationSH<-subset(correlationSH,correlationSH$Freq>0.20) #subset it as we want
correlationSH
#Correlation Matrix IQ####

correlationIQ <- cor(TrainingData_Final_iq)
View(correlationIQ)
corrplot(correlationIQ,type= "upper",tl.cex = 0.5 )

correlationIQ[lower.tri(correlationIQ,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
correlationIQ<-as.data.frame(as.table(correlationIQ))  #Turn into a 3-column table
correlationIQ <- na.omit(correlationIQ)  #Get rid of the stuff we flagged above
correlationIQ<- correlationIQ[order(-abs(correlationIQ$Freq)),]    #Sort by highest correlation (whether + or -)
correlationIQ<-subset(correlationIQ,correlationIQ$Freq>0.15) #subset it as we want
correlationIQ



TrainingData_Final_sj <- slide(TrainingData_Final_sj, "total_cases", NewVar = "AWeekEarlier", slideBy = -1)  # create  variable
TrainingData_Final_sj <- slide(TrainingData_Final_sj, "reanalysis_specific_humidity_g_per_kg", NewVar = "AWeekEarlierHum", slideBy = -1)  # create  variable

TrainingData_Final_sj <- TrainingData_Final_sj[-1,]

TrainingData_Final_iq <- slide(TrainingData_Final_iq, "total_cases", NewVar = "AWeekEarlier", slideBy = -1)  # create  variable
TrainingData_Final_iq <- slide(TrainingData_Final_iq, "total_cases", NewVar = "AYearEarlier", slideBy = -26)  # create  variable

TrainingData_Final_iq <- TrainingData_Final_iq[-(1:26),]






set.seed(342)

trainSet_sj <- TrainingData_Final_sj[1:680,]
testSet_sj <- TrainingData_Final_sj[681:nrow(TrainingData_Final_sj),]


rfFit2 <- train(total_cases ~ reanalysis_specific_humidity_g_per_kg + AWeekEarlier  ,data=trainSet_sj, method = "rf", tuneLength= 5)
rfFit2

predictionSJ <- predict(rfFit2, testSet_sj)
testSet_sj$prediction <-predictionSJ 

plot(testSet_sj$total_cases, testSet_sj$total_cases - predictionSJ)

resSJ <- postResample(predictionSJ,testSet_sj$total_cases)
resSJ

TestData_sj$AWeekEarlier <- NA
TestData_sj$FinalPrediction <- NA
TestData_sj$AWeekEarlier[1]<-3




for(i in 1:nrow(TestData_sj)){
  TestData_sj$FinalPrediction[i] <- predict(rfFit2, TestData_sj)[i]
  TestData_sj$AWeekEarlier[i+1]=TestData_sj$FinalPrediction[i]
  
}







View(TestData_sj)
round(TestData_sj$FinalPrediction)


#IQ####
set.seed(123)

trainSet_iq <- TrainingData_Final_iq[1:319,]
testSet_iq <- TrainingData_Final_iq[319:nrow(TrainingData_Final_iq),]

plot(trainSet_iq$total_cases)
plot(testSet_iq$total_cases)



#fitControl <- trainControl(method = "repeatedcv", number = 10) #cross validation

plot( TrainingData_Final_iq$total_cases)
rfFit3 <- train(total_cases ~ reanalysis_specific_humidity_g_per_kg + AWeekEarlier + AYearEarlier , data=trainSet_iq, method = "rf", tuneLength= 15)
rfFit3

predIQ <- predict(rfFit3, testSet_iq)
testSet_iq$prova <- predIQ
testSet_iq$error <- predIQ - testSet_iq$total_cases
plot(testSet_iq$error)
plot(testSet_iq$total_cases, testSet_iq$error )
plot(predIQ)
resIQ <- postResample(predIQ,testSet_iq$total_cases)
resIQ
TestData_iq$AWeekEarlier <- NA
TestData_iq$AYearEarlier <- NA
TestData_iq$FinalPrediction <- NA

TestData_iq$AWeekEarlier[1]<-9


i=0


for( i in 1:26){
  TestData_iq$AYearEarlier[i]=TrainingData_Final_iq$AYearEarlier[nrow(TrainingData_Final_iq)-26+i]
  
}

for(i in 1:nrow(TestData_iq)){
  TestData_iq$FinalPrediction[i] <- predict(rfFit3, TestData_iq)[i]
  TestData_iq$AWeekEarlier[i+1]=TestData_iq$FinalPrediction[i]

}

for(i in 129:nrow(TestData_iq)){
  TestData_iq$FinalPrediction[i] <- predict(rfFit3, TestData_iq)[i]
}


round(TestData_iq$FinalPrediction)

newVecots <- c(TestData_sj$FinalPrediction,TestData_iq$FinalPrediction)


  Prediction$total_cases <- round(newVecots)

  
  write.csv(Prediction,"~/Desktop/name.csv", row.names = FALSE)

plot(TrainingData_Final_sj$total_cases)

