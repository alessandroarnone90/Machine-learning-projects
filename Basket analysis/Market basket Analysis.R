pacman:: p_load(caret,treemap,tidyr,party,RColorBrewer,ggplot2,dplyr,readr,rpart, rpart.plot, corrplot,arules,arulesViz,stringdist)
Transaction <- read.transactions("~/Downloads/ElectronidexTransactions2017.csv", rm.duplicates = FALSE,sep = ",",format = c("basket"))
ImportData <- read_delim("~/Desktop/IndexItems.csv",";", escape_double = FALSE, trim_ws = TRUE)


#delete 00000 values
Transaction<-Transaction[which(size(Transaction)!= 0)]

IndexItems <- c()
IndexItems$Product<-Transaction@itemInfo$labels
IndexItems<- as.data.frame(IndexItems)

for(i in 1:nrow(IndexItems)){
  
  topMatches <- amatch(IndexItems$Product[i],ImportData$Product,method="lv", maxDist = Inf)
  IndexItems$Type[i]=ImportData[topMatches,2]
  
}  



IndexItems$Type <- as.factor(unlist(as.vector(IndexItems$Type)))


#when the graph does not work
dev.off()

totalSize <- c()

for(i in 1:length(Transaction)){
  
  totalSize[i]<- size(Transaction)[i]
}

View(totalSize)





#Starting Point####

TransactionByCategory<-aggregate(Transaction,IndexItems$Type)



#create a DF with data from transaction data
TransactionDF <- as.data.frame(t(as.matrix(Transaction@data)))
colnames(TransactionDF) <- Transaction@itemInfo$labels

#ScoreTransaction - create a new dataset of kind of transaction#### 


giveMeProductType <- function(x){
  IndexItems[IndexItems$Product == x,2] 
}


productType <- c()
transactionType<-c()
transactionType$Score<-NA
transactionType$CustomerType<-NA
transactionType$Gamer <- NA
transactionType$GameScore <- NA
transactionType$AppleCustomer <- NA
transactionType$AppleScore <- NA

gamesMainProduct <- c("Eluktronics Pro Gaming Laptop","Alienware AW17R4-7345SLV-PUS 17 Laptop", 
                      "CYBERPOWER Gamer Desktop")

gamesSecondaryProduct <- c("Redragon Gaming Mouse","AOC Monitor","Sceptre Monitor",
                           "Gaming Mouse Professional","Backlit LED Gaming Keyboard","Rii LED Keyboard","Rii LED Gaming Keyboard & Mouse Combo", 
                           "EagleTec Wireless Combo Keyboard and Mouse","Zombie Gaming Headset","PC Gaming Headset","XIBERIA Gaming Headset", "Computer Game" )

appleMainProduct <- c("Apple Macbook Pro", "iMac", "Apple MacBook Air")

appleSecondaryProduct <- c("Apple Wireless Keyboard", "Apple Wired Keyboard", "Apple TV","Apple Magic Keyboard", "Apple Earpods")


for( i in 1:nrow(TransactionDF)){
  Score_Accessories <- 0
  Score_ActiveHeadphones <- 0
  Score_ComputerCords <- 0
  Score_ComputerHeadphones <- 0
  Score_ComputerStands <- 0
  Score_Desktop <- 0
  Score_ExternalHarddrives <- 0
  Score_Keyboard <- 0
  Score_Laptop <- 0
  Score_Mice <- 0
  Score_Monitor <- 0
  Score_MouseAndKeyboardCombo <- 0
  Score_PrinterInk <- 0
  Score_Printers <- 0
  Score_SmarthomeDevices <- 0
  Score_Speaker <- 0
  Score_Tablets <- 0
  Score_ComputerGame <- 0
  Score_Software <- 0
  TotalScore <-0
  GamerScore <- 0
  GamerLevelForBusiness <-0
  AppleScore <-0
  AppleLevelForBusiness <-0
  

  
  for (l in 1:ncol(TransactionDF)){
    
    
    if (TransactionDF[i,l]==1){
      
       productType <- giveMeProductType(colnames(TransactionDF[l]))
  
      if(productType=="Accessories"){
        Score_Accessories <-Score_Accessories + 1
        }
    
      if(productType=="ActiveHeadphones"){
      Score_ActiveHeadphones <-Score_ActiveHeadphones + 1
        }
    
      if(productType=="ComputerCords"){
      Score_ComputerCords <-Score_ComputerCords + 1
        }
    
      if(productType=="ComputerHeadphones"){
      Score_ComputerHeadphones <-Score_ComputerHeadphones + 1
      }
       
      if(productType=="ComputerStands"){
      Score_ComputerStands <-Score_ComputerStands + 1
      }
       
      if(productType=="Desktop"){
      Score_Desktop <-Score_Desktop + 1
      }
       
      if(productType=="ExternalHarddrives"){
      Score_ExternalHarddrives <-Score_ExternalHarddrives + 1
      }
       
      if(productType=="Keyboard"){
      Score_Keyboard <-Score_Keyboard + 1
      }
      
      if(productType=="Laptop"){
      Score_Laptop <-Score_Laptop + 1
      }
      
      if(productType=="Mice"){
      Score_Mice <-Score_Mice + 1
      }
      
      if(productType=="Monitor"){
      Score_Monitor <-Score_Monitor + 1
      }
     
      if(productType=="MouseAndKeyboardCombo"){
      Score_MouseAndKeyboardCombo <-Score_MouseAndKeyboardCombo + 1
      }
     
      if(productType=="PrinterInk"){
      Score_PrinterInk <-Score_PrinterInk + 1
      }
     
      if(productType=="Printers"){
      Score_Printers <-Score_Printers + 1
      }
     
      if(productType=="SmarthomeDevices"){
      Score_SmarthomeDevices <-Score_SmarthomeDevices + 1
      }
     
      if(productType=="Speaker"){
      Score_Speaker <-Score_Speaker + 1
      }
      
      if(productType=="Tablets"){
      Score_Tablets <-Score_Tablets + 1
      }
      
      if(productType=="ComputerGame"){
         Score_ComputerGame <-Score_ComputerGame + 1
       }
    
      if(productType=="Software"){
         Score_Software <-Score_Software + 1
       }
       
       
      if(colnames(TransactionDF[l]) %in% gamesMainProduct == TRUE){
      GamerScore <-GamerScore + 100
      GamerLevelForBusiness <- GamerLevelForBusiness + 1
       }
       
      if(colnames(TransactionDF[l]) %in% gamesSecondaryProduct == TRUE){
      GamerScore <-GamerScore + 1
      GamerLevelForBusiness <- GamerLevelForBusiness + 1
      }
      
      if(colnames(TransactionDF[l]) %in% appleMainProduct == TRUE){
         AppleScore <-AppleScore + 100
         AppleLevelForBusiness <- AppleLevelForBusiness + 1
       }
       
      if(colnames(TransactionDF[l]) %in% appleSecondaryProduct == TRUE){
         AppleScore <- AppleScore + 1
       }
       
       
     } 

    
    }
    
    
    
    TotalScore <- Score_ActiveHeadphones +Score_ComputerCords +Score_ComputerHeadphones + Score_ComputerStands+
                  Score_Desktop + Score_ExternalHarddrives + Score_Keyboard + Score_Laptop + Score_Mice +
                  Score_Monitor + Score_MouseAndKeyboardCombo + Score_PrinterInk + Score_Printers + Score_SmarthomeDevices +
                  Score_Speaker + Score_Tablets + Score_Accessories + Score_ComputerGame + Score_Software

     
    transactionType$Score[i] <- TotalScore
    
    if(TotalScore > 8 | Score_ActiveHeadphones>= 2 | Score_ComputerCords>=2 | Score_ComputerHeadphones >=2 | Score_ComputerStands >=2 |
       Score_Desktop>=2| Score_ExternalHarddrives >=2 | Score_Keyboard >=2| Score_Laptop>=2 | Score_Mice >=2|
       Score_Monitor>=2 | Score_MouseAndKeyboardCombo>=2 | Score_PrinterInk >=2 |Score_Printers >=2 | Score_SmarthomeDevices >=2|
       Score_Speaker >=2 | Score_Tablets>=2 |Score_MouseAndKeyboardCombo + Score_Keyboard >=2 | Score_MouseAndKeyboardCombo + Score_Mice >=2 
       | Score_Desktop + Score_Laptop>=2 | Score_ComputerGame>=2 | Score_Software >=2 )
        {
        transactionType$CustomerType[i] <- "Business" 
        } else{
                   transactionType$CustomerType[i] <- "FinalCustomer"
                        }
         
    if(transactionType$CustomerType[i] == "FinalCustomer"){
        if(GamerScore>=2){
        transactionType$Gamer[i]="Gamer"
         }
        else{
             transactionType$Gamer[i]="No Gamer"
            }
    }
    
    if(transactionType$CustomerType[i] == "Business"){
      
      if(GamerLevelForBusiness > sum(Score_Desktop, Score_Laptop, Score_Monitor)/2 & GamerScore>100 & GamerScore>AppleScore ){
        transactionType$Gamer[i]="Gamer"
      }
      else {
        
        transactionType$Gamer[i]="No Gamer"
      }
    }
    
    if(transactionType$CustomerType[i] == "FinalCustomer"){
      if(AppleScore>=1){
        transactionType$AppleCustomer[i]="AppleFun"
      }
      else{
        transactionType$AppleCustomer[i]="NoAppleFun"
      }
    }
    
    if(transactionType$CustomerType[i] == "Business"){
      
      if(AppleLevelForBusiness > sum(Score_Desktop, Score_Laptop )/2 & AppleScore>100 ){
        transactionType$AppleCustomer[i]="AppleFun"
      }
      else {
        
        transactionType$AppleCustomer[i]="NoAppleFun"
      }
    }
    
    
    
    
    transactionType$AppleScore[i] <- AppleScore
    transactionType$GameScore[i] <- GamerScore

}
    
transactionType<- as.data.frame(transactionType)




TransactionBusiness <- Transaction[which(transactionType$Customer== "Business")]
TransactionFinalCustomer <- Transaction[- which(transactionType$Customer=="Business")]
TransactionBusinessGamer <- Transaction[which(transactionType$Customer== "Business" & transactionType$Gamer=="Gamer")]
TransactionBusinessNoGamer <- Transaction[which(transactionType$Customer== "Business" & transactionType$Gamer=="No Gamer")]
TransactionFinalCustomerGamer <- Transaction[which(transactionType$Customer== "FinalCustomer" & transactionType$Gamer=="Gamer")]
TransactionFinalCustomerNoGamer <- Transaction[which(transactionType$Customer== "FinalCustomer" & transactionType$Gamer=="No Gamer")]

TransactionBusinessCategory <- TransactionByCategory[which(transactionType$Customer=="Business")]
TransactionFinalCustomerCategory <- TransactionByCategory[-which(transactionType$Customer=="Business")]
#Analysis for Product - Business ####




itemFrequencyPlot(TransactionBusiness, type="relative", topN=10, horiz=TRUE, col="steelblue3",
                  xlab='', main="Item frequency - relative")


BusinessRule<- apriori (TransactionBusiness, parameter = list(supp = 0.05, conf = 0.2, minlen=2),
                        appearance = list(rhs=IndexItems$Product[which(IndexItems$Type!="Desktop" & IndexItems$Type!="Laptop")]))
inspect(sort(BusinessRule, by="lift"))
plot(BusinessRule, method="graph")   
plot(BusinessRule)


#Analysis for Product - FinalCustomers ####

TransactionFinalCustomerRule <- apriori (TransactionFinalCustomer, parameter = list(supp = 0.005, conf = 0.1, minlen=2),
                        appearance = list(rhs=IndexItems$Product[which(IndexItems$Type!="Desktop" & IndexItems$Type!="Laptop")]))
inspect(sort(TransactionFinalCustomerRule, by="lift"))

is.redundant(TransactionFinalCustomerRule)
plot(TransactionFinalCustomerRule, method="graph", control=list(type="items")) 
plot(TransactionFinalCustomerRule)


#Analysis for Product - Business - Gamer ####
TransactionBusinessGamerRole<- apriori (TransactionBusinessGamer, parameter = list(supp = 0.1, conf = 0.1, minlen=2),
                        appearance = list(rhs=IndexItems$Product[which(IndexItems$Type!="Desktop" & IndexItems$Type!="Laptop")]))
inspect(sort(TransactionBusinessGamerRole, by="supp"))
summary(TransactionBusinessGamerRole)
is.redundant(TransactionBusinessGamerRole)
plot(TransactionBusinessGamerRole, method="graph", control=list(type="items")) 
plot(TransactionBusinessGamerRole)


#Analysis for Product - Business - No Gamer ####
TransactionBusinessNoGamerRole<- apriori (TransactionBusinessNoGamer, parameter = list(supp = 0.05, conf = 0.10, minlen=2),
                                        appearance = list(rhs=IndexItems$Product[which(IndexItems$Type!="Desktop" & IndexItems$Type!="Laptop")]))
inspect(sort(TransactionBusinessNoGamerRole, by="confidence"))
summary(TransactionBusinessNoGamerRole)
is.redundant(TransactionBusinessNoGamerRole)
plot(TransactionBusinessNoGamerRole, method="graph", control=list(type="items")) 
plot(TransactionBusinessNoGamerRole)

#Analysis for Product - Final Customer - Gamer ####

#Analysis for Product - FinalCustomer - No Gamer ####






#Analysis for Category - Business - Gamer ####
TransactionByCategoryBusinessGamer <- TransactionByCategory[which(transactionType$Customer== "Business" & transactionType$Gamer == "Gamer")]


#Analysis for Category - Business - No Gamer ####
TransactionByCategoryBusinessNoGamer <- TransactionByCategory[which(transactionType$Customer== "Business" & transactionType$Gamer == "No Gamer")]








View(TransactionDF)




# can be changed to the number of interest
itemFrequencyPlot(TransactionBusinessGamer,
                  type="relative",
                  topN=10,
                  horiz=TRUE,
                  col="steelblue3",
                  xlab='',
                  main="Item frequency - relative")


# Top products - can be changed to the number of interest
itemFrequencyPlot((Transaction),
                  type="absolute",
                  
                  topN=10, 
                  horiz=TRUE,
                  col='steelblue3',
                  
                  main='Item frequency - relative')


barplot(sort(table(unlist(LIST(Transaction))))[1:10],
        horiz=TRUE,
        las=1,
        col='steelblue3',
        xlab='',
        main='Frequency, relative')


 #if i want the relative i add the total ntransaction on top
tbl<- crossTable(Transaction, sort=TRUE)
tbl["Dell Desktop","iMac"]

image(Transaction, itemFrequency(Transaction)>0.05 )

itemFrequency(Transaction)


secondaryCategory <- c("Accessories", "ActiveHeadphones",
                        "ComputerCords","ComputerGame","ComputerHeadphones", 
                        "ComputerStands","ExternalHarddrives", "Keyboard","Mice", 
                        "Monitor","MouseAndKeyboardCombo","Printers", "PrinterInk", 
                        "SmarthomeDevices", "Software","Speaker")

FirstApriori<- apriori (TransactionByCategoryFinalCustomer, parameter = list(supp = 0.01, conf = 0.1, minlen=2), appearance = list(rhs=secondaryCategory))
inspect(FirstApriori)


FirstApriori<- apriori (TransactionFinalCustomerGamer, parameter = list(supp = 0.04, conf = 0.1, minlen=2))
inspect(FirstApriori)
apriori()


order(inspect(FirstApriori))

summary(FirstApriori)
      
#Analysis for Category - Final Customer - Gamer ####
TransactionFinalCustomerGamer <- Transaction[which(transactionType$Customer== "FinalCustomer" & transactionType$Gamer=="Gamer")]

#Analysis for Category - FinalCustomer - No Gamer ####
TransactionFinalCustomerNoGamer <- Transaction[which(transactionType$Customer== "FinalCustomer" & transactionType$Gamer=="No Gamer")]










#graph####


dimension <- data.frame(
  group = c("B2B", "B2C"),
  value = c(5248, 4585)
)


graph1 <- ggplot(dimension, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Dark2",name = "Transaction type" ) +
  theme(axis.title = element_blank(),panel.grid  = element_blank(), panel.background = element_blank())
  


itemFrequencyPlot(TransactionBusiness, topN=5, type="relative", col=brewer.pal(9,"Set2"))
itemFrequencyPlot(TransactionFinalCustomer, topN=5, type="relative", col=brewer.pal(9,"Set2"))

secondplot <- data.frame(
  group1 = c("Game Console", "Software","Display", "Printer","Smartphone"),
  value1 = c(8720, 4268,2428,2036,1808)
)

x <-sort(table(unlist(LIST(Transaction))))
barplot(x[1:5],
        las=1,
        col='steelblue3',
        
        main='Frequency, absolute')


ggplot(secondplot, aes(x =reorder(group1,-value1), y=value1)) +
geom_col(aes(fill=group1))+
scale_fill_discrete(name = "Products Category - Blackwells Electronic") +
theme(axis.title = element_blank(),panel.grid  = element_blank(), panel.background = element_blank())


                  