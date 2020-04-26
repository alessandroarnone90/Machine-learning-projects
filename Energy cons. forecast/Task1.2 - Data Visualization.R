#### ---- Libraries ----
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(zoo)
  library(plotly)
  library(tidyverse)
  library(caret)

#### ---- Importing Data ----

  EnergyConsumption <- readRDS(file = '/Users/alessandro/M3T1/EnergyConsumptionClean_3years.RDS')
  
#### ---- Data Wrangling & Munging ----

  ## Group by Quarter
  Group_Quarter <- EnergyConsumption %>% group_by(Quarter) %>%
    summarise(
              Sum_GAP=sum(GAP),
              Sum_Others=sum(Others),
              Sum_Kitchen=sum(Kitchen),
              Sum_Laundry=sum(Laundry),
              Sum_Heater_AC=sum(Heater_AC)
    )
  
  
  ## Group by Month
  

  
  ## Group by Seasons
  
  Group_Seasons <- EnergyConsumption %>% group_by(Seasons) %>%
    summarise(
              Avg_temp= avg
    )
  
  
## Group by Date
  
  Group_Date <- EnergyConsumption %>% group_by(Date) %>%
    summarise(
              Sum_GAP=sum(GAP),
              Sum_Others=sum(Others),
              Sum_Kitchen=sum(Kitchen),
              Sum_Laundry=sum(Laundry),
              Sum_Heater_AC=sum(Heater_AC)
    )
  
  ## Group by Weekday
  
  Group_Weekday <- EnergyConsumption %>% group_by(Weekday) %>%
    summarise(
              Sum_GAP=sum(GAP),
              Sum_Others=sum(Others),
              Sum_Kitchen=sum(Kitchen),
              Sum_Laundry=sum(Laundry),
              Sum_Heater_AC=sum(Heater_AC)
    )
  
  
  ## Group by Weekend
  
  Group_Weekend <- EnergyConsumption %>% group_by(Weekend) %>%
    summarise(
              Sum_GAP=sum(GAP),
              Sum_Others=sum(Others),
              Sum_Kitchen=sum(Kitchen),
              Sum_Laundry=sum(Laundry),
              Sum_Heater_AC=sum(Heater_AC)
    )
  

  ## Group by DayTime
  
  Group_DayTime <- EnergyConsumption %>% group_by(DayTime) %>%
    summarise(
              Sum_GAP=sum(GAP),
              Sum_Others=sum(Others),
              Min_Kitchen=min(Kitchen),
              Max_Kitchen=max(Kitchen),
              Mean_Kitchen=mean(Kitchen),
              SD_Kitchen=sd(Kitchen),
              Sum_Kitchen=sum(Kitchen),
              Min_Laundry=min(Laundry),
              Max_Laundry=max(Laundry),
              Mean_Laundry=mean(Laundry),
              SD_Laundry=sd(Laundry),
              Sum_Laundry=sum(Laundry),
              Min_Heater_AC=min(Heater_AC),
              Max_Heater_AC=max(Heater_AC),
              Mean_Heater_AC=mean(Heater_AC),
              SD_Heater_AC=sd(Heater_AC),
              Sum_Heater_AC=sum(Heater_AC)
    )
  
  
  ##Group by Day
  Group_Day <- EnergyConsumption %>% group_by(Day) %>%
    summarise(Min_GAP=min(GAP),
              Max_GAP=max(GAP),
              Mean_GAP=mean(GAP),
              SD_GAP=sd(GAP),
              Sum_GAP=sum(GAP),
              Min_Others=min(Others),
              Max_Others=max(Others),
              Mean_Others=mean(Others),
              SD_Others=sd(Others),
              Sum_Others=sum(Others),
              Min_Kitchen=min(Kitchen),
              Max_Kitchen=max(Kitchen),
              Mean_Kitchen=mean(Kitchen),
              SD_Kitchen=sd(Kitchen),
              Sum_Kitchen=sum(Kitchen),
              Min_Laundry=min(Laundry),
              Max_Laundry=max(Laundry),
              Mean_Laundry=mean(Laundry),
              SD_Laundry=sd(Laundry),
              Sum_Laundry=sum(Laundry),
              Min_Heater_AC=min(Heater_AC),
              Max_Heater_AC=max(Heater_AC),
              Mean_Heater_AC=mean(Heater_AC),
              SD_Heater_AC=sd(Heater_AC),
              Sum_Heater_AC=sum(Heater_AC)
    )
  
  ## Group by Month in 2007
  Group_2007<- EnergyConsumption %>% filter(Year == 2007) %>% group_by(Month) %>%
    summarise(Min_GAP=min(GAP),
              Max_GAP=max(GAP),
              Mean_GAP=mean(GAP),
              SD_GAP=sd(GAP),
              Sum_GAP=sum(GAP),
              Min_Others=min(Others),
              Max_Others=max(Others),
              Mean_Others=mean(Others),
              SD_Others=sd(Others),
              Sum_Others=sum(Others),
              Min_Kitchen=min(Kitchen),
              Max_Kitchen=max(Kitchen),
              Mean_Kitchen=mean(Kitchen),
              SD_Kitchen=sd(Kitchen),
              Sum_Kitchen=sum(Kitchen),
              Min_Laundry=min(Laundry),
              Max_Laundry=max(Laundry),
              Mean_Laundry=mean(Laundry),
              SD_Laundry=sd(Laundry),
              Sum_Laundry=sum(Laundry),
              Min_Heater_AC=min(Heater_AC),
              Max_Heater_AC=max(Heater_AC),
              Mean_Heater_AC=mean(Heater_AC),
              SD_Heater_AC=sd(Heater_AC),
              Sum_Heater_AC=sum(Heater_AC)
    )
  
  ##Group by Month in 2008 
  Group_2008<- EnergyConsumption %>% filter(Year == 2008) %>% group_by(Month) %>%
    summarise(Min_GAP=min(GAP),
              Max_GAP=max(GAP),
              Mean_GAP=mean(GAP),
              SD_GAP=sd(GAP),
              Sum_GAP=sum(GAP),
              Min_Others=min(Others),
              Max_Others=max(Others),
              Mean_Others=mean(Others),
              SD_Others=sd(Others),
              Sum_Others=sum(Others),
              Min_Kitchen=min(Kitchen),
              Max_Kitchen=max(Kitchen),
              Mean_Kitchen=mean(Kitchen),
              SD_Kitchen=sd(Kitchen),
              Sum_Kitchen=sum(Kitchen),
              Min_Laundry=min(Laundry),
              Max_Laundry=max(Laundry),
              Mean_Laundry=mean(Laundry),
              SD_Laundry=sd(Laundry),
              Sum_Laundry=sum(Laundry),
              Min_Heater_AC=min(Heater_AC),
              Max_Heater_AC=max(Heater_AC),
              Mean_Heater_AC=mean(Heater_AC),
              SD_Heater_AC=sd(Heater_AC),
              Sum_Heater_AC=sum(Heater_AC)
    )
  
  ##Group by Month in 2009
  
  Group_2009<- EnergyConsumption %>% filter(Year == 2009) %>% group_by(Month) %>%
    summarise(Min_GAP=min(GAP),
              Max_GAP=max(GAP),
              Mean_GAP=mean(GAP),
              SD_GAP=sd(GAP),
              Sum_GAP=sum(GAP),
              Min_Others=min(Others),
              Max_Others=max(Others),
              Mean_Others=mean(Others),
              SD_Others=sd(Others),
              Sum_Others=sum(Others),
              Min_Kitchen=min(Kitchen),
              Max_Kitchen=max(Kitchen),
              Mean_Kitchen=mean(Kitchen),
              SD_Kitchen=sd(Kitchen),
              Sum_Kitchen=sum(Kitchen),
              Min_Laundry=min(Laundry),
              Max_Laundry=max(Laundry),
              Mean_Laundry=mean(Laundry),
              SD_Laundry=sd(Laundry),
              Sum_Laundry=sum(Laundry),
              Min_Heater_AC=min(Heater_AC),
              Max_Heater_AC=max(Heater_AC),
              Mean_Heater_AC=mean(Heater_AC),
              SD_Heater_AC=sd(Heater_AC),
              Sum_Heater_AC=sum(Heater_AC)
    )
  
  
  ## Grouping ONE SINGLE DAY (Randomly selected, fall season)
   OneDayUse_Fall <- EnergyConsumption %>% 
    filter(DateTime > "2008-11-20" & DateTime < "2008-11-21")
  
   ## Grouping ONE SINGLE DAY (Randomly selected, Winter season)
   OneDayUse_Winter <- EnergyConsumption %>% 
     filter(DateTime > "2009-01-20" & DateTime < "2009-01-21") 
   
   ## Grouping ONE SINGLE DAY (Randomly selected, Spring season)
   OneDayUse_Spring <- EnergyConsumption %>% 
     filter(DateTime > "2007-04-12" & DateTime < "2007-04-13") 
   
   ## Grouping ONE SINGLE DAY (Randomly selected, Summer season)
   OneDayUse_Summer <- EnergyConsumption %>% 
     filter(DateTime > "2007-08-10" & DateTime < "2007-08-11") 
   
#### ---- Data Visualization with Bars ----
  
  #Mean GAP by Seasons
  ggplot(Group_Seasons, aes(x=Seasons, y=Mean_GAP)) + geom_col() + ggpubr::rotate_x_text()
  
  #Mean Others by Seasons
  ggplot(Group_Seasons, aes(x=Seasons, y=Mean_Others)) + geom_col() + ggpubr::rotate_x_text()

  #Mean Kitchen by Seasons
  ggplot(Group_Seasons, aes(x=Seasons, y=Mean_Kitchen)) + geom_col() + ggpubr::rotate_x_text()
  
  #Mean Lanudry by Seasons
  ggplot(Group_Seasons, aes(x=Seasons, y=Mean_Laundry)) + geom_col() + ggpubr::rotate_x_text()
  
  #Mean Air Cond by Seasons
  ggplot(Group_Seasons, aes(x=Seasons, y=Mean_Heater_AC)) + geom_col() + ggpubr::rotate_x_text()

  #Mean GAP per Month 2007
  ggplot(Group_2007, aes(x=Month, y=Mean_GAP)) + geom_col() + ggpubr::rotate_x_text()
  
  #Mean GAP per Month 2008
  ggplot(Group_2008, aes(x=Month, y=Mean_GAP)) + geom_col() + ggpubr::rotate_x_text()
  
  #Mean GAP per Month 2009
  ggplot(Group_2009, aes(x=Month, y=Mean_GAP)) + geom_col() + ggpubr::rotate_x_text()
  
  #Mean GAP per Month 2010
  ggplot(Group_2010, aes(x=Month, y=Mean_GAP)) + geom_col() + ggpubr::rotate_x_text()
  
  #Mean GAP per Weekday
  ggplot(Group_Weekday, aes(x=Weekday, y=Mean_GAP)) + geom_col() + ggpubr::rotate_x_text()
  
  #Mean GAP per Date (Month with Year)
  ggplot(Group_Date, aes(x=Date, y=Mean_GAP)) + geom_col() + ggpubr::rotate_x_text()
  
  #Mean GAP per Weekend
  ggplot(Group_Weekend, aes(x=Weekend, y=Mean_GAP)) + geom_col() + ggpubr::rotate_x_text()
  
  #Mean GAP per Year
  ggplot(Group_Year, aes(x=Year, y=Mean_GAP)) + geom_col() + ggpubr::rotate_x_text()
  
  #Mean GAP per Day
  ggplot(Group_Day, aes(x=Day, y=Mean_GAP)) + geom_col() + ggpubr::rotate_x_text()
  
  #Mean GAP per DayTime
  ggplot(Group_DayTime, aes(x=DayTime, y=Mean_GAP)) + geom_col() + ggpubr::rotate_x_text()

  #Mean GAP pero Month  
  ggplot(Group_Month, aes(x=Month, y=Mean_GAP)) + geom_col() + ggpubr::rotate_x_text()

  #Mean GAP per Quarter
  ggplot(Group_Quarter, aes(x=Quarter, y=Mean_GAP)) + geom_col() + ggpubr::rotate_x_text()

   
  #GAP for one day - Fall (2008-11-20 to 2008-11-21)
  ggplot(OneDayUse_Fall, aes(x = DateTime, y = GAP)) + 
    geom_line(size = 1, color = "blue") + theme_bw() +
    labs(x = "Day", y = "W/h", title = "Global Active Power Usage from 11/20 to 11/21 in 2008")
   
  #One day use of Air Conditioner - Fall 
  ggplot(OneDayUse_Fall, aes(x = DateTime, y = Heater_AC)) + 
    geom_line(size = 1, color="blue") +  theme_bw() + 
    labs(x = "Day", y = "W/h", title = "Sub-meter 3 from 11/20 to 11/21 in 2008") 
  
  #One day use of Air Conditioner - Winter 
  ggplot(OneDayUse_Winter, aes(x = DateTime, y = Heater_AC)) + 
    geom_line(size = 1, color="blue") +  theme_bw() + 
    labs(x = "Day", y = "W/h", title = "Sub-meter 3 from 01/20 to 01/21 in 2009") 
  
  #One day use of Air Conditioner - Spring 
  ggplot(OneDayUse_Spring, aes(x = DateTime, y = Heater_AC)) + 
    geom_line(size = 1, color="blue") +  theme_bw() + 
    labs(x = "Day", y = "W/h", title = "Sub-meter 3 from 04/12 to 04/12 in 2007") 
  
  #One day use of Air Conditioner - Summer
  ggplot(OneDayUse_Summer, aes(x = DateTime, y = Heater_AC)) + 
    geom_line(size = 1, color="blue") +  theme_bw() + 
    labs(x = "Day", y = "W/h", title = "Sub-meter 3 from 08/10 to 08/11 in 2007") 

  #Trials on colouring the months by Season
    #Add Season to Group_Month
    Group_Month<- Group_Month %>% 
      mutate (Season=
        case_when(Month %in% c("January", "February", "December") ~ "Winter",
                  Month %in% c("March", "April", "May") ~ "Spring",
                  Month %in% c("June", "July", "August") ~ "Summer",
                  Month %in% c("September", "October", "November") ~ "Fall",
                  ))
    
    #Plotting by Season                                       
    ggplot(Group_Month) + 
      geom_col(mapping = aes(x = Month, y =Mean_GAP, fill = Season,), size = 1.2, alpha = 0.8) + 
      theme_bw()+ ggpubr::rotate_x_text()+
      scale_fill_manual(values=c("Summer" = "orange", "Winter" = "blue", "Fall"= "brown", "Spring"="green"))+
      labs(x = "Month", y = "W/h", title = "Average Active Power per Season") +
      geom_hline(yintercept = mean(
        Group_Month$Mean_GAP[Group_Month$Season == "Summer"]),color= "orange", size=1.2)+
      geom_hline(yintercept = mean(
        Group_Month$Mean_GAP[Group_Month$Season == "Winter"]),color= "blue", size=1.2)+
      geom_hline(yintercept = mean(
        Group_Month$Mean_GAP[Group_Month$Season == "Fall"]),color= "brown", size=1.2)+
      geom_hline(yintercept = mean(
        Group_Month$Mean_GAP[Group_Month$Season == "Spring"]),color= "green", size=1.2)
    
#### ---- Data Visualization with Plotly and Granularity ----
  
  ##One Day Distribution - January
    
    ## Subset the 9th day of January 2008 - 10 Minute frequency
    houseDay_january <- filter(EnergyConsumption, Year == 2007 & Month == "January" & Day == 9 & (Minute == 0 | Minute == 10 | Minute == 20 | Minute == 30 | Minute == 40 | Minute == 50))
    
    ## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
    plot_ly(houseDay_january, x = ~houseDay_january$DateTime, y = ~houseDay_january$Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~houseDay_january$Laundry, name = 'Laundry Room', mode = 'lines') %>%
      add_trace(y = ~houseDay_january$Heater_AC, name = 'Water Heater & AC', mode = 'lines') %>%
      layout(title = "Power Consumption January 9th, 2008",
             xaxis = list(title = "Time"),
             yaxis = list (title = "Power (Wh)"))
   
  ##One Day Distribution - August 
    houseDay_august <- filter(EnergyConsumption, Year == 2008 & Month == "August", Day==11 & (Minute == 0 | Minute == 10 | Minute == 20 | Minute == 30 | Minute == 40 | Minute == 50))
  
    plot_ly(houseDay_august, x = ~houseDay_august$DateTime, y = ~houseDay_august$Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~houseDay_august$Laundry, name = 'Laundry Room', mode = 'lines') %>%
      add_trace(y = ~houseDay_august$Heater_AC, name = 'Water Heater & AC', mode = 'lines') %>%
      layout(title = "Power Consumption August 11th, 2008",
             xaxis = list(title = "Time"),
             yaxis = list (title = "Power (Wh)"))
  
    ##One Day Distribution - March 
    houseDay_march <- filter(EnergyConsumption, Year == 2008 & Month == "March", Day==17 & (Minute == 0 | Minute == 10 | Minute == 20 | Minute == 30 | Minute == 40 | Minute == 50))
    
    plot_ly(houseDay_march, x = ~houseDay_march$DateTime, y = ~houseDay_march$Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~houseDay_march$Laundry, name = 'Laundry Room', mode = 'lines') %>%
      add_trace(y = ~houseDay_march$Heater_AC, name = 'Water Heater & AC', mode = 'lines') %>%
      layout(title = "Power Consumption March 17th, 2008",
             xaxis = list(title = "Time"),
             yaxis = list (title = "Power (Wh)"))
    
    ##One Day Distribution - October
    houseDay_october <- filter(EnergyConsumption, Year == 2008 & Month == "October", Day==15 & (Minute == 0 | Minute == 10 | Minute == 20 | Minute == 30 | Minute == 40 | Minute == 50))
    
    plot_ly(houseDay_october, x = ~houseDay_october$DateTime, y = ~houseDay_october$Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~houseDay_october$Laundry, name = 'Laundry Room', mode = 'lines') %>%
      add_trace(y = ~houseDay_october$Heater_AC, name = 'Water Heater & AC', mode = 'lines') %>%
      layout(title = "Power Consumption October 15th, 2008",
             xaxis = list(title = "Time"),
             yaxis = list (title = "Power (Wh)"))
    
    ##One Month Distribution - October
    houseMonth_october <- filter(EnergyConsumption, Year == 2008 & Month == "October")
    
    plot_ly(houseMonth_october, x = ~houseMonth_october$DateTime, y = ~houseMonth_october$Kitchen, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~houseMonth_october$Laundry, name = 'Laundry Room', mode = 'lines') %>%
      add_trace(y = ~houseMonth_october$Heater_AC, name = 'Water Heater & AC', mode = 'lines') %>%
      layout(title = "Power Consumption January 9th, 2008",
             xaxis = list(title = "Time"),
             yaxis = list (title = "Power (kWh)"))
  

#### ---- Data Visualization with Energy Lines and Gathering ----
    
    #Grouping by Year and Month to see total consumption
    graphData <- EnergyConsumption %>% 
      group_by(Date) %>% 
      summarise('Kitchen' = sum(Kitchen),
                'Laundry Room' = sum(Laundry), 
                'Water heater & A/C' = sum(Heater_AC),
                'Non-submetered' = sum(Others)) %>% 
                  gather('Kitchen', 
                        'Laundry Room', 
                        'Water heater & A/C', 
                        'Non-submetered', 
                          key = Submeter, 
                          value = "Monthly_Consumption")
  
    #Plotting
    p<-ggplot(graphData, aes(x = Date, y = Monthly_Consumption, group= Submeter, colour = Submeter)) +
      geom_line(size = 1) + theme_bw() + xlab("Dates") + ylab ("Total Consumption (Wh)")+
      labs(title ="Monthly Energy use") + theme(plot.title = element_text(hjust = 0.5)) +
      ggpubr::rotate_x_text()
    ggplotly(p)
   
    #Grouping by Day  
    graphData2 <- EnergyConsumption %>% 
      group_by(Weekday) %>% 
      summarise('Kitchen' = mean(Kitchen),
                'Laundry Room' = mean(Laundry), 
                'Water heater & A/C' = mean(Heater_AC),
                'Non-submetered' = mean(Others)) %>% 
      gather('Kitchen', 
             'Laundry Room', 
             'Water heater & A/C', 
             'Non-submetered', 
             key = Submeter, 
             value = "Average_Consumption")
    
    #Plotting
    ggplot(graphData2, aes(x = Weekday, y = Average_Consumption, group= Submeter, colour = Submeter)) +
      geom_line(size = 1) + theme_bw() + xlab("Weekday") + ylab ("Average Consumption (Wh)")+
      labs(title ="Weekday Energy use") +
      ggpubr::rotate_x_text()
    
    #Grouping by DayTime  
    graphData3 <- EnergyConsumption %>% 
      group_by(DayTime) %>% 
      summarise('Kitchen' = mean(Kitchen),
                'Laundry Room' = mean(Laundry), 
                'Water heater & A/C' = mean(Heater_AC),
                'Non-submetered' = mean(Others)) %>% 
      gather('Kitchen', 
             'Laundry Room', 
             'Water heater & A/C', 
             'Non-submetered', 
             key = Submeter, 
             value = "Average_Consumption")
    
    #Plotting
    p3<- ggplot(graphData3, aes(x = DayTime, y = Average_Consumption, group= Submeter, colour = Submeter)) +
      geom_line(size = 1) + theme_bw() + xlab("Day Time") + ylab ("Average Consumption (Wh)")+
      labs(title ="Energy Use per Day Time") +
      ggpubr::rotate_x_text()
    ggplotly(p3)
    
    #Plotting One full week of use 
    graphData4 <- EnergyConsumption %>% filter (Week==15) %>%
      group_by(Weekday) %>%
      summarise('Kitchen' = sum(Kitchen),
                'Laundry Room' = sum(Laundry), 
                'Water heater & A/C' = sum(Heater_AC),
                'Non-submetered' = sum(Others)) %>% 
      gather('Kitchen', 
             'Laundry Room', 
             'Water heater & A/C', 
             'Non-submetered', 
             key = Submeter, 
             value = "Total_Consumption")
    
    # Plotting
    p2<- ggplot(graphData4, aes(x = Weekday, y = Total_Consumption, group= Submeter, colour = Submeter)) +
      geom_line(size = 1) + theme_bw() + xlab("Day") + ylab ("Total Consumption Wh")+
      labs(title ="Energy use from 2007/04/09 to 2007/04/15 ") + theme(plot.title = element_text(hjust = 0.5)) +
      ggpubr::rotate_x_text()
    ggplotly(p2)
    
    #Grouping by Year
    graphData5 <- EnergyConsumption %>% 
      group_by(Year) %>% 
      summarise('Kitchen' = sum(Kitchen),
                'Laundry Room' = sum(Laundry), 
                'Water heater & A/C' = sum(Heater_AC),
                'Non-submetered' = sum(Others)) %>% 
      gather('Kitchen', 
             'Laundry Room', 
             'Water heater & A/C',
             'Non-submetered',
             key = Submeter, 
             value = "Yearly_Consumption")
    
    #Plotting
    p4<-ggplot(graphData5, aes(x = Year, y = Yearly_Consumption, group= Submeter, colour = Submeter, fill=Submeter)) +
      geom_bar(stat="identity", position="fill", color="black") + theme_bw() + xlab("Dates") + ylab ("Total Consumption")+
      labs(title ="Proportion of Yearly Energy Use") + theme(plot.title = element_text(hjust = 0.5)) +
      ggpubr::rotate_x_text()
    ggplotly(p4)
    
    #Grouping by Quarter
    graphData6 <- EnergyConsumption %>% 
      group_by(Quarter) %>% 
      summarise('Energy_Consumed' = mean(GAP))
    
    #Plotting
    p5<-ggplot(graphData6, aes(x = Quarter, y = Energy_Consumed)) +
      geom_col(fill="blue", color="black") + theme_bw() + xlab("Season") + ylab ("Average Consumption (Wh)")+
      labs(title ="Energy Consumed by Season") + theme(plot.title = element_text(hjust = 0.5)) +
      ggpubr::rotate_x_text()
    ggplotly(p5)
    #We could do the same for Quarters, months, weeks and so on    
#### ---- Other Charts and Visualizations ----
    
    pieData<-EnergyConsumption %>% group_by(Year) %>% summarise(Sum1_Kitchen=sum(Kitchen),
                                                                 Sum2_Laundry=sum(Laundry),
                                                                 Sum3_Heater=sum(Heater_AC),
                                                                 Sum4_Others=sum(Others))
                                                            
    
    pieData %>% filter(Year == 2008) %>% 
      plot_ly(labels = ~c("Kitchen","Laundry","Heater_AC","Others"), 
              values = ~c(Sum1_Kitchen,Sum2_Laundry,Sum3_Heater,Sum4_Others), 
              type = "pie",   
              textposition = "inside",
              textinfo = "label+percent",
              showlegend = F) %>% 
      layout(title = "Total Energy Consumed in 2008",
             xaxis = list(showgrid = FALSE,zeroline = FALSE,showticklabels = FALSE),
             yaxis = list(showgrid = FALSE,zeroline = FALSE,showticklabels = FALSE)) -> pie2008power
pie2008power    

## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
library(forecast)
## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(EnergyConsumption, Weekday == "Monday" & Hour == 20 & Minute == 1)
## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Heater_AC, frequency=52, start=c(2007,1))
library(ggplot2)
library(ggfortify)
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3") 
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season ) 
summary(fitSM3)
autoplot(fitSM3$residuals)
forecastfitSM3 <- forecast(fitSM3, h=20)
forecastfitSM3$residuals
postResample(fitSM3$residuals,forecastfitSM3$residuals)
plot(forecastfitSM3)
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))
summary(forecastfitSM3c)
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")
## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
tsSM3_070809weekly2 <- ts(tsSM3_070809weekly*2)
seqplot.ts(tsSM3_070809weekly, tsSM3_070809weekly)
autoplot(tsSM3_070809weekly,tsSM3_070809weekly2)
forecast


PRI    