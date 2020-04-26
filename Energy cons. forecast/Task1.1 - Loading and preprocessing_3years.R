#### ---- Libraries ----
  library(RMySQL)
  library(dplyr)
  library(lubridate) #To deal with TimeSeries
  library(rstudioapi) #To set manually WD, but does not work
  library(zoo) #As yearmon
  library(tidyverse)  

#### ---- Importing Data ----

  #http://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption
  ## Create a database connection 
    con = dbConnect(MySQL(), user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

  ## List the tables contained in the database
    dbListTables(con)

  ## Lists attributes contained in a table
    dbListFields(con,"yr_2006")

  ## Use asterisk to specify all attributes or attribute names to specify attributes for download
    yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
    yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
    yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
    yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
    yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power,
                      Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

    #Voltage and intensity are P=V*I, and the reactive power is just the 
    #comple power given by capacitors and inductors
  
#### ---- Preprocessing----
    
    ## Combine tables into one dataframe using dplyr
    EnergyConsumption <- bind_rows(yr_2006, yr_2007, yr_2008, yr_2009, yr_2010)

    ## Combine Date and Time attribute values in a new attribute column
    EnergyConsumption <-cbind(EnergyConsumption,paste(EnergyConsumption$Date,EnergyConsumption$Time), stringsAsFactors=FALSE)
    

    ## Give the new attribute in the 6th column a header name 
    names(EnergyConsumption)
    colnames(EnergyConsumption)[7] <-"DateTime"
    
    ## Move the DateTime attribute within the dataset
    EnergyConsumption<- subset(EnergyConsumption, select=c(DateTime,Date:Sub_metering_3))
    ## Convert DateTime from POSIXlt to POSIXct 
    EnergyConsumption$DateTime <- as.POSIXct(EnergyConsumption$DateTime, "%Y/%m/%d %H:%M:%S")
    
    ## Add the time zone
    attr(EnergyConsumption$DateTime, "tzone") <- "Europe/Paris"
    
    ## Inspect the data types
    str(EnergyConsumption)
    
    ## Create "year" attribute with lubridate
    EnergyConsumption$Year <- year(EnergyConsumption$DateTime)
    EnergyConsumption$Quarter <- quarter(EnergyConsumption$DateTime)
    EnergyConsumption$Month <- month(EnergyConsumption$DateTime)
    EnergyConsumption$Week <- week(EnergyConsumption$DateTime)
    EnergyConsumption$Weekday <- weekdays(EnergyConsumption$DateTime)
    EnergyConsumption$Day <- day(EnergyConsumption$DateTime)
    EnergyConsumption$Hour <- hour(EnergyConsumption$DateTime)
    EnergyConsumption$Minute <- minute(EnergyConsumption$DateTime)
    
    
#### ---- Feature Engineering ----
    
    ## Global active power is in kW, while the sub-metering measurements are W/h
    EnergyConsumption <- EnergyConsumption %>% mutate(GAP=Global_active_power*1000/60)
    
    ## Electricity not measured by the sub-meters
    
    EnergyConsumption <- EnergyConsumption %>% 
      mutate(Others=GAP - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)
    
    ## Renaming
    
    EnergyConsumption <- EnergyConsumption %>% rename(Kitchen = Sub_metering_1,
                                                      Laundry = Sub_metering_2,
                                                      Heater_AC = Sub_metering_3)
    
    ## Remove old Date and Time variables
    EnergyConsumption$Date<-NULL
    EnergyConsumption$Time<-NULL
    EnergyConsumption$Global_active_power<-NULL
    EnergyConsumption$Sub_metering_1<-NULL
    EnergyConsumption$Sub_metering_2<-NULL
    EnergyConsumption$Sub_metering_3<-NULL
    
    ## Relocating columns
    
    EnergyConsumption<- subset(EnergyConsumption, select=c(DateTime, GAP, Others, Kitchen:Heater_AC, Year:Minute))
 
    ## NA Values - We do not have
    sum(is.na(EnergyConsumption))
    
    ## Create another variable that shows year and month in one column
    EnergyConsumption$Date <- as.yearmon(paste(EnergyConsumption$Year, 
                                               EnergyConsumption$Month,
                                               sep = "-"))
      #EnergyConsumption$Date<-format(EnergyConsumption$Date, "%B %Y") 
        #This is to change the format (Capital B means the whole month name)
    
    ## Changing Data Types

    
    EnergyConsumption <- EnergyConsumption %>% 
      mutate_at(c("Year", "Date", "Month","Quarter","Weekday"), as.factor)
    

    #There is a small portion of Data from Jan 2010 taken (half a day after midnight)
    #Filter the tail data from 2010
    str(EnergyConsumption)
    EnergyConsumption <- filter(EnergyConsumption,  Year!=2006)
    EnergyConsumption$Year<-factor(EnergyConsumption$Year)
    levels(EnergyConsumption$Year)
    summary(EnergyConsumption)
    
    ##Changing Months, Weekday and season to factors
    levels(EnergyConsumption$Month)
    EnergyConsumption$Month <- factor(EnergyConsumption$Month,
                                   levels = c(1:12),
                                   labels = c("January","February","March","April",
                                              "May", "June", "July", "August", "September",
                                              "October", "November", "December"))
    levels(EnergyConsumption$Month)     
    
    levels(EnergyConsumption$Weekday) #For some reason, they are not properly ordered
    EnergyConsumption$Weekday <- 
      factor(EnergyConsumption$Weekday, levels =c("Monday","Tuesday","Wednesday",
                                                  "Thursday", "Friday","Saturday",
                                                  "Sunday"), ordered = T)
    levels(EnergyConsumption$Weekday)
    
    levels(EnergyConsumption$Quarter)
    EnergyConsumption$Quarter <- factor(EnergyConsumption$Quarter,
                           levels = c(1:4),
                           labels = c("Winter",
                                      "Spring",
                                      "Summer",
                                      "Fall"))
    levels(EnergyConsumption$Quarter)
    
    ## Create Seasons for each year
    levels(EnergyConsumption$Date)
    
    EnergyConsumption <- EnergyConsumption  %>% mutate(Seasons = 
          case_when(Date %in% c("Jan 2007", "Feb 2007") ~ "Winter 2007",
                    Date %in% c("Mar 2007", "Apr 2007", "May 2007") ~ "Spring 2007",
                    Date %in% c("Jun 2007", "Jul 2007", "Aug 2007") ~ "Summer 2007",
                    Date %in% c("Sep 2007", "Oct 2007", "Nov 2007") ~ "Fall 2007",
                    Date %in% c("Dec 2007", "Jan 2008", "Feb 2008") ~ "Winter 2008",
                    Date %in% c("Mar 2008", "Apr 2008", "May 2008") ~ "Spring 2008",
                    Date %in% c("Jun 2008", "Jul 2008", "Aug 2008") ~ "Summer 2008",
                    Date %in% c("Sep 2008", "Oct 2008", "Nov 2008") ~ "Fall 2008",
                    Date %in% c("Dec 2008", "Jan 2009", "Feb 2009") ~ "Winter 2009",
                    Date %in% c("Mar 2009", "Apr 2009", "May 2009") ~ "Spring 2009",
                    Date %in% c("Jun 2009", "Jul 2009", "Aug 2009") ~ "Summer 2009",
                    Date %in% c("Sep 2009", "Oct 2009", "Nov 2009") ~ "Fall 2009",
                    Date %in% c("Dec 2009") ~ "Winter 2010" ))

    ## It would be nice to group in Morning, Afternoon and so on... 
   
     EnergyConsumption <- EnergyConsumption %>%  
      mutate(DayTime =  ifelse(Hour %in% 6:12, "Morning",
                            ifelse(Hour %in% 13:18, "Afternoon",
                                 ifelse(Hour %in% 19:22, "Evening", "Night"))))
     
    ##And also separate between weekend and weekday
      
     EnergyConsumption <- EnergyConsumption %>% 
       mutate(Weekend =
                ifelse(Weekday ==  "Saturday" |  Weekday ==  "Sunday", 
                  "Weekend", "Weekday"))
    
    ## Change data types
    str(EnergyConsumption)
    EnergyConsumption$Weekend<-as.factor(EnergyConsumption$Weekend)
    EnergyConsumption$DayTime<-as.factor(EnergyConsumption$DayTime)
    EnergyConsumption$Seasons<-as.factor(EnergyConsumption$Seasons)
    levels(EnergyConsumption$Seasons)

    #Factor seasons is disordered
    levels(EnergyConsumption$Seasons)
    EnergyConsumption$Seasons <- 
      factor(EnergyConsumption$Seasons, levels =c("Winter 2007","Spring 2007","Summer 2007",
                                                  "Fall 2007", "Winter 2008","Spring 2008",
                                                  "Summer 2008","Fall 2008","Winter 2009",
                                                  "Spring 2009","Summer 2009","Fall 2009",
                                                  "Winter 2010"), ordered = T)
    levels(EnergyConsumption$Seasons)
    
    #DayTime is disordered
    levels(EnergyConsumption$DayTime)
    EnergyConsumption$DayTime <- 
      factor(EnergyConsumption$DayTime, levels =c("Morning", "Afternoon", "Evening", "Night"), ordered = T)
    levels(EnergyConsumption$DayTime)
    
    ## Final Data Summary
    summary(EnergyConsumption)
    sink("summarydata.txt", split=TRUE)
    print(summary(EnergyConsumption))
    sink()
    
    #FINAL REORDER
    names(EnergyConsumption)
    EnergyConsumption<- subset(EnergyConsumption, 
                               select=c(DateTime, GAP, Others, Kitchen:Heater_AC, 
                                        Year, Quarter, Month, Seasons, Date, Week, 
                                        Weekday, Weekend, DayTime, Day, Hour, Minute))
    
   
    ## To save the clean data as rds
    EnergyConsumptionClean<-EnergyConsumption
    #saveRDS(EnergyConsumptionClean, file = "EnergyCleanData.rds")
    saveRDS(EnergyConsumptionClean, '/Users/alessandro/M3T1/EnergyConsumptionClean_3years.RDS')
    
    #Save CSV
    write.csv2(EnergyConsumptionClean,'/Users/alessandro/M3T1/EnergyConsumptionClean_3years.csv', row.names = FALSE)
    
