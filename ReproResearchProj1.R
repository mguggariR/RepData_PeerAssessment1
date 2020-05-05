library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

## load file as a data frame 
        rawDF <- read.csv("activity.csv", colClasses = c("numeric","Date","numeric"))

## Calculate daily sums of steps for each day as a new data frame
        DailySumsDF <- as.data.frame( rawDF %>% group_by(date) %>%
                summarise(dailySteps=sum(steps, na.rm = TRUE)))

## Create a histogram of daily steps with ggplot 
       ggplot(data=DailySumsDF, aes(x=dailySteps)) +
                geom_histogram(color="darkblue", fill="lightblue", binwidth = 200) +
                labs(title = "Histogram of number of steps taken each day")

## create mean and median of dayly number of steps
        dailyMean <- mean(DailySumsDF$dailySteps)
        dailyMedian <- median(DailySumsDF$dailySteps)

## Calculate average number of steps for each time interval as a new data frame
        StepsPerIntervalDF <- as.data.frame( rawDF %>% group_by(interval) %>%
                                          summarise(intervalMean=mean(steps,na.rm = TRUE)))
        
## create a plot of average number of steps for each time interval        
        ggplot(data=StepsPerIntervalDF, aes(x=interval, y=intervalMean))+ 
                geom_line(color="steelblue")+
                labs(title="Average number of steps for each time interval", ylab="Mean of steps")
        
## the interval with maximum mean value of steps
        MaxStepInterval <- StepsPerIntervalDF[StepsPerIntervalDF$intervalMean==max(StepsPerIntervalDF$intervalMean),"interval"]                
                
##Number of missing values in dataframe denoted by NA        
        naCount <- sum(is.na(rawDF$steps))        

         
        
 ## Create a new dataframe with NAs replaced by avarage for that interval
        ModDF <- rawDF %>%
                merge(StepsPerIntervalDF, by.y="interval")%>%
                arrange(date)%>%
                mutate(ModSteps=ifelse(is.na(steps),intervalMean,steps))
        
## Calculate daily sums of steps for each day with and without missing as a new data frame
        ModFinalDF <- as.data.frame( ModDF %>% group_by(date) %>%
                                summarise(dailySteps=sum(steps, na.rm = TRUE),ModDailySteps=sum(ModSteps)))
        xDF <- select(ModFinalDF, dailySteps, ModDailySteps)
        gg <- melt(xDF)
        names(gg) <- c("variable", "steps")

## Create a histogram of daily steps from modified dataset with ggplot 
        ggplot(gg, aes(x=steps, fill=variable)) +
                geom_histogram(binwidth=200)+
                facet_grid(variable~.)+
                labs(title = "Histogram with and without filling missing data")        
                
## create mean and median of daily number of steps with modified dataset
        ModdailyMean <- mean(ModFinalDF$ModDailySteps)
        ModdailyMedian <- median(ModFinalDF$ModDailySteps)      
                         
## *******************
        myFunc <- function(x){
                ifelse((weekdays(x, abbreviate=TRUE) == "Sat" | 
                                weekdays(x, abbreviate=TRUE) == "Sun"), "WeekEnd", "WeekDay")
                       }
                         
## new Data Frame factored with week and weekend days
        ModDayDF <- mutate(ModDF, Day=myFunc(date)) 
        
        myGrSumDF <- as.data.frame(ModDayDF %>% 
                        select(date, Day, interval, ModSteps)%>% 
                        group_by(Day, interval)%>% 
                        summarise(AverageSteps=mean(ModSteps)))
        
        ggplot(data=myGrSumDF, aes(x=interval, y=AverageSteps, color=Day, group=Day)) + 
                geom_line()+
                labs(title="Average steps for each time interval")+
                facet_grid(Day ~ .)
                
        
        
        ModWkendDF <- filter(ModDayDF, Day=="WeekEnd")                
        ModWkdayDF <- filter(ModDayDF, Day=="WeekDay")
        
        
        
        myTestDF <- select(ModDayDF, date, Day, interval, ModSteps)
        myTestDF1 <- filter(myTestDF, (interval=="0" | interval=="5" |interval=="10" | interval=="15"))
        myGrTestDF <- myTestDF1 %>% group_by(Day, interval)
        myGrSumDF <-  as.data.frame( summarise(myGrTestDF, intervalMean=mean(ModSteps)))
        
        stepsperday <- as.data.frame( myTestDF1 %>% group_by(Day) %>% group_by(interval)%>%
                                              summarise(intervalMean=mean(ModSteps))) 
 
## Calculate average number of steps for each time interval for both week days in a new data frame
        WkStepsPerIntervalDF <- as.data.frame( ModWkdayDF %>% group_by(Day) %>% group_by(interval)%>%
                                                     summarise(intervalMean=mean(ModSteps)))       
## create a plot of average number of steps for each time interval        
        ggplot(data=WkStepsPerIntervalDF, aes(x=interval, y=intervalMean))+ 
                geom_line(color="steelblue")+
                labs(title="Average number of steps for each time interval", ylab="Mean of steps")  
        
## Calculate average number of steps for each time interval for both weekend days in a new data frame
        WkEndStepsPerIntervalDF <- as.data.frame( ModWkendDF %>% group_by(Day) %>% group_by(interval)%>%
                                                       summarise(intervalMean=mean(ModSteps)))         
        ggplot(data=WkEndStepsPerIntervalDF, aes(x=interval, y=intervalMean))+ 
                geom_line(color="red")+
                labs(title="Average number of steps for each time interval during weekend", ylab="Mean of steps")  
        
  testDF <-  ModDayDF  %>% group_by(Day) %>% group_by(interval)   
          
        