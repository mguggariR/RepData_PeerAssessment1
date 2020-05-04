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
                
## create mean and median of dayly number of steps
        ModdailyMean <- mean(ModFinalDF$ModDailySteps)
        ModdailyMedian <- median(ModFinalDF$ModDailySteps)      
                         
                         
                         
 
 
        
        
        