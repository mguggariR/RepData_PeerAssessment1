library(dplyr, warn.conflicts=FALSE, verbose = FALSE, quietly = TRUE)
library(ggplot2)
library(tidyr)
library(reshape2, warn.conflicts=FALSE, verbose = FALSE, quietly = TRUE)

## load file as a data frame 
        rawDF <- read.csv("activity.csv", colClasses = c("numeric","Date","numeric"))

## Calculate daily sums of steps for each day as a new data frame
        DailySumsDF <- as.data.frame( rawDF %>% group_by(date) %>%
                summarise(dailySteps=sum(steps, na.rm = TRUE)))

## Create a histogram of daily steps with ggplot 
       HistDailySteps <- ggplot(data=DailySumsDF, aes(x=dailySteps)) +
                geom_histogram(color="darkblue", fill="lightblue", binwidth = 200) +
                ggtitle("Histogram of number of steps taken each day")+
                xlab("Number of steps per day")+
                theme(plot.title = element_text(color="red", size=14, face="bold"))
       HistDailySteps
       png(filename = "./figure/HistDailySteps.png", width = 480, height = 480) 
       HistDailySteps
       dev.off()

## create mean and median of dayly number of steps
        dailyMean <- mean(DailySumsDF$dailySteps)
        dailyMedian <- median(DailySumsDF$dailySteps)
        
## ********************
## Calculate average number of steps for each time interval as a new data frame
        StepsPerIntervalDF <- as.data.frame( rawDF %>% group_by(interval) %>%
                                          summarise(intervalMean=mean(steps,na.rm = TRUE)))
        
## create a plot of average number of steps for each time interval        
        StepsPerIntPlot <- ggplot(data=StepsPerIntervalDF, aes(x=interval, y=intervalMean))+ 
                geom_line(color="steelblue")+
                labs(title="Average number of steps for each time interval", y="Mean of steps")

        StepsPerIntPlot
        
        png(filename = "./figure/StepsPerIntPlot.png", width = 480, height = 480) 
        StepsPerIntPlot
        dev.off()
        
                
## the interval with maximum mean value of steps
        MaxStepInterval <- StepsPerIntervalDF[StepsPerIntervalDF$intervalMean==max(StepsPerIntervalDF$intervalMean),"interval"]                

## *************************************                
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
        gg <- melt(xDF, measure.vars = c(1,2), value.name = "value")
        names(gg) <- c("variable", "steps")

## Create a histogram of daily steps from modified dataset with ggplot 
        HistDoublePlot <- ggplot(gg, aes(x=steps, fill=variable)) +
                geom_histogram(binwidth=200)+
                facet_grid(variable~.)+
                labs(title = "Histogram with and without filling missing data") 
        HistDoublePlot
        png(filename = "./figure/HistDoublePlot.png", width = 480, height = 480) 
        HistDoublePlot
        dev.off()
                
## create mean and median of daily number of steps with modified dataset
        ModdailyMean <- mean(ModFinalDF$ModDailySteps)
        ModdailyMedian <- median(ModFinalDF$ModDailySteps)      
                         
## *******************
## a function to identify weekday oe weekend         
        myFunc <- function(x){
                ifelse((weekdays(x, abbreviate=TRUE) == "Sat" | 
                                weekdays(x, abbreviate=TRUE) == "Sun"), "WeekEnd", "WeekDay")
                       }
## new Data Frame factored with week and weekend days
        ModDayDF <- mutate(ModDF, Day=as.factor(myFunc(date))) 
        
        myGrSumDF <- as.data.frame(ModDayDF %>% 
                        select(date, Day, interval, ModSteps)%>% 
                        group_by(Day, interval)%>% 
                        summarise(AverageSteps=mean(ModSteps)))
        
        panelPlot <- ggplot(data=myGrSumDF, aes(x=interval, y=AverageSteps, color=Day, group=Day)) + 
                geom_line()+
                labs(title="Average steps for each time interval")+
                facet_grid(Day ~ .)
                
        
        panelPlot

        png(filename = "./figure/panelPlot.png", width = 480, height = 480) 
        panelPlot
        dev.off()       
        

        