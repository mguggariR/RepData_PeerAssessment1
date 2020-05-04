library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

## load file as a data frame 
        rawDF <- read.csv("activity.csv", colClasses = c("numeric","Date","numeric"))

# spDF1<- split(rawDF[,"steps"],rawDF$date)
# spDFF <- sapply(spDF1, sum, na.rm=TRUE)
# FinalDF <- data.frame(date=as.Date(names(spDFF)), sum=spDFF)

## Calculate daily sums of steps for each day as a new data frame
        FinalDF <- as.data.frame( rawDF %>% group_by(date) %>%
                summarise(dailySteps=sum(steps, na.rm = TRUE)))

## Create a histogram of daily steps with ggplot 
       ggplot(data=FinalDF, aes(x=dailySteps)) +
                geom_histogram(color="darkblue", fill="lightblue", bins = 30) +
                labs(title = "Histogram of number of steps taken each day")

        myPlot

## create mean and median of dayly number of steps
        dailyMean <- mean(FinalDF$dailySteps)
        dailyMedian <- median(FinalDF$dailySteps)

## Calculate average number of steps for each time interval as a new data frame
        FinalDF2 <- as.data.frame( rawDF %>% group_by(interval) %>%
                                          summarise(intervalMean=mean(steps,na.rm = TRUE)))
        
## create a plot of average number of steps for each time interval        
        ggplot(data=FinalDF2, aes(x=interval, y=intervalMean))+ 
                geom_line(color="steelblue")+
                labs(title="Average number of steps for each time interval", ylab="Mean of steps")
        
## the interval with maximum mean value of steps
        MaxInterval <- FinalDF2[FinalDF2$intervalMean==max(FinalDF2$intervalMean),"interval"]                
                
##Number of missing values in dataframe denoted by NA        
        naCount <- sum(is.na(rawDF$steps))        

        ## naCount <- data.frame( NAcounts=sapply(rawDF, function(y) sum(length(which(is.na(y))))))              
        
 ## Create a new dataframe with NAs replaced by avarage for that interval
        # newDF <- merge(rawDF,FinalDF2, by.y="interval")
        # newDF1 <- arrange(newDF, date)
        # newDF2 <- mutate(newDF1, ModSteps=ifelse(is.na(steps),intervalMean,steps))
                         
        ModDF <- rawDF %>%
                merge(FinalDF2, by.y="interval")%>%
                arrange(date)%>%
                mutate(ModSteps=ifelse(is.na(steps),intervalMean,steps))
        
        ## Calculate daily sums of steps for each day with and without missing as a new data frame
        ModFinalDF <- as.data.frame( ModDF %>% group_by(date) %>%
                                summarise(dailySteps=sum(steps, na.rm = TRUE),ModDailySteps=sum(ModSteps)))
        
        xDF <- select(ModFinalDF, dailySteps, ModDailySteps)
        
        gg <- melt(xDF)
        names(gg) <- c("variable", "steps")
        ggplot(gg, aes(x=steps, fill=variable)) +
                geom_histogram(binwidth=200)+
                facet_grid(variable~.)+
                labs(title = "Histogram with and without filling missing data")        
                
        ## Create a histogram of daily steps from modified dataset with ggplot 
        # ggplot(data=ModFinalDF) +
        #         geom_histogram(aes(x=ModDailySteps, color="Modified", fill="Modified"), bins = 50) +
        #         geom_histogram(aes(x=dailySteps, color="Original", fill="Original"), bins=50)+
        #         labs(title = "Histogram of modified number of steps taken each day", ylab="steps")
        
        ## create mean and median of dayly number of steps
        ModdailyMean <- mean(FinalDF$dailySteps)
        ModdailyMedian <- median(FinalDF$dailySteps)      
                         
                         
                         
 
 
        
        
        