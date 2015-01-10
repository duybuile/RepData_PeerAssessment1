PA1_template <- function(){
  
  ninterval = 24*12
  ndays = 61
  
  dat <- read.csv(file = "activity.csv", header = TRUE, sep = ",")
  head(dat, 5)
  
  steps <- dat$steps
  steps <- steps[!is.na(steps)]
  
  
  
  library(datasets)
  hist(steps )
  
  days <- seq(from=as.Date('2012-10-01'), to=as.Date("2012-11-30"),by='days' )
  
  totStep <- numeric(ndays)
  
  for( i in seq_along(days)){
    sub = subset(dat, as.Date(dat$date) == days[i])
    totStep[i] <- sum(sub$steps, na.rm = TRUE)
  }
    
  meanStep <- numeric(ndays)
  medianStep <- numeric(ndays)
  for ( i in seq_along(days) )
  {
    
    sub = subset(dat, as.Date(dat$date) == days[i])
    meanStep[i] <- mean(sub$steps, na.rm = TRUE)
    medianStep[i] <- median(sub$steps, na.rm = TRUE)
  }
  
  
  #part 3
  meanDay <- numeric(ninterval)
  interval <- dat[1:ninterval,]$interval
  for( i in 1:length(interval)){
    
    sub = subset(dat, dat$interval == interval[i])
    meanDay[i] = mean(sub$steps, na.rm = TRUE)
  }
  
  plot(x = interval, y = meanDay, type = "l", 
       main="Time series of 5 minutes interval",
       xlab = "Interval", ylab = "Averaged steps across all days",
       col = "blue")
  
  interval[which.max(meanDay)]
  
  #part 4
  #total number of missing values
  sum(is.na(x = dat))
  #fill in missing values
  newdat <- dat
  for( i in 1:ninterval){
    
    newdat[which(is.na(newdat[newdat$interval==interval[i],])),]$steps <- meanDay[i]
    
  } 
}