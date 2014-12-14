dt<-read.table('activity.csv',sep=',',header=T)
dt$date<-strptime(dt$date,'%Y-%m-%d')
s<-split(dt,as.Date(dt$date))
daily.steps<-sapply(s,function(x) sum(x$steps))
hist(daily.steps)

mean(daily.steps,na.rm=T)
median(daily.steps,na.rm=T)

s<-split(dt,dt$interval)
mean.interval<-sapply(s,function(x) mean(x$steps,na.rm=T))

intervals<-as.numeric(names(mean.interval))

plot(intervals,mean.interval,type='l')
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
intervals[mean.interval==max(mean.interval)]

missing<-is.na(dt$steps)
sum(missing)
filled<-dt
filled$steps[missing]<-mean.interval[as.character(filled$interval[missing])]

s<-split(filled,as.Date(filled$date))
daily.steps.filled<-sapply(s,function(x) sum(x$steps))
hist(daily.steps.filled)
mean(daily.steps.filled)
median(daily.steps.filled)

#rename
d<-filled

day<-weekdays(d$date)
weekend<-day=="Saturday" | day=="Sunday"
w<-numeric()
w[weekend]<-'weekend'
w[!weekend]<-'weekday'
w<-factor(w,levels=c('weekend','weekday'))

d$weekend<-w

dat<-aggregate(d$steps,by=list(d$weekend,d$interval),FUN=mean)
names(dat)<-c('weekday','interval','steps')
xyplot(steps~interval|weekday,data=dat,layout=c(1,2),type='l')




