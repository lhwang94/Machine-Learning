
rm(list=ls())

wd=paste('/Users/luhaowang/Download/',sep="")

# Set the working directory
setwd(wd)

library(ISLR)

attach(Auto)

set.seed (1)
lm.fit=lm(mpg~horsepower ,data=Auto)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto)
lm.fit4=lm(mpg~poly(horsepower,4),data=Auto)


quartz()
png(filename ="MPG_HORSEPOWER.png",
    width =800, height = 800,units = "px", 
    pointsize = 13,bg = "white", res =100)
plot(horsepower,mpg,ylim=c(5,50))
dev.off() 
dev.off()

quartz()
png(filename ="MPG_HORSEPOWER_2.png",
    width =800, height = 800,units = "px", 
    pointsize = 13,bg = "white", res =100)
plot(horsepower,mpg,ylim=c(5,50))
lines(horsepower,lm.fit$fitted.values, type="p",col="red", lwd=2)
dev.off() 
dev.off()


quartz()
png(filename ="MPG_HORSEPOWER_3.png",
    width =800, height = 800,units = "px", 
    pointsize = 13,bg = "white", res =100)
plot(horsepower,mpg,ylim=c(5,50))
lines(horsepower,lm.fit2$fitted.values, type="d",col="blue", lwd=2)
dev.off() 
dev.off()


quartz()
png(filename ="MPG_HORSEPOWER_4.png",
    width =800, height = 800,units = "px", 
    pointsize = 13,bg = "white", res =100)
plot(horsepower,mpg,ylim=c(5,50))
lines(horsepower,lm.fit3$fitted.values, type="p",col="green", lwd=2)
dev.off() 
dev.off()


quartz()
png(filename ="MPG_HORSEPOWER_5.png",
    width =800, height = 800,units = "px", 
    pointsize = 13,bg = "white", res =100)
plot(horsepower,mpg,ylim=c(5,50))
lines(horsepower,lm.fit4$fitted.values, type="p",col="black", lwd=2)
dev.off() 
dev.off()




