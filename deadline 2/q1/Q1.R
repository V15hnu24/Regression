library(ggpubr)
library(ggplot2)
library(caTools)
library(car)
library(ggpubr)
library(tidyverse)
library(magrittr)
library(stargazer)
library(dplyr)


data <- read.csv("C:/Users/799vi/Desktop/Courses/eco/dataAssignment2/data.csv")


#Considering both v28
d <- data %>% filter(data$season=="Rabi")
df <- d %>% select(gdp,beds,tap,v28 )
rg <- lm(d$v41 ~., df)
summary(rg)

d <- data %>% filter(data$season=="Kharif")
df <- d %>% select(gdp,beds,tap,v28 )
rg <- lm(d$v41 ~., df)
summary(rg)


#Considering both v16
d <- data %>% filter(data$season=="Rabi")
df <- d %>% select(gdp,beds,tap,v16 )
rg <- lm(d$v41 ~., df)
summary(rg)

d <- data %>% filter(data$season=="Kharif")
df <- d %>% select(gdp,beds,tap,v16 )
rg <- lm(d$v41 ~., df)
summary(rg)

#Considering both v16 and v28
d <- data %>% filter(data$season=="Rabi")
df <- d %>% select(gdp,beds,tap,v16, v28 )
rg <- lm(d$v41 ~., df)
summary(rg)

d <- data %>% filter(data$season=="Kharif")
df <- d %>% select(gdp,beds,tap,v16, v28 )
rg <- lm(d$v41 ~., df)
summary(rg)

d <- data %>% filter(data$season=="Rabi")
df <- d %>% select(gdp,beds,tap,v16, v28)
rg <- lm(d$v41 ~., df)
summary(rg)

#c
r <- residuals(rg)
p1 <- hist(r)
plot( p1, col=rgb(0,0,1,1/4),ylim=c(0,8000), main="Residual in Rabi",xlim=c(-100,100),xlab="value")


res <- cor.test(d$v41,d$v16,method = "pearson",use = "complete.obs")
res
res <- cor.test(d$v41,d$v28,method = "pearson",use = "complete.obs")
res

d <- data %>% filter(data$season=="Kharif")
df <- d %>% select(gdp,beds,tap,v16, v28,index)
rg <- lm(d$v41 ~., df)
summary(rg)

r <- residuals(rg)
r
p1 <- hist(r)
plot( r, col=rgb(1,0,0,1/4),ylim=d$v41, main="Residual in Kharif",xlim=d$index,xlab="value")

#plot for b
x<- d$index[1:15183]
y<-r[1:15183]
length(x)
length(y)
plot(x,y, xlab = 'Index', ylab = 'Residuals')

#plot for c
x<- 
y<-predict()
length(x)
length(y)
plot(x,y, xlab = 'Index', ylab = 'Residuals')


res <- cor.test(d$v41,d$v16,method = "pearson",use = "complete.obs")
res
res <- cor.test(d$v41,d$v28,method = "pearson",use = "complete.obs")
res

dk <- mutate(d,l_beds = log(d$beds))
d <- dk %>% filter(data$season=="Rabi")
df <- d %>% select(dk$gdp,dk$l_beds,dk$tap,dk$v16, d$v28,d$index)
rg <- lm(d$v41 ~., df)
summary(rg)

#c
r <- residuals(rg)
p1 <- hist(r)
plot( r, col=rgb(1,0,0,1/4),ylim=d$v41, main="Residual in Kharif",xlim=d$index,xlab="value")

x<- d$index[1:15183]
y<-r
length(x)
length(y)
plot(x,y, xlab = 'Index', ylab = 'Residuals')



data <- read.csv("C:/Users/799vi/Desktop/Courses/eco/dataAssignment2/updated.csv")

#Best model
dk <- mutate(data,l_beds = log(beds))
d <- dk %>% filter(dk$season=="Rabi")
df <- d %>% select(gdp,l_beds,tap,v28,index)
rg <- lm(d$v41 ~., df)
summary(rg)
sum(residuals(rg))

r <- residuals(rg)
k <- d$gdp[1:15177]*r
k <- k[!is.na(k)]
sum(k,na.rm = TRUE)
p1 <- hist(k)
plot( p1, col=rgb(1,0,0,1/4),ylim=c(0,10000), main="rabi",xlim=c(-750000,500000),xlab="value")

r <- residuals(rg)
k <- d$tap[1:15177]*r
sum(k,na.rm = TRUE)
p1 <- hist(k)
plot( p1, col=rgb(1,0,0,1/4),ylim=c(0,10000), main="rabi",xlim=c(-2500,2500),xlab="value")

r <- residuals(rg)
k <- d$l_beds[1:15177]*r
sum(k,na.rm = TRUE)
p1 <- hist(k)
plot( p1, col=rgb(1,0,0,1/4),ylim=c(0,10000), main="rabi",xlim=c(-1000,2000),xlab="value")

r <- residuals(rg)
k <- d$v28[1:15177]*r
sum(k,na.rm = TRUE)
p1 <- hist(k)
plot( p1, col=rgb(1,0,0,1/4),ylim=c(0,10000), main="rabi",xlim=c(-1000,2000),xlab="value")

r <- residuals(rg)
k <- d$index[1:15177]*r
sum(k,na.rm = TRUE)
p1 <- hist(k)
plot( p1, col=rgb(1,0,0,1/4),ylim=c(0,10000), main="rabi",xlim=c(-500,500),xlab="value")


x = d$v41
y = d$index
length(x)
length(y)
plot(x,y, ylab=' Yield INdex',
     xlab='Health indicator')

x=d$v41[1:15177]
y=predict(rg)
length(x)
length(y)
plot(x,y, ylab='Predicted Values',
     xlab='Actual Values',main = 'Health indicator')

x<- d$index[1:15183]
y<-r[1:15183]
length(x)
length(y)
plot(x,y, xlab = 'Index', ylab = 'Residuals')

r <- residuals(rg)
p1 <- hist(r)
plot( p1, col=rgb(1,1,0,1/4),ylim=c(0,8900), main="Residual in Rabi",xlim=c(-75,100),xlab="value")


dk <- mutate(data,l_beds = log(beds))
d <- dk %>% filter(dk$season=="Kharif")
df <- d %>% select(gdp,l_beds,tap,v28,index)
rg <- lm(d$v41 ~., df)
summary(rg)

r <- residuals(rg)
k <- d$gdp[1:16851]*r
sum(k,na.rm = TRUE)
p1 <- hist(k)
plot( p1, col=rgb(1,0,0,1/4),ylim=c(0,15000), main="Kharif",xlim=c(-75000,50000),xlab="value")

r <- residuals(rg)
k <- d$tap[1:16851]*r
sum(k,na.rm = TRUE)
p1 <- hist(k)
plot( p1, col=rgb(1,0,0,1/4),ylim=c(0,10000), main="Kharif",xlim=c(-2500,2500),xlab="value")

r <- residuals(rg)
k <- d$l_beds[1:16851]*r
sum(k,na.rm = TRUE)
p1 <- hist(k)
plot( p1, col=rgb(1,0,0,1/4),ylim=c(0,10000), main="Kharif",xlim=c(-1000,2000),xlab="value")

r <- residuals(rg)
k <- d$v28[1:16851]*r
sum(k,na.rm = TRUE)
p1 <- hist(k)
plot( p1, col=rgb(1,0,0,1/4),ylim=c(0,10000), main="Kharif",xlim=c(-1000,2000),xlab="value")

r <- residuals(rg)

k <- d$index[1:16851]*r
sum(k,na.rm = TRUE)
p1 <- hist(k)
plot( p1, col=rgb(1,0,0,1/4),ylim=c(0,10000), main="Kharif",xlim=c(-500,500),xlab="value")


x = d$v41
y = d$index
length(x)
length(y)
plot(x,y, ylab=' Yield INdex',
     xlab='Health indicator')

x=d$v41[1:16851]
y=predict(rg)
length(x)
length(y)
plot(x,y, ylab='Predicted Values',
     xlab='Actual Values',main = 'Health indicator')

x<- d$index[1:15183]
y<-r[1:15183]
length(x)
length(y)
plot(x,y, xlab = 'Index', ylab = 'Residuals')

r <- residuals(rg)
p1 <- hist(r)
plot( p1, col=rgb(1,0,0,1/4),ylim=c(0,8900), main="Residual in Kharif",xlim=c(-75,100),xlab="value")
