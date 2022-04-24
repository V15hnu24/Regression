# Data Assignment1 
# Vishnu Vardhan 2020480

rm(list =ls())

install.packages("tidyverse")
install.packages("magrittr")
install.packages("stargazer")

library(tidyverse)
library(magrittr)
library(stargazer)

directory <- "C:/Users/799vi/Desktop/eco/dataAssignment1/"
getwd()
data <- read.csv(paste0(directory, "vish_final1.csv"))

str(data)

# Mean and standard deviation for Q2a

data %>% stargazer(type = "text")
data %>% select(v40,v42,v43,v44,v45,v46) %>% stargazer(type = "text")

# Median of Q2a

df1 <- data %>% select(v40,v42,v43,v44,v45,v46) 
apply(df1,2,median,na.rm =TRUE)

#Mode for Q2 a
install.packages("DescTools")
library("DescTools")     

Mode(data$v40,na.rm =TRUE)   
Mode(data$v42,na.rm =TRUE)
Mode(data$v43,na.rm =TRUE)
Mode(data$v44,na.rm =TRUE)
Mode(data$v45,na.rm =TRUE)
Mode(data$v46,na.rm =TRUE)

# Histograms

install.packages("ggplot2")
library(ggplot2)

#Year-wise histograms

#v40

Yearwise_Sepsis_v40 <- data[data$year==2011,"v40"]
ydata_2012 <- data[data$year==2012,"v40"]
ydata_2013 <- data[data$year==2013,"v40"]
ydata_2014 <- data[data$year==2014,"v40"]
ydata_2015 <- data[data$year==2015,"v40"]
ydata_2016 <- data[data$year==2016,"v40"]

p1 <- hist(Yearwise_Sepsis_v40)
p2 <- hist(ydata_2012,ylim=c(0,10000))
p3 <- hist(ydata_2013,ylim=c(0,10000))
p4 <- hist(ydata_2014,ylim=c(0,10000))
p5 <- hist(ydata_2015,ylim=c(0,10000))
p6 <- hist(ydata_2016,ylim=c(0,10000))

plot( p1, col=rgb(0,0,1,1/4),ylim=c(0,7000), main="Year wise Sepsis",xlim=c(0,100),xlab="value")
plot( p2, col=rgb(1,1,0,1/4),add=T)
plot( p3, col=rgb(1,0,1,1/4),add=T)
plot( p4, col=rgb(1,0,0,1/4),add=T)
plot( p5, col=rgb(0,1,1,1/4),add=T)
plot( p6, col=rgb(0,1,0,1/4),add=T)
legend("right", legend = c("2011", "2012","2013","2014","2015","2016"),
       lwd = 4, col = c(rgb(0,0,1,1/4),rgb(1,1,0,1/4),rgb(1,0,1,1/4),rgb(1,0,0,1/4),rgb(0,1,1,1/4),rgb(0,1,0,1/4)))

#V42

ydata_2011 <- data[data$year==2011,"v42"]
ydata_2012 <- data[data$year==2012,"v42"]
ydata_2013 <- data[data$year==2013,"v42"]
ydata_2014 <- data[data$year==2014,"v42"]
ydata_2015 <- data[data$year==2015,"v42"]
ydata_2016 <- data[data$year==2016,"v42"]

p1 <- hist(ydata_2011,ylim=c(0,3000))
p2 <- hist(ydata_2012,ylim=c(0,3000))
p3 <- hist(ydata_2013,ylim=c(0,3000))
p4 <- hist(ydata_2014,ylim=c(0,3000))
p5 <- hist(ydata_2015,ylim=c(0,3000))
p6 <- hist(ydata_2016,ylim=c(0,3000))

plot( p1, col=rgb(0,0,1,1/4),ylim=c(0,2500),main="Year wise lbw",xlim=c(0,100),xlab="value")
plot( p2, col=rgb(1,1,0,1/4),add=T)
plot( p3, col=rgb(1,0,1,1/4),add=T)
plot( p4, col=rgb(1,0,0,1/4),add=T)
plot( p5, col=rgb(0,1,1,1/4),add=T)
plot( p6, col=rgb(0,1,0,1/4),add=T)
legend("right", legend = c("2011", "2012","2013","2014","2015","2016"),
       lwd = 4, col = c(rgb(0,0,1,1/4),rgb(1,1,0,1/4),rgb(1,0,1,1/4),rgb(1,0,0,1/4),rgb(0,1,1,1/4),rgb(0,1,0,1/4)))

#v43
ydata_2011 <- data[data$year==2011,"v43"]
ydata_2012 <- data[data$year==2012,"v43"]
ydata_2013 <- data[data$year==2013,"v43"]
ydata_2014 <- data[data$year==2014,"v43"]
ydata_2015 <- data[data$year==2015,"v43"]
ydata_2016 <- data[data$year==2016,"v43"]

p1 <- hist(ydata_2011)
p2 <- hist(ydata_2012,ylim=c(0,10000))
p3 <- hist(ydata_2013,ylim=c(0,10000))
p4 <- hist(ydata_2014,ylim=c(0,10000))
p5 <- hist(ydata_2015,ylim=c(0,10000))
p6 <- hist(ydata_2016,ylim=c(0,10000))

plot( p1, col=rgb(0,0,1,1/4),ylim=c(0,7000),main="Year wise pneumonia",xlim=c(0,100),xlab="value")
plot( p2, col=rgb(1,1,0,1/4),add=T)
plot( p3, col=rgb(1,0,1,1/4),add=T)
plot( p4, col=rgb(1,0,0,1/4),add=T)
plot( p5, col=rgb(0,1,1,1/4),add=T)
plot( p6, col=rgb(0,1,0,1/4),add=T)
legend("right", legend = c("2011", "2012","2013","2014","2015","2016"),
       lwd = 4, col = c(rgb(0,0,1,1/4),rgb(1,1,0,1/4),rgb(1,0,1,1/4),rgb(1,0,0,1/4),rgb(0,1,1,1/4),rgb(0,1,0,1/4)))

#v44
ydata_2011 <- data[data$year==2011,"v44"]
ydata_2012 <- data[data$year==2012,"v44"]
ydata_2013 <- data[data$year==2013,"v44"]
ydata_2014 <- data[data$year==2014,"v44"]
ydata_2015 <- data[data$year==2015,"v44"]
ydata_2016 <- data[data$year==2016,"v44"]

p1 <- hist(ydata_2011,ylim=c(0,10000))
p2 <- hist(ydata_2012,ylim=c(0,10000))
p3 <- hist(ydata_2013,ylim=c(0,10000))
p4 <- hist(ydata_2014,ylim=c(0,10000))
p5 <- hist(ydata_2015,ylim=c(0,10000))
p6 <- hist(ydata_2016,ylim=c(0,10000))

plot( p1, col=rgb(0,0,1,1/4),ylim=c(0,8000),main="Year wise diarrhoea",xlim=c(0,100),xlab="value")
plot( p2, col=rgb(1,1,0,1/4),add=T)
plot( p3, col=rgb(1,0,1,1/4),add=T)
plot( p4, col=rgb(1,0,0,1/4),add=T)
plot( p5, col=rgb(0,1,1,1/4),add=T)
plot( p6, col=rgb(0,1,0,1/4),add=T)
legend("right", legend = c("2011", "2012","2013","2014","2015","2016"),
       lwd = 4, col = c(rgb(0,0,1,1/4),rgb(1,1,0,1/4),rgb(1,0,1,1/4),rgb(1,0,0,1/4),rgb(0,1,1,1/4),rgb(0,1,0,1/4)))

#v45
ydata_2011 <- data[data$year==2011,"v45"]
ydata_2012 <- data[data$year==2012,"v45"]
ydata_2013 <- data[data$year==2013,"v45"]
ydata_2014 <- data[data$year==2014,"v45"]
ydata_2015 <- data[data$year==2015,"v45"]
ydata_2016 <- data[data$year==2016,"v45"]

p1 <- hist(ydata_2011,ylim=c(0,10000))
p2 <- hist(ydata_2012,ylim=c(0,10000))
p3 <- hist(ydata_2013,ylim=c(0,10000))
p4 <- hist(ydata_2014,ylim=c(0,10000))
p5 <- hist(ydata_2015,ylim=c(0,10000))
p6 <- hist(ydata_2016,ylim=c(0,10000))

plot( p1, col=rgb(0,0,1,1/4),ylim=c(0,8000),main="Year wise fever",xlim=c(0,100),xlab="value")
plot( p2, col=rgb(1,1,0,1/4),add=T)
plot( p3, col=rgb(1,0,1,1/4),add=T)
plot( p4, col=rgb(1,0,0,1/4),add=T)
plot( p5, col=rgb(0,1,1,1/4),add=T)
plot( p6, col=rgb(0,1,0,1/4),add=T)
legend("right", legend = c("2011", "2012","2013","2014","2015","2016"),
       lwd = 4, col = c(rgb(0,0,1,1/4),rgb(1,1,0,1/4),rgb(1,0,1,1/4),rgb(1,0,0,1/4),rgb(0,1,1,1/4),rgb(0,1,0,1/4)))

#v46
ydata_2011 <- data[data$year==2011,"v46"]
ydata_2012 <- data[data$year==2012,"v46"]
ydata_2013 <- data[data$year==2013,"v46"]
ydata_2014 <- data[data$year==2014,"v46"]
ydata_2015 <- data[data$year==2015,"v46"]
ydata_2016 <- data[data$year==2016,"v46"]

p1 <- hist(ydata_2011,ylim=c(0,10000))
p2 <- hist(ydata_2012,ylim=c(0,10000))
p3 <- hist(ydata_2013,ylim=c(0,10000))
p4 <- hist(ydata_2014,ylim=c(0,10000))
p5 <- hist(ydata_2015,ylim=c(0,10000))
p6 <- hist(ydata_2016,ylim=c(0,10000))

plot( p1, col=rgb(0,0,1,1/4),ylim=c(0,8000),main="Year wise measles",xlim=c(0,100),xlab="value")
plot( p2, col=rgb(1,1,0,1/4),add=T)
plot( p3, col=rgb(1,0,1,1/4),add=T)
plot( p4, col=rgb(1,0,0,1/4),add=T)
plot( p5, col=rgb(0,1,1,1/4),add=T)
plot( p6, col=rgb(0,1,0,1/4),add=T)
legend("right", legend = c("2011", "2012","2013","2014","2015","2016"),
       lwd = 4, col = c(rgb(0,0,1,1/4),rgb(1,1,0,1/4),rgb(1,0,1,1/4),rgb(1,0,0,1/4),rgb(0,1,1,1/4),rgb(0,1,0,1/4)))

#season-wise histograms

#v40
datakharif <- data[data$season=="Kharif", "v40"]
datarabi <- data[data$season == "Rabi", "v40"]
datasummer <- data[data$season == "Summer", "v40"]
datawholeyear <- data[data$season == "Whole Year", "v40"]

p1 <- hist(datakharif)
p2 <- hist(datarabi, ylim =c(0,15000))
p3 <- hist(datasummer, ylim =c(0,15000))
p4 <- hist(datawholeyear, ylim =c(0,15000))

plot( p1, col=rgb(0,0,1,1/4),ylim=c(0,12000),main="Season wise sepsis",xlim=c(0,100),xlab="value")
plot( p2, col=rgb(1,1,0,1/4),add=T)
plot( p3, col=rgb(1,0,1,1/4),add=T)
plot( p4, col=rgb(1,0,0,1/4),add=T)
legend("right", legend = c("Kharif", "Rabi","Summer","Whole Year"),
       lwd = 4, col = c(rgb(0,0,1,1/4),rgb(1,1,0,1/4),rgb(1,0,1,1/4),rgb(1,0,0,1/4)))

#v42
datakharif <- data[data$season=="Kharif", "v42"]
datarabi <- data[data$season == "Rabi", "v42"]
datasummer <- data[data$season == "Summer", "v42"]
datawholeyear <- data[data$season == "Whole Year", "v42"]

p1 <- hist(datakharif, ylim =c(0,15000))
p2 <- hist(datarabi, ylim =c(0,15000))
p3 <- hist(datasummer, ylim =c(0,15000))
p4 <- hist(datawholeyear, ylim =c(0,15000))

plot( p1, col=rgb(0,0,1,1/4),ylim=c(0,4000),main="Season wise lbw",xlim=c(0,100),xlab="value")
plot( p2, col=rgb(1,1,0,1/4),add=T)
plot( p3, col=rgb(1,0,1,1/4),add=T)
plot( p4, col=rgb(1,0,0,1/4),add=T)
legend("right", legend = c("Kharif", "Rabi","Summer","Whole Year"),
       lwd = 4, col = c(rgb(0,0,1,1/4),rgb(1,1,0,1/4),rgb(1,0,1,1/4),rgb(1,0,0,1/4)))

#43
datakharif <- data[data$season=="Kharif", "v43"]
datarabi <- data[data$season == "Rabi", "v43"]
datasummer <- data[data$season == "Summer", "v43"]
datawholeyear <- data[data$season == "Whole Year", "v43"]

p1 <- hist(datakharif, ylim =c(0,15000))
p2 <- hist(datarabi, ylim =c(0,15000))
p3 <- hist(datasummer, ylim =c(0,15000))
p4 <- hist(datawholeyear, ylim =c(0,15000))


plot( p1, col=rgb(0,0,1,1/4),ylim=c(0,10000),main="Season wise pneumonia",xlim=c(0,100),xlab="value")
plot( p2, col=rgb(1,1,0,1/4),add=T)
plot( p3, col=rgb(1,0,1,1/4),add=T)
plot( p4, col=rgb(1,0,0,1/4),add=T)
legend("right", legend = c("Kharif", "Rabi","Summer","Whole Year"),
       lwd = 4, col = c(rgb(0,0,1,1/4),rgb(1,1,0,1/4),rgb(1,0,1,1/4),rgb(1,0,0,1/4)))

#44
datakharif <- data[data$season=="Kharif", "v44"]
datarabi <- data[data$season == "Rabi", "v44"]
datasummer <- data[data$season == "Summer", "v44"]
datawholeyear <- data[data$season == "Whole Year", "v44"]

p1 <- hist(datakharif, ylim =c(0,15000))
p2 <- hist(datarabi, ylim =c(0,15000))
p3 <- hist(datasummer, ylim =c(0,15000))
p4 <- hist(datawholeyear, ylim =c(0,15000))

plot( p1, col=rgb(0,0,1,1/4),ylim=c(0,20000),main="Season wise diarrhoea",xlim=c(0,100),xlab="value")
plot( p2, col=rgb(1,1,0,1/4),add=T)
plot( p3, col=rgb(1,0,1,1/4),add=T)
plot( p4, col=rgb(1,0,0,1/4),add=T)
legend("right", legend = c("Kharif", "Rabi","Summer","Whole Year"),
       lwd = 4, col = c(rgb(0,0,1,1/4),rgb(1,1,0,1/4),rgb(1,0,1,1/4),rgb(1,0,0,1/4)))

#45
datakharif <- data[data$season=="Kharif", "v45"]
datarabi <- data[data$season == "Rabi", "v45"]
datasummer <- data[data$season == "Summer", "v45"]
datawholeyear <- data[data$season == "Whole Year", "v45"]

p1 <- hist(datakharif, ylim =c(0,15000))
p2 <- hist(datarabi, ylim =c(0,15000))
p3 <- hist(datasummer, ylim =c(0,15000))
p4 <- hist(datawholeyear, ylim =c(0,15000))

plot( p1, col=rgb(0,0,1,1/4),ylim=c(0,15000),main="Season wise fever",xlim=c(0,100),xlab="value")
plot( p2, col=rgb(1,1,0,1/4),add=T)
plot( p3, col=rgb(1,0,1,1/4),add=T)
plot( p4, col=rgb(1,0,0,1/4),add=T)
legend("right", legend = c("Kharif", "Rabi","Summer","Whole Year"),
       lwd = 4, col = c(rgb(0,0,1,1/4),rgb(1,1,0,1/4),rgb(1,0,1,1/4),rgb(1,0,0,1/4)))

#v46
datakharif <- data[data$season=="Kharif", "v46"]
datarabi <- data[data$season == "Rabi", "v46"]
datasummer <- data[data$season == "Summer", "v46"]
datawholeyear <- data[data$season == "Whole Year", "v46"]

p1 <- hist(datakharif, ylim =c(0,15000))
p2 <- hist(datarabi, ylim =c(0,15000))
p3 <- hist(datasummer, ylim =c(0,15000))
p4 <- hist(datawholeyear, ylim =c(0,15000))

plot( p1, col=rgb(0,0,1,1/4),ylim=c(0,20000),main="Season wise measles",xlim=c(0,100),xlab="value")
plot( p2, col=rgb(1,1,0,1/4),add=T)
plot( p3, col=rgb(1,0,1,1/4),add=T)
plot( p4, col=rgb(1,0,0,1/4),add=T)
legend("right", legend = c("Kharif", "Rabi","Summer","Whole Year"),
       lwd = 4, col = c(rgb(0,0,1,1/4),rgb(1,1,0,1/4),rgb(1,0,1,1/4),rgb(1,0,0,1/4)))