library(ggpubr)
library(ggplot2)
library(caTools)
library(car)
library(ggpubr)
library(tidyverse)
library(magrittr)
library(stargazer)
library(dplyr)

directory <- "C:/Users/799vi/Desktop/eco/dataAssignment1/Q1/"
setwd(directory)
data <- read.csv(paste0(directory, "data.csv"))
getwd()

d <- data %>% select(v40,gdp,beds,tap)

cor(d, use = "complete.obs")

d <- data %>% select(v40,gdp,beds,tap,growth_rate)
cor(d, use = "complete.obs")


d <- data %>% select(v40,gdp,beds,tap,index)
cor(d, use = "complete.obs")

data <- read.csv("C:/Users/799vi/Desktop/vish_final1/vish_final1.csv")

df <- mutate(data,
             l_gdp = log(gdp),
             l_beds = log(beds),
             l_taps = log(taps),
             l_index = log(index)
)
df
head(df)


dfx <- df %>% select(l_gdp,l_beds,l_taps,l_index)
cor(dfx, use="complete.obs")
