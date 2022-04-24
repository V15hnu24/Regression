library(ggpubr)
library(ggplot2)
library(caTools)
library(car)
library(ggpubr)
library(tidyverse)
library(magrittr)
library(stargazer)
library(dplyr)

data <- read.csv("C:/Users/799vi/Desktop/vish_final1/summation.csv")

df <- data %>% select(gdp,beds,taps,cash_i,cereal_i,cc_i,pulse_i,oil_i,hort_i,)

rg <- lm(data$v40~., df)
summary(rg)


df <- mutate(data,
             l_cash = log(cash_i),
             l_cc = log(cc_i),
             l_creal = log(creal_i),
             l_pluse = log(pluse_i),
             l_oil = log(oil_i),
             l_hort = log(hort_i)
)
df <- mutate(data,
             l_gdp = log(gdp),
             l_beds = log(beds),
             l_taps = log(taps),
             l_index = log(index)
)

df1 <- df %>% select(gdp_i,beds_i,taps_i,cash_i,cereal_i,cc_i,pulse_i,oil_i,hort_i)
rg <- lm(df$v40 ~., df1)
rg