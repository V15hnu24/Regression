install.packages("car")
library(caTools)
library(car)


data <- read.csv("C:/Users/799vi/Desktop/vish_final1/vish_final1.csv")

#A
df1 <- data %>% select(gdp,beds,taps) 


first <- lm(data$v40 ~gdp, df1)
summary(first)

#B

d <- data %>% filter(data$cropcategory=="Cash")

df <- d %>% select(gdp,beds,taps,index)
rg <- lm(d$v40 ~., df)
summary(rg)

d <- data %>% filter(data$cropcategory=="Cereal")

df <- d %>% select(gdp,beds,taps,index)
rg <- lm(d$v40 ~., df)
summary(rg)

d <- data %>% filter(data$cropcategory=="Oilseed")

df <- d %>% select(gdp,beds,taps,index)
rg <- lm(d$v40 ~., df)
summary(rg)

d <- data %>% filter(data$cropcategory=="Pulse")

df <- d %>% select(gdp,beds,taps,index)
rg <- lm(d$v40 ~., df)
summary(rg)

d <- data %>% filter(data$cropcategory=="Horticulture")

df <- d %>% select(gdp,beds,taps,index)
rg <- lm(d$v40 ~., df)
summary(rg)

d <- data %>% filter(data$cropcategory=="Coarse Cereal")

df <- d %>% select(gdp,beds,taps,index)
rg <- lm(d$v40 ~., df)
summary(rg)

d <- length(data$year)
d

# Code to find growth rate.
for (i in 1:d) {
  print(i)
  if(data$year[i]!=2011 && data$year[i-1]==data$year[i]-1){
    print(data$year[i])
    data$g_index[i] = (data$index[i] - data$index[i-1] )/data$index[i-1]
  }
}

for (i in 1:d) {
  print(data$g_index[i])
}
#write.csv(data,"C:/Users/799vi/Desktop/eco/dataAssignment1/vish_final1_for_growth.csv",row.names = FALSE)

#D
for (i in 1:700) {
  print(data$g_index[i])
}

d <- data %>% filter(data$cropcategory=="Cash")

df <- d %>% select(gdp,beds,taps,g_index)
rg <- lm(d$v40 ~., df)
summary(rg)


d <- data %>% filter(data$cropcategory=="Cereal")

df <- d %>% select(gdp,beds,taps,g_index)
df_new <- df
df_new[is.na(df_new) | df_new == "Inf"]
rg <- lm(d$v40 ~., df)
summary(rg)

d <- data %>% filter(data$cropcategory=="Oilseed")

df <- d %>% select(gdp,beds,taps,index)
rg <- lm(d$v40 ~., df)
summary(rg)

d <- data %>% filter(data$cropcategory=="Pulse")

df <- d %>% select(gdp,beds,taps,index)
rg <- lm(d$v40 ~., df)
summary(rg)

d <- data %>% filter(data$cropcategory=="Horticulture")

df <- d %>% select(gdp,beds,taps,index)
rg <- lm(d$v40 ~., df)
summary(rg)

d <- data %>% filter(data$cropcategory=="Coarse Cereal")

df <- d %>% select(gdp,beds,taps,index)
rg <- lm(d$v40 ~., df)
summary(rg)


