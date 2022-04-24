#Creating new log columns 

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

write.csv(df,"C:/Users/799vi/Desktop/eco/dataAssignment1/updated3.csv")

data <- read.csv("C:/Users/799vi/Desktop/eco/dataAssignment1/updated1.csv")

d <- df %>% filter(df$cropcategory=="Cash")

df1 <- d %>% select(l_gdp,l_beds,l_taps,l_index)
rg <- lm(d$v40 ~., df1)
summary(rg)

d <- df %>% filter(df$cropcategory=="Cereal")

df1 <- d %>% select(l_gdp,l_beds,l_taps,l_index)
rg <- lm(d$v40 ~., df1)
summary(rg)

d <- df %>% filter(df$cropcategory=="Oilseed")

df1 <- d %>% select(l_gdp,l_beds,l_taps,l_index)
rg <- lm(d$v40 ~., df1)
summary(rg)


d <- df %>% filter(df$cropcategory=="Pulse")

df1 <- d %>% select(l_gdp,l_beds,l_taps,l_index)
rg <- lm(d$v40 ~., df1)
summary(rg)


d <- df %>% filter(df$cropcategory=="Horticulture")

df1 <- d %>% select(l_gdp,l_beds,l_taps,l_index)
rg <- lm(d$v40 ~., df1)
summary(rg)

d <- df %>% filter(df$cropcategory=="Coarse Cereal")

df1 <- d %>% select(l_gdp,l_beds,l_taps,l_index)
rg <- lm(d$v40 ~., df1)
summary(rg)


data <- read.csv("C:/Users/799vi/Desktop/eco/dataAssignment1/vish_final11.csv")
d <- length(data$year)

for (i in 1:d) {
  if(data$cropcategory[i]==1){
    data$cash_i[i] = data$g_index[i]
    data$cereal_i[i] = data$index[i]
    data$cc_i[i] = data$index[i]
    data$oil_i[i] = data$index[i]
    data$pulse_i[i] = data$index[i]
    data$hort_i[i] = data$index[i]
  }
  else if(data$cropcategory[i]==2){
    data$cereal_i[i] = data$g_index[i]
    data$cc_i[i] = data$index[i]
    data$oil_i[i] = data$index[i]
    data$pulse_i[i] = data$index[i]
    data$hort_i[i] = data$index[i]
    data$cash_i[i] = data$index[i]
    
  }
  else if(data$cropcategory[i]==3){
    
    data$cc_i[i] = data$g_index[i]
    data$oil_i[i] = data$index[i]
    data$pulse_i[i] = data$index[i]
    data$hort_i[i] = data$index[i]
    data$cash_i[i] = data$index[i]
    data$cereal_i[i] = data$index[i]
  }
  else if(data$cropcategory[i]==4){
    
    data$oil_i[i] = data$g_index[i]
    data$pulse_i[i] = data$index[i]
    data$hort_i[i] = data$index[i]
    data$cash_i[i] = data$index[i]
    data$cereal_i[i] = data$index[i]
    data$cc_i[i] = data$index[i]
  }
  else if(data$cropcategory[i]==5){
    
    data$pulse_i[i] = data$g_index[i]
    data$hort_i[i] = data$index[i]
    data$cash_i[i] = data$index[i]
    data$cereal_i[i] = data$index[i]
    data$cc_i[i] = data$index[i]
    data$oil_i[i] = data$index[i]
  }  
  else if(data$cropcategory[i]==6){
    
    data$hort_i[i] = data$g_index[i]
    data$pulse_i[i] = data$index[i]
    data$cash_i[i] = data$index[i]
    data$cereal_i[i] = data$index[i]
    data$cc_i[i] = data$index[i]
    data$oil_i[i] = data$index[i]
  }
  print(i)
}
write.csv(data,"C:/Users/799vi/Desktop/vish_final1/summation.csv")
data <- read.csv("C:/Users/799vi/Desktop/vish_final1/summation.csv")
df <- data %>% select(gdp,beds,taps,cash_i,cereal_i,cc_i,pulse_i,oil_i,hort_i)
rg <- lm(data$v40~., df)
summary(rg)