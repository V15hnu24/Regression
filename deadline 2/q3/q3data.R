data <- read.csv("C:/Users/799vi/Desktop/Courses/eco/dataAssignment2/data.csv")
 
d <- length(data$year)

south <- c("Andhra Pradesh", "Telangana", "Karnataka", "Kerala" ,"Tamil Nadu")
north <- c("Himachal Pradesh", "Punjab", "Uttarakhand", "Uttar Pradesh", "Haryana")
east <- c("Bihar", "Orissa", "Jharkhand" ,"West Bengal")
west <- c("Rajasthan", "Gujarat", "Goa","Maharashtra")
central <- c("Madhya Pradesh","Chhattisgarh")
north_east <- c("Assam", "Sikkim", "Nagaland", "Meghalaya", "Manipur", "Mizoram", "Tripura","Arunachal Pradesh")

for (i in 1:d) {
  if(data$state[i] %in% south){
    data$zone[i] = "South"
  }
  else if(data$state[i] %in% east){
    data$zone[i] = "East"
  }
  else if(data$state[i] %in% central){
    data$zone[i] = "Central"
  }
  else if(data$state[i] %in% north_east){
    data$zone[i] = "North East"
  }
  else if(data$state[i] %in% west){
    data$zone[i] = "West"
  }
  else if(data$state[i] %in% north){
    data$zone[i] = "North"}
}
write.csv(data, "C:/Users/799vi/Desktop/Courses/eco/dataAssignment2/data_zones.csv" )