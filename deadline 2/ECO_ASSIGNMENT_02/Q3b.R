# This R script contains code for question 3 part b)

install.packages("dplyr")
library(dplyr)

main_k1 = main_K
main_r1 = main_R
seq = 1:28829
for(i in seq){

  if(main_k1[i,5] == "South"){
    main_k1[i,5] = "South" 
  }
  else{
    main_k1[i,5] = "Not South"
  }
}

seq = 1:25738
for(i in seq){
  
  if(main_r1[i,5] == "South"){
    main_r1[i,5] = "South" 
  }
  else{
    main_r1[i,5] = "Not South"
  }
}
# main_k1$v41[is.na(main_k1$v41)] <- mean(main_k1$v41)


df1 = main_k1 %>%
  select(zone, v41) %>%
  filter(zone=="South" | zone=="Not South")
  group_by(zone)
# for Kharif
t.test(data = df1, df1$v41~df1$zone)  

# For Rabi


df1 = main_r1 %>%
  select(zone, v41) %>%
  filter(zone=="South" | zone=="Not South")
group_by(zone)
t.test(data = df1, df1$v41~df1$zone)  

