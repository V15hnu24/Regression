# This R script contains code for question 3 part a) and c)


path <- "data_zones.csv"
main_D<-read.csv(path)
main_D <- subset (main_D, select = -X)
main_D <- subset (main_D, select = -X.1)
main_D$zone[is.na(main_D$zone)] <- "No Classification"
# main_s = data.frame(main_D$state, main_D$zone)
main_D['D_north']= rep(NA, 70572)
main_D['D_south']= rep(NA, 70572)
main_D['D_central']= rep(NA, 70572)
main_D['D_west']= rep(NA, 70572)
main_D['D_east']= rep(NA, 70572)
main_D['D_north_east']= rep(NA, 70572)
seq = 1:70572

for(i in seq){
  if(main_D[i,5] == "North"){
      main_D[i,75] = 1;
      
      main_D[i,76] = 0; 
      main_D[i,77] = 0; 
      main_D[i,78] = 0; 
      main_D[i,79] = 0; 
      main_D[i,80] = 0; 
  }
  else if(main_D[i,5] == "South"){
    main_D[i,76] = 1;
    
    main_D[i,75] = 0; 
    main_D[i,77] = 0; 
    main_D[i,78] = 0; 
    main_D[i,79] = 0; 
    main_D[i,80] = 0; 
  }
  else if(main_D[i,5] == "Central"){
    main_D[i,77] = 1;
    
    main_D[i,76] = 0; 
    main_D[i,75] = 0; 
    main_D[i,78] = 0; 
    main_D[i,79] = 0; 
    main_D[i,80] = 0; 
  }
  else if(main_D[i,5] == "West"){
    main_D[i,78] = 1;
    
    main_D[i,76] = 0; 
    main_D[i,77] = 0; 
    main_D[i,75] = 0; 
    main_D[i,79] = 0; 
    main_D[i,80] = 0; 
  }
  else if(main_D[i,5] == "East"){
    main_D[i,79] = 1;
    
    main_D[i,76] = 0; 
    main_D[i,77] = 0; 
    main_D[i,78] = 0; 
    main_D[i,75] = 0; 
    main_D[i,80] = 0; 
    
    }
  else if(main_D[i,5] == "North East"){
    main_D[i,80] = 1;
    
    main_D[i,76] = 0; 
    main_D[i,77] = 0; 
    main_D[i,78] = 0; 
    main_D[i,79] = 0; 
    main_D[i,75] = 0; 
  }
  else{
    main_D[i,75] = 0; 
    main_D[i,76] = 0; 
    main_D[i,77] = 0; 
    main_D[i,78] = 0; 
    main_D[i,79] = 0; 
    main_D[i,80] = 0; 
  }
}

main_R = main_D
main_K = main_D
main_K= subset(main_K, main_K$season == "Kharif")
main_R= subset(main_R, main_R$season == "Rabi")

# Q3 part  1)
# Dont need to include a dummy variable for D_north_east because to represent 6 different groups only 
# 5 dummy variables are required.

model_kharif = lm(v41~log(beds)+gdp+tap+index+v28+D_south+D_north+D_central+D_west+D_east, data=main_K)
model_rabi = lm(v41~log(beds)+gdp+tap+index+v28+D_south+D_north+D_central+D_west+D_east, data=main_R)
summary(model_kharif)
summary(model_rabi)
# Q-3 part 3) 
# FOR season = Kharif

model_r = lm(v41~log(beds)+gdp+tap+index+v28,data=main_K)
model_u = lm(v41~log(beds)+gdp+tap+index+v28+D_south, data=main_K)

rss_r= sum(resid(model_r)^2)
rss_u = sum(resid(model_u)^2)
f_test_kharif = ((rss_r - rss_u)* 28829)/(rss_u*5) 
# 416.486
f_test_kharif
# FOR season = Rabi

model_r = lm(v41~log(beds)+gdp+tap+index+v28,data=main_R)
model_u = lm(v41~log(beds)+gdp+tap+index+v28+D_south, data=main_R)

rss_r= sum(resid(model_r)^2)
rss_u = sum(resid(model_u)^2)
f_test_rabi = ((rss_r - rss_u)* 25738)/(rss_u*5) 
#  371.831
f_test_rabi
#sctest(modell)
#strucchange::breakpoints(main_D$v41~1)




   
