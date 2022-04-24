# This R script contains code for question 2

path <- "update_main.csv"
install.packages("stargazer")
library(stargazer)
main<-read.csv(path)
main_q3 = main
set.seed(1)
main <- subset (main, select = -X)
main_subset = data.frame(main$v41, main$index, main$season)

main_kharif_df = subset(main_subset, main_subset$main.season == "Kharif")
model_kharif = lm(main.v41 ~ main.index, data = main_kharif_df)

beta_0_kharif = model_kharif$coefficients[1]
beta_1_kharif = model_kharif$coefficients[2]

main_rabi_df = subset(main_subset, main_subset$main.season == "Rabi")
model_rabi = lm(main.v41 ~ main.index, data = main_rabi_df)

beta_0_rabi = model_rabi$coefficients[1]
beta_1_rabi = model_rabi$coefficients[2]

# sample size (as given in the assignment, 80% of the dataset is choosen randomly)  
#n = 23063

# sample size (90%)
n = 25946
# number of experiments
M = 2000

slope_kharif_vec<- rep(0,M)
intercept_kharif_vec <- rep(0,M)

#  simulations 
for(i in 1:M){
  temp_kharif = main_kharif_df[sample(nrow(main_kharif_df), n),]
  model_monte_carlo_kharif = lm(main.v41 ~ main.index, data = temp_kharif)
  slope_kharif_vec[i] <- model_monte_carlo_kharif$coefficients[2]
  intercept_kharif_vec[i] <- model_monte_carlo_kharif$coefficients[1]
}

slope_rabi_vec<- rep(0,M)
intercept_rabi_vec <- rep(0,M)
# sample size (as given in the assignment, 80% of the dataset is choosen randomly)  
n = 20590

#sample size 90%
n =23164
# number of experiments
M = 2000

#  simulations 
for(i in 1:M){
  temp_rabi = main_rabi_df[sample(nrow(main_rabi_df), n),]
  model_monte_carlo_rabi = lm(main.v41 ~ main.index, data = temp_rabi)
  slope_rabi_vec[i] <- model_monte_carlo_rabi$coefficients[2]
  intercept_rabi_vec[i] <- model_monte_carlo_rabi$coefficients[1]
}



monte_estimates_kharif_vec <- data.frame(beta_1 = slope_kharif_vec, beta_0 = intercept_kharif_vec)
monte_estimates_rabi_vec<- data.frame(beta_1 = slope_rabi_vec, beta_0 = intercept_rabi_vec)

stargazer(monte_estimates_kharif_vec,type="text")
stargazer(monte_estimates_rabi_vec,type="text")

kharif_vec = data.frame(lapply(monte_estimates_kharif_vec,mean))
rabi_vec = data.frame(lapply(monte_estimates_rabi_vec,mean))



