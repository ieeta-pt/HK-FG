# clear all variables
rm(list=ls())
# clear the console
cat("\014")

library(foreach)
library(doParallel)

# Fixating the seed
set.seed(1) 


full_start_time <- Sys.time()

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister_dopar()


REP = 10

# Initialize a parallel backend with N cores (adjust as needed)
num_cores <- detectCores()

num_clusters <- 10
cl <- makeCluster(num_clusters)
registerDoParallel(cl)

full_start_time <- Sys.time()

foreach(t = 1:REP) %dopar% {
  # Load the necessary libraries
  library(forecast)
  library(ggplot2)
  
  ################################################## Change to the pretended value (NN-size of the time series)
  p=1
  d=0
  q=0
  phi1 = -0.3
  NN=c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
  num_iter = 1000
  ##################################################
  
  rights <- list()
  wrongs_00 <- list()
  wrongs_01 <- list()
  wrongs_22 <- list()
  
  
  for (i in 1:length(NN)){
    
    right = 0
    wrong_00 = 0
    wrong_01 = 0
    wrong_22 = 0
    
    n=NN[i]
    
    for(j in 1:num_iter){
      
      # Generate ARMA(p,0) values
      arma_values <- arima.sim(list(order = c(p,d,q), ar=c(phi1)), n = n)
      
      
      # Find the best ARMA order using auto.arima (Hyndman-Khandakar algorithm)
      best_order <- auto.arima(arma_values, seasonal = FALSE, stationary = TRUE, approximation = FALSE)
      
      if (best_order$arma[1] == p & best_order$arma[2]== q){
        right = right + 1
      }
      if (best_order$arma[1] == 0 & best_order$arma[2]== 0){
        wrong_00 = wrong_00 + 1 
      }
      if (best_order$arma[1] == 2 & best_order$arma[2]== 2){
        wrong_22 = wrong_22 + 1 
      }
      if (best_order$arma[1] == 0 & best_order$arma[2]== 1){
        wrong_01 = wrong_01 + 1 
      }
      
    }
    
    rights[[i]] <- right
    wrongs_00[[i]] <- wrong_00
    wrongs_01[[i]] <- wrong_01
    wrongs_22[[i]] <- wrong_22
    
  }
  
  file_name <- paste("try", t, "_rights_ar1_",phi1,".txt", sep = "")
  lapply(rights, write, file_name, append = TRUE, ncolumns = length(NN))
  
  file_name <- paste("try", t, "_wrongs00_ar1_",phi1,".txt", sep = "")
  lapply(wrongs_00, write, file_name, append = TRUE, ncolumns = length(NN))
  
  file_name <- paste("try", t, "_wrongs01_ar1_",phi1,".txt", sep = "")
  lapply(wrongs_01, write, file_name, append = TRUE, ncolumns = length(NN))
  
  file_name <- paste("try", t, "_wrongs22_ar1_",phi1,".txt", sep = "")
  lapply(wrongs_22, write, file_name, append = TRUE, ncolumns = length(NN))
  
  
}


full_end_time <- Sys.time()
elapsed_time_seconds <- as.numeric(difftime(full_end_time, full_start_time, units = "secs"))

cat("Full Elapsed time:", elapsed_time_seconds, " (s)\n")