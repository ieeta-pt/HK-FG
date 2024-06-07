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
  p=0
  d=0
  q=0
  sigma2 = c(1,2,3,4)
  NN=c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
  num_iter = 1000
  ##################################################
  
  rights <- list()
  wrongs_10 <- list()
  wrongs_01 <- list()
  wrongs_22 <- list()
  
  for(s in 1:length(sigma)){
    var = sigma2[s]
    for (i in 1:length(NN)){
      
      right = 0
      wrong_10 = 0
      wrong_01 = 0
      wrong_22 = 0
      
      n=NN[i]
      
      for(j in 1:num_iter){
        
        # Generate ARMA(0,0) values
        arma_values <- rnorm(n, mean = 0, sd = var)
        
        
        # Find the best ARMA order using auto.arima (Hyndman-Khandakar algorithm)
        best_order <- auto.arima(arma_values, seasonal = FALSE, stationary = TRUE)
        
        if (best_order$arma[1] == p & best_order$arma[2]== q){
          right = right + 1
        }
        if (best_order$arma[1] == 1 & best_order$arma[2]== 0){
          wrong_10 = wrong_10 + 1 
        }
        if (best_order$arma[1] == 2 & best_order$arma[2]== 2){
          wrong_22 = wrong_22 + 1 
        }
        if (best_order$arma[1] == 0 & best_order$arma[2]== 1){
          wrong_01 = wrong_01 + 1 
        }
        
      }
      
      rights[[i]] <- right
      wrongs_10[[i]] <- wrong_10
      wrongs_01[[i]] <- wrong_01
      wrongs_22[[i]] <- wrong_22
      
    }
    
    file_name <- paste("try", t, "_rights_arma00_var",var,".txt", sep = "")
    lapply(rights, write, file_name, append = TRUE, ncolumns = length(NN))
    
    file_name <- paste("try", t, "_wrongs10_arma00_var",var,".txt", sep = "")
    lapply(wrongs_10, write, file_name, append = TRUE, ncolumns = length(NN))
    
    file_name <- paste("try", t, "_wrongs01_arma00_var",var,".txt", sep = "")
    lapply(wrongs_01, write, file_name, append = TRUE, ncolumns = length(NN))
    
    file_name <- paste("try", t, "_wrongs22_arma00_var",var,".txt", sep = "")
    lapply(wrongs_22, write, file_name, append = TRUE, ncolumns = length(NN))
  }
  
}


full_end_time <- Sys.time()
elapsed_time_seconds <- as.numeric(difftime(full_end_time, full_start_time, units = "secs"))

cat("Full Elapsed time:", elapsed_time_seconds, " (s)\n")