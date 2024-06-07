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

# Define the folder path
folder_path <- "Orders"

# Create the folder if it doesn't exist
if (!file.exists(folder_path)) {
  dir.create(folder_path)
}

full_start_time <- Sys.time()

foreach(t = 1:REP) %dopar% {
  # Load the necessary libraries
  library(forecast)
  library(ggplot2)
  
  # Define the folder path
  folder_path <- "Orders"
  
  ################################################## Change to the pretended value (NN-sizes of the time series)
  p=2
  d=0
  q=0
  phi1 = 0.2
  phi2 = 0.6
  NN=c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
  num_iter = 1000
  ##################################################
  
  rights <- list()
  
  
  for (i in 1:length(NN)){
    
    right = 0
    
    n=NN[i]
    
    all_orders <- list()
    
    for(j in 1:num_iter){
      
      ################################################## Change to the pretended value
      arma_values <- arima.sim(list(order = c(p,d,q), ar=c(phi1,phi2)), n = n)
      
      
      # Find the best ARMA order using auto.arima (Hyndman-Khandakar algorithm)
      best_order <- auto.arima(arma_values, seasonal = FALSE, stationary = TRUE, approximation = FALSE)
      
      if (best_order$arma[1] == p & best_order$arma[2]== q){
        right = right + 1
      }
      
      # get p and q
      order_components <- best_order$arma[1:2]
      
      all_orders[[j]] <- order_components
      
    }
    
    ################################################## Change to the pretended value
    file_name <- file.path(folder_path,paste("try", t, "_N_",n,"_arma",p,q,"_",phi1,"_",phi2,".txt", sep = ""))
    # Your code to write to the file (e.g., using write or write.table)
    lapply(all_orders, write, file_name, append = TRUE, ncolumns = length(NN))
    
  }
  

}


full_end_time <- Sys.time()
elapsed_time_seconds <- as.numeric(difftime(full_end_time, full_start_time, units = "secs"))

cat("Full Elapsed time:", elapsed_time_seconds, " (s)\n")