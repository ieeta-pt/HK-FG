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
  library(lmtest)
  
  # Define the folder path
  folder_path <- "Orders"
  
  ################################################## Change to the pretended value (NN-sizes of the time series)
  real_p=2
  real_d=0
  real_q=0
  phi1 = 0.2
  phi2 = 0.6
  NN=c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
  num_iter = 1000
  alpha = 0.01
  ##################################################
  
  rights <- list()
  
  
  for (i in 1:length(NN)){
    
    right = 0
    
    n=NN[i]
    
    all_orders <- list()
    
    for(j in 1:num_iter){
      
      ################################################## Change to the pretended value
      arma_values <- arima.sim(list(order = c(real_p,real_d,real_q), ar=c(phi1,phi2)), n = n)
      
      
      # Find the best ARMA order using auto.arima (Hyndman-Khandakar algorithm)
      best_order <- auto.arima(arma_values, seasonal = FALSE, stationary = TRUE, approximation = FALSE)
      
      
      p = best_order$arma[1]
      q = best_order$arma[2]
      
      final_p <- 0
      final_q <- 0
      
      stop = FALSE
      
      while(stop != TRUE){
        fit3 <- tryCatch({
          Arima(data, order = c(p,0,q))
        }, error = function(e) {
          NULL
        })
        
        if(is.null(fit3)){
          fit3 <- Arima(data, order = c(p,0,q), method = 'ML')
        }
        
        results_of_fit <- coeftest(fit3)
        fit_result <- results_of_fit[, 4][-length(results_of_fit[, 4])]
        
        idx_p <- FALSE
        idx_q <- FALSE
        if(length(fit_result)>0){
          for(i in 1:length(fit_result)){
            if (!is.na(fit_result[[i]])) {
              if(fit_result[[i]] > alpha){
                if(i==p & p != 0){
                  idx_p <- TRUE
                }
                if(i==p+q & q != 0){
                  idx_q <- TRUE
                }
              }
            }
          }
        }
        
        if(idx_p == TRUE & idx_q == FALSE){
          p = p-1
        }
        else if(idx_p == FALSE & idx_q == TRUE){
          q = q-1
        }
        else if(idx_p == TRUE & idx_q == TRUE){
          if(fit3$coef[[p]] < fit3$coef[[p+q]]){
            p = p-1
          }
          else if(fit3$coef[[p]] > fit3$coef[[p+q]]){
            q = q-1
          }
          
        }else{
          final_p <- p
          final_q <- q
          stop = TRUE
        }
      }
      
      # get p and q
      order_components <- c(final_p, final_q)
      
      all_orders[[j]] <- order_components
      
    }
    
    ################################################## Change to the pretended value
    file_name <- file.path(folder_path,paste("Improved_try", t, "_N_",n,"_arma",real_p,real_q,"_",phi1,"_",phi2,".txt", sep = ""))
    # Your code to write to the file (e.g., using write or write.table)
    lapply(all_orders, write, file_name, append = TRUE, ncolumns = length(NN))
    
  }
  
  
}


full_end_time <- Sys.time()
elapsed_time_seconds <- as.numeric(difftime(full_end_time, full_start_time, units = "secs"))

cat("Full Elapsed time:", elapsed_time_seconds, " (s)\n")