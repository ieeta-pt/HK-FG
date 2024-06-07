# clear all variables
rm(list=ls())
# clear the console
cat("\014")

# Load the necessary libraries
library(forecast)
library(ggplot2)
# Fixating the seed
set.seed(1) # 10 for trainning, 1 for testing



generate_coefficients <- function(coeff){
  if(coeff == 0){
    return(c())
  }
  
  if(coeff == 1){
    phi1 <- runif(1, min = -0.9999999, max = 0.9999999)
    
    return(c(phi1))
  }
  if(coeff == 2){
    while (TRUE) {
      phi1 <- runif(1, min = -0.9999999, max = 0.9999999)
      # Check constraints and generate phi2
      phi2 <- runif(1, min = -0.9999999, max = 0.9999999)
      
      if (-1 < phi2 && phi2 < 1 && phi1 + phi2 < 1 && phi2 - phi1 < 1 && phi2 != 0) {
        break
      } 
    }
    
    return(c(phi1,phi2))
  }
  
  if(coeff >= 3){
    # Generate random roots satisfying |r_j| > 1
    random_roots <- runif(coeff, min = 1.5, max = 5)
    # Compute the coefficients of the autoregressive polynomial
    phi <- poly(random_roots)
    
    return(c(phi))
  }
  
}


min.p = 0
min.q = 0
max.p = 5
max.q = 5
n = 1000
REP = 500  # 5000 for trainning, 500 for testing


full_start_time <- Sys.time()

for(p in min.p:max.p){
  for(q in min.q:max.q){
    idx = 0
    while(idx != REP){
      phi <- generate_coefficients(p)
      theta <- generate_coefficients(q)
      
      
      stop = FALSE
      count = 0
      
      while(stop != TRUE){
        arma_values <- tryCatch({
          arima.sim(list(order = c(p, 0, q), ar = c(phi), ma = c(theta)), n = n)
        }, error = function(e) {
          NULL
        })
        
        if(!is.null(arma_values)){
          stop = TRUE
        }else{
          if(count == 50){
            stop = TRUE
          }else{
            count = count + 1
          }
        }
      }
      
      if(!is.null(arma_values)){
        idx <- idx + 1
        
        #################### Save Time Series
        
        # Convert p and q to strings and concatenate them to form the folder path
        folder_path <- paste("TimeSeries",as.character(p), as.character(q), sep = "_")
        
        # Create the folder if it doesn't exist
        if (!file.exists(folder_path)) {
          dir.create(folder_path)
        }
        
        file_name <- file.path(folder_path,paste("ts_",idx,".txt", sep = ""))
        # Your code to write to the file (e.g., using write or write.table)
        lapply(arma_values, write, file_name, append = TRUE, ncolumns = length(arma_values))
        
        
        #################### Save ACF
        
        # Convert p and q to strings and concatenate them to form the folder path
        folder_path <- paste("ACF",as.character(p), as.character(q), sep = "_")
        
        # Create the folder if it doesn't exist
        if (!file.exists(folder_path)) {
          dir.create(folder_path)
        }
        
        acf_values <- acf(arma_values, lag.max = max.p*4, plot = FALSE)$acf
        file_name <- file.path(folder_path,paste("acf_",idx,".txt", sep = ""))
        # Your code to write to the file (e.g., using write or write.table)
        lapply(acf_values, write, file_name, append = TRUE, ncolumns = length(acf_values))
        
        
        #################### Save PACF
        
        
        # Convert p and q to strings and concatenate them to form the folder path
        folder_path <- paste("PACF",as.character(p), as.character(q), sep = "_")
        
        # Create the folder if it doesn't exist
        if (!file.exists(folder_path)) {
          dir.create(folder_path)
        }
        
        pacf_values <- pacf(arma_values, lag.max = max.p*4, plot = FALSE)$acf
        file_name <- file.path(folder_path,paste("pacf_",idx,".txt", sep = ""))
        # Your code to write to the file (e.g., using write or write.table)
        lapply(pacf_values, write, file_name, append = TRUE, ncolumns = length(pacf_values))
        
        #################### Save Coefficients
        
        
        # Convert p and q to strings and concatenate them to form the folder path
        folder_path <- paste("Coefficients",as.character(p), as.character(q), sep = "_")
        
        # Create the folder if it doesn't exist
        if (!file.exists(folder_path)) {
          dir.create(folder_path)
        }
        
        coeffs = c(phi,theta)
        file_name <- file.path(folder_path,paste("coeffs_",idx,".txt", sep = ""))
        # Your code to write to the file (e.g., using write or write.table)
        lapply(coeffs, write, file_name, append = TRUE, ncolumns = length(coeffs))
      }
      
    }
  }
}


full_end_time <- Sys.time()
elapsed_time_seconds <- as.numeric(difftime(full_end_time, full_start_time, units = "secs"))

cat("Full Elapsed time:", elapsed_time_seconds, " (s)\n")