# clear all variables
rm(list=ls())
# clear the console
cat("\014")

# Load the necessary libraries
library(forecast)
library(ggplot2)
library(lmtest)
# Fixating the seed
set.seed(1) 

NN = 500
right = 0
wrong = 0
right_hynd = 0
wrong_hynd = 0

total_time_hynd = 0
total_time_improv = 0

alpha = 0.01

for (real_p in 0:5) {
  for (real_q in 0:5) {
    orders <- list()
    orders_hynd <- list()
    for (j in 1:NN) {
      # Construct the file path
      file_path <- paste("TEST/TimeSeries_", real_p, "_", real_q, "/ts_", j, ".txt", sep="")
      
      # Read the content from the text file
      data <- scan(file_path, what=numeric(), quiet=TRUE)
      data <- c(data)
      
      start_time <- Sys.time()
      best_order <- auto.arima(data, seasonal = FALSE, stationary = TRUE, approximation = FALSE)
      end_time <- Sys.time()
      
      elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      total_time_hynd = total_time_hynd + elapsed_time
      
      hynd_p = best_order$arma[1]
      hynd_q = best_order$arma[2]
      
      if(hynd_p == real_p && hynd_q == real_q){
        right_hynd = right_hynd + 1
      }else{
        wrong_hynd = wrong_hynd + 1
      }
      orders_hynd[[j]] <- c(hynd_p, hynd_q)
      
      final_p <- 0
      final_q <- 0
      
      stop = FALSE
      p = hynd_p
      q = hynd_q
      
      start_time <- Sys.time()
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
      
      end_time <- Sys.time()
      
      elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      total_time_improv = total_time_improv + elapsed_time
      
      if(final_p == real_p && final_q == real_q){
        right = right + 1
      }else{
        wrong = wrong + 1
      }
      orders[[j]] <- c(final_p, final_q)
    }
    
    file_name <- paste("Hynd_ARMA",as.character(real_p), as.character(real_q), sep = "_")
    file_name <- paste(file_name, ".txt", sep = "")  # Adding ".txt" extension
    lapply(orders_hynd, write, file_name, append = TRUE, ncolumns = length(orders_hynd))
    
    file_name <- paste("ARMA",as.character(real_p), as.character(real_q), sep = "_")
    file_name <- paste(file_name, ".txt", sep = "")  # Adding ".txt" extension
    lapply(orders, write, file_name, append = TRUE, ncolumns = length(orders))
  }
  cat(real_p, "- Done\n")
}

perc_hynd = right_hynd/(right_hynd+wrong_hynd)
cat("Hynd Accuracy:", perc_hynd)
cat("Hynd Elapsed Time:", total_time_hynd)


perc = right/(right+wrong)
cat("Improv Hynd Accuracy:", perc)
cat("Improv Hynd Elapsed Time:", total_time_improv + total_time_hynd)











