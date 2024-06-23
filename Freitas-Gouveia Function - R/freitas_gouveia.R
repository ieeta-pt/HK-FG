# clear all variables
rm(list=ls())
# clear the console
cat("\014")

# Import necessary libraries
library(forecast)
library(lmtest)

############################################################### Set up data
p = 1
d = 0
q = 0
phis = c(0.6)
thetas = c()
N = 1000

data <- arima.sim(list(order = c(1,0,0), ar=c(phis), ma=c(thetas)), n = N)
# change data to the time series to model/study and run the rest of the code (no need for any more changes)


opt = 3
# opt = 1: return only the Freitas-Gouveia model
# opt = 2: return the LSTM model and the improved Hyndman-Khandakar algorithm
# opt = 3: return the LSTM model, the Hyndman-Khandakar algorithm and the improved Hyndman-Khandakar algorithm
# opt = 4: return only the improved Hyndman-Khandakar algorithm
# opt = 5: return only the Hyndman-Khandakar algorithm

alpha = 0.01

############################################################### Freitas-Gouveia model


if(opt<=3){
  temp_hyndman <- auto.arima(data, max.p = 0, max.q = 0, stationary = FALSE)
  temp = arimaorder(temp_hyndman)
  d = temp[2]
  
  if(d>0){
    diff_data = diff(data, differences = d)
  }else{
    diff_data = data
  }
  
  library(reticulate)
  library(keras)
  model <- load_model_tf('freitas-gouveia.keras')
  acf_values <- acf(diff_data, plot = FALSE, lag.max = 20)$acf
  pacf_values <- pacf(diff_data, plot = FALSE, lag.max = 20)$acf
  
  # remove first value of acf
  acf_values <- acf_values[-1]
  X_test <- array(0, dim = c(1, 2, 10))
  X_test[1, 1, ] <- acf_values
  X_test[1, 2, ] <- pacf_values
  
  predictions <- model$predict(X_test)
  lstm_p = predictions[[1]]
  lstm_q = predictions[[2]]
  lstm_p <- round(lstm_p)
  lstm_d = d
  lstm_q <- round(lstm_q)
}

############################################################### Hyndman-Khandakar algorithm
if(opt !=1){
  hyndman = auto.arima(data)
  orders = arimaorder(hyndman)
  hynd_p = orders[1]
  hynd_d = orders[2]
  hynd_q = orders[3]
}

if(opt == 2 || opt == 3 || opt == 4){
  imp_hynd_p = hynd_p
  imp_hynd_d = hynd_d
  imp_hynd_q = hynd_q
  
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
}

if(opt<=3){
  print(paste("Freitas-Gouveia algorithm returned ARIMA(", lstm_p, ",", lstm_d, ",", lstm_q, ")"))
}

if(opt == 2 || opt == 3 || opt == 5){
  print(paste("Hyndman-Khandkar algorithm returned ARIMA(", hynd_p, ",", hynd_d, ",", hynd_q, ")"))
  
}


if(opt == 2 || opt == 3 || opt == 4){
  print(paste('Improved Hyndman-Khandkar algorithm returned ARIMA(', final_p, ',', imp_hynd_d, ',', final_q, ')'))
}
