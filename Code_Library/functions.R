
# Packload ----------------------------------------------------------------
library(dplyr)
library(lubridate)
library(zoo)
library(parallel)
library(ggplot2)
library(gridExtra)

# Functions ---------------------------------------------------------------

#Reads & creates a datetime column, the path must be the folder in which the data is contained.
#It reads every csv file, so seperate different time series into different folders
read_csv_folder <- function(path){
  
  file_names <- list.files(path = path, pattern="*.csv")
  
  data_return <- lapply(file_names, function(x) read.csv(paste(path, x, sep = ""), stringsAsFactors = FALSE) %>%
                          mutate(date = sub(".*_ *(.*?) *.csv.*", "\\1",  x)) %>%
                          mutate(date = as.POSIXct(paste(date, utcsec), format = "%Y%m%d %H:%M:%S")) %>% 
                          select(-utcsec))
  
  data_return <- do.call(rbind, data_return)
  return(data_return)
}

# Simulation --------------------------------------------------------------

#Inverse Gaussian distribution


IG=function(n,mu,lambda){
  X=numeric()
  for(i in 1:n){
    N=rnorm(1,0,1)
    Y=N^2
    X_1=mu[i] + (mu[i]^2*Y)/(2*lambda[i])-(mu[i])/(2*lambda[i])*sqrt(4*mu[i]*lambda[i]*Y+mu[i]^2*Y^2)
    U=runif(1,0,1)
    X[i]=ifelse(U<=mu[i]/(X_1+mu[i]),X_1,mu[i]^2/X_1)
  }
  return(X)
}

#Normal inverse gaussian distribution

Sub_NIG_sim <- function(T, delta, ...){
  list2env(list(...), environment())
  t_grid <- seq(0, T, by = delta)
  mu <- diff(t_grid)
  lambda <- mu^2/kappa
  IGs <- IG(n=length(t_grid),mu=mu,lambda=lambda)
  N_sim <- rnorm(length(t_grid), 0, 1)
  
  delta_X <- sigma * N_sim * sqrt(IGs) + theta * IGs + mu_nig * delta
  
  return(cumsum(delta_X))
}



# Estimation --------------------------------------------------------------

# Function used for maximum likelihood estimation with the data from the NIG process

Nig_Like <- function(param, delta_X, delta){
  sigma <- param[1]
  theta <- param[2]
  kappa <- param[3]
  mu <- param[4]
  
  if(sigma < 0 ){
    return(Inf)
  }
  
  -1 * sum( log(
    (delta/pi * exp(delta/kappa) * sqrt(theta^2/(kappa*sigma^2) + 1/kappa^2)) *
      exp((theta/sigma^2)*(delta_X-mu) ) * (besselK(x = 
                                                (sqrt(theta^2 + sigma^2/kappa)/sigma^2)*sqrt(
                                                  (delta_X-mu)^2 + delta^2*sigma^2/kappa), nu = 1))/
      sqrt( (delta_X-mu)^2 + delta^2*sigma^2/kappa)
  )
  )
}


Nig_Like_wo_mu <- function(param, delta_X, delta){
  sigma <- param[1]
  theta <- param[2]
  kappa <- param[3]
  mu <- 0
  
  if(sigma < 0 ){
    return(Inf)
  }
  
  -1 * sum( log(
    (delta/pi * exp(delta/kappa) * sqrt(theta^2/(kappa*sigma^2) + 1/kappa^2)) *
      exp((theta/sigma^2)*(delta_X-mu) ) * (besselK(x = 
                                                      (sqrt(theta^2 + sigma^2/kappa)/sigma^2)*sqrt(
                                                        (delta_X-mu)^2 + delta^2*sigma^2/kappa), nu = 1))/
      sqrt( (delta_X-mu)^2 + delta^2*sigma^2/kappa)
  )
  )
}


Optim_w_restarts_MLE <- function(n, delta_X, delta, mu_incl = TRUE){
  delta_X <- delta_X
  n_cores <- 2
  cluster <- parallel::makeCluster(n_cores)
  parallel::clusterExport(cl = cluster, varlist = list("delta_X","delta", "Nig_Like"), envir = environment())
  
  all_runs <- parallel::parLapply(cl = cluster, X = 1:n, fun = function(X){
    if(mu_incl == TRUE){
    opt_single_run <- try(optim(par = c(runif(1), runif(1), runif(1), runif(1)), fn = Nig_Like, delta_X = delta_X,
                                delta = delta))
    }else{
      opt_single_run <- try(optim(par = c(runif(1), runif(1), runif(1)), fn = Nig_Like_wo_mu, delta_X = delta_X,
                                  delta = delta))
    }
    if(class(opt_single_run) == "try-error"){
      data.frame(sigma = NA,
                 theta = NA,
                 kappa = NA,
                 mu = NA,
                 opt_val = Inf)
    }else{
      if(mu_incl == TRUE){
      data.frame(sigma = opt_single_run$par[1],
                 theta = opt_single_run$par[2],
                 kappa = opt_single_run$par[3],
                 mu = opt_single_run$par[4],
                 opt_val = opt_single_run$value[1])
      }else{
        data.frame(sigma = opt_single_run$par[1],
                   theta = opt_single_run$par[2],
                   kappa = opt_single_run$par[3],
                   opt_val = opt_single_run$value[1])
      }
    }
  }
  )
  parallel::stopCluster(cluster)
  
  return(
    do.call(rbind, all_runs) %>% as.data.frame() %>%  arrange(opt_val) %>% slice(1)
  )
}

#Various moments from the NIG process

NIG_moment_1 <- function(theta, mu, delta){
  (theta + mu)*delta 
}

NIG_moment_2 <- function(sigma, theta, kappa, delta){
  (sigma^2 + theta^2*kappa)*delta 
}

NIG_moment_3 <- function(sigma, theta, kappa, delta){
  (3*sigma^2*theta*kappa + 3*theta^3*kappa^2)*delta 
}

NIG_moment_4 <- function(sigma, theta, kappa, delta){
  (3*sigma^4*kappa + 18*sigma^2*theta^2*kappa^2 + 15*theta^4*kappa^3)*delta +
    3 * NIG_moment_2(sigma, theta, kappa, delta)^2 
}

sample_central_moment <- function(delta_X, order){
  (1/length(delta_X)) * sum( (delta_X - mean(delta_X))^order ) 
}


# Function used for method of moment estimation with the data from the NIG process
moment_diff <- function(param, delta_X, delta){
  
  sigma <- param[1]
  theta <- param[2]
  kappa <- param[3]
  mu <- param[4]
  
  if(sigma < 0 ){
    return(Inf)
  }
  
  return( 
    sum(
      (mean(delta_X) - NIG_moment_1(theta = theta, delta = delta, mu = mu))^2 +
        (sample_central_moment(delta_X, order = 2) - NIG_moment_2(sigma = sigma, theta = theta, kappa = kappa, delta = delta))^2 +
        (sample_central_moment(delta_X, order = 3) - NIG_moment_3(sigma = sigma, theta = theta, kappa = kappa, delta = delta))^2 +
        (sample_central_moment(delta_X, order = 4) - NIG_moment_4(sigma = sigma, theta = theta, kappa = kappa, delta = delta))^2 
    )
  )
}

#Running multiple times with different starting points
Optim_w_restarts_MoM <- function(n, delta_X, delta){
  n_cores <- 2
  cluster <- parallel::makeCluster(n_cores)
  parallel::clusterExport(cl = cluster, varlist = list("delta_X","delta", "moment_diff",
                                                       "NIG_moment_1", "NIG_moment_2", "NIG_moment_3",
                                                       "NIG_moment_4", "sample_central_moment"))
  
  all_runs <- parallel::parLapply(cl = cluster, X = 1:n, fun = function(X){
    opt_single_run <- try(optim(par = c(runif(1), runif(1), runif(1), runif(1)), fn = moment_diff, delta_X = delta_X,
                                delta = delta))
    if(class(opt_single_run) == "try-error"){
      data.frame(sigma = NA,
                 theta = NA,
                 kappa = NA, opt_val = Inf)
    }else{
      data.frame(sigma = opt_single_run$par[1],
                 theta = opt_single_run$par[2],
                 kappa = opt_single_run$par[3],
                 mu = opt_single_run$par[4],
                 opt_val = opt_single_run$value[1])
    }
  }
  )
  parallel::stopCluster(cluster)
  
  return(
    do.call(rbind, all_runs) %>% as.data.frame() %>%  arrange(opt_val) %>% slice(1)
  )
}


# Various sample moments needed for the analytical method of moment with the NIG process

hat_VAR <-  function(delta_X){
  1/(length(delta_X) - 1) * sum( (delta_X - mean(delta_X))^2 )
}

hat_skw <- function(delta_X){
  sample_central_moment(delta_X, order = 3)/sample_central_moment(delta_X, order = 2)^(3/2)
}

hat_krt <- function(delta_X){
  (sample_central_moment(delta_X, order = 4) / sample_central_moment(delta_X, order = 2)^2) - 3
}

hat_sigma_sqr <- function(delta_X, epsi, delta){
  (hat_VAR(delta_X = delta_X)/delta)*(1/(1 + epsi))
}

hat_kappa <- function(delta_X, epsi, delta){
  (delta/3)*hat_krt(delta_X = delta_X)*((1 + epsi)/(1 + 5*epsi))
}

hat_theta <- function(delta_X, epsi, delta){
  sample_central_moment(delta_X, order = 3)/
    (delta*hat_sigma_sqr(delta_X, epsi, delta)*hat_kappa(delta_X, epsi, delta)) *
    (1/(3 + 3*epsi))
}

hat_mu <- function(delta, delta_X, epsi){
  (1/delta) * (sum(delta_X)/length(delta_X)) - hat_theta(delta_X = delta_X, epsi = epsi, delta = delta)
}


# Function used for analytical method of moment estimation with the data from the NIG process

MoM_Analytic <- function(delta_X, delta){
  
  opt_epsi <-(hat_skw(delta_X = delta_X)^2)/(3*hat_krt(delta_X = delta_X) - 5*(hat_skw(delta_X = delta_X)^2))
  
  ## Using epsilon to calculate moments through analytical expressions
  
  cfs_sigma_sqrt <- sqrt(hat_sigma_sqr(delta_X, opt_epsi, delta))
  cfs_theta <- hat_theta(delta_X, opt_epsi, delta)
  cfs_kappa <- hat_kappa(delta_X, opt_epsi, delta)
  cfs_mu <- hat_mu(delta = delta, delta_X = delta_X, epsi = opt_epsi)
  
  return(data.frame(sigma = cfs_sigma_sqrt, theta = cfs_theta, kappa = cfs_kappa, mu = cfs_mu))
}


#Maximum likelihood using the parameters from the analytical method of moments as initial parameters.

MLE_n_MoM_Analytic <- function(delta_X, delta, mu_incl = TRUE){
  MoM_Analytic_fit <- MoM_Analytic(delta_X = delta_X, delta = delta)
  
  if(mu_incl == TRUE){
  MLE_n_MoM_analytic_fit <- data.frame(rbind(optim(par = c(MoM_Analytic_fit$sigma,
                                                           MoM_Analytic_fit$theta,
                                                           MoM_Analytic_fit$kappa,
                                                           MoM_Analytic_fit$mu), fn = Nig_Like, delta_X = delta_X,
                                                   delta = delta)$par))
  colnames(MLE_n_MoM_analytic_fit) <- c("sigma", "theta", "kappa", "mu")
  }else{
    MLE_n_MoM_analytic_fit <- data.frame(rbind(optim(par = c(MoM_Analytic_fit$sigma,
                                                             MoM_Analytic_fit$theta,
                                                             MoM_Analytic_fit$kappa), fn = Nig_Like_wo_mu, delta_X = delta_X,
                                                     delta = delta)$par)) 
    colnames(MLE_n_MoM_analytic_fit) <- c("sigma", "theta", "kappa")
  }

  MLE_n_MoM_analytic_fit
  
}


# Black Scholes maximum likelihood

BS_MLE <- function(x_fit, delta, n_runs = 10){
  
  BS_lik <- function(param, X_fit, delta){
    sigma <- param[1]
    mu <- param[2]
    
    -sum(dnorm(x = X_fit, mean = (mu-(1/2)*sigma^2)*delta, sd = sigma*sqrt(delta), log = TRUE))
  }
  
  cores <- 3
  cluster <- makeCluster(cores)
  clusterExport(cluster, ls(), environment())
  
  all_runs <- parLapply(cluster, 1:n_runs, fun = function(ins){
    
    opt_single_run  <- try(optim(par = c(runif(1,1e-9,1-1e-9  ), runif(1,1e-9,1-1e-9  )),
                                 fn = BS_lik, X_fit = x_fit, delta = delta,
                                 method = "L-BFGS-B", lower = c(1e-9, -Inf), upper = c(1-1e-9, Inf)))
    
    if(class(opt_single_run) == "try-error"){
      data.frame(sigma = NA,
                 mu = NA,
                 opt_val = Inf)
    }else{
      data.frame(sigma = opt_single_run$par[1],
                 mu = opt_single_run$par[2],
                 opt_val = opt_single_run$value[1])
    }
  })
  
  stopCluster(cluster)
  
  all_runs <- do.call(rbind, all_runs)
  all_runs <- all_runs[order(all_runs$opt_val),]
  
  return(
    all_runs %>% slice(1)
  )
}

#Bootstrap for NIG param

bootstrap_NIG_param <- function(data_sample, bootstrap_n, cores, delta, with_mu = FALSE){
  bootstraps <- lapply(1:bootstrap_n, FUN = function(input){sample(x = data_sample, replace = TRUE)})
  
  cluster <- makeCluster(cores)
  clusterExport(cluster, c("MLE_n_MoM_Analytic", "MoM_Analytic",
                           "hat_skw", "sample_central_moment", "hat_krt",
                           "hat_sigma_sqr", "hat_VAR", "hat_theta", "hat_kappa",
                           "hat_mu", "Nig_Like_wo_mu", "Nig_Like", "delta"), environment())
  
  if(with_mu == FALSE){
    bootstrap_params_wo_mu <- parLapply(cluster, bootstraps, function(input){
      MLE_n_MoM_Analytic(delta_X = input, delta = delta, mu_incl = FALSE)
    })
    stopCluster(cluster)
    return(bootstrap_params_wo_mu)
    
  }else{
    
    bootstrap_params_wo_mu <- parLapply(cluster, bootstraps, function(input){
      MLE_n_MoM_Analytic(delta_X = input, delta = delta, mu_incl = FALSE)
    })
    bootstrap_params_w_mu <- parLapply(cluster, bootstraps, function(input){
      MLE_n_MoM_Analytic(delta_X = input, delta = delta, mu_incl = TRUE)
    })
    stopCluster(cluster)
    return(list(
      bootstrap_params_wo_mu = bootstrap_params_wo_mu,
      bootstrap_params_w_mu = bootstrap_params_w_mu
    ))
  }
}

bootstrap_BS_param <- function(data_sample, bootstrap_n, delta){
  bootstraps <- lapply(1:bootstrap_n, FUN = function(input){sample(x = data_sample, replace = TRUE)})
  
    bootstrap_params <- lapply(bootstraps, function(input){
      BS_MLE(x_fit = input, delta = delta, n_runs = 10)
    })
    
    
    return(bootstrap_params)
}


bootstrap_sd <- function(bootstrap_list, n_bootstraps){
  do.call(rbind, bootstrap_list) %>% apply(., 2, function(input){
    sqrt(1/(n_bootstraps-1)*sum((input - mean(input))^2))
  })
}

# Validation --------------------------------------------------------------

week_fit_calc <- function(data_input, sim_runs = 10, cores = 3, ...){
  cat("Default run 5 min")
  data_input <- data_input %>% mutate(week_n = isoweek(Date))
  total_weeks <- rev(data_input$week_n %>% unique())
  highest_week <- total_weeks[1]
  delta <- 1/60
  cluster <- makeCluster(cores)
  clusterExport(cluster, c("MLE_n_MoM_Analytic", "MoM_Analytic", "data_input", "total_weeks",
                           "hat_skw", "sample_central_moment", "hat_krt", "hat_sigma_sqr", "hat_VAR",
                           "hat_theta", "hat_kappa", "hat_mu", "Nig_Like", "Sub_NIG_sim", "IG" ), environment())
  
  
  quants <- parLapply(cluster, total_weeks, function(input){
    week_list <- seq(highest_week, input, by = -1)
    train_df <- data_input[which(data_input$week_n %in%  week_list),]
    train_df_diff <- diff(log(train_df$Price))
    train_params <- MLE_n_MoM_Analytic(delta_X = train_df_diff, delta = delta)
    NIG_sim_quant <- lapply(1:sim_runs, FUN = function(input){ quantile(train_df[1,]$Price*exp(Sub_NIG_sim(T = nrow(train_df)*delta, delta = delta,
                                                                                                           sigma = train_params$sigma, theta = train_params$theta,
                                                                                                           kappa = train_params$kappa, mu_nig = train_params$mu))[-(nrow(train_df)+1)])})
    NIG_sim_quant <- do.call(rbind, NIG_sim_quant)
    Actual_qaunt <- quantile(train_df$Price)
    MAE <- mean(abs(NIG_sim_quant - Actual_qaunt))
    data.frame(MAE = MAE, week_n = input)
  })
  
  stopCluster(cluster)
  
  quants <- do.call(rbind, quants)
  return(arrange(quants, MAE))
}

# Pricing with NIG --------------------------------------------------------

#Esscher h
esscher_h_special_case <- function(sigma, theta, kappa){
  (-2*theta*kappa-sigma^2*kappa)/(sigma^2*kappa*2)
}

esscher_h <- function(sigma, theta, kappa, mu, r, delta){
 
  h_relation <- function(param, sigma, theta, kappa, mu, r){
    distance <- abs(exp(r) - (NIG_MGF(sigma = sigma, theta = theta, kappa = kappa, mu = mu, h = (param+1), t = 1) /
      NIG_MGF(sigma = sigma, theta = theta, kappa = kappa, mu = mu, h = param, t = 1)))
    if(distance == "NaN"){
      return(Inf)
    }else{
      distance
    }
  }
  
esscher_runs <- lapply(X = 1:50, FUN = function(input){
  single_run <- optim(par = runif(n = 1, min = -10, max = 10), fn = h_relation,
                      sigma = sigma, theta = theta, kappa = kappa, mu = mu, r = r,
                      method = "Brent", lower = -100, upper = 100)
  data.frame(h = single_run$par, value = single_run$value)
})

esscher_runs <- do.call(rbind, esscher_runs) 
esscher_runs[order(esscher_runs$value),][1,1]
}

#NIG measure 
NIG_measure <- function(theta, sigma, kappa, x){
  C <- (sqrt(theta^2 + sigma^2/kappa))/(2*pi*sigma*sqrt(kappa))
  A <- theta/sigma^2
  B <- (sqrt(theta^2 + sigma^2/kappa))/sigma^2
  
  return(
    (C/abs(x))*exp(A*x)*besselK(nu = 1, x = B * abs(x))
  )
}



#Moment generating function for NIG 
NIG_MGF=function(sigma,theta,kappa, mu,h,t){
  exp(t*(1/kappa-(1/kappa)*sqrt(1-h^2*sigma^2*kappa-2*theta*h*kappa) + mu*h))
}

Radon_Nik_Esscher <-  function(x_sim,h, delta, sigma, theta, kappa, mu){
  return(
    exp(h*x_sim) / NIG_MGF(sigma = sigma, theta = theta,  mu = mu,
                           kappa = kappa, h = h, t = delta)
  )
}


# Call option payoff
Euro_Call_payoff <- function(x,k){
  max(x - k, 0)
}


option_pricing_path <- function(sigma, theta, kappa, mu_nig, T,
                                delta, h, M, K, r, to_mean = TRUE, first_price = FALSE, ...){
  
  list2env(list(...), environment())
  cores <- 10
  cluster <- makeCluster(cores)
  clusterExport(cluster, ls(), environment())
  
  NIG_price_path <- parLapply(cluster, 1:M, fun = function(runs){
    Sub_NIG_sim(T = T, delta = delta, sigma = sigma, theta = theta, kappa = kappa, mu_nig = mu_nig)
  })
  stopCluster(cluster)
  
  NIG_price_path <- do.call(cbind, NIG_price_path)
  NIG_price_path <- NIG_price_path[-nrow(NIG_price_path),]
  
  if(first_price == TRUE){
    NIG_price_path <- NIG_price_path[nrow(NIG_price_path),]
    R_N_E <- Radon_Nik_Esscher(x_sim = NIG_price_path, h = h, delta = delta,
                               sigma = sigma, theta = theta, kappa = kappa, mu = mu_nig)
    NIG_price_path <-  S_0[1]*exp(NIG_price_path)
    return(lapply(1:length(NIG_price_path), FUN = function(input){
      Euro_Call_payoff(
        x = NIG_price_path[input], k = K) * R_N_E[input]
    }) %>% do.call(what = rbind))
  }else{
    NIG_price_path <- apply(NIG_price_path, 2, function(input){rev(input)})
    
    R_N_E <- Radon_Nik_Esscher(x_sim = NIG_price_path, h = h, delta = delta,
                               sigma = sigma, theta = theta, kappa = kappa, mu = mu_nig)
    NIG_price_path <-  S_0*exp(NIG_price_path)
    
    NIG_price_path <- apply(NIG_price_path, c(1,2),
                            FUN = function(input){
                              Euro_Call_payoff(
                                x = input, k = K)
                            }) * R_N_E
  }
  if(to_mean == TRUE){
    pay_off <- apply(NIG_price_path, MARGIN = 1, FUN = function(input){
      mean(input)
    })
  }else{
    return(NIG_price_path)
  }
  
  return(pay_off)
}

#Black Scholes Call price
BS_Call <- function(S, K, r, T, sigma) {
  d1  <-  (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  d2  <-  d1 - sigma*sqrt(T)
  S * pnorm(d1)  - K*exp(-r*T)*pnorm(d2)
}


# Delta hedging -----------------------------------------------------------

backward_finite_diff <- function(S_1, S_0, C_1, C_0){
  delta=(C_1 - C_0) / (S_1 - S_0)
  return(ifelse(abs(S_1 - S_0) < 0.0001,0,delta))
}

BS_delta=function(S, K, r, T, sigma){
  d1 <- (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  delta <- pnorm(d1)
  return(delta)
}

delta_hedge <- function(Price_seq, delta_seq,initial_price){
  
  hedge_df <- data.frame(deltas = delta_seq,
                         prices = Price_seq,
                         bank = 0,
                         Position_val = 0)
  
  
  for(i in 1:nrow(hedge_df)){
    if(i == 1){
      hedge_df$bank[1] <- initial_price - (hedge_df$deltas[1]*hedge_df$prices[1])
      hedge_df$Position_val[1] <- hedge_df$deltas[1]*hedge_df$prices[1]
      hedge_df$portfolio_val[1] <- hedge_df$bank[1] +  hedge_df$Position_val[1]
    }else{
      hedge_df$bank[i] <-  hedge_df$bank[i-1] - (hedge_df$deltas[i] - hedge_df$deltas[i-1])*hedge_df$prices[i]
      hedge_df$Position_val[i] <- hedge_df$deltas[i]*hedge_df$prices[i]
      hedge_df$portfolio_val[i] <- hedge_df$bank[i] +  hedge_df$Position_val[i]
    }
  }
  
  return(hedge_df)
}


# Variance optimal hedging ------------------------------------------------

variance_optimal_hedge <- function(C_0, S, expir_T, K, delta, kappa, sigma, theta, time_grid){
  
  ##helpfuns setup
  NIG_cum_gen_func <- function(z, delta, kappa, sigma, theta){
     (1/kappa - 1/kappa * sqrt(1 - z^2*sigma^2*kappa - 2*theta*z*kappa))
  }
  
  gamma_z <- function(z, delta, kappa, sigma, theta){
    (NIG_cum_gen_func(z+1, delta, kappa, sigma, theta) - NIG_cum_gen_func(z, delta, kappa, sigma, theta) - NIG_cum_gen_func(1, delta, kappa, sigma, theta)) /
      (NIG_cum_gen_func(2, delta, kappa, sigma, theta) - 2*NIG_cum_gen_func(1, delta, kappa, sigma, theta))
  }
  
  eta_z <-  function(z, delta, kappa, sigma, theta){
    NIG_cum_gen_func(z, delta, kappa, sigma, theta) - NIG_cum_gen_func(1, delta, kappa, sigma, theta)*gamma_z(z, delta, kappa, sigma, theta)
  }
  
  lambda <-  NIG_cum_gen_func(1, delta, kappa, sigma, theta)/(NIG_cum_gen_func(2, delta, kappa, sigma, theta) - 2* NIG_cum_gen_func(1, delta, kappa, sigma, theta))
  
  call_option_measure <- function(z, K){
    (1/(2*pi*1i)) * ((K^(1-z))/(z*(z-1)))
  }
  
  
  H_integrand <- function(z, S, t, K, expir_T, delta, kappa, sigma, theta, time_grid){
    S[t]^(z) * exp(eta_z(z, delta, kappa, sigma, theta)*(expir_T-time_grid[t])) * call_option_measure(z, K)  
  }
  
  xi_integrand <- function(z, S, t, K, expir_T, delta, kappa, sigma, theta, time_grid){
    S[t-1]^(z-1) * gamma_z(z, delta, kappa, sigma, theta) *
      exp(eta_z(z, delta, kappa, sigma, theta)*(expir_T-time_grid[t])) * call_option_measure(z, K)
  }
  
  ##Initial portfolio def
  H <- data.frame(H = rep(NA, (expir_T*(1/delta))))
  xi <- data.frame(xi = rep(NA, (expir_T*(1/delta))))
  phi <- data.frame(phi = rep(NA, (expir_T*(1/delta))))
  G <- data.frame(G = rep(0, (expir_T*(1/delta)))) #Setting 0 for sum function
  V <- data.frame(V = rep(NA, (expir_T*(1/delta))))
  
  h_temp <- integrate(function(z) Re(H_integrand(z = (2 + 1i*z), S = S, t = 1, expir_T = expir_T, K = K, delta = delta,
                                                  kappa = kappa, sigma = sigma, theta = theta, time_grid = time_grid)),
                       lower = -Inf, upper = Inf, subdivisions = 100000)$value + 
    1i*integrate(function(z) Im(H_integrand(z = (2 + 1i*z), S = S, t = 1, expir_T = expir_T, K = K, delta = delta,
                                            kappa = kappa, sigma = sigma, theta = theta, time_grid = time_grid)) ,
                 lower = -Inf, upper = Inf, subdivisions = 100000)$value
  H[1,1] <- sqrt(Re(h_temp)^2 + Im(h_temp)^2)
  V[1,1] <- H[1,1]
  G[1,1] <- 0
  
  missing_deltas_due_tue_tolerance <- NULL
  
  for(t in 2:(expir_T*(1/delta))){

    h_temp <- try(integrate(function(z) Re(H_integrand(z = (2 + 1i*z), S = S, t = t, expir_T = expir_T, K = K, delta = delta,
                                                   kappa = kappa, sigma = sigma, theta = theta, time_grid = time_grid)),
                        lower = -Inf, upper = Inf, subdivisions = 100000, rel.tol = 1e-8)$value + 
      1i*integrate(function(z) Im(H_integrand(z = (2 + 1i*z), S = S, t = t, expir_T = expir_T, K = K, delta = delta,
                                              kappa = kappa, sigma = sigma, theta = theta, time_grid = time_grid)) ,
                   lower = -Inf, upper = Inf, subdivisions = 100000, rel.tol = 1e-8)$value, silent = TRUE)
    if(class(h_temp) == "try-error"){
      H[t,1] <- 0
      xi[t,1] <- 0
      phi[t,1] <- 0
      G[t,1] <- G[t-1,1] + phi[t,1]*(S[t]-S[t-1])
      missing_deltas_due_tue_tolerance <- c(missing_deltas_due_tue_tolerance, t)
      cat(paste("\r", t, "    ", round((t/(expir_T*(1/delta) - 1))*100), "%  |", "Missing due to tol:", TRUE))
      next
    }else{
      H[t,1] <- sqrt(Re(h_temp)^2 + Im(h_temp)^2)
      }
    
    xi_temp <- try(integrate(function(z) Re(xi_integrand(z = (2 + 1i*z), S = S, t = t, expir_T = expir_T, K = K, delta = delta,
                                                     kappa = kappa, sigma = sigma, theta = theta, time_grid = time_grid)),
                         lower = -Inf, upper = Inf, subdivisions = 100000, rel.tol = 1e-8)$value + 
      1i*integrate(function(z) Im(xi_integrand(z = (2 + 1i*z), S = S, t = t, expir_T = expir_T, K = K, delta = delta,
                                               kappa = kappa, sigma = sigma, theta = theta, time_grid = time_grid)),
                   lower = -Inf, upper = Inf, subdivisions = 100000, rel.tol = 1e-8)$value, silent = TRUE)
    
    if(class(xi_temp) == "try-error"){
      H[t,1] <- 0
      xi[t,1] <- 0
      phi[t,1] <- 0
      G[t,1] <- G[t-1,1] + phi[t,1]*(S[t]-S[t-1])
      missing_deltas_due_tue_tolerance <- c(missing_deltas_due_tue_tolerance, t)
      
      cat(paste("\r", t, "    ", round((t/(expir_T*(1/delta) - 1))*100), "% |", "Missing due to tol:", TRUE))
      next
    }else{
      xi[t,1] <- sqrt(Re(xi_temp)^2 + Im(xi_temp)^2)
      }
    
    phi[t,1] <- xi[t,1] + (lambda/S[t-1]) * (H[t-1,1] - V[1,1] - G[t-1,1])
    
    G[t,1] <- G[t-1,1] + phi[t,1]*(S[t]-S[t-1])
    cat(paste("\r", t, "    ", round((t/(expir_T*(1/delta) - 1))*100), "% |", "Missing due to tol:", FALSE))
  }
  
  return_list <- list(
    phi = phi,
    cum_gain = G,
    xi = xi,
    H = H,
    missing_pos = missing_deltas_due_tue_tolerance
  )
  
  return(return_list)
}

# Utility functions -------------------------------------------------------

#High resolution PNG 
HR_Png <- function(name, figure){
  png(paste(name,".png", sep =""), units = "px", width = 1600, height = 800, res = 300)
  print(figure)
  dev.off()
}

#Q-Q plot
QQ_plot_base <- function(x, y){
  order_x <- x[order(x)]
  order_y <- y[order(y)]
  ggplot() + geom_point(aes(x = order_x, y = order_y)) +
    geom_abline(intercept = 0, slope = 1, color = "red", size = 1) + 
    xlab("Actual log return") + ylab("Simulated log returns")
}

#Single X qqplot
QQ_plot <- function(nig, bs, actual){
  input_df <- reshape2::melt(data.frame("NIG" = sort(nig), "BS" = sort(bs), "Actual" = sort(actual)), id.var = "Actual")
  
  ggplot(input_df) + geom_point(aes(x = Actual, y = value, color = variable)) +
    geom_line(aes(x = Actual, y = Actual), color = "black", size = 1) + 
    xlab("Actual log return") + ylab("Simulated log returns") + coord_cartesian(ylim=c(min(input_df$value), max(input_df$value))) +
    scale_color_manual(labels = c("NIG", "BS"),
                       breaks = c("NIG", "BS"),
                       values = c("Red", "Blue"),
                       name = "") 
}

##Multi X qqplot
QQ_plot_multi <- function(x_axis, y_axis, xlab, ylab){
  colnames(y_axis) <- paste(seq(1:ncol(y_axis)))
  comb <- data.frame(y_axis, act_return = x_axis )
  
  comb <- apply(comb, 2, FUN = function(input){(diff(log(input)))}) 
  comb <- comb %>% na.omit() %>% apply(2, sort) %>% as.data.frame()
  
  comb <- reshape2::melt(comb, id.vars = c("act_return"))
  
  ggplot(comb) + geom_point(aes(x = act_return, y = value, group = variable), color = "gray") +
    geom_line(aes(x = act_return, y = act_return), color = "red", alpha = 0.35, size = 1.2) + 
    coord_cartesian(ylim=c(min(comb$value), max(comb$value))) + xlab(xlab) + ylab(ylab)
}

validation_plots <- function(nig_sigma, nig_theta, nig_kappa, bs_sigma, nig_mu, bs_mu, n, delta, T, Price_seq, sep = FALSE){
  set.seed(123)
  NIG_prices <- Price_seq[1]*exp(Sub_NIG_sim(T=T,delta=delta,sigma=nig_sigma,theta=nig_theta,kappa=nig_kappa, mu_nig = nig_mu))[-(n+1)]
  set.seed(123)
  BS_prices <- sde::sde.sim(model = "BS" ,X0=Price_seq[1], theta = c(bs_mu ,bs_sigma) , delta=delta, N=(n-1))
  
  if(sep == FALSE){
  combined_df <- rbind(data.frame(Price = diff(log(NIG_prices)), class = "NIG"),
                       data.frame(Price = diff(log(BS_prices)), class = "BS"),
                       data.frame(Price = diff(log(Price_seq)), class = "Actual")
  )

  return(
    list(
      boxplot = ggplot(combined_df, aes(x=class, y=Price)) + geom_boxplot(lwd = 0.01, outlier.size = 1, color = "red") + coord_flip() + 
        ylab("Log returns") + xlab(""), 
      qqplot = QQ_plot(nig = diff(log(NIG_prices)), bs = diff(log(BS_prices)), actual =  diff(log(Price_seq)))
    )
  )
  }
  if(sep == TRUE){
    browser()
    return(
      list(
        boxplot = ggplot(combined_df, aes(x=class, y=Price)) + geom_boxplot(lwd = 0.01, outlier.size = 1, color = "red") + coord_flip() + 
          ylab("Log returns") + xlab(""), 
        qqplot = QQ_plot(nig = diff(log(NIG_prices)), bs = diff(log(BS_prices)), actual =  diff(log(Price_seq)))
      )
    )
  }
}


#Data sync function
sync_data <- function(data, tick_time){
  all_dates <- as.Date(data$date) %>% unique()
  all_hours <- hour(data$date) %>% unique()
  
  tick_grid <- data.frame("tick_grid" = seq(as.POSIXct(paste(all_dates[1], paste("0",min(all_hours),":00:00", sep = "")), tz = "UTC"),
                                            as.POSIXct(paste(all_dates[length(all_dates)],
                                                             paste(max(all_hours)+1,":00:00", sep = "")), tz = "UTC"), paste(tick_time)))
  tick_grid <- subset(tick_grid, format(tick_grid, format = "%H:%M:%S") >= "09:30:00" &
                        format(tick_grid, format = "%H:%M:%S") <= "16:00:00")
  
  synced_data <- data.frame(Date = rep(tick_grid[1,1], nrow(tick_grid)), Price = rep(NA, nrow(tick_grid)))
  synced_data[1,] <- data.frame(tick_grid[1,1], data$price[1])
  
  pb <-  pbapply::timerProgressBar(min = 1, max = nrow(synced_data), initial = 0) 
  
  for(i in 2:nrow(synced_data)){
    prices_bw_ticks <- subset(data, date >= tick_grid[i-1,1] & date <= tick_grid[i,1] )
    if(nrow(prices_bw_ticks) == 0){
      syn_tick <- data.frame(Date =  tick_grid[i,1], Price = synced_data[i-1,2])
    }else{
      prices_bw_ticks <- prices_bw_ticks[order(prices_bw_ticks$date, decreasing = TRUE),]
      syn_tick <- data.frame(Date =  tick_grid[i,1], Price = prices_bw_ticks$price[1])
    }
    synced_data[i,] <- syn_tick 
    pbapply::setTimerProgressBar(pb,i)
  }
  
  return(synced_data)
}


# split the data into weeks and find estimates based on each week. 

param_week=function(df,delta){
  parameter_week <- matrix(nrow=length(unique(isoweek(df$Date))),ncol=4)
  for (i in unique(isoweek(df$Date))) {
    
    nig_fit=tryCatch({
      week <- df[ isoweek(df$Date) == i, ]
      fit_week <- diff(log(week$Price))
      nig_fit <- MLE_n_MoM_Analytic(delta_X = fit_week, delta = delta)
    }, error=function(e) {
      week <- df[ isoweek(df$Date) == i, ]
      fit_week <- diff(log(week$Price))
      nig_fit <- Optim_w_restarts_MLE(100,delta_X = fit_week, delta = delta)[-5]
    })
    
    
    parameter_week[i,]=as.matrix(nig_fit)
    
  }
  return(parameter_week)
}

param_week_plot=function(df,delta,week){

    
    nig_fit=tryCatch({
      data <- df[ isoweek(df$Date) == week, ]
      fit_week <- diff(log(data$Price))
      nig_fit <- MLE_n_MoM_Analytic(delta_X = fit_week, delta = delta)
    }, error=function(e) {
      data <- df[ isoweek(df$Date) == week, ]
      fit_week <- diff(log(data$Price))
      nig_fit <- Optim_w_restarts_MLE(100,delta_X = fit_week, delta = delta)[-5]
    })
    data <- df[ isoweek(df$Date) == week, ]
    fit_week <- diff(log(data$Price))
    BS_fit=BS_MLE(fit_week,  delta)
    Period1_figs <- validation_plots(nig_sigma =  as.numeric(nig_fit[1]), nig_theta = as.numeric(nig_fit[2]),
                                     nig_kappa = as.numeric(nig_fit[3]), nig_mu = as.numeric(nig_fit[4]), bs_sigma = as.numeric(BS_fit[1]),
                                     bs_mu = as.numeric(BS_fit[2]), n = length(data$Price), delta = delta, T = length(data$Price) * delta,
                                     Price_seq = data$Price)
    
    period1_boxplot <- Period1_figs$boxplot
    period1_qqplot <- Period1_figs$qqplot
    output=list()
    output$boxplot=period1_boxplot
    output$qqplot=period1_qqplot
  return(output)
}

param_month=function(df,delta){
  parameter_month <- matrix(nrow=length(unique(month(df$Date))),ncol=4)
  for (i in unique(month(df$Date))) {
    
    nig_fit=tryCatch({
      month <- df[ month(df$Date) == i, ]
      fit_month <- diff(log(month$Price))
      nig_fit <- MLE_n_MoM_Analytic(delta_X = fit_month, delta = delta)
    }, error=function(e) {
      week <- df[ month(df$Date) == i, ]
      fit_month <- diff(log(month$Price))
      nig_fit <- Optim_w_restarts_MLE(100,delta_X = fit_month, delta = delta)[-5]
    })
    
    
    parameter_month[i,]=as.matrix(nig_fit)
    
  }
  return(parameter_month)
}


param_month_plot=function(df,delta,month){

  
  nig_fit=tryCatch({
    data <- df[ which(isoweek(df$Date) %in% month), ]
    fit_month <- diff(log(data$Price))
    nig_fit <- MLE_n_MoM_Analytic(delta_X = fit_month, delta = delta,mu_incl = FALSE)
  }, error=function(e) {
    data <- df[which(isoweek(df$Date) %in% month), ]
    fit_month <- diff(log(data$Price))
    nig_fit <- Optim_w_restarts_MLE(100,delta_X = fit_month, delta = delta,mu_incl = FALSE)[-4]
  })
  data <- df[ which(isoweek(df$Date) %in% month), ]
  fit_month <- diff(log(data$Price))
  BS_fit=BS_MLE(fit_month,  delta)
  Period1_figs <- validation_plots(nig_sigma =  as.numeric(nig_fit[1]), nig_theta = as.numeric(nig_fit[2]),
                                   nig_kappa = as.numeric(nig_fit[3]), nig_mu = 0, bs_sigma = as.numeric(BS_fit[1]),
                                   bs_mu = as.numeric(BS_fit[2]), n = length(data$Price), delta = delta, T = length(data$Price) * delta,
                                   Price_seq = data$Price)
  
  period1_boxplot <- Period1_figs$boxplot
  period1_qqplot <- Period1_figs$qqplot
  output=list()
  output$boxplot=period1_boxplot
  output$qqplot=period1_qqplot
  return(output)
}
