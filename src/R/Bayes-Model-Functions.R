
library(rstan)
library(rstanarm)

customized_stratified_kfold_CV <- function(data, k, stratified_target) {
  
  # Customized K-Fold Cross Validation
  # Make sure our train datasets include all the genetID.
  # ======
  # data : data to split
  # k : number of folds
  # stratified_target: <string> column_name that used as the target to stratify by. 
  # random_k_fold: <bool> specify whether sample by random for each folds (duplicated data might exist)
  # rand_select_nobs: <float, int> number or proportion to sample in each fold.
  # ======
  # Output: <list>
  ## ioslated : <data.frame>
  ## Folds:
  ###   fold1:
  ###   fold2:
  ###   ...:
  ###   foldk:
    
  
  # Identify rows in the specified column that appear less than k times
  freq <- table(data[[stratified_target]])
  less_than_k <- names(freq[freq < k])
  
  # separate data with genetID samples less than k
  isolated_data <- data[data[[stratified_target]] %in% less_than_k, ]
  remaining_data <- data[!data[[stratified_target]] %in% less_than_k, ]
  
  # create k_fold output
  k_fold_datasets <- list()
  
  # store the isolated datasets separately
  k_fold_datasets[['isolated']] <- isolated_data
  
  # get k_fold index for the remain datasets
  k_fold_id <- caret::createFolds(remaining_data[[stratified_target]], k = k, list = TRUE)  

  for(i in 1:k){
    k_fold_datasets[['Folds']][[paste0('fold',i)]] <- remaining_data[as.vector(k_fold_id[[i]]), ]
  }

  
  return(k_fold_datasets)
}

rstan_mixEff_lasso <- function(data, formula, location = 0, lambda, chains = 4, iter = 2000, refresh = 0, adapt_delta = NULL, QR = FALSE, sparse = FALSE){
  
  # Mixed Effect Lasso Bayesian Regression
  # ===
  # data : data
  # formula : formula of the regression model
  # chain: How many chains for the stan program to conduct the HMC sampler (by default)
  # iter: number of iteration in each chain
  # refresh: set to 0 to cancel the output in console.
  # adapt_delta: <float> specify the  target average proposal acceptance probability during Stan's adaptation period. (when using HMC)
  # QR: <bool> if TRUE applies a scaled qr decomposition to the design matrix
  # sparse: <bool> A logical scalar (defaulting to FALSE) indicating whether to use a sparse representation of the design (X) matrix. 
  ## It is not possible to specify both QR = TRUE and sparse = TRUE.
  # ===

  # Fit the model with the specified Laplace prior scale
  fit <- stan_glmer(formula, data = data, family = neg_binomial_2(), 
                    prior = laplace(location = location, scale = lambda, autoscale = FALSE), 
                    prior_covariance = decov(),
                    chains = chains, iter = iter, refresh = refresh,
                    adapt_delta = adapt_delta, QR = QR, sparse = sparse)

  return(fit)
}

customizedRandomizedSearchCV <- function(data, formula, fold_number, stratified_target, MCMC_parms = NULL, randomseed = NULL, lambda_dist, ...){
  # Randomized Search K-Fold Validation Applying Customized K-Fold CV
  # =====
  # data : <class = data.frame> data to fit model
  # formula: <object = formula> formula object to pass to the fit model function
  # fold_number: <int> number k for k-fold CV.
  # stratified_target: <string> column name that is the stratified target for k-fold cv.
  # MCMC_parms: <list> list for controlling MCMC sampling method(chains, iter, refresh, adapt_delta, QR, sparse)
  # location: <int/float> mean of the laplace prior
  # lambda_dist: <callable distribution function> distribution function that lambda is going to sample from
  # ... : named parameters of the lambda_dist:
  # # e.g. : lambda_dist = rnorm, ... = (n = 1 , mean = 0, sd = 1)
  # =====
  # Return: <list> MSE_array
  # illustration of matrix for first two dimensions
  # =====
  #          | fold1 | fold2 | fold3 | fold4 | fold5
  # ------------------------------------------------
  # lamba_1  |       |       |       |       |
  # lamba_2  |       |       |       |       |
  # lamba_3  |       |       |       |       |
  # lamba_4  |       |       |       |       |
  # ...      |       |       |       |       |
  # lambda_p |       |       |       |       |

  
  folds_info <- customized_stratified_kfold_CV(data = data, k = fold_number, stratified_target = stratified_target)
  
  if(is.null(randomseed)){
    params_value_vec <- do.call(lambda_dist, list(...))
  }
  else{
    set.seed(randomseed)
    params_value_vec <- do.call(lambda_dist, list(...))
  }
  
  n_lambda <- length(params_value_vec)
  
  MSE_array <- array(NA, dim = c(n_lambda, fold_number, 2), dimnames = list(params_value_vec, paste0('fold', c(1:5)), c('train', 'valid'))) # third dimension is to store both MSE of training data and validation data.
  
  response <- strsplit(as.character(formula), "~")[[2]]

  isolated_df <- folds_info[["isolated"]]
  
  for(l in 1:n_lambda){ # loop over each lambda candidates
    lambda <- params_value_vec[l]
    
    # summary for each lambda
    L_summary <- list()
    
    for(k in 1:fold_number){ # loop over each fold
      
      # normal k-fold process
      folds <- folds_info[["Folds"]]
      valid_df <- folds[[k]]
      train <- do.call(rbind, folds[-k])
      
      # concatenate the datasets that contains not enough observation for a specific genetID to be included in CV.

      train_df <- rbind(isolated_df, train)
      
      index <- 1
      
      if (!is.null(MCMC_parms)){ # arguments of MCMC params have been passed to the function
        
        # override MCMC settings
        chains <- MCMC_parms[['chains']]
        iter <- MCMC_parms[['iter']]
        refresh <- MCMC_parms[['refresh']]
        adapt_delta <- MCMC_parms[['adapt_delta']]
        QR <- MCMC_parms[['QR']]
        sparse <- MCMC_parms[['sparse']]
        
        fit <- rstan_mixEff_lasso(data = train_df, formula = formula, lambda = lambda, chains = chains, iter = iter, refresh = refresh, adapt_delta = adapt_delta, QR = QR, sparse = sparse)
        
        train_fitted <- fitted(fit)
        train_resid <- resid(fit)
        # calculate MSE for train data
        MSE_train <- sum(train_resid^2) / length(train_resid)
        MSE_array[l,k,1] <- MSE_train
        
        valid_fitted <- posterior_predict(fit, newdata = valid_df) # we use posterior predict when we apply a Bayesian Regression
        valid_resid <- valid_fitted - valid_df[[response]]
        # calculate MSE for validation data
        MSE_valid <- sum(valid_resid^2) / length(valid_resid)
        MSE_array[l,k,2] <- MSE_valid
        
      }
      else{ # MCMC by default (refresh = 0)
        
        fit <- rstan_mixEff_lasso(data = train_df, formula = formula, lambda = lambda)
        
        train_fitted <- fitted(fit)
        train_resid <- resid(fit)
        # calculate MSE for train data
        MSE_train <- sum(train_resid^2) / length(train_resid)
        MSE_array[l,k,1] <- MSE_train
        
        valid_fitted <- posterior_predict(fit, newdata = valid_df) # we use posterior predict when we apply a Bayesian Regression
        valid_resid <- valid_fitted - valid_df[[response]]
        # calculate MSE for validation data
        MSE_valid <- sum(valid_resid^2) / length(valid_resid)
        MSE_array[l,k,2] <- MSE_valid
      }

    }
    
  }
  
  return(MSE_array)
}

find_best_tune <- function(MSE_array, which_MSE = 'valid'){
  
  # Find the best lambda for refitting model with entire dataset
  #=====
  # MSE_array: <array dim(l,k,2)> 
  # which_MSE: <string> 'both'/'train'/'valid', default = 'both'
  # return:
  ## which_MSE == 'both' : <vector> best lambda vector
  ## which_MSE == 'train'/'valid' : <float> best lambda value
  
  train_MSE_mat <- MSE_array[,,1]
  valid_MSE_mat <- MSE_array[,,2]
  
  lambdas_vec <- as.numeric(dimnames(MSE_array)[[1]])
  
  # calculate mean MSE for each "lambda"
  train_mean_MSE <- apply(train_MSE_mat, 1, mean, simplify = TRUE)
  valid_mean_MSE <- apply(valid_MSE_mat, 1, mean, simplify = TRUE)
  
  # extract best lambda
  best_train_lambda <- lambdas_vec[which.min(train_mean_MSE)]
  best_valid_lambda <- lambdas_vec[which.min(valid_mean_MSE)]
  
  if (which_MSE == 'both'){
    best_lambdas <- c(best_train_lambda, best_valid_lambda)
    names(best_lambdas) <- c('train', 'valid')
    return(best_lambdas)
  }
  else if(which_MSE == 'train'){
    return(best_train_lambda)
  }
  else{ # which_MSE == 'valid'
    return(best_valid_lambda)
  }
  
}


