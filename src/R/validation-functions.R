MSE <- function(y_true, y_pred){
  n <- length(y_true)
  return(sum((y_true - y_pred)^2)/n)
}

RMSE <- function(y_true, y_pred){
  return(sqrt(MSE(y_true, y_pred)))
}

MAPE <- function(y_true, y_pred){
  n <- length(y_true)
  return(abs(sum(((y_true+1) - (y_pred+1))/(y_true+1))) / n)
}

MAE <- function(y_true, y_pred){
  n <- length(y_true)
  return(sum(abs(y_true - y_pred))/n)
}
