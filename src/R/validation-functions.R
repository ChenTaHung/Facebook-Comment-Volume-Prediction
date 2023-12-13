library(arm)
MSE <- function(y_true, y_pred){
  n <- length(y_true)
  y_pred <- as.integer(y_pred)
  return(sum((y_true - y_pred)^2)/n)
}

RMSE <- function(y_true, y_pred){
  y_pred <- as.integer(y_pred)
  return(sqrt(MSE(y_true, y_pred)))
}

MAPE <- function(y_true, y_pred){
  n <- length(y_true)
  y_pred <- as.integer(y_pred)
  return(abs(sum(((y_true+1) - (y_pred+1))/(y_true+1))) / n)
}

MAE <- function(y_true, y_pred){
  n <- length(y_true)
  y_pred <- as.integer(y_pred)
  return(sum(abs(y_true - y_pred))/n)
}

evaluate_residuals <- function(residual) {
  residuals_info <- summary(residual)
  return(residuals_info)
}


# fitted ~ resid plot -----------------------------------------------------

plot_residuals <- function(fitted, residvalue) {
  plot(residvalue ~ fitted,
       xlab = "Fitted Values",
       ylab = "Residuals",
       main = "Residuals vs Fitted")
  abline(h = 0, col = "red")
}

plot_qq <- function(residvalue) {
  qqnorm(residvalue)
  qqline(residvalue, col = "red")
}

plot_binned_residuals <- function(fittedvalue, residvalue) {

  binnedplot(fittedvalue, residvalue,
             xlab = "Fitted values",
             ylab = "Residuals",
             main = "Binned Residual Plot")
}


