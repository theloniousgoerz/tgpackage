#' My Linear Model (lm) function
#'
#' This function fits and summarizes the results of a linear model
#'   estimated with Ordinary Least Squares.
#'
#' @param formula A formula object \code{Y~X}.
#' @param data A data frame or matrix.
#' @keywords inference,prediction
#'
#' @return Returns a list object with a model table containing the \code{coefficients},
#'   \code{t-values}, degrees of freedom, and p-values computed from the t-distribution.
#'   These regression coefficients should be exactly the same as the base R lm().
#'
#' @examples
#' my_lm(lifeExp ~ gdpPercap,data = gapminder :: gapminder)
#' my_lm(lifeExp ~ gdpPercap + country ,data = gapminder :: gapminder)
#'
#' @export
my_lm <- function(formula,data) {
  # Data
  # Create design matrix w/ intercept
  X = model.matrix(formula,data)
  # Create model response vector
  Y = model.response(model.frame(formula,data))
  # Calculate degrees of freedom
  df =  nrow(data) - ncol(X)
  # Parameters
  # Beta
  beta = solve(t(X) %*% X) %*% t(X) %*% Y
  # Varaince
  sigma2 = sum((Y - X%*%beta)^2 /df)
  # Calculate sigma * var mat
  sig_var = sigma2 * solve(t(X) %*% X)
  # Calculate standard error
  se.fit = sqrt(diag(sig_var))
  # Calculate t-vale
  t = (beta - 0) / se.fit
  # Calculate p-value (two-tailed)
  p =  2 * pt(abs(t), df = df, lower.tail = F)
  # Returns beta,se,t,p,df
  # Wrap data frame in a matrix then as a table
  model = as.table(as.matrix(data.frame("Estimate" = beta,
                                        "Std. Error" = se.fit,
                                        "T-value" = t,
                                        "Pr(>|t|)" = p,
                                        "DF" =  df,
                                        # fixes odd naming
                                        check.names = F)))
  # return model
  return(model)
}
