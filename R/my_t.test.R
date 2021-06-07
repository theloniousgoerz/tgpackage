#' My_t_test
#'
#' This function runs a left, right, and two-sided t test.
#'
#' @param x Numeric input data to use in your function.
#' @param alternative Character specifying what the alternative hypothesis is: less, greater,two.sided.
#' @param mu The null hypothesis or your test.
#' @keywords inference.
#'
#' @return Returns a list with the t.test output and a model table and code
#'   \code{Hypothesis, T-statistic, DF, P-value}. It also returns a call of the hypothesis tested.
#'
#' @import gapminder
#' @importFrom stats model.frame model.matrix model.response na.omit predict pt sd
#'
#' @examples
#' my_t.test(gapminder$lifeExp,alternative = "two.sided",mu = 60)
#' my_t.test(gapminder$lifeExp,alternative = "less",mu = 60)
#'
#' @export
my_t.test <- function(x,alternative,mu) {
  # Evaluate numeracy
  if (is.numeric(x) == F) {
    # Stop if the data is not numeric
    stop("Stop: X Is not numeric!")}
  # Calculate T statistic
  # observed - alternative
  test_stat = (mean(x) - mu) /
    # divide by the sd divided by data
    (sd(x)/ sqrt(length(x)))
  # calculate the degrees of freedom
  df = length(x) -1
  # Condition to evaluate the hypothesis type
  if (alternative %in% c("greater","two.sided","less") == F) {
    # Condition if failed
    stop("Unsupported Hypothesis type: Choose, greater,less, or two.sided") }
  # Condition if greater
  if (alternative == "greater") {
    # Calculate the p value for the right tail
    p_value = pt(abs(test_stat),
                 # degrees of freedom
                 df, lower.tail = T)
    # Condition for less than
  } else if (alternative == "less") {
    # Calculate the p value for lower tail
    p_value = pt(abs(test_stat),
                 # degrees of freedom
                 df, lower.tail = F)
    # condition for the two.sided
  } else {
    # two sided condition
    p_value =  2 * pt(-abs(test_stat),
                      # degrees of freedom
                      df)
  }
  # Output for function
  # define the alternative hypothesis outcome
  alternative = paste("Alternative Hypoethesis: The true mean is",
                      # alt hypothesis type
                      alternative,"than",
                      # null hypothesis
                      mu,
                      # Separate argument
                      sep = " ")
  # Returns a list
  # Alternative hypothesis call

  return(list(Hypothesis = alternative,
              # Test statistic calculated
              "T-Statistic" = paste("The t-statistic is:",
                                    # Test stat with a separation
                                    # round digits to 3
                                    round(test_stat,digits = 3), sep = " "),
              # Degrees of freedom
              "DF" = paste("Degrees of Freedom:",df, sep = " "),
              # P value for the model
              "P Value" =   paste("P-Value:",
                                  # round the p-value for readability
                                  round(p_value, digits = 3), sep = " ")))
}
