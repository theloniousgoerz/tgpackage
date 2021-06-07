#' my_rf_cv
#'
#' This function calculate a cross validation error for a random forest using the "palmerpenguins" data.
#'
#' @param k Is the number of K folds to run.
#' @keywords prediction
#'
#' @import randomForest
#' @import palmerpenguins
#' @importFrom randomForest randomForest
#' @return A numeric calculation of the \code{cv_err}.
#'
#' @examples
#' my_rf_cv(k = 3)
#' my_rf_cv(k = 5)
#'
#' @export
my_rf_cv <- function(k) {
  # Require packages for function.
  # Define data.
  data = penguins %>%
    # Clean data for NA
    na.omit() %>%
    # Define data input.
    # Select Vars
    select(flipper_length_mm,
           bill_length_mm,
           bill_depth_mm,
           body_mass_g)
  # Define fold.
  fold = sample(rep(1:k,length = nrow(data)))
  data = cbind(data, fold)
  # Create a list for the data
  rf_list = list()
  mse_list = list()
  for (i in 1:k) {
    # Define data
    data_train = data %>%
      # Filter folds to all but i
      filter(fold != i) %>%
      # Rm fold from computation.
      select(-fold)
    # Filter test data to i.
    data_test = data %>% dplyr::filter(fold == i)
    # Define the y outcome to calculate against
    data_test_y = data_test$body_mass_g
    # Remove fold from data test.
    data_test = data_test %>% select(-fold)
    # run random forest model
    rand_for <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
                             # Data.
                             data = data_train,
                             # Set ntree to 100
                             ntree = 100)
    # Compute prediction on test data.
    prediction <-  predict(rand_for, newdata = data_test)
    # Assign random forest estimates to a list.
    rf_list[[i]] <- rand_for
    # Compute MSE
    # Compute MSE
    mse = mean((prediction - data_test_y)^2)
    # Add MSE to a list.
    mse_list[[i]] <- mse
  }
  # Compute cross validation error.
  cv_err = mean(unlist(mse_list))
  # Return Cross validation error.
  return(cv_err)
  # Returns the CV error which is type double.
}
utils::globalVariables(c("bill_depth_mm","bill_length_mm","body_mass_g"))
