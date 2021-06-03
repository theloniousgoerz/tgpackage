#' My_knn_cv
#'
#' This function calculates a k-fold cross validation for the k nearest neighbors algorithm.
#'
#' @param train Is a matrix with no NAs or missing values that is used to train the model.
#' @param cl Is the true classification of the training data.
#' @param k_nn Is the number of nearest neighbors to include in the cross validation calculation.
#' @param k_cv Is the number of folds to use for the cross validation (common Ks are 2,5, and 10).
#' @keywords prediction
#'
#' @examples
#' set.seed(1)
#'
#' rand_data <- data.frame(x1 = rnorm(100,0,1),x2 = rnorm(100,2,1))
#' rand_data_cl <- data.frame(y = rbinom(100,1,.3))
#' my_knn_cv(rand_data,rand_data_cl,k_nn = 5,k_cv = 5)
#' my_knn_cv(rand_data,rand_data_cl,k_nn = 5,k_cv = 10)
#'
#' @import class
#' @importFrom class knn
#'
#' @return Type list with a \code{cv_err} object and the predicted classification \code{class} output.
#'
#' @export
my_knn_cv <- function(train,cl,k_nn,k_cv) {
  # Depends on:
  # Create fold arg, with k partitions.
  fold = sample(rep(1:k_cv,length = nrow(train)))
  # Add train to fold vector.
  data = cbind(train,fold)
  # Create one with class
  data_w_cl = cbind(fold,cl)
  # Create full data for eventual KNN calc.
  # Clone CL.
  cl_full <- as_vector(cl)
  # Clone train.
  train_full <- train
  # Create a list object for later values.
  knn_list = list()
  knn_error_list = list()
  knn_corr_list = list()
  # Iterate through the Ks.
  for (i in 1:k_cv) {
    # Create trainind and test data.
    data_train = data %>% dplyr:: filter(fold != i) %>%
      select(-fold)
    data_test = data %>% dplyr ::filter(fold == i) %>%
      select(-fold)
    cl = data_w_cl %>% dplyr ::filter(fold != i) %>%
      select(-fold)
    # Create a prediction vector.
    cl_predict = data_w_cl %>% dplyr:: filter(fold == i) %>%
      select(-fold)
    # Run KNN.
    # Coerce type to work in the KNN function.
    cl = as_vector(cl)
    cl_predict = as_vector(cl_predict)
    knn_iter <-  knn(data_train,
                     data_test,
                     cl = cl,
                     k = k_nn)
    # Calculate the correctly classified
    knn_test_eq = knn_iter == cl_predict
    # Calculate the number of TRUEs
    num_corr_class = sum(knn_test_eq)
    # Calculate the correct classification rate
    pct_corr_class = num_corr_class / length(knn_iter)
    # Calculate the missclassification
    pct_miss_class = 1-pct_corr_class
    # Store these in the list
    knn_corr_list[[i]] <- pct_corr_class
    knn_list[[paste("pred_class",i,sep = "_")]] <- knn_iter
    knn_error_list[[paste("pct_miss_class",i,sep = "_")]] <- pct_miss_class
  }
  # Calculate return values.
  # Calculate the mean missclassification
  # calculate the prediction for KNN
  # Predict the final classification.
  knn_prediction = knn(train_full,
                       train_full,
                       cl = cl_full, k = k_nn)
  ## Returns a KNN predictd off of all the test data.
  # Calculate CV Error.
  cv_err = mean(unlist(knn_error_list))
  # Create an output list
  list_out = list()
  # Assign the values to a list.
  list_out[["class"]] <- knn_prediction
  list_out[["cv_err"]] <- cv_err
  return(list_out)
  # Returns a list of the models and their predictions
  # Returns the cv error
}
