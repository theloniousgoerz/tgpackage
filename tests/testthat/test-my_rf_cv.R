# Test for my_rf_cv.
test_that("My_rf_cv works as expected and returns a double.", {
  expect_type(my_rf_cv(5), type = "double")
})

test_that("My_rf_cv throws an error", {
  expect_error(my_rf_cv("five"))
})
