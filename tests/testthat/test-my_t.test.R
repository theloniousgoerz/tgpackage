# Test for my_t.test.
test_that("My_t.test works as intended", {
  expect_equal(my_t.test(x = rep(1:5,lenth.out = 100), alternative = "greater", mu = 3),
               list(Hypothesis = "Alternative Hypoethesis: The true mean is greater than 3",
                    "T-Statistic" = "The t-statistic is: 0",
                    "DF" = "Degrees of Freedom: 4",
                    "P Value" = "P-Value: 0.5"))
  expect_equal(my_t.test(x = rep(1:5,lenth.out = 100), alternative = "less", mu = 3),
               list(Hypothesis = "Alternative Hypoethesis: The true mean is less than 3",
                    "T-Statistic" = "The t-statistic is: 0",
                    "DF" = "Degrees of Freedom: 4",
                    "P Value" = "P-Value: 0.5"))
  expect_equal(my_t.test(x = rep(1:5,lenth.out = 100), alternative = "two.sided", mu = 3),
               list(Hypothesis = "Alternative Hypoethesis: The true mean is two.sided than 3",
                    "T-Statistic" = "The t-statistic is: 0",
                    "DF" = "Degrees of Freedom: 4",
                    "P Value" = "P-Value: 1"))

}
)

test_that( "Non-specificed argument to alternative returns an error", {
  expect_error(my_t.test(x = rep(1:5, length.out = 100), alternative = 4,mu = 5))
  expect_error(my_t.test(x = rep(1:5, length.out = 100), alternative = "more than", mu = 5))
}
)
