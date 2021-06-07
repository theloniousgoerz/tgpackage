# Test that the my KNN_cv works
test_that("My_knn_cv returns an object of type list", {
  expect_type(my_knn_cv(train = gapminder::gapminder[,3:4],
                      cl = gapminder::gapminder[5],
                      k_nn = 5,
                      k_cv = 5), type = "list")
}
)

test_that("My_knn_cv returns an error", {
  expect_error(my_knn_cv(train = gapminder::gapminder[,3:4],
                         cl = rep("Country", nrow(gapminder)),
                         k_nn = 5,
                         k_cv = 5))
}
)


