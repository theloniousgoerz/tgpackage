# Tests for my_lm.
test_that("My_lm works as expected", {
  expect_equal(my_lm(pop ~ lifeExp,data = gapminder::gapminder)[1,1],
               as.numeric(lm(pop ~ lifeExp,data = gapminder::gapminder)$coefficients[1]))
}
)

test_that("My_lm throws an error", {
  expect_error(my_lm(gapminder$pop~gapminder$country))
}
)
