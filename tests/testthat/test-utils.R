test_that("utils returns null for no attribute", {
  x <- 2
  attr(x, "an_attribute") <- 3

  expect_equal(attr_default(x, "an_attribute"), 3)
  expect_null(attr_default(x, "DOESNT_EXIST"))
  expect_equal(attr_default(x, "DOESNT_EXIST", default = 6), 6)
})
