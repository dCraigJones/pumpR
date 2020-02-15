test_that("draw graph has the correct input", {
  expect_error(Draw.Graph(1000,100,500))
  expect_error(Draw.Graph(1000,500,100,250, 50, 100))
})
