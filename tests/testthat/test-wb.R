test_that("indexing works", {
  wb <- wb(
    c(magick::logo, magick::logo),
    col_annot = data.frame(drug = c("x", "y")),
    row_annot = c("one", "two")
  )
  expect_error(wb[, 3])
  expect_error(wb[3, ])
  expect_error(wb[, c(-1, 2)])
  expect_error(wb[c(-1, 2), ])
  expect_no_error(wb[-1, ])
  expect_no_error(wb[c(2, 2, 1, 2), c(1)])
})
