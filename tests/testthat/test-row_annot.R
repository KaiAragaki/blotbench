test_that("row_annot is well-formed", {
  wb <- wb(
    c(magick::logo, magick::logo),
    col_annot = data.frame(cond = "drug"),
    row_annot = c("one", "two")
  )
  expect_error(row_annot(wb) <- "one")
  expect_no_error(row_annot(wb) <- c("one", "two"))
  expect_error(row_annot(wb) <- data.frame(wrong_name = c("one", "two")))
  expect_no_error(row_annot(wb) <- data.frame(name = c("one", "two")))
  expect_error(
    row_annot(wb) <- data.frame(name = c("one", "two"), extra_col = "other")
  )
})

test_that("Just row_annot NULL is not ok", {
  expect_no_error(
    wb <- wb(magick::logo, col_annot = NULL, row_annot = NULL)
  )
  expect_error(
    wb <- wb(magick::logo, col_annot = data.frame(cond = "x"), row_annot = NULL)
  )
})
