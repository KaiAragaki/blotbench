test_that("Just col_annot NULL is not ok", {
  expect_no_error(
    wb <- wb(magick::logo, col_annot = NULL, row_annot = NULL)
  )
  expect_error(
    wb <- wb(magick::logo, col_annot = NULL, row_annot = c("actin"))
  )
  expect_error({
    wb <- wb(
      magick::logo,
      col_annot = data.frame(drug = "x"),
      row_annot = c("actin")
    )
    col_annot(wb) <- NULL
  })
})
