context("Plot CLIVAR data")

test_that("plotSection plots a section", {
  #test using A16S
  stations <- plotSection("A16S")
  expect_type(stations, "list")
})
