context("Get CLIVAR Data")

test_that("getCruises returns all cruises", {
  #as of March 2018
  cruises <- getCruises()
  expect_type(cruises, "character")
  expect_length(cruises, 65)
} )

test_that("getStations returns all stations", {
  #test using A16S
  stations <- getStations("A16S")
  expect_length(stations, 59)
})
