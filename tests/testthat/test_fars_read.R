context("Test the function that reads in data from a file")

test_that(" the function returns an error if the file does not exist",{

  expect_error(fars_read("bogus_filename.csv"))

})

