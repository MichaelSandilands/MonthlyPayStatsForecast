# Test 1: Verify the download function works.
# This integration test connects to the live website to ensure that the download
# functionality is working correctly. In a data pipeline, a failure here is
# a crucial signal that the data source has changed and the pipeline needs
# to be updated.
test_that("cbi_download downloads and saves a file", {
  # Create a temporary directory to save the downloaded file,
  # ensuring that the test does not affect the user's file system.
  temp_dir <- tempdir()
  dest_path <- file.path(temp_dir, "test_payments.xlsx")

  # Run the function.
  suppressMessages(cbi_download(dest_path = dest_path))

  # Assert that the file was successfully created at the specified path.
  expect_true(file.exists(dest_path))

  df <- read_excel(dest_path,
                   sheet = "Master Data")

  # Check number of columns
  expect_equal(ncol(df), 11)

  # check column names
  expect_equal(
    colnames(df),
    c(
      'Reporting Period', 'Table', 'Category', 'DSI', 'Channel Type',
      'Geographical Description', 'Series Description', 'Sector', 'Sub-sector',
      'Observation Type', 'Observation Value'
    )
  )

  # check column types
  expected_classes <- c(
    "POSIXct",            # Reporting Period
    "POSIXt",             # Reporting Period
    rep("character", 9),  # first 9 are character
    "numeric"             # Observation Value
  )

  actual_classes <- lapply(df, class) %>% unlist() %>% unname()
  expect_equal(actual_classes, expected_classes)

  # Clean up the temporary file after the test.
  file.remove(dest_path)

})
