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

  # Clean up the temporary file after the test.
  file.remove(dest_path)
})
