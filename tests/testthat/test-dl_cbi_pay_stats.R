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

  # File should not exist yet
  expect_false(file.exists(dest_path))

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
      "Reporting Period", "Table", "Category", "DSI", "Channel Type",
      "Geographical Description", "Series Description", "Sector", "Sub-sector",
      "Observation Type", "Observation Value"
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

  # Get the initial file information (including modification time)
  initial_mod_time <- file.info(dest_path)$mtime
  # Run the function again
  suppressMessages(cbi_download(dest_path = dest_path))
  # Get the new file information
  final_mod_time <- file.info(dest_path)$mtime
  # Check that the file's modification time has not changed
  # This confirms that the file was not overwritten.
  expect_equal(initial_mod_time, final_mod_time)

  # Clean up the temporary file after the test.
  file.remove(dest_path)

})

# Test 2: Verify the data cleaning function works as expected.
# This test checks the entire pipeline by first downloading the actual data
# and then applying the cleaning function. This ensures that the data validation
# and cleaning steps are compatible with the real-world data source.
test_that("cbi_read_data cleans the data correctly", {
  # Define the path where the data will be saved.
  master_data_path <- file.path(tempdir(), "test_monthly_card_payments.xlsx")

  # Download the data, making this a full pipeline check.
  cbi_download(dest_path = master_data_path)

  # Skip this test if the file doesn't exist, preventing a false negative.
  skip_if_not(file.exists(master_data_path),
              "Master data file not found, skipping test.")

  # Run the data cleaning function on the downloaded file.
  df_clean <- cbi_read_data(file_path = master_data_path)

  # check column names
  expect_equal(
    colnames(df_clean),
    c(
      "reporting_period", "table", "category", "dsi", "channel_type",
      "geographical_description", "series_description", "sector", "sub_sector",
      "observation_type", "observation_value"
    )
  )

  # Assert that the 'reporting_period' column
  # has been correctly converted to a Date class.
  expect_true(lubridate::is.Date(df_clean$reporting_period))

  # Check that the missing values are present in the 'sector'
  # and 'sub_sector' columns.
  expect_true(sum(is.na(df_clean$sector)) == 0)
  expect_true(sum(is.na(df_clean$sub_sector)) == 0)

  # Check for error when column is missing in a dummy file.
  dummy_df_bad <- tibble(
    `Wrong name` = as.Date(c("2023-01-01", "2023-02-01"))
  )
  temp_file_bad <- tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb = wb, sheetName = "Master Data")
  openxlsx::writeDataTable(
    wb = wb,
    sheet = "Master Data",
    x = dummy_df_bad
  )

  openxlsx::saveWorkbook(wb, temp_file_bad)

  expect_error(cbi_read_data(temp_file_bad),
    str_c(
      "Unexpected column in data"
    )
  )

  file.remove(temp_file_bad)

  # Clean up the downloaded file.
  file.remove(master_data_path)
})
