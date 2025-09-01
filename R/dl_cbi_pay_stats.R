#' Download Monthly Card Payment Statistics
#'
#' This function downloads the latest monthly card payment statistics
#' from the Central Bank of Ireland website. It will only download a new file
#' if the content has changed since the last download.
#'
#' @param dest_path The full path where the Excel file should be saved,
#'   including the file name (e.g., "data/my_payments.xlsx").
#' @return The function downloads the file and returns the file path invisibly.
#' @export
#' @importFrom rvest read_html html_elements html_attr
#' @importFrom stringr str_c str_detect
#' @importFrom readxl read_excel
#' @importFrom utils download.file
cbi_download <- function(dest_path = "data/raw_monthly_card_payments.xlsx") {

  # The specific URL that contains the download links
  data_page_url <- "https://www.centralbank.ie/statistics/data-and-analysis/monthly-card-payment-statistics"
  base_url <- "https://www.centralbank.ie"

  # Find the link to the latest Excel file on the specific data page
  links <- rvest::read_html(data_page_url) %>%
    rvest::html_elements(".xls") %>%
    rvest::html_attr("href")

  monthly_card_pay_stats_url <- stringr::str_c(
    base_url,
    links[!stringr::str_detect(links, "discontinued")],
    sep = ""
  )

  # Check to make sure we found exactly one link
  if (length(monthly_card_pay_stats_url) != 1) {
    stop("Could not find a unique monthly card payments URL.")
  }

  # Download the file to a temporary location
  temp_path <- tempfile(fileext = ".xlsx")
  download.file(monthly_card_pay_stats_url, destfile = temp_path, quiet = TRUE)

  # Create the destination directory if it doesn't exist
  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)

  # Check if the new file is identical to the existing one
  if (file.exists(dest_path) &&
      identical(readxl::read_excel(dest_path, sheet = "Master Data"),
                readxl::read_excel(temp_path, sheet = "Master Data"))) {
    file.remove(temp_path)
    message("No new data found. Using existing file.")
  } else {
    # If a new file is found, save it to the specified destination
    file.copy(temp_path, dest_path, overwrite = TRUE)
    file.remove(temp_path) # Now, remove the original temp file
    message("New data downloaded and saved to: ", dest_path)
  }

  return(invisible(dest_path))
}
