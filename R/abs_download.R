# function to download NSMHW data from ABS website

download_abs <- function() {
  source <- "https://www.abs.gov.au/statistics/health/mental-health/national-study-mental-health-and-wellbeing/2020-21/Mental-health-tables-1-14.zip"
  temp <- tempfile()
  download.file(source, temp)
  zip::unzip(temp, exdir = "source/abs-downloads")
  
  file_paths <- list.files("source/abs-downloads", pattern = "\\.xlsx$", full.names = TRUE)

  return(file_paths)
}