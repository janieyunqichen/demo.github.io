# function to download AIHW data

download_aihw <- function(link, data_file_name){
  folder_path <- "source/aihw-downloads"
  
  # Create the directory if it doesn't exist
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
  }
  file_path <- file.path(folder_path, data_file_name)
  download.file(link, destfile = file_path, mode = "wb", quiet = FALSE)

  return(file_path)
}