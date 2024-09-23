# For downloading source csv data
download_rogs <- function(link, data_file_name){
  folder_path <- "source/rogs-downloads"
  
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
  }
  file_path <- file.path(folder_path, data_file_name)
  download.file(link, destfile = file_path, mode = "wb", quiet = FALSE)
  
  return(file_path)
}