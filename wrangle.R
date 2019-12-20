
#' Read data from csv files
#'
#' @param data_files list of file paths
#'
#' @return list of dataframes
#'
read_dat <- function(data_files) {
  
  data_names <- data_files %>% basename() %>% file_path_sans_ext()
  
  dat <- lapply(data_files, read.csv)
  
  names(dat) <- data_names
  
  return(dat)
  
}


