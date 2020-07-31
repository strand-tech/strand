# Load all data files under a directory into a data frame
#
# It is expected that files loaded will contain the date in YYYYmmdd format in
# the filename.
load_data_files <- function(in_loc) {
  if (!file.exists(in_loc)) {
    stop(paste0("Location not found: ", in_loc))
  }
  all_files <- list.files(in_loc, full.names = TRUE)
  
  lapply(all_files,
         function(x) {
           this_date <- as.Date(gsub("^.*(\\d{8})[^\\/]*$", "\\1", x), format = "%Y%m%d")
           feather::read_feather(x) %>%
             mutate(date = this_date)
         }) %>% bind_rows
}