#' Make Basic Flextable
#' 
#' @description Make a flextable with preferred formatting
#' @param x The data.frame to use for flextable
#' @param title The string to use as the table title
#' @param col_names A character vector of preferred column names for flextable. 
#' Length of character vector must be equal to the number of columns. Defaults 
#' to NULL, in which case the column names of x are used in the flextable.
#' @param hlines The row numbers to draw horizontal lines beneath. Defaults to 
#' "all", can be "all", "none", or a numeric vector.
#' @return A flextable object with the argued formatting
make_ft = function(x, 
                   title = NULL, 
                   col_names = NULL, 
                   hlines = "all") {
  # Create reused values
  brdr <- fp_border(color = "black", width = 1)
  numcols <- ncol(x)
  numrows <- nrow(x)
  
  # set_header_labels takes a named vector
  if (!is.null(col_names)) names(col_names) <- colnames(x)
  
  # Initiate basic flextable 
  ft <- x %>%
    flextable() %>%
    set_header_labels(values = col_names) %>%
    vline(j = 1:(numcols-1), border = brdr, part = "body") %>%
    align(align = "center", part = "header") %>%
    bold(part = "header")

  
  # Add horizontal lines
  if (is.numeric(hlines)) {
    ft <- hline(ft, i = hlines, border = brdr, part = "body")
  } else if (hlines == "all") {
    ft <- hline(ft, border = brdr, part = "body")
  } else if (hlines == "none") {
  } else {
    stop("hlines argument must be 'all', 'none', or a numeric vector")
  }
  
  # add title
  if (!is.null(title)) {
    ft <- ft %>%
      add_header_row(top = TRUE, values = title, colwidths = numcols) %>%
      fontsize(i = 1, size = 24, part = "header") %>%
      padding(padding = 20, i = 1, part = "header")
  }
  ft <- border(ft, i = numrows, border.bottom = fp_border(color = "black", 
                                                          width = 2))
  return(ft)
}
