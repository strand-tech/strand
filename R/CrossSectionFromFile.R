#' CrossSectionFromFile class
#'
#' @description Class for providing access to cross-sectional data from binary
#'   feather files.
#'
#' @details The \code{CrossSectionFromFile} class provides an interface to
#'   cross-sectional data that is contained in binary feather files.
#'
#' @export
CrossSectionFromFile <-  R6Class(
  "CrossSectionFromFile",
  inherit = CrossSection,
  
  private = list(
    input_dir = NULL,
    input_prefix = NULL
  ),
  
  public = list(
    
    #' @description Create a new \code{CrossSectionFromFile} object.
    #' @param carry_forward Boolean that indicates whether data in the object
    #'   should be carried forward from one period to the next.
    #' @param input_dir Filesystem (directory) location of input data files.
    #' @param input_prefix String prefix for the files that should be loaded
    #'   into the cross-section.
    #' @return A new \code{CrossSectionFromFile} object.
    initialize = function(carry_forward, input_dir, input_prefix) {
      super$initialize(carry_forward)
      private$input_dir <- input_dir
      private$input_prefix <- input_prefix
      invisible(self)
    },
    
    #' @description Get raw data for a given date.
    #' @param date Date for which we want data.
    #' @return A data frame of data for \code{date}.
    getRaw = function(date) {
      read_feather(paste0(private$input_dir, "/", private$input_prefix, "_",
                          format(date, "%Y%m%d"), ".feather"))
    },
   
    #' @description Print the cross-section object.
    print = function() {
      cat("CrossSection:\n",
          "input_dir = ", private$input_dir, "\n",
          "input_prefix = ", private$input_prefix, "\n", sep = "")
    }
  
  ))
