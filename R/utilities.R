#' Transforms an input variable to a normal distribution.
#' 
#' \code{normalize} transforms an \code{in_var} to a normal distribution using 
#' ranks. Output is also adjusted in accordance with the arguments passed in using 
#' \code{within_var}, \code{out_sd} and \code{adj_var}. It is, generally, 
#' impossible for the result to meet all these requirements exactly. See
#' comments in the code for details.
#' 
#' @param x data frame, containing all the necessary input data.
#'    
#' @param in_var character vector, the name of the variable to be adjusted.
#'   
#' @param within_var character vector, variables within which we 
#'   will manipulate the distribution of \code{in_var} to be N(0,1).
#'   
#' @param out_sd a list of lists which specify the output standard deviations to
#'   use for different levels of specified variables. The variables here have no
#'   necessary connection with the variables in \code{within_var}.
#'   
#' @param adj_var character vector, the name of the variable(s) to which we 
#'   want \code{in_var} to be neutral. For now, neutral means uncorrelated 
#'   with. In later versions, we hope this will mean no decile/quintile exposure
#'   to.
#'   
#' @param loops numeric, number of iterations to run.
#'   
#' @return a numeric vector equal in length to \code{in_var}.
#'
#' @export
normalize <- function(x, 
                      in_var = NULL, 
                      within_var = NULL, 
                      out_sd = NULL,
                      adj_var = NULL,
                      loops = 3){
  
  # Map to a normal distribution using only rank information. Keep NAs as NAs. 
  # Identical inputs get the same output. Do so for each level of the 
  # within_var, if present. Output standard deviation, for each level of each 
  # within_var, is set to 1 by default. To have something else happen, you must
  # provide a conforming list to out_sd. 
  
  # Always keep in mind that, by *requesting* a given output standard 
  # deviation you are not, in any way, *guaranteeing* that you get that result 
  # because the last thing that the code does is to set the final vector to be 
  # N(0, 1). This comes up a lot in the test cases since it is especially 
  # noticable with extreme inputs and short vectors.
  
  # For intuition, consider the conflict between setting a subset, say FIN 
  # stocks, to standard deviation 0.5 (and setting other sectors to 1) and 
  # then, immediately after, requiring the standard deviation of the entire 
  # vector to be 1. As soon as you do that, you must increase the standard 
  # deviation for FIN to some amount above 0.5 (and the standard deviation of 
  # non-FIN sectors to some number above 1). In other words, you can't really
  # specify output standard deviations. The most you can do is point toward
  # relative standard deviations.
  
  # One way to think about this is that you should not care whether or not the 
  # absolute value of the standard deviation within a certain level is X. You 
  # can't control that. Instead, you should care about the standard deviation
  # for that level, relative to other levels.
  
  # In other words, the most natural uses case is for a variable like, say, 
  # m.sec to appear in both within_var and in out_sd. You want all m.sec values
  # to have a standard deviation of 1, but you want the standard deviation of,
  # say, FIN stocks to be 0.5.
  
  # It is a dicier, but still allowed, option to leave a variable like, say, 
  # m.ind, out of within_var but include it in out_sd. You can *request* that 
  # BIOTC have a standard deviation of 0.3, but you can't guarantee the 
  # relationship of this result relative to the standard deviation for other 
  # levels of m.ind. Of course, it is highly unlikely that this will produce a
  # dramatically "wrong" answer for anything but pathological inputs, but do
  # keep the issue in mind.
  
  # Two cases, x is a vector or x is a data.frame. Vector is easy.
  
  if (is.vector(x)){
    stopifnot(is.null(in_var) && is.null(within_var) && is.null(adj_var))
    return(.norm(x))
  }
  
  # If there is an out_sd, let's validate it.
  
  stopifnot(is.list(out_sd) | is.null(out_sd))
  
  if (is.list(out_sd)){
    for( i in names(out_sd)){              # Go through elements of out_sd
      stopifnot(is.array(out_sd[[i]]))     # Better be an array
      for (j in names(out_sd[[i]])){       # Go through elements of this within_var            
        stopifnot(j %in% unique(x[[i]]))   # Those elements ought to exist in the dataframe. Might          
      }                                    # weaken this last since it is not really necessary . . .         
    }        
  }  
  
  # out_sd needs to look something like this.
  
  # z <- list(country = array(c(2, 2), 
  #   dimnames = list(c("HKG", "AUS"))), 
  #          market = array(c(3), 
  #   dimnames = list(c("developed"))))
  
  # There are few/no changes after one iteration of the loop because, I think, 
  # you are just bouncing back and forth between forcing the overall 
  # distribution back to N(0, 1) and then setting the specified standard 
  # deviations for the out_sd you want. It does seem like, for each within_var,
  # the ratio of the different sds for each level does come through, more or
  # less. But the sd for a level in one within_var is unconnected to the sd for
  # a level in a different within_var. Also, you can't hack things by changing
  # the relative magnitude of sds across different within_vars.
  
  # Summary: you get an overall normal vector. You get the ratio of sds within 
  # a given within_var that you inputted, roughly. But you can't control 
  # absolute magnitudes, as best I can tell. 
  
  # If not a vector, then do some checks. Only keep the variables we need.
  
  stopifnot(is.data.frame(x))
  all_vars <- unique(c(in_var, within_var, names(out_sd), adj_var))
  stopifnot(all(all_vars %in% names(x)))
  stopifnot(is.numeric(x[[in_var]]))
  
  x <- x[all_vars]
  
  # Previous versions allowed you to specify an output standard deviation for 
  # this simple case, but that was too complex to deal with. So, if both 
  # within_var, out_sd and adj_var are NULL, just normalize in_var and return.
  
  if (is.null(within_var) && is.null(out_sd) && is.null(adj_var)) {
    return(.norm(x[[in_var]]))      
  }
  
  # Give in_var a reasonable distribution to start with. Using the "new" hack
  # --- i.e., not replacing in_var in place --- to make debugging easier.
  
  x$new <- .norm(x[[in_var]]) 
  
  # Start the loop. It is not clear if the loop is that useful, but it can 
  # hardly hurt. There are three separate parts to the loop. First, we deal 
  # with variables listed in within_var (which may require some information 
  # from the same variable in out_sd). Second, we deal with variables in out_sd
  # that are not part of within_var. Third, handle variables in adj_var by
  # running adjust().
  
  for (i in seq(loops)) {
    
    # Do a loop for each within_var. This means that what goes first/last is 
    # determined by the order of within_var. Some testing suggests that the
    # order does not matter much.
    
    # Note the importance of normalization. If you don't do that,
    # then the last within_var ends up perfect and other subsets are
    # left wherever they are. But, by forcing the final vector to be
    # normal, it means that order of within_var ends up mattering
    # very little. It would be nice to have a firmer grasp on these
    # dynamics.
    
    # There are two separate cases two consider. First, those variables that 
    # exist in within_var and (sometimes) in out_sd and, second, those 
    # variables that are only in out_sd. We handle the former first. That is, 
    # we first loop through all within_var, both standardizing all the levels 
    # and then adjusting any out_sd. After that is done, we handle the
    # variables that *only* appear in out_sd.
    
    for (j in within_var) {
      
      # First get all the standard deviations for the levels the same at 1.
      # Note that this will screw up any within_var levels that were fixed in
      # previous loops.
      
      x$new <- unsplit(lapply(split(x$new, x[[j]]), .norm), x[[j]])
      
      # Second, check to see if there is an out_sd and, if so, find out if one 
      # of the out_sd applies to this within_var. Might be able to clean this
      # code up.
      
      if(is.list(out_sd)) {
        
        if(j %in% names(out_sd)) {
          
          # If it does, figure out which levels and then just hack out those
          # levels to the desired sd.
          
          for(k in names(out_sd[[j]])) {          
            x$new <- ifelse(x[[j]] %in% k, x$new * out_sd[[j]][[k]], x$new)
          }
        }
      }
    }
    
    x$new <- .norm(x$new)
    
    # Now, second major stage, deal with variables that appear in out_sd but 
    # not in within_var. Trickiest issue is whether to mean-adjust within 
    # levels for these variables. Above, this happens automatically in each 
    # loop, because we are doing something for every level of each within_var. 
    # Here, if we wanted to do it, we would need to subtract out the mean for 
    # both rows in that level and rows not in that level. Something like:
    
    #         x$new <- ifelse(x[[j]] %in% k, 
    #                         x$new - mean(x$new[  x[[j]] %in% k], na.rm = TRUE), 
    #                         x$new - mean(x$new[! x[[j]] %in% k], na.rm = TRUE))
    
    # But it seems like, in most of our use cases, we don't want to do that. For
    # example, we may want to pull in the scores of say, biotech stocks but, if
    # they have a negative average score we want to keep that negative average
    # rather than setting the mean to zero. Since this is the most common use
    # case, we will leave the mean value untouched.
    
    # Keep in mind that the output mean will, probably, be different than the 
    # input mean because of the final step normalization, especially for cases
    # (like test cases) which come in with means very different from zero.
    
    for (j in names(out_sd)[! names(out_sd) %in% within_var]) {
      for (k in names(out_sd[[j]])) {
        
        # We could make the mean zero for k and !k observations, as described
        # above. In fact, we could add this as an option somehow. But for now,
        # we don't bother. We simply adjust the standard deviation of k
        # observations. This leaves the standard deviations of other levels
        # unaffected.
        
        # Note that we're not sure the SD of the group starts out at 1, so we
        # need to calculate the sd.adj.factor.
        
        curr.sd <- sd(x$new[x[[j]] %in% k], na.rm = TRUE)
        sd.adj.factor <- out_sd[[j]][[k]] / curr.sd
        
        
        # If we don't have more than one distinct non-NA value for x$new in the
        # subset, we won't get a valid SD.  Check before making the adjustment.
        
        if (! is.na(sd.adj.factor) & ! is.infinite(sd.adj.factor)) {
          x$new <- ifelse(x[[j]] %in% k, x$new * sd.adj.factor, x$new)
        }
      }
    }
    
    x$new <- .norm(x$new)
    
    
    # Third major stage, dealing with adj_var. Note that we hard-code in the 
    # number of loops at 5. We assume that, if you pass in an adj_var, you want
    # to be both uncorrelated and have no spread exposures, so you definately
    # want to loop. Five seems a reasonable choice and speed is rarely an issue
    # for this function.
    
    if (! is.null(adj_var)) {
      
      # make sure that in any adj_var column, not everything is 0
      for (adj in adj_var) {
        stopifnot(!all(x[[adj]] %in% 0))
      }
      
      x$new <- adjust(x, in_var = "new", adj_var = adj_var, loops = 5)
      x$new <- .norm(x$new)
    }
    
    # End of the loop. Note that, by ending with the variables in adj_var, we
    # give them a special status. That is, we do a better job of "fixing" them
    # then we do of fixing other variables.
    
  }
  
  # sanity check to make sure our iterative process doesn't produce
  # unintended consequences. For all within_vars, the mean has to be
  # [-0.1, 0.1].
  #
  # Also check that the ratio of max sd of a category in within_var
  # and min sd of a category in within_var is < 5.
  
  if (!is.null(within_var)) {
    for (j in within_var) {
      stopifnot(all(abs(unlist(lapply(split(x$new, x[[j]]) , mean, na.rm = TRUE))) < 0.1, na.rm = TRUE),
                max(unlist(lapply(split(x$new, x[[j]]), sd, na.rm = TRUE))) /
                  min(unlist(lapply(split(x$new, x[[j]]), sd, na.rm = TRUE))) < 5
      )
    }
  }
  
  # sanity check to make sure our iterative process doesn't produce
  # unintended consequences. For all adj_vars, if it's a numerical
  # variable, the abs(correlation) is < 0.1; if it's a categorical
  # variable, the abs(mean) < 0.1
  
  if (!is.null(adj_var)) {
    for (j in adj_var) {
      if (class(x[[j]]) == "numeric") {
        stopifnot(abs(cor(x$new, normalize(x[[j]]), use = "p")) < 0.1)
      } else {
        stopifnot(all(abs(unlist(lapply(split(x$new, x[[j]]), mean, na.rm = TRUE))) < 0.1, na.rm = TRUE))
      }
    }
  }
  
  # for all the out_sd variables, make sure that ratio of max sd and
  # min sd is < 5.
  
  if (!is.null(out_sd)) {
    all.out_sd.vars <- names(out_sd)
    
    for (j in all.out_sd.vars) {
      stopifnot(max(unlist(lapply(split(x$new, x[[j]]), sd, na.rm = TRUE))) /
                  min(unlist(lapply(split(x$new, x[[j]]), sd, na.rm = TRUE))) < 5
      )
    }
    
  }
  
  # sanity check on final alpha
  stopifnot(abs(mean(x$new, na.rm = TRUE)) < 0.1,
            sd(x$new, na.rm = TRUE) < 1.1,
            sd(x$new, na.rm = TRUE) > 0.9)
  
  return(x$new)
}


# \code{.norm} transforms \code{x} to a normal distribution
# 
# @param x numeric vector.
# @param out_sd standard deviation of the output. Default is 1.
#   
# @return a numeric vector equal in length to x with a normal distribution of
#   mean 0 and standard deviation \code{out_sd}.
.norm <- function(x, out_sd = 1){
  
  stopifnot(is.vector(x))
  stopifnot(is.numeric(x))
  
  # We've been seeing issues where functions like lm() return results
  # containing very small discrepancies.  For example, residuals(lm(...)) can
  # return a vector where 2 values should be identical, but the first is 1 and
  # the second is 1.000000000000001 (i.e. 1 + 1e-15).  My (limited)
  # understanding of floating point arithmetic leads me to believe this is not
  # an R bug, but rather an unfortunate by-product of standard floating point
  # calculations.  Normally this would not be an issue, but when that vector is
  # later normalized the 2 values can become substantially different.
  
  # Our solution to this is to first put the vector on a standard scale, then
  # round to a fairly tight tolerance, and finally call the rank / qnorm.  This
  # should fix almost all of the cases above while not changing anything else.
  
  x <- x - mean(x, na.rm = TRUE)
  x <- x /   sd(x, na.rm = TRUE)
  x <- round(x, 11)
  
  
  # Tricky issues arise when we have large number of NAs. That is why we must
  # explicitly scale by the number of non-NA values, I think.
  
  # Only a one-liner, but an important one. The use of scale is quite the hack,
  # but I had trouble getting anything more elegant to work.
  
  return(as.vector(scale(qnorm( rank(x, na.last = "keep") / (sum(! is.na(x)) + 1)))) * out_sd)
  
}

#' Transform an input variable to be uncorrelated with other variables.
#' 
#' \code{adjust} makes \code{in_var} neutral (uncorrelated with) the variables 
#' in \code{adj_var}. Complexity arises because we should ensure that all the 
#' variables used have normal distributions. Also, we generally want to be 
#' neutral in two senses: first, uncorrelated with each \code{adj_var} and, 
#' second, no exposure to any \code{adj_var} when looking at decile/quintile 
#' spreads of the output variable. Of course, we would also like the output 
#' variable to be highly correlated with \code{in_var}. We achieve this by 
#' running both equal-weighted and sigmoid-weighted regressions in a loop.
#' 
#' @param x data frame, containing all the necessary input data.
#'   
#' @param in_var character vector, the name of the variable to be 
#'   \code{adjust}ed.
#'   
#' @param adj_var character vector, the name of the variable(s) to be used in 
#'   the \code{adjust}ment process.
#'   
#' @param loops numeric scalar, the number of times to run the pair of 
#'   adjustment regressions.  The default is \code{NULL}, which just runs the 
#'   equal-weighted regression a single time.
#'   
#' @return a numeric vector equal in length to \code{in_var} which is 
#'   uncorrelated with all the variables in \code{adj_var}.
#'
#' @export
adjust <- function(x, in_var, adj_var, loops = NULL){
  
  # Ought to think about the case of lots of missing values in one of the
  # adj_vars. 
  
  # Ought to consider causing the output to have the same standard deviations 
  # within different factor levels of the adj_var. Not sure how this function
  # should connect, if at all, with normalize.
  
  stopifnot(is.character(in_var) && is.character(adj_var))
  stopifnot(length(in_var)  == 1)
  stopifnot(length(adj_var) >= 1)
  
  
  all_var <- c(in_var, adj_var)
  stopifnot(all(all_var %in% names(x)))
  stopifnot(is.numeric(x[[in_var]]))
  
  y <- x[all_var]
  
  # Always wrestle with what to do with non-numeric variables. Might want to 
  # put this logic into normalize. After all, trying to normalize a factor is 
  # probably a mistake. Or, might handle 0/1 variables in a different way; 
  # recall Andrew Gelman's discussion. Or might want to do cooler stuff with
  # factors. But this is good enough for now.
  
  for (i in all_var){
    if (is.numeric(y[[i]])){
      y[i] <- normalize(y[[i]])
    }
  }
  
  # Change character variables to factors to avoid annoying warnings in lm.
  
  for(i in adj_var){
    if(is.character(y[[i]]) || is.logical(y[[i]])){
      y[i] <- as.factor(y[[i]])
    }
  }  
  
  # Now we run regressions, first tail-weighted, then equal-weighted.
  # The tail-weighting is intended to reduce the exposure to the
  # adj_var factors in actual portfolios; this is more important
  # than orthogonality.  Obviously, we could do a lot more magical
  # stuff at this stage.
  
  # The particular weighting function is chosen to:
  #   1) Be non-negative.
  #   2) Be continuous.
  #   3) Be symmetric around 0.
  #   4) Be low around 0 and high in the tails.
  #   5) Have a shallow slope between 0 and 1.
  #   6) Have a steep slope between 1 and 2.
  #   7) Have a shallow slope > 2.
  
  # The steep slope between 1 and 2 is desirable because a stock with
  # a score of 2 is likely to be in a portfolio; a stock with a score
  # of 1 is unlikely.  Differences between scores of 0 and 1 or
  # between 2 and 3 are much less important.
  
  # Given the multi-stage regressions, we loop over the regressions
  # to make it more likely we have converged to a fixed point.  In
  # the old (equal-weighted only) version, this was unnecessary.
  
  # Output is the normalized residuals. Intuition: In general, the in_var will 
  # be a good thing. We want the rows that have more of the good thing than 
  # they "should," at least according to the adj_var. For example, if in_var is
  # book-to-price and adj_var is industry, then we want the stocks that have a
  # higher book-to-price than the industry average.
  
  # Put differently, our initial score (book-to-price) is a mixture
  # of signal and noise.  Our adj_var (industry) is virtually all
  # noise, but is significantly correlated with book-to-price.  By
  # adjusting the score to industry, we get a residual
  # (industry-relative book-to-price) which has a higher signal /
  # noise ratio and is a more potent alpha score.
  
  form <- as.formula(paste(in_var, paste(adj_var, collapse = "+"), sep = "~"))
  
  for (i in seq.int(loops)) {
    wt <- 1 - 1 / (1 + exp(4.8 * (abs(y[[in_var]]) - 1.5)))
    y[[in_var]] <- normalize(residuals(lm(form, data = y, weights = wt, na.action = "na.exclude")))
    y[[in_var]] <- normalize(residuals(lm(form, data = y,               na.action = "na.exclude")))
  }
  
  return(normalize(residuals(lm(form, data = y, na.action = "na.exclude"))))
}