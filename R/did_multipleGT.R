




# Program 1
did_multiplegt_core <- function(data){
  
}


#' Title
#'
#' @param data 
#' @param variable_list 
#' @param RECAT_treatment 
#' @param THRESHOLD_stable_treatment 
#' @param trends_nonparam 
#' @param trends_lin 
#' @param controls 
#' @param weight 
#' @param placebo 
#' @param dynamic 
#' @param breps 
#' @param cluster 
#' @param covariances 
#'
#' @return
#' @export
#'
#' @examples
did_multipleGT_check <- function(data,
                                 variable_list,
                                 RECAT_treatment = NULL,
                                 THRESHOLD_stable_treatment = 0,
                                 trends_nonparam = NULL,
                                 trends_lin = NULL,
                                 controls,
                                 weight = NULL,
                                 placebo = 0,
                                 dynamic = 0,
                                 breps = 0,
                                 cluster = NULL,
                                 covariances = FALSE){
  # First 10 lines in the stata program drops missing and produces a subsample
  # if `if` defined.
  #
  # We probably don't want to drop data/produce subsamples since this
  # isn't common in R?
  data <- as.data.frame(data) # Since tibble naming a bit weird
  # Creating the Y, G, T, D variables
  # TODO: Just pass these as args directly instead of a list?
  data$outcome_XX <- data[, variable_list[1]]
  data$group_XX <-  data[,variable_list[2]]
  data$time_XX <- with(data, ave(rep(1, nrow(data)), variable_list[3], FUN = seq_along))
  data$treatment_XX <- data[, variable_list[4]]
  
  # When the weight option is specified, the data has to be at the (g,t) level.
  n_by_group <- aggregate(formula = outcome_XX ~ group_XX + time_XX, data = data, FUN = length)
  if (max(n_by_group$outcome_XX) > 1 & !is.null(weight)) {
    stop("You have specified the weight option but your data is not aggregated 
          at the group*time level, the command cannot run, aggregate your data
          at the group*time level before running it.")
  }
  # Counting time periods and checking at least two time periods
  if (length(unique(na.omit(data$time_XX))) < 2) {
    stop("There are less than two time periods in the data, the command cannot run.")
  }
  
  # Creating a discretized treatment even if recat_treatment option not specified
  if (!is.null(RECAT_treatment)) {
    data$D_cat_XX <- data[, "RECAT_treatment"]
  } else {
    data$D_cat_XX <- data[, "treatment_XX"]
  }
  #  Creating groups of recategorized treatment, to ensure we have an ordered 
  # treatment with interval of 1 between consecutive values
  data$d_cat_group_XX <- with(data, ave(rep(1, nrow(data)), D_cat_XX, FUN = seq_along))
  if (length(unique(data$d_cat_group_XX)) == 1) {
    stop("Either the treatment variable or the recategorized treatment in the 
         recat_treatment option takes only one value, the command cannot run.")
  }
  
  # Checking that the number in threshold_stable_treatment is positive
  if (THRESHOLD_stable_treatment < 0) {
    stop("The number in the threshold_stable_treatment option should be greater than or equal to 0.")
  }
  
  # Checking that the trends_nonparam and trends_lin options have not been jointly specified
  if (!is.null(trends_nonparam) & !is.null(trends_lin)) {
    stop("The trends_nonparam and trends_lin options cannot be specified at the same time.")
  }
  
  # Checking that number of placebos requested is admissible
  if (placebo > (max(data$time_XX) - 2)) {
    stop("The number of placebo estimates you have requested is too large:
         it should be at most equal to the number of time periods in your data minus 2.")
  }
  # Checking that number of dynamic effects requested is admissible
  if (dynamic > (max(data$time_XX) - 2)) {
    stop("The number of dynamic effects you have requested is too large:
         it should be at most equal to the number of time periods in your data minus 2.")
  }
  # Checking that number of bootstrap replications requested greater than 2
  if (breps == 1) {
    stop("The number of bootstrap replications should be equal to 0, or greater than 1.")
  }
  
  if (!is.logical(covariances)) {
    stop("Covariances must be of type logical - TRUE returns covariances and FALSE
         does not.")
  }
  return(data)
}

did_multipleGT_results <- function(){
  
}