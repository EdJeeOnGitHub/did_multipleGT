

# Program 1
did_multiplegt <- function(data,
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
                           cluster,
                           covariances = FALSE){

  # Performs sanity checks and renames some variables such as
  # outcome_XX etc.
  check_output <- did_multiplegt_check(data = data,
                                       variable_list = variable_list,
                                       RECAT_treatment = RECAT_treatment,
                                       THRESHOLD_stable_treatment = THRESHOLD_stable_treatment,
                                       trends_nonparam = trends_nonparam,
                                       trends_lin = trends_lin,
                                       controls = controls,
                                       weight = weight,
                                       placebo = placebo,
                                       dynamic = dynamic,
                                       breps = breps,
                                       cluster = cluster,
                                       covariances = covariances
                                       )
  
  
  if (is.null(weight)) {
    # If weight isn't specified collapse data at (g, t) level 
    collapsed_df <- aggregate(formula = cbind(outcome_XX, treatment_XX, d_cat_group_XX) ~ group_XX + time_XX, data = check_output, FUN = mean)
    # Hacky way to get number of obs per group - probably a better way to do this
    n_by_group <- aggregate(formula = outcome_XX  ~ group_XX + time_XX, data = check_output, FUN = length)
    collapsed_df$counter <- n_by_group$outcome_XX
  } else { # if weight is specified just use weight as counter
    collapsed_df <- check_output
    collapsed_df$counter <- check_output[, weight]
  }
  
   # Ordering by group and time
   collapsed_df <- collapsed_df[order(collapsed_df$group_XX, collapsed_df$time_XX), ] 
   # Generating first diffed Y and treatment
   collapsed_df$diff_y_XX <- with(collapsed_df, ave(outcome_XX, group_XX, FUN = function(x){c(NA, diff(x))}))
   collapsed_df$diff_d_XX <- with(collapsed_df, ave(treatment_XX, group_XX, FUN = function(x){c(NA, diff(x))}))
   # Lag D_cat
   collapsed_df$lag_d_cat_group_XX <- with(collapsed_df, ave(d_cat_group_XX, group_XX, FUN = function(x){c(lag(x), NA)}))
   did_multiplegt_core(data = collapsed_df,
                       THRESHOLD_stable_treatment = THRESHOLD_stable_treatment,
                       trends_nonparam = trends_nonparam,
                       trends_lin = trends_lin,
                       controls = controls)
   
   
   
}


did_multiplegt_core <- function(data,
                                THRESHOLD_stable_treatment = 0,
                                trends_nonparam = NULL,
                                trends_lin = NULL,
                                controls,
                                time,
                                group_int,
                                max_time = 0,
                                counter_placebo = 0,
                                counter_dynamic = 0,
                                bootstrap_rep = 0){
  # Looping over time periods
  for (time_t in unique(data$time_XX)) {
    
    D_min <- min(data[data$time_XX == time_t, "lag_d_cat_group_XX"])
    D_max <- max(data[data$time_XX == time_t, "lag_d_cat_group_XX"])
    # Looping over values of lag_D at time t
    for (d in D_min:D_max) {
      
    }
  }
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
did_multiplegt_check <- function(data,
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
  data$time_XX <- factor(data[, variable_list[3]]) # I'm 60% certain this will replicate stata's `group()`
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
  data$d_cat_group_XX <- factor(data[, "D_cat_XX"])
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
  if (placebo > (length(unique(data$time_XX)) - 2)) {
    stop("The number of placebo estimates you have requested is too large:
         it should be at most equal to the number of time periods in your data minus 2.")
  }
  # Checking that number of dynamic effects requested is admissible
  if (dynamic > (length(unique(data$time_XX)) - 2)) {
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

#' @title did_multipleGT_results
#'
#' @description Program #3: Runs and boostraps did_multiplegt_estim
#' @param data
#' @param variable_list
#' @param RECAT_treatment
#' @param THRESHOLD_stable_treatment
#' @param trends_nonparam
#' @param trends_lin
#' @param controls
#' @param counter
#' @param placebo
#' @param dynamic
#' @param breps
#' @param cluster
#' @param covariances
#' @keywords bootstrap
#' @export
#' @examples
#' did_multipleGT_results()
did_multipleGT_results <- function(data,
                                   variable_list,
                                   RECAT_treatment,
                                   THRESHOLD_stable_treatment,
                                   trends_nonparam,
                                   trends_lin,
                                   controls,
                                   counter,
                                   placebo,
                                   dynamic,
                                   breps,
                                   cluster,
                                   covariances){
  # TODO: add clusters in bootstrap_sample
  # TODO: pass arguments to did_multiplegt_estim (maybe same options object for all funcs?)
  if (breps > 0) {
    too_many_controls <- 0
    # TODO: set up df cols below appropriately
    bootstrap_df <- data.frame(iteration = numeric(),
                               effect = numeric())
    for (i in 1:breps) {
      # print iteration
      if (i %% 100 == 0){
        print(paste0("Iteration: ", i, " of ", breps))
      }
      bootstrap_sample <- sample(data, 1L) # psuedocode
      # TODO: sampling without preexisting package
      # could use sample_n(data, nrow(data), replace=TRUE)
      # from dplyr/momocs
      bootstrap_rep <- 1

      did_multiplegt_estim <- did_multiplegt_estim(data,
                                                   variable_list,
                                                   RECAT_treatment,
                                                   THRESHOLD_stable_treatment,
                                                   trends_nonparam,
                                                   trends_lin,
                                                   controls,
                                                   counter,
                                                   placebo,
                                                   dynamic,
                                                   breps,
                                                   cluster,
                                                   covariances)
      bootstrap_df[i, iteration] <- i
    }
    # TODO: put into matrix
  }

  if (too_many_controls == 1) {
    error_message <- paste0(
      "In some bootstrap replications, the command had to run regressions with",
      "\n",
      "more control variables than the sample size, so the controls could not",
      "\n",
      "be accounted for. Typically, this issue only affects a small number of",
      "\n",
      "observations. If you still want to solve this problem, you may reduce",
      "\n",
      "the number of control variables. You may also use the recat_treatment",
      "\n",
      "option to discretize your treatment. Finally, you could reduce the",
      "\n",
      "number of placebos and/or dynamic effects requested."
    )
    stop(error_message)

  # indicate that program will run main estimation
  bootstrap_rep <- 0
  # initialising the too many controls scalar
  too_many_controls <- 0

  # TODO: figure out what's happening here
  # presumably its collecting central estimates
  # (with the above procedure giving SEs)
  # before outputting everything to a table?
  did_multiplegt_estim <- did_multiplegt_estim(data,
                                               variable_list,
                                               RECAT_treatment,
                                               THRESHOLD_stable_treatment,
                                               trends_nonparam,
                                               trends_lin,
                                               controls,
                                               counter,
                                               placebo,
                                               dynamic,
                                               breps,
                                               cluster,
                                               covariances)

  if (too_many_controls == 1) {
    error_message <- paste0(
      "At some point, the command had to run regressions with more control",
      "\n",
      "variables than the sample size, so the controls could not be accounted",
      "\n",
      "for. Typically, this issue only affects a small number of observations.",
      "\n",
      "If you still want to solve this problem, you may reduce the number of",
      "\n",
      "control variables. You may also use the recat_treatment option to",
      "\n",
      "discretize your treatment. Finally, you could reduce the number of",
      "\n",
      "placebos and/or dynamic effects requested."
    )
    stop(error_message)
  }

}
