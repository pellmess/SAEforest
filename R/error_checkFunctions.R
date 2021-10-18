# THROWIN A CLASS ERROR FOR PRINT SUMMARY AND PLOT OPTIONS

 class_error <- function(object){
  if(!inherits(object, "SAEforest")){
    stop("object has to be created by the SAEforest package for SAEforest methods to work.")
  }
}


# Function called in SAEforest_mean
input_checks_mean <- function(Y, X, dName, survey_data, census_data, initialRandomEffects,
                              ErrorTolerance, MaxIterations, mse, B, importance) {
  if (!is.numeric(Y) || sum(Y == survey_data) != dim(survey_data)[1]) {
    stop("Y must be a metric vector containing the target variable. Additionally Y must be contained
         in the data frame of survey sample data. See also help(SAEforest_mean)")
  }

  if (sum(!(X %in% survey_data)) != 0) {
    stop("X specifies the explanatory variabels from the sample data set and must be contaiend in in
       the survey sample set. See also help(SAEforest_mean)")
  }

  if (!is.data.frame(survey_data) || !is.data.frame(census_data)) {
    stop("survey_data must be a data frame containing the survey sample data. census_data must be a data frame
    containing the population data. See also help(SAEforest_mean).")
  }

  if (!is.character(dName) || length(dName) != 1) {
    stop("dName must be an input of class character determining the variable name of the domain of interest provided in the
    survey as well as the population data. See also help(SAEforest_mean).")
  }

  if (dim(census_data)[1] < dim(survey_data)[1]) {
    stop("The population data set cannot have less observations than the
         sample data set.")
  }

  if (is.null(survey_data[[dName]]) || is.null(census_data[[dName]])) {
    stop(paste('The survey sample data and the population data must contain information on domains. Both data frames
    must contain a column labelled by the same name specified under the input of "dName"'))
  }

  if (!all(unique(as.character(survey_data[[dName]])) %in%
    unique(as.character(census_data[[dName]])))) {
    stop("The survey sample data contains domains that are
         not contained in the population data.")
  }

  if (!is.numeric(initialRandomEffects) || (length(initialRandomEffects) != 1 && length(initialRandomEffects) != length(Y))) {
    stop(paste("initialRandomEffects specify initional values of random effects for the MERF. Acceptable inputs are
         single values such as the default of 0 or numeric vectors of length: ", length(Y)))
  }

  if (!is.numeric(MaxIterations) || length(MaxIterations) != 1 || MaxIterations < 2) {
    stop("MaxIterations needs to be a single integer value, determining
          the number of maximum iterations for the convergence of the MERF algorithm. The value must be at least
          2. See also help(MERFranger).")
  }

  if (!is.numeric(ErrorTolerance) || length(ErrorTolerance) != 1 || ErrorTolerance <= 0) {
    stop("ErrorTolerance needs to be a single integer value, determining
          the tolerance to monitor the convergence of the MERF algorithm. The value must be greater than 0. See also help(MERFranger).")
  }

  if (is.null(mse) || !(mse == "none" || mse == "analytic" || mse == "nonparametric")) {
    stop("The options for mse are ''none'', ''analytic'', ''nonparametric'' ")
  }

  if (mse != "none" && !(is.numeric(B) && length(B) == 1 && B > 1)) {
    stop("If MSE-estimation is specified, B needs to be a single integer value, determining
          the number of MSE-bootstrap replications. The value must be larger than 1. See also help(SAEforest_mean).")
  }

  if (is.null(importance) || !(importance == "none" || importance == "impurity" || importance == "impurity_corrected" || importance == "permutation")) {
    stop('Variable importance is needed for vip plots. To reduce runtime importance can be set to "none". Furhter options are "impurity", "impurity_corrected" or "permutation".
         See details on the variable importance mode with help(ranger).')
  }
}


# Function called in SAEforest_nonLin
input_checks_nonLin <- function(Y, X, dName, survey_data, census_data, initialRandomEffects,
                                ErrorTolerance, MaxIterations, mse, B, threshold, importance) {
  if (!is.numeric(Y) || sum(Y == survey_data) != dim(survey_data)[1]) {
    stop("Y must be a metric vector containing the target variable. Additionally Y must be contained
         in the data frame of survey sample data. See also help(SAEforest_nonLin)")
  }

  if (sum(!(X %in% survey_data)) != 0) {
    stop("X specifies the explanatory variabels from the sample data set and must be contaiend in in the
         survey sample set. See also help(SAEforest_nonLin)")
  }

  if (!is.data.frame(survey_data) || !is.data.frame(census_data)) {
    stop("survey_data must be a data frame containing the survey sample data. census_data must be a data frame
    containing the population data. See also help(SAEforest_nonLin).")
  }

  if (!is.character(dName) || length(dName) != 1) {
    stop("dName must be an input of class character determining the variable name of the domain of interest provided in the
    survey as well as the population data. See also help(SAEforest_nonLin).")
  }

  if (dim(census_data)[1] < dim(survey_data)[1]) {
    stop("The population data set cannot have less observations than the
         sample data set.")
  }

  if (is.null(survey_data[[dName]]) || is.null(census_data[[dName]])) {
    stop(paste('The survey sample data and the population data must contain information on domains. Both data frames
    must contain a column labelled by the same name specified under the input of "dName"'))
  }

  if (!all(unique(as.character(survey_data[[dName]])) %in%
    unique(as.character(census_data[[dName]])))) {
    stop("The survey sample data contains domains that are
         not contained in the population data.")
  }

  if (!is.numeric(initialRandomEffects) || (length(initialRandomEffects) != 1 && length(initialRandomEffects) != length(Y))) {
    stop(paste("initialRandomEffects specify initional values of random effects for the MERF. Acceptable inputs are
         single values such as the default of 0 or numeric vectors of length: ", length(Y)))
  }

  if (!is.numeric(MaxIterations) || length(MaxIterations) != 1 || MaxIterations < 2) {
    stop("MaxIterations needs to be a single integer value, determining
          the number of maximum iterations for the convergence of the MERF algorithm. The value must be at least
          2. See also help(MERFranger).")
  }

  if (!is.numeric(ErrorTolerance) || length(ErrorTolerance) != 1 || ErrorTolerance <= 0) {
    stop("ErrorTolerance needs to be a single integer value, determining
          the tolerance to monitor the convergence of the MERF algorithm. The value must be greater than 0. See also help(MERFranger).")
  }

  if (is.null(mse) || !(mse == "none" || mse == "wild" || mse == "nonparametric")) {
    stop("The options for mse are ''none'', ''nonparametric'' and ''wild''.")
  }

  if (mse != "none" && !(is.numeric(B) && length(B) == 1 && B > 1)) {
    stop("If MSE-estimation is specified, B needs to be a single integer value, determining
          the number of MSE-bootstrap replications. The value must be larger than 1. See also help(SAEforest_nonLin).")
  }

  if (!is.null(threshold) && !(is.numeric(threshold) && length(threshold) == 1)) {
    stop("threshold needs to be a single numeric value or a function of y.
          If it is NULL 60% of the median of Y is selected as threshold.
         See also help(SAEforest_nonLin).")
  }

  if (is.null(importance) || !(importance == "none" || importance == "impurity" || importance == "impurity_corrected" || importance == "permutation")) {
    stop('Variable importance is needed for vip plots. To reduce runtime importance can be set to "none". Furhter options are "impurity", "impurity_corrected" or "permutation".
         See details on the variable importance mode with help(ranger).')
  }


  # MIGHT BE USEFUL IF ALLOWING FOR custom indicators.

  #    if (!is.null(custom_indicator)) {

  #    if (!inherits(custom_indicator, "list")) {
  #      stop("Additional indicators need to be added in argument custom_indicator
  #           as a list of functions. For help see Example 2 in help(ebp).")
  #    }

  #    N_custom <- length(custom_indicator)
  #    for (i in seq_len(N_custom)) {
  #      if (!inherits(custom_indicator[[i]], "function")) {
  #        stop("The elements of the list need to be functions. These Functions
  #             for custom indicators need to have exactly the following
  #             two arguments: y, threshold; even though a threshold might not
  #             included in the indicator. For help see Example 2 in help(ebp).")
  #      }
  #      else if (inherits(custom_indicator[[i]], "function")
  #               && !all(names(formals(custom_indicator[[i]])) == c("y", "threshold"))) {
  #        stop("Functions for custom indicators need to have exactly the following
  #             two arguments: y, threshold; even though a threshold might not
  #             included in the indicator. For help see Example 2 in help(ebp).")
  #      }
  #    }
  #  }
  #  }
}


# Function called in SAEforest_nonLin
input_checks_meanAGG <- function(Y, X, dName, survey_data, Xcensus_agg, initialRandomEffects,
                                 ErrorTolerance, MaxIterations, mse, B, popnsize, OOsample_obs,
                                 ADDsamp_obs, w_min, importance) {
  if (!is.numeric(Y) || sum(Y == survey_data) != dim(survey_data)[1]) {
    stop("Y must be a metric vector containing the target variable. Additionally Y must be contained
         in the data frame of survey sample data. See also help(SAEforest_meanAGG)")
  }

  if (sum(!(X %in% survey_data)) != 0) {
    stop("X specifies the explanatory variabels from the sample data set and must be contaiend in in the
         survey sample set. See also help(SAEforest_meanAGG)")
  }

  if (!is.data.frame(survey_data) || !is.data.frame(Xcensus_agg)) {
    stop("survey_data must be a data frame containing the survey sample data. Xcensus_agg must be a data frame
    containing the population data. See also help(SAEforest_meanAGG).")
  }

  if (!is.character(dName) || length(dName) != 1) {
    stop("dName must be an input of class character determining the variable name of the domain of interest provided in the
    survey as well as the population data. See also help(SAEforest_meanAGG).")
  }


  if (is.null(survey_data[[dName]]) || is.null(census_data[[dName]])) {
    stop(paste('The survey sample data and the population data must contain information on domains. Both data frames
    must contain a column labelled by the same name specified under the input of "dName"'))
  }

  if (!all(unique(as.character(survey_data[[dName]])) %in%
    unique(as.character(Xcensus_agg[[dName]])))) {
    stop("The survey sample data contains domains that are
         not contained in the population data.")
  }

  if (!is.data.frame(popnsize) || is.null(popnsize[[dName]]) || dim(popnsize)[1] != dim(Xcensus_agg)[1] ||
    dim(popnsize)[2] > 2 || !is.numeric(popnsize[, !colnames(popnsize) %in% dName])) {
    stop(paste("popnsize must be a data frame with two columns: One column named ", dName, "must contain the domains specifiers.
         The other column must contain information on the population size of domains. See also help(SAEforest_meanAGG)."))
  }

  if (!is.numeric(initialRandomEffects) || (length(initialRandomEffects) != 1 && length(initialRandomEffects) != length(Y))) {
    stop(paste("initialRandomEffects specify initional values of random effects for the MERF. Acceptable inputs are
         single values such as the default of 0 or numeric vectors of length: ", length(Y)))
  }

  if (!is.numeric(MaxIterations) || length(MaxIterations) != 1 || MaxIterations < 2) {
    stop("MaxIterations needs to be a single integer value, determining
          the number of maximum iterations for the convergence of the MERF algorithm. The value must be at least
          2. See also help(MERFranger).")
  }

  if (!is.numeric(ErrorTolerance) || length(ErrorTolerance) != 1 || ErrorTolerance <= 0) {
    stop("ErrorTolerance needs to be a single integer value, determining
          the tolerance to monitor the convergence of the MERF algorithm. The value must be greater than 0. See also help(MERFranger).")
  }

  if (!is.numeric(OOsample_obs) || length(OOsample_obs) != 1 || OOsample_obs < 0) {
    stop('OOsample_obs needs to be a single integer value, determining
          the amount of observations sampled from the "closest" area for out-of-sample areas.
         See also help(SAEforest_meanAGG).')
  }

  if (!is.numeric(ADDsamp_obs) || length(ADDsamp_obs) != 1 || ADDsamp_obs < 0) {
    stop('ADDsamp_obs needs to be a single integer value, determining
          the amount of observations sampled from the "closest" area in the case of failure of calculation
          of calibration weights. See also help(SAEforest_meanAGG).')
  }

  if (!is.numeric(w_min) || length(w_min) != 1 || w_min < 2 || w_min > dim(Xcensus_agg)[2]) {
    stop("w_min needs to be a single integer value, determining
          the minimum amount of covariates incorporating auxilliary information for the assessment of
          calibration weights. Thus, w_min must be smaller or equal to the number of existing covariates.
         See also help(SAEforest_meanAGG).")
  }

  if (is.null(mse) || !(mse == "none" || mse == "nonparametric")) {
    stop("The options for mse are ''none'' or ''nonparametric''.")
  }

  if (mse != "none" && !(is.numeric(B) && length(B) == 1 && B > 1)) {
    stop("If MSE-estimation is specified, B needs to be a single integer value, determining
          the number of MSE-bootstrap replications. The value must be larger than 1. See also help(SAEforest_meanAGG).")
  }

  if (is.null(importance) || !(importance == "impurity" || importance == "impurity_corrected" || importance == "permutation")) {
    stop('The calculation of calibration weights requires a concept of variable importance.
         The options are passed to the MERF and must be "impurity", "impurity_corrected" or "permutation".
         See details on the variable importance mode with help(ranger). See details on the caluclation of calibration weights help(SAEforest_meanAGG).')
  }
}
