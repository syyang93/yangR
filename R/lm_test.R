#' Function that will allow for (apply) to be used for linear regressions 
#' 
#' This function will do a single linear regression and extract the coefficients from the regression.
#' 
#' @param lm_data Our dependent variables.  Column = dependent variable, rows = observations (in this case, individuals) (numeric vector)
#' @param count Our independent variable (numeric vector)
#' @param covariates Covariates associated with each individual. Columns = covariates, rows = observations. (data.frame)
#' @param to_correct A string containing additional covariates to correct for.  ex: '+as.factor(covariates$DTHHRDY)+as.factor(covariates$RACE)' 
#' 
#' @export
#' 
#' @return Coefficients from the regression
#' 
#' @examples
#' for_test <- blood_full[,1:20702] # the transcripts you're testing as your dependent variables
#' covariates <- blood_full[,20703:21001] # the covariates
#' MT_count <- as.numeric(blood_full[,21002]) # the MT_count
#' lm_results <- apply(for_test, 2, lm_test, count = MT_count, covariates = covariates, correct_for = '+as.factor(covariates$DTHHRDY)+as.factor(covariates$RACE)')

lm_test <- function(lm_data, count, covariates = '', correct_for = ''){
  count <- as.numeric(count)
  formula <- as.formula(paste0('lm_data~count', correct_for))
  lm_MT <- lm(formula, na.action=na.exclude)
  care <- coef(summary(lm_MT))["count",]
}