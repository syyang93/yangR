#' Function that will allow for (apply) to be used for linear regressions 
#' 
#' This function will do a single linear regression and extract the coefficients from the regression.
#' 
#' @param lm_data Our dependent variables.  Column = dependent variable, rows = observations (in this case, individuals) (numeric vector)
#' @param count Our independent variable (numeric vector)
#' @param covariates Covariates associated with each individual. Columns = covariates, rows = observations. (data.frame)
#' @param to_correct A string containing additional covariates to correct for.  ex: '+as.factor(covariates$DTHHRDY)+as.factor(covariates$RACE)' 
#' @return Coefficients from the regression

lm_test <- function(lm_data, count, covariates = '', correct_for = ''){
  count <- as.numeric(count)
  formula <- as.formula(paste0('lm_data~count', correct_for))
  lm_MT <- lm(formula, na.action=na.exclude)
  care <- coef(summary(lm_MT))["count",]
}