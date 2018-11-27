#' Looks at the residuals vs. predicted values for a model 
#' 
#' @param data the data you are trying to do models on
#' @param model the model that you would like to look at
#' 
#' @export
#' 
#' @return returns number of nas
#' 
#' @examples
#' look.resid(no surgery, model = lm(mito_percents ~ sex, data = no_surgery))

look.resid <- function(data, model){
  data$predict <- scale(predict(model))
  data$resids <- scale(resid(model))
  mod <- summary(lm(resids~predict, data = data))
  print(mod)
  ggplot(data, aes(predict, resids)) + geom_point()
}