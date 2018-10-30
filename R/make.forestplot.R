#' Function for pretty forestplots, adapted from Ryan
#'
#' @param model.slot the position of the thing you're comparing to within the model summary results
#' @param numfits The number of models you're looking at
#' @param model_names The names for your models in a vector of characters
#' @param fit1 The first model
#' @param fit2 The second model (and so on and so forth.  you need to put in four fits.)
#' @param inc The increments for the xticks
#'
#' @export
#'
#' @return A nice forestplot
#'
#' @examples
#' model_names <- c("coxph(survival) ~ mtDNA-CN", "Model 1 + Age, Sex, Race", "Model 2 + Hypertension, Diabetes, Smoking", 'Model 3 + BMI, SBP, Troponin, AF history')
#' make.forestplot(model_names = model_names, fit1 = fit1, fit2 = fit2, fit3 = fit3, fit4 = fit4)


make.forestplot <- function(model.slot = 4, numfits = 4, model_names, fit1, fit2, fit3, fit4, inc = 0.1){
  require(forestplot)
  
  HR.comparison <- structure(list(
    mean = c(NA, signif(fit1$coef[model.slot,2], digits=3), signif(fit2$coef[model.slot,2], digits=3), signif(fit3$coef[model.slot,2], digits=3), signif(fit4$coef[model.slot,2], digits=3)), 
    lower = c(NA, signif(fit1$conf.int[model.slot,"lower .95"], 3), signif(fit2$conf.int[model.slot,"lower .95"], 3), signif(fit3$conf.int[model.slot,"lower .95"], 3), signif(fit4$conf.int[model.slot,"lower .95"], 3)), 
    upper = c(NA, signif(fit1$conf.int[model.slot,"upper .95"], 3), signif(fit2$conf.int[model.slot,"upper .95"], 3), signif(fit3$conf.int[model.slot,"upper .95"], 3), signif(fit4$conf.int[model.slot,"upper .95"], 3))), .Names = c("mean","lower","upper"), 
    row.names = c(NA, -5L), class="data.frame")
  
  names_of_models <- as.list(c("Model", model_names))
  model_numbers <- as.list(c('Model #', 1:numfits))
  hazard_ratios <- as.list(c('HR', signif(HR.comparison[-1,1], 3)))
  
  # this may need some customization
  p_values <- as.list(c('p-value', signif(fit1$coef[model.slot,5], 4), signif(fit2$coef[model.slot,5], 4), signif(fit3$coef[model.slot,5], 4), signif(fit4$coef[model.slot,5], 4)))
  
  # sometimes this list doesn't work --> check the website that ryan sent you (1 out of 3 ways to do this)
  tabletext <- list(model_numbers,
                    names_of_models,
                    hazard_ratios,
                    p_values)
  
  min <- round(min(na.omit(HR.comparison$lower))-0.05, digits = 1)
  max <- round(max(na.omit(HR.comparison$upper))+ 0.05, digits = 1)
  tick_places <- seq(min, max, by = inc)
  forestplot(tabletext,
             txt_gp = fpTxtGp(cex = 1, ticks = gpar(cex = 1)),
             hrzl_lines = list("2" = gpar(lty=2, lwd=2), "6" = gpar(lwd=2, columns=1:5)),
             HR.comparison, new_page = TRUE, align = "l",
             xlog=FALSE, zero = 1, clip = c(min, max), xticks = tick_places,
             lwd.zero = 3, lwd.axis = 3, lwd.ci = 2.5, graphwidth = unit(4,"in"), colgap = unit(10,"mm"),
             col = fpColors(box = "cadetblue",line = "cadetblue", hrz_lines = "black"))
}
