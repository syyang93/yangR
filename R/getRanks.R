#' Performs rank-order analysis on qPCR triplicates.  
#' Interpretation: significant pipetting order effect = 1st replicate pipetted is enriched for largest (max) deltaCT value. And 3rd replicate pipetted is enriched for smallest (min) deltaCT value

#' 
#' @param mydata_deltaCT a data frame that contains deltaCT information. 
#' 
#' @export
#' 
#' @return Returns results of the rank-order analysis
#' 
#' @examples
#' 	ranks <- getRanks(mydata_deltaCT)


getRanks <- function(mydata_deltaCT) {
  rank.deltaCT = data.frame(matrix(NA, ncol = 6))
  names(rank.deltaCT) = c("Sample.Name", "Min.CT", "Mid.CT", 
                          "Max.CT", "CT.Mt_ND1.diff", "CT.RPPH1.diff")
  samplelist = levels(as.factor(mydata_deltaCT$Sample.Name))
  for (i in 1:length(samplelist)) {
    test = subset(mydata_deltaCT, na.omit(Sample.Name == 
                                            samplelist[i]))
    test$Well.Position = as.numeric(gsub("[A-Z]", "", test$Well.Position))
    test.ordered = test[order(test$Well.Position), ]
    test.ordered$deltaCT = as.numeric(test.ordered$deltaCT)
    if (nrow(test) == 3) {
      first = which(test.ordered$deltaCT == min(test.ordered$deltaCT), 
                    arr.ind = T)
      second = which(test.ordered$deltaCT == median(test.ordered$deltaCT), 
                     arr.ind = T)
      third = which(test.ordered$deltaCT == max(test.ordered$deltaCT), 
                    arr.ind = T)
      sample = test.ordered$Sample.Name[1]
      ct.mt.diff = test.ordered$CT.Mt_ND1[1] - test.ordered$CT.Mt_ND1[3]
      ct.rpph1.diff = test.ordered$CT.RPPH1[1] - test.ordered$CT.RPPH1[3]
      rank.deltaCT[i, ] = c(test$Sample.Name[1], first, 
                            second, third, ct.mt.diff, ct.rpph1.diff)
    }
  }
  rank.deltaCT = na.omit(rank.deltaCT)
  rank.deltaCT$CT.Mt_ND1.diff = as.numeric(rank.deltaCT$CT.Mt_ND1.diff)
  rank.deltaCT$CT.RPPH1.diff = as.numeric(rank.deltaCT$CT.RPPH1.diff)
  Min = table(rank.deltaCT$Min.CT)
  Mid = table(rank.deltaCT$Mid.CT)
  Max = table(rank.deltaCT$Max.CT)
  t = rbind(Min, Mid, Max)
  return(t)
}
