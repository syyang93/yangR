#' Adjusts for pipetting order
#' 	data.deltaCT.PO.clean <- getRanks(mydata_deltaCT)


addPOadjust <-function(data.deltaCT.PO.clean) {
  PO2.effect=summary(lm(deltaCT ~ as.factor(PO), data=data.deltaCT.PO.clean))$coefficients["as.factor(PO)2","Estimate"]
  PO3.effect=summary(lm(deltaCT ~ as.factor(PO), data=data.deltaCT.PO.clean))$coefficients["as.factor(PO)3","Estimate"]
  
  deltaCT.POadjusted=rep(NA,nrow(data.deltaCT.PO.clean))
  for (l in 1:nrow(data.deltaCT.PO.clean)){
    test=data.deltaCT.PO.clean[l,]
    if (test$PO=="2"){
      deltaCT.POadjusted[l]=test$deltaCT - PO2.effect
    } else if (test$PO=="3") {
      deltaCT.POadjusted[l]=test$deltaCT - PO3.effect
    } else if (test$PO=="1"){
      deltaCT.POadjusted[l]=test$deltaCT
    }
  }
  data.deltaCT.PO.clean$deltaCT.POadjusted=deltaCT.POadjusted
  return(data.deltaCT.PO.clean)
}		
