#' Filters out samples that have deltaCTs with a standard deviation > 0.5
#' Filters out the most deviant replicate first, and then if SD still > 0.5, excludes entire sample

#' @param data.deltaCT a data frame that contains deltaCT information. 
#' @param removed a file path that you would like to write removed samples to
#' @param platename the name of the plate that you are working with
#' 
#' @export
#' 
#' @return Returns a data frame that has removed samples with deltaCT > 0.5 


filterSD <- function(mydata_withPO, data.SD.means, platename) {
  data.by.sample.above=subset(data.SD.means, SD.deltaCT>0.5)
  if (nrow(data.by.sample.above)>0){
    samplelist.above=data.by.sample.above$Sample.Name
    data.deltaCT.above=mydata_withPO[mydata_withPO$Sample.Name %in% samplelist.above, ]
    samples.to.rm=data.frame(matrix(NA,ncol=2))
    names(samples.to.rm)=c("Sample.Name","Well.Position")
    for (i in 1:length(samplelist.above)){
      test=subset(data.deltaCT.above, Sample.Name==samplelist.above[i])
      std.dev=sd(test$deltaCT)
      if(nrow(test) < 3){
        test <- dplyr::select(test, Sample.Name, Well.Position)
        samples.to.rm <- rbind(samples.to.rm, test)
      }else if (std.dev >0.5){
        test$deltaCT=as.numeric(test$deltaCT)
        
        # testing standard deviation of different combos
        assign(test$Well.Position[1],sd(test$deltaCT[2:3])) 
        assign(test$Well.Position[2],sd(test$deltaCT[c(1,3)]))
        assign(test$Well.Position[3],sd(test$deltaCT[1:2]))
        bad.sample=min(as.numeric(mget(test$Well.Position)))
        bad.sample2=test[which(bad.sample==mget(test$Well.Position)),c("Sample.Name","Well.Position")]
        
        # new standard deviation
        test2=test[!(test$Well.Position %in% bad.sample2$Well.Position),]
        std.dev=sd(test2$deltaCT)
        if (std.dev <0.5){
          samples.to.rm=rbind(samples.to.rm, test[which(bad.sample==mget(test$Well.Position)),c( "Sample.Name","Well.Position")])
        } else if (std.dev >0.5){
          samples.to.rm=rbind(samples.to.rm, test[,c("Sample.Name","Well.Position")])
        }
      }
    }
    samples.to.rm=na.omit(samples.to.rm)
    print(samples.to.rm)
 #   write.table(samples.to.rm, file = paste0(removed, 'deltaCT_SD_0.5_plate_', platename), quote=F, row.names=F, sep="\t", col.names=T)
    # removing said samples
    data.deltaCT.PO.clean=mydata_withPO[!(mydata_withPO$Well.Position %in% samples.to.rm$Well.Position),]
  } else if (nrow(data.by.sample.above)==0) {data.deltaCT.PO.clean=mydata_withPO}
  return(data.deltaCT.PO.clean)
}
