load("../../lara/data/data.RData")

.proportionTable <- function(actual){
  allProps <- c()
  for(i in seq(ncol(actual))){
    for(j in seq(along = data$labels[[i]])){
      allProps <- c(allProps, mean(actual[, i] == data$labels[[i]][j]))
    }
  }
  names(allProps) <- unlist(data$labels)
  
  allProps <- allProps[!names(allProps) %in% c("torsoRotation_0", "rightHand_0", "leftHand_0", "noHand_0")]
  
  return(allProps)
}

.weightedF1 <- function(f1, props){
  f1[is.nan(f1)] <- 0
  return(sum(props * f1) / sum(props))
}

props <- .proportionTable(data$valActual)

allRes <- matrix(NA, ncol = 18, nrow = 6)
for(i in 1:6){
  load(paste0(i, "_res.RData"))
  allRes[i, 1:17] <- res$f1Val["f1", ] 
  allRes[i,18] <- .weightedF1(res$f1Val["f1", ], props)
}
# These results are exactly the same as before
for(i in 1:6){
  load(paste0(i, "_res.RData"))
  res2 <- res
  load(paste0("../manualRules/", i, "_res.RData"))
  cat("i: ", mean(res$predValMarg == res2$predValMarg), " are the same and ",
      mean(abs(res$predValMarg - res2$predValMarg)[res$predValMarg != res2$predValMarg]),
      " is the mean absolute difference for those that are not.\n")
}
# i:  1  are the same and  NaN  is the mean absolute difference for those that are not.
# i:  1  are the same and  NaN  is the mean absolute difference for those that are not.
# i:  1  are the same and  NaN  is the mean absolute difference for those that are not.
# i:  1  are the same and  NaN  is the mean absolute difference for those that are not.
# i:  1  are the same and  NaN  is the mean absolute difference for those that are not.
# i:  0.9592487  are the same and  7.532885e-07  is the mean absolute difference for those that are not.
