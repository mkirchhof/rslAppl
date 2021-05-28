load("../../lara/data/data.RData")
load("res.RData")

highestDiffs <- matrix(NA, ncol = 8, nrow = 100)
highestActivation <- matrix(NA, ncol = 8, nrow = 100)

for(i in 1:8){
  load(paste0(i, "_pred.RData"))
  diffs <- rowSums(abs(pred[, 1:40] - res$predTestMarg))
  highestDiffs[, i] <- names(head(sort(diffs, decreasing = TRUE), 100))
  highestActivation[, i] <- head(order(pred$fulfilled, decreasing = TRUE), 100)
}

write.csv(highestDiffs, "highestDiffs.csv")
write.csv(highestActivation, "highestActivations.csv")
