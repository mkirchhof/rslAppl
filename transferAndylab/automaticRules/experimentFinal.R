# Get the ID that SLURM hands over to the script as argument
folds <- as.integer(Sys.getenv("PBS_ARRAYID"))
if(is.na(folds)){
  folds <- 1
}
cat("ID = ", folds, "\n")

nRules <- 90

# Dependencies:
library("parallel")
source("rsl.R")
library(microbenchmark)
# And the data.RData have to be located in the directory


f1 <- function(pred, actual){
  res <- matrix(NA, ncol = 17, nrow = 3)
  colnames(res) <- c("gait", "step", "stand", "up", "center", "down", "noMotion", "torsoRotation",
                     "rightHand", "leftHand", "noHand", "bulky", "handy", "utility", "cart",
                     "computer", "noItem")
  rownames(res) <- c("precision", "recall", "f1")
  
  res["precision", "gait"] <- sum((actual$L1 == "gait" & pred$L1 == "gait")) / 
    sum(pred$L1 == "gait")
  res["precision", "step"] <- sum((actual$L1 == "step" & pred$L1 == "step")) / 
    sum(pred$L1 == "step")
  res["precision", "stand"] <- sum((actual$L1 == "stand" & pred$L1 == "stand")) / 
    sum(pred$L1 == "stand")
  res["precision", "up"] <- sum((actual$L2 == "up" & pred$L2 == "up")) / 
    sum(pred$L2 == "up")
  res["precision", "center"] <- sum((actual$L2 == "center" & pred$L2 == "center")) / 
    sum(pred$L2 == "center")
  res["precision", "down"] <- sum((actual$L2 == "down" & pred$L2 == "down")) / 
    sum(pred$L2 == "down")
  res["precision", "noMotion"] <- sum((actual$L2 == "noMotion" & pred$L2 == "noMotion")) / 
    sum(pred$L2 == "noMotion")
  res["precision", "torsoRotation"] <- sum((actual$L3 == "torsoRotation_1" & pred$L3 == "torsoRotation_1")) / 
    sum(pred$L3 == "torsoRotation_1")
  res["precision", "rightHand"] <- sum((actual$L4 == "rightHand_1" & pred$L4 == "rightHand_1")) / 
    sum(pred$L4 == "rightHand_1")
  res["precision", "leftHand"] <- sum((actual$L5 == "leftHand_1" & pred$L5 == "leftHand_1")) / 
    sum(pred$L5 == "leftHand_1")
  res["precision", "noHand"] <- sum((actual$L6 == "noHand_1" & pred$L6 == "noHand_1")) / 
    sum(pred$L6 == "noHand_1")
  res["precision", "bulky"] <- sum((actual$L7 == "bulky" & pred$L7 == "bulky")) / 
    sum(pred$L7 == "bulky")
  res["precision", "handy"] <- sum((actual$L7 == "handy" & pred$L7 == "handy")) / 
    sum(pred$L7 == "handy")
  res["precision", "utility"] <- sum((actual$L7 == "utility" & pred$L7 == "utility")) / 
    sum(pred$L7 == "utility")
  res["precision", "cart"] <- sum((actual$L7 == "cart" & pred$L7 == "cart")) / 
    sum(pred$L7 == "cart")
  res["precision", "computer"] <- sum((actual$L7 == "computer" & pred$L7 == "computer")) / 
    sum(pred$L7 == "computer")
  res["precision", "noItem"] <- sum((actual$L7 == "noItem" & pred$L7 == "noItem")) / 
    sum(pred$L7 == "noItem")
  
  res["recall", "gait"] <- sum((actual$L1 == "gait" & pred$L1 == "gait")) / 
    sum(actual$L1 == "gait")
  res["recall", "step"] <- sum((actual$L1 == "step" & pred$L1 == "step")) / 
    sum(actual$L1 == "step")
  res["recall", "stand"] <- sum((actual$L1 == "stand" & pred$L1 == "stand")) / 
    sum(actual$L1 == "stand")
  res["recall", "up"] <- sum((actual$L2 == "up" & pred$L2 == "up")) / 
    sum(actual$L2 == "up")
  res["recall", "center"] <- sum((actual$L2 == "center" & pred$L2 == "center")) / 
    sum(actual$L2 == "center")
  res["recall", "down"] <- sum((actual$L2 == "down" & pred$L2 == "down")) / 
    sum(actual$L2 == "down")
  res["recall", "noMotion"] <- sum((actual$L2 == "noMotion" & pred$L2 == "noMotion")) / 
    sum(actual$L2 == "noMotion")
  res["recall", "torsoRotation"] <- sum((actual$L3 == "torsoRotation_1" & pred$L3 == "torsoRotation_1")) / 
    sum(actual$L3 == "torsoRotation_1")
  res["recall", "rightHand"] <- sum((actual$L4 == "rightHand_1" & pred$L4 == "rightHand_1")) / 
    sum(actual$L4 == "rightHand_1")
  res["recall", "leftHand"] <- sum((actual$L5 == "leftHand_1" & pred$L5 == "leftHand_1")) / 
    sum(actual$L5 == "leftHand_1")
  res["recall", "noHand"] <- sum((actual$L6 == "noHand_1" & pred$L6 == "noHand_1")) / 
    sum(actual$L6 == "noHand_1")
  res["recall", "bulky"] <- sum((actual$L7 == "bulky" & pred$L7 == "bulky")) / 
    sum(actual$L7 == "bulky")
  res["recall", "handy"] <- sum((actual$L7 == "handy" & pred$L7 == "handy")) / 
    sum(actual$L7 == "handy")
  res["recall", "utility"] <- sum((actual$L7 == "utility" & pred$L7 == "utility")) / 
    sum(actual$L7 == "utility")
  res["recall", "cart"] <- sum((actual$L7 == "cart" & pred$L7 == "cart")) / 
    sum(actual$L7 == "cart")
  res["recall", "computer"] <- sum((actual$L7 == "computer" & pred$L7 == "computer")) / 
    sum(actual$L7 == "computer")
  res["recall", "noItem"] <- sum((actual$L7 == "noItem" & pred$L7 == "noItem")) / 
    sum(actual$L7 == "noItem")
  
  res["f1", ] <- 2 * (res["precision", ] * res["recall", ]) / (res["precision", ] + res["recall", ])
  
  return(res)
}


# evalPerformance - evaluates hamming loss, accuracy and log-likelihood on 
#                   test, validation and train data and the time it took per 
#                   sample for a prediction on the test dataset
.evalPerformance <- function(rsl, train, trainActual, val, valActual, test, testActual, cl = NULL){
  # cat("Predicting on train...\n")
  # predTrainMarg <- predict(rsl, train)
  # logLTrainMarg <- median(.labelwiseLogLikelihood(predTrainMarg, trainActual))
  # predTrainMarg <- .probabilisticToCrispData(rsl, predTrainMarg)
  # accTrainMarg <- accuracy(predTrainMarg, trainActual)
  # hamTrainMarg <- hammingLoss(predTrainMarg, trainActual)
  # # likTrainMarg <- .avgLogLikelihood(rsl, train, trainActual)
  # likTrainMarg <- NA
  # # cat("Predicting train2...\n")
  # try({predTrainJoint <- predict(rsl, train, type = "joint", method = "approximate")
  # predTrainJoint <- .probabilisticToCrispData(rsl, predTrainJoint)
  # accTrainJoint <- accuracy(predTrainJoint, trainActual)})
  
  # cat("Predicting on val...\n")
  # predValMarg <- predict(rsl, val, method = "approximate", cluster = cl)
  # logLValMarg <- median(.labelwiseLogLikelihood(predValMarg, valActual))
  # predValMargCrisp <- .probabilisticToCrispData(rsl, predValMarg, tieBreak = "random")
  # predValMargCrisp <- predValMargCrisp[, -c(8, 9, 10)]
  # accValMarg <- accuracy(predValMargCrisp, valActual)
  # hamValMarg <- hammingLoss(predValMargCrisp, valActual)
  # # likValMarg <- .avgLogLikelihood(rsl, val, valActual, cluster = cl)
  # likValMarg <- NA
  # # try({predValJoint <- predict(rsl, val, type = "joint", method = "exact", cluster = cl)
  # # predValJointCrisp <- .probabilisticToCrispData(rsl, predValJoint)
  # # accValJoint <- accuracy(predValJointCrisp, valActual)})
  # colnames(predValMargCrisp) <- colnames(valActual)
  # f1Val <- f1(predValMargCrisp, valActual)
  
  cat("Predicting on test...\n")
  predTime <- microbenchmark(predTestMarg <- predict(rsl, test, cluster = cl, method = "approximate"), times = 1)$time / nrow(test)
  logLTestMarg <- median(.labelwiseLogLikelihood(predTestMarg, testActual))
  predTestMargCrisp <- .probabilisticToCrispData(rsl, predTestMarg, tieBreak = "random")
  predTestMargCrisp <- predTestMargCrisp[, -c(8, 9, 10)]
  accTestMarg <- accuracy(predTestMargCrisp, testActual)
  hamTestMarg <- hammingLoss(predTestMargCrisp, testActual)
  # likTestMarg <- .avgLogLikelihood(rsl, test, testActual)
  likTestMarg <- NA
  # try({predTestJoint <- predict(rsl, test, type = "joint", method = "approximate", cluster = cl)
  # predTestJointCrisp <- .probabilisticToCrispData(rsl, predTestJoint)
  # accTestJoint <- accuracy(predTestJointCrisp, testActual)})
  colnames(predTestMargCrisp) <- colnames(testActual)
  f1Test <- f1(predTestMargCrisp, testActual)
  
  return(list(#accTrain = accTrainMarg, hamTrain = hamTrainMarg, logLikTrain = likTrainMarg, labelwiseLogLTrain = logLTrainMarg, accTrainMPE = accTrainJoint,
              #accVal = accValMarg, hamVal = hamValMarg, logLikVal = likValMarg, labelwiseLogLVal = logLValMarg, predValMarg = predValMarg, f1Val = f1Val))
              accTest = accTestMarg, hamTest = hamTestMarg, logLikTest = likTestMarg, labelwiseLogLTest = logLTestMarg, predTestMarg = predTestMarg, f1Test = f1Test,
              avgPredTime = predTime))
}


# .buildRSL - builds an rsl given a ruleset and a labelset
.buildRSL <- function(labels, rules){
  rsl <- createRSL()
  for(i in seq(along = labels)){
    rsl <- addClassifier(rsl, names(labels)[i], labels[[i]], accuracy = 1)
  }
  for(i in seq(along = rules)){
    probs <- rules[[i]]$p
    names(probs) <- rules[[i]]$labels
    rsl <- .addNoisyOR(rsl, probs)
  }
  
  return(rsl)
}


# Initialize cluster
ntasks <- 10
cl <- makeCluster(ntasks)
clusterSetRNGStream(cl, iseed=22122020 - 1)
set.seed(22122020 - 1)
clusterEvalQ(cl, source("rsl.R"))


# load data
load("../../lara/data/data.RData")
load("9_rsl.RData")
# Change colnames of xyzActual stuff
colnames(data$trainActual) <- .getAllLabelNodes(rsl)[1:7]
colnames(data$valActual) <- .getAllLabelNodes(rsl)[1:7]
colnames(data$testActual) <- .getAllLabelNodes(rsl)[1:7]
classes1 <- c("st", "wa", "kn", "cr", "si", "ly")
classes2 <- c("u", "bf", "bs", "os", "oh")
classes3 <- c("re", "pi", "pl", "rl", "ca", "fm", "sc", "id")
data$trainActual$L8 <- NA_character_
data$trainActual$L9 <- NA_character_
data$trainActual$L10 <- NA_character_


# evaluate and save
clusterSetRNGStream(cl, iseed=22122020 - 1)
set.seed(22122020 - 1)
res <- .evalPerformance(rsl, data$train, data$trainActual,
                        data$val, data$valActual,
                        data$test, data$testActual, cl = cl)
save(res, file = "res.RData")


# stop cluster
stopCluster(cl)
