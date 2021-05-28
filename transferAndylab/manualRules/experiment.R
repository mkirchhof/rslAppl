# Get the ID that SLURM hands over to the script as argument
folds <- as.integer(Sys.getenv("PBS_ARRAYID"))
if(is.na(folds)){
  folds <- 1
}
cat("ID = ", folds, "\n")

ruleSet <- 1:3
rProb <- c(0.6, 0.9)
todo <- expand.grid(ruleSet, rProb)
ruleSet <- todo[folds, 1]
rProb <- todo[folds, 2]


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

  cat("Predicting on val...\n")
  browser()
  predValMarg <- predict(rsl, val, showProgress = TRUE, method = "approximate", cluster = cl)
  predValMarg <- predValMarg[, unlist(sapply(rsl$labels[colnames(valActual)], "[[", "names"))]
  logLValMarg <- median(.labelwiseLogLikelihood(predValMarg, valActual))
  predValMargCrisp <- .probabilisticToCrispData(rsl, predValMarg, tieBreak = "random")
  accValMarg <- accuracy(predValMargCrisp, valActual)
  hamValMarg <- hammingLoss(predValMargCrisp, valActual)
  # likValMarg <- .avgLogLikelihood(rsl, val, valActual, cluster = cl)
  likValMarg <- NA
  # try({predValJoint <- predict(rsl, val, type = "joint", method = "exact", cluster = cl)
  # predValJointCrisp <- .probabilisticToCrispData(rsl, predValJoint)
  # accValJoint <- accuracy(predValJointCrisp, valActual)})
  f1Val <- f1(predValMargCrisp, valActual)
  
  # cat("Predicting on test...\n")
  # predTime <- microbenchmark(predTestMarg <- predict(rsl, test, cluster = cl), times = 1)$time / nrow(test)
  # predTestMarg <- predTestMarg[, unlist(sapply(rsl$labels[colnames(testActual)], "[[", "names"))]
  # logLTestMarg <- median(.labelwiseLogLikelihood(predTestMarg, testActual))
  # predTestMargCrisp <- .probabilisticToCrispData(rsl, predTestMarg)
  # accTestMarg <- accuracy(predTestMargCrisp, testActual)
  # hamTestMarg <- hammingLoss(predTestMargCrisp, testActual)
  # # likTestMarg <- .avgLogLikelihood(rsl, test, testActual)
  # likTestMarg <- NA
  # # try({predTestJoint <- predict(rsl, test, type = "joint", method = "approximate", cluster = cl)
  # # predTestJointCrisp <- .probabilisticToCrispData(rsl, predTestJoint)
  # # accTestJoint <- accuracy(predTestJointCrisp, testActual)})
  # f1Test <- f1(predTestMargCrisp, testActual)
  
  return(list(#accTrain = accTrainMarg, hamTrain = hamTrainMarg, logLikTrain = likTrainMarg, labelwiseLogLTrain = logLTrainMarg, accTrainMPE = accTrainJoint,
              accVal = accValMarg, hamVal = hamValMarg, logLikVal = likValMarg, labelwiseLogLVal = logLValMarg, predValMarg = predValMarg, f1Val = f1Val))#,
              # accTest = accTestMarg, hamTest = hamTestMarg, logLikTest = likTestMarg, labelwiseLogLTest = logLTestMarg, predTestMarg = predTestMarg, f1Test = f1Test,
              # avgPredTime = predTime))
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
rsl <- .buildRSL(data$labels, list())
# Change colnames of xyzActual stuff
colnames(data$trainActual) <- .getAllLabelNodes(rsl)
colnames(data$valActual) <- .getAllLabelNodes(rsl)
colnames(data$testActual) <- .getAllLabelNodes(rsl)
classes1 <- c("st", "wa", "kn", "cr", "si", "ly")
classes2 <- c("u", "bf", "bs", "os", "oh")
classes3 <- c("re", "pi", "pl", "rl", "ca", "fm", "sc", "id")
rsl <- addClassifier(rsl, "generalPosture", classes1, accuracy = 1)
rsl <- addClassifier(rsl, "detailedPosture", classes2, accuracy = 1)
rsl <- addClassifier(rsl, "currentAction", classes3, accuracy = 1)
data$trainActual$L8 <- NA_character_
data$trainActual$L9 <- NA_character_
data$trainActual$L10 <- NA_character_

# Add rules
rules1 <- c("wa <- gait",
           "st,kn,cr <- stand",
           "st <- step",
           "u <- center",
           "bf, bs <- down",
           "os, oh <- up",
           "u <- noMotion",
           "re, pi, pl, rl, ca, fm, sc <- rightHand_1",
           "re, pi, pl, rl, ca, fm, sc <- leftHand_1",
           "id <- noHand_1",
           "re, pi, pl, rl, ca <- bulky",
           "re, pi, pl, rl, ca <- handy")
rules2 <- c("stand, step <- st",
            "gait, step <- wa",
            "stand <- kn",
            "stand <- cr",
            "center, noMotion <- u",
            "down <- bf",
            "down <- bs",
            "up <- os",
            "up <- oh",
            "bulky, handy, utility, cart <- re",
            "bulky, handy, utility, cart <- pi",
            "bulky, handy, utility, cart <- pl",
            "bulky, handy, utility, cart <- rl",
            "bulky, handy, utility, cart <- ca",
            "utility, cart <- fm",
            "utility, cart <- sc",
            "noHand_1 <- id",
            "rightHand_1, leftHand_1 <- re",
            "rightHand_1, leftHand_1 <- pi",
            "rightHand_1, leftHand_1 <- pl",
            "rightHand_1, leftHand_1 <- rl",
            "rightHand_1, leftHand_1 <- ca")
if(ruleSet == 1){
  rules <- rules1
} else if(ruleSet == 2){
  rules <- rules2
} else if(ruleSet == 3){
  rules <- c(rules1, rules2)
}

for(r in rules){
  cat("Adding rule ", r, "\n")
  rsl <- addRule(rsl, r, prob = rProb)
}

# Add XOR rules for hands
rsl <- addRule(rsl, "rightHand_1, leftHand_1 <- noHand_0", prob = 0.999)
rsl <- addRule(rsl, "noHand_0 <- leftHand_1", prob = 0.999)
rsl <- addRule(rsl, "noHand_0 <- rightHand_1", prob = 0.999)

# evaluate and save
clusterSetRNGStream(cl, iseed=22122020 - 1)
set.seed(22122020 - 1)
res <- .evalPerformance(rsl, data$train, data$trainActual,
                        data$val, data$valActual,
                        data$test, data$testActual, cl = cl)
save(res, file = paste0(folds, "_res.RData"))


# stop cluster
stopCluster(cl)
