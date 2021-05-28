# Get the ID that SLURM hands over to the script as argument
folds <- as.integer(Sys.getenv("PBS_ARRAYID"))
if(is.na(folds)){
  folds <- 1
}
cat("ID = ", folds, "\n")

sportsRules = 1:6
logisticsRules = 1:6
prob = 1:2
todo <- expand.grid(sportsRules, logisticsRules, prob)
set.seed(123)
todo <- todo[sample(nrow(todo)), ]
sportRuleType <- todo[folds, 1]
logRuleType  <- todo[folds, 2]
probType <- todo[folds, 3]
rProb <- c(0.6, 0.9)[probType]


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
load("../../LaraIMU/data/data.RData")
rsl <- .buildRSL(data$labels, list())
rsl <- addClassifier(rsl, "activity", colnames(data$train), accuracy = 1)

# Add latent variable nodes
attrTable <- read.csv("../../realdispIMU/attributeTable.csv", stringsAsFactors = FALSE, row.names = 1)
logTable <- read.csv("../../realdispIMU/logisticsTable.csv", stringsAsFactors = FALSE, row.names = 1)
for(i in seq(ncol(logTable))){
  logTable[, i][is.na(logTable[, i])] <- ""
}
for(i in seq(ncol(attrTable))){
  rsl <- addLabels(rsl, paste0(colnames(attrTable)[i], "_", c(0, 1)))
  rsl <- addClassifier(rsl, i, paste0(colnames(attrTable)[i], "_", c(0, 1)), accuracy = 1)
}
for(i in seq(ncol(data$valActual))){
  newName <- .classifierIDtoLabelID(rsl, .classifierToID(rsl, colnames(data$valActual)[i]))
  colnames(data$trainActual)[i] <- newName
  colnames(data$valActual)[i] <- newName
  colnames(data$testActual)[i] <- newName
}

# ruleVariant1 - Construct rules of form C1 <- A1, !A2 from attribute-class table x
ruleVariant1 <- function(x){
  rules <- c()
  for(i in seq(nrow(x))){
    eligible <- x[i, ] != ""
    rule <- paste(rownames(x)[i], "<-", paste(paste0(colnames(x)[eligible], "_", x[i, ][eligible]), collapse = ","))
    rules <- c(rules, rule)
  }
  
  return(rules)
}

# ruleVariant2 - Construct rules of form C1 <- A1, A3 from attribute-class table x
ruleVariant2 <- function(x){
  rules <- c()
  for(i in seq(nrow(x))){
    if(any(x[i, ] == "1")){
      rule <- paste(rownames(x)[i], "<-", paste(paste0(colnames(x)[x[i, ] == "1"], "_", 1), collapse = ","))
      rules <- c(rules, rule)
    }
  }
  
  return(rules)
}

# ruleVariant3 - Construct rules of form C1, C2, ... <- A1 from attribute-class table x
ruleVariant3 <- function(x){
  rules <- c()
  for(i in seq(ncol(x))){
    if(any(x[, i] == "1")){
      rule <- paste0(paste0(rownames(x)[x[, i] == "1"], collapse = ","), "<-", colnames(x)[i], "_1")
      rules <- c(rules, rule)
    }
  }
  
  return(rules)
}

# ruleVariant4 - Construct rules of form C1, C2, ... <- A1 aswell as C4, C5 <- !A1 from attribute-class table x
ruleVariant4 <- function(x){
  rules <- ruleVariant3(x)
  for(i in seq(ncol(x))){
    if(any(x[, i] == "0")){
      rule <- paste0(paste0(rownames(x)[x[, i] == "0"], collapse = ","), "<-", colnames(x)[i], "_0")
      rules <- c(rules, rule)
    }
  }
  
  return(rules)
}

# ruleVariant5 - Construct rules of form A1 <- C1; A2 <- C1 from attribute-class table x
ruleVariant5 <- function(x){
  rules <- c()
  for(i in seq(nrow(x))){
    for(j in seq(ncol(x))){
      if(x[i, j] == "1"){
        rule <- paste0(colnames(x)[j], "_1<-", rownames(x)[i])
        rules <- c(rules, rule)
      }
    }
  }
  
  return(rules)
}

# ruleVariant6 - Construct rules of form A1 <- C1; !A2 <- C1 from attribute-class table x
ruleVariant6 <- function(x){
  rules <- c()
  for(i in seq(nrow(x))){
    for(j in seq(ncol(x))){
      if(x[i, j] != ""){
        rule <- paste0(colnames(x)[j], "_", x[i, j], "<-", rownames(x)[i])
        rules <- c(rules, rule)
      }
    }
  }
  
  return(rules)
}

ruleVariants <- list(ruleVariant1, ruleVariant2, ruleVariant3, ruleVariant4, ruleVariant5, ruleVariant6)

# Add rules
rules <- c(ruleVariants[[sportRuleType]](attrTable), ruleVariants[[logRuleType]](logTable))
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
