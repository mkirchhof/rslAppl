load("../../LaraIMU/data/data.RData")
source("rsl.R")

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

allRes <- matrix(NA, ncol = 18, nrow = 72)
for(i in 1:72){
  load(paste0(i, "_res.RData"))
  allRes[i, 1:17] <- res$f1Val["f1", ] 
  allRes[i,18] <- .weightedF1(res$f1Val["f1", ], props)
}

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

sportsRules = 1:6
logisticsRules = 1:6
prob = 1:2
todo <- expand.grid(sportsRules, logisticsRules, prob)
set.seed(123)
todo <- todo[sample(nrow(todo)), ]

isSame <- matrix(NA, ncol = 2, nrow = 72)
for(i in 1:72){
  load(paste0(i, "_res.RData"))
  res2 <- res
  load(paste0("../manualRules/", i, "_res.RData"))
  isSame[i, ] <- c(mean(res$predValMarg == res2$predValMarg), 
                   mean(abs(res$predValMarg - res2$predValMarg)[res$predValMarg != res2$predValMarg]))
}
isSame
#            [,1]       [,2]
#  [1,] 1.0000000        NaN
#  [2,] 1.0000000        NaN
#  [3,] 1.0000000        NaN
#  [4,] 1.0000000        NaN
#  [5,] 0.2857143 0.11632567
#  [6,] 1.0000000        NaN
#  [7,] 1.0000000        NaN
#  [8,] 1.0000000        NaN
#  [9,] 0.2857143 0.09113680
# [10,] 1.0000000        NaN
# [11,] 1.0000000        NaN
# [12,] 1.0000000        NaN
# [13,] 1.0000000        NaN
# [14,] 1.0000000        NaN
# [15,] 1.0000000        NaN
# [16,] 1.0000000        NaN
# [17,] 1.0000000        NaN
# [18,] 1.0000000        NaN
# [19,] 1.0000000        NaN
# [20,] 1.0000000        NaN
# [21,] 1.0000000        NaN
# [22,] 1.0000000        NaN
# [23,] 1.0000000        NaN
# [24,] 1.0000000        NaN
# [25,] 1.0000000        NaN
# [26,] 1.0000000        NaN
# [27,] 0.2857143 0.05188829
# [28,] 1.0000000        NaN
# [29,] 1.0000000        NaN
# [30,] 0.2857143 0.10136995
# [31,] 1.0000000        NaN
# [32,] 0.2857143 0.04657121
# [33,] 1.0000000        NaN
# [34,] 1.0000000        NaN
# [35,] 1.0000000        NaN
# [36,] 0.2857143 0.07978888
# [37,] 1.0000000        NaN
# [38,] 1.0000000        NaN
# [39,] 1.0000000        NaN
# [40,] 1.0000000        NaN
# [41,] 1.0000000        NaN
# [42,] 1.0000000        NaN
# [43,] 0.2857143 0.04766990
# [44,] 1.0000000        NaN
# [45,] 1.0000000        NaN
# [46,] 0.2857143 0.15152528
# [47,] 1.0000000        NaN
# [48,] 1.0000000        NaN
# [49,] 1.0000000        NaN
# [50,] 0.2857143 0.04717665
# [51,] 1.0000000        NaN
# [52,] 0.2857143 0.04728804
# [53,] 1.0000000        NaN
# [54,] 1.0000000        NaN
# [55,] 1.0000000        NaN
# [56,] 1.0000000        NaN
# [57,] 1.0000000        NaN
# [58,] 1.0000000        NaN
# [59,] 1.0000000        NaN
# [60,] 1.0000000        NaN
# [61,] 1.0000000        NaN
# [62,] 1.0000000        NaN
# [63,] 0.2857143 0.05021324
# [64,] 1.0000000        NaN
# [65,] 1.0000000        NaN
# [66,] 1.0000000        NaN
# [67,] 1.0000000        NaN
# [68,] 1.0000000        NaN
# [69,] 1.0000000        NaN
# [70,] 0.2857143 0.16047569
# [71,] 1.0000000        NaN
# [72,] 1.0000000        NaN

# Which ones are different:
todo[isSame[, 1] < 1, ]
#    Var1 Var2 Var3
# 42    6    1    2
# 54    6    3    2
# 36    6    6    1
# 66    6    5    2
# 12    6    2    1
# 48    6    2    2
# 6     6    1    1
# 60    6    4    2
# 18    6    3    1
# 30    6    5    1
# 24    6    4    1
# 72    6    6    2
# Exactly all that have sports-rule-type 6
mean(isSame[, 2], na.rm = TRUE) # 0.08261913

# Are those now data dependent?
rsl <- .buildRSL(data$labels, list())
preds <- list()
for(i in which(isSame[, 1] < 1)){
  load(paste0(i, "_res.RData"))
  crisp <- .probabilisticToCrispData(rsl, res$predValMarg, tieBreak = "random")
  tab <- apply(crisp, 2, table)
  preds <- c(preds, tab)
}
preds


# Fit linear model
todo$y <- allRes[, 18]
todo[,1] <- factor(todo[, 1])
todo[,2] <- factor(todo[, 2])
todo[,3] <- factor(todo[, 3])
mod <- lm(log(y) ~ Var1 + Var2 + Var3, data = todo)
summary(mod)
plot(mod)
mod <- lm(y ~ Var1 + Var2 + Var3, data = todo)
summary(mod)
plot(mod)
# Call:
# lm(formula = y ~ Var1 + Var2 + Var3, data = todo)
# 
# Residuals:
#       Min        1Q    Median        3Q       Max 
# -0.040887 -0.010806  0.000070  0.009278  0.053695 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.380284   0.007913  48.059  < 2e-16 ***
# Var12       -0.002096   0.007913  -0.265   0.7920    
# Var13       -0.004508   0.007913  -0.570   0.5710    
# Var14        0.003518   0.007913   0.445   0.6582    
# Var15        0.009477   0.007913   1.198   0.2357    
# Var16        0.014002   0.007913   1.770   0.0819 .  
# Var22       -0.018630   0.007913  -2.354   0.0218 *  
# Var23        0.071921   0.007913   9.089 6.99e-13 ***
# Var24        0.047748   0.007913   6.034 1.08e-07 ***
# Var25        0.005219   0.007913   0.660   0.5121    
# Var26       -0.001569   0.007913  -0.198   0.8435    
# Var32        0.007704   0.004568   1.686   0.0969 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.01938 on 60 degrees of freedom
# Multiple R-squared:  0.7716,	Adjusted R-squared:  0.7297 
# F-statistic: 18.43 on 11 and 60 DF,  p-value: 2.353e-15
mod <- lm(y ~ Var1 * Var2 + Var3, data = todo)
summary(mod)
plot(mod)
# Seems like rule variant 5 (and 4) works best for sports -> latent and 
# rule variant 3 (and 4) for latent -> logistics. 
# Rule's p does not really matter.
bestIndex <- which(todo$Var1 == 4 & todo$Var2 == 4)
allRes[bestIndex, ]
head(todo[order(todo[,4], decreasing = TRUE), ])
#    Var1 Var2 Var3         y
# 22    4    4    1 0.4768452
# 58    4    4    2 0.4760325
# 53    5    3    2 0.4604443
# 23    5    4    1 0.4594166
# 51    3    3    2 0.4594000
# 14    2    3    1 0.4594000
# We chose rule variant 4 for both rules, and p = 0.6 as it performed best

# Final results:
load("res.RData")
crisp <- .probabilisticToCrispData(rsl, res$predTestMarg, tieBreak = "random")
apply(crisp, 2, table)
# $L1
# 
# stand  step 
# 26353 26194 
# 
# $L2
# 
#   center noMotion 
#    26239    26308 
# 
# $L3
# 
# torsoRotation_0 torsoRotation_1 
#           26308           26239 
# 
# $L4
# 
# rightHand_1 
#       52547 
# 
# $L5
# 
# leftHand_1 
#      52547 
# 
# $L6
# 
# noHand_0 
#    52547 
# 
# $L7
# 
#    bulky     cart computer    handy   noItem  utility 
#     9016     8710     8821     8551     8787     8662

# So what this actually does is it has some constant predictions and some random ones.
# But nothing really data dependent :(
propsTest <- .proportionTable(data$testActual)
f1Final <- c(res$f1Test["f1", ], .weightedF1(res$f1Test["f1", ], propsTest))
#          gait          step         stand            up        center          down      noMotion 
#           NaN    0.47393156    0.38319485           NaN    0.55692990           NaN    0.31317805 
# torsoRotation     rightHand      leftHand        noHand         bulky         handy       utility 
#    0.01517038    0.91486747    0.82201424           NaN    0.19478896    0.22471334    0.16651778 
#          cart      computer        noItem               
#    0.14920325    0.06539208    0.12283959    0.48444102 

# Comparison to the non-informative case
load("resEmpty.RData")
f1Final <- c(res$f1Test["f1", ], .weightedF1(res$f1Test["f1", ], propsTest))
#          gait          step         stand            up        center          down      noMotion 
#    0.27879152    0.37817828    0.32164634    0.13516813    0.35291288    0.08477649    0.23858929 
# torsoRotation     rightHand      leftHand        noHand         bulky         handy       utility 
#    0.01566165    0.63073249    0.58277346    0.23575913    0.19312398    0.22125493    0.16106984 
#          cart      computer        noItem               
#    0.14116129    0.06710176    0.13099248    0.37924112 