load("../../lara/data/data.RData")
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



props <- .proportionTable(data$valActual)

allRes <- matrix(NA, ncol = 18, nrow = 6)
for(i in 1:6){
  load(paste0(i, "_res.RData"))
  allRes[i, 1:17] <- res$f1Val["f1", ] 
  allRes[i,18] <- .weightedF1(res$f1Val["f1", ], props)
}

ruleSet <- 1:3
rProb <- c(0.6, 0.9)
todo <- expand.grid(ruleSet, rProb)

# Fit linear model
todo$y <- allRes[, 18]
todo[,1] <- factor(todo[, 1])
todo[,2] <- factor(todo[, 2])
mod <- lm(log(y) ~ Var1 + Var2, data = todo)
summary(mod)
# Call:
# lm(formula = log(y) ~ Var1 + Var2, data = todo)
# 
# Residuals:
#        1        2        3        4        5        6 
#  0.28043 -0.22798 -0.05245 -0.28043  0.22798  0.05245 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  -1.0772     0.2982  -3.613   0.0688 .
# Var12         0.3538     0.3652   0.969   0.4349  
# Var13         0.3793     0.3652   1.039   0.4081  
# Var20.9      -0.4298     0.2982  -1.442   0.2862  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3652 on 2 degrees of freedom
# Multiple R-squared:  0.6314,	Adjusted R-squared:  0.07852 
# F-statistic: 1.142 on 3 and 2 DF,  p-value: 0.4983
plot(mod)
mod <- lm(y ~ Var1 + Var2, data = todo)
summary(mod)
# Call:
# lm(formula = y ~ Var1 + Var2, data = todo)
# 
# Residuals:
#         1         2         3         4         5         6 
#  0.074337 -0.072475 -0.001862 -0.074337  0.072475  0.001862 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.37645    0.08478   4.440   0.0472 *
# Var12        0.08222    0.10384   0.792   0.5115  
# Var13        0.09762    0.10384   0.940   0.4464  
# Var20.9     -0.13473    0.08478  -1.589   0.2530  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1038 on 2 degrees of freedom
# Multiple R-squared:  0.6395,	Adjusted R-squared:  0.09863 
# F-statistic: 1.182 on 3 and 2 DF,  p-value: 0.4887
plot(mod)
# Nothing is significant, which might be due to the small data.
# (high standard error, 2 degrees of freedom)
# Of the effect sizes, it appears that rule variant 3 and p = 0.6 is the best,
# so we will choose this.

# Final results:
load("res.RData")
rsl <- .buildRSL(data$labels, list())
crisp <- .probabilisticToCrispData(rsl, res$predTestMarg, tieBreak = "random")
apply(crisp, 2, table)
# $L1
# 
#  gait stand  step 
# 30024   916 25588 
# 
# $L2
# 
#   center     down noMotion       up 
#    25060     4510    25488     1470 
# 
# $L3
# 
# torsoRotation_0 torsoRotation_1 
#           28538           27990 
# 
# $L4
# 
# rightHand_0 rightHand_1 
#        5141       51387 
# 
# $L5
# 
# leftHand_0 leftHand_1 
#       5141      51387 
# 
# $L6
# 
# noHand_0 
#    56528 
# 
# $L7
# 
#     cart computer   noItem  utility 
#    14066    14294    14119    14049 

# Does not seem to be randomly sampling.

propsTest <- .proportionTable(data$testActual)
f1Final <- c(res$f1Test["f1", ], .weightedF1(res$f1Test["f1", ], propsTest))
   #       gait          step         stand            up        center          down      noMotion torsoRotation     rightHand      leftHand 
   # 0.57666621    0.41593998    0.07607650    0.31869281    0.53266634    0.53594227    0.34203336    0.01626016    0.93312920    0.84368346 
   #     noHand         bulky         handy       utility          cart      computer        noItem               
   #        NaN           NaN           NaN    0.19450559    0.17710603    0.07447829    0.14200586    0.47977516 
res$hamTest # 0.4348212
res$labelwiseLogLTest # -6.419465
res$accTest # 0.007429946
