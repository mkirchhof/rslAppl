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

sportsRules = 1:6
logisticsRules = 1:6
prob = 1:2
todo <- expand.grid(sportsRules, logisticsRules, prob)
set.seed(123)
todo <- todo[sample(nrow(todo)), ]

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
# -0.037964 -0.008460 -0.000269  0.006595  0.043158 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.379065   0.006968  54.402  < 2e-16 ***
# Var12       -0.002096   0.006968  -0.301   0.7646    
# Var13       -0.004508   0.006968  -0.647   0.5201    
# Var14        0.003518   0.006968   0.505   0.6155    
# Var15        0.009477   0.006968   1.360   0.1789    
# Var16       -0.300465   0.006968 -43.122  < 2e-16 ***
# Var22       -0.014376   0.006968  -2.063   0.0434 *  
# Var23        0.079278   0.006968  11.378  < 2e-16 ***
# Var24        0.051104   0.006968   7.334 6.72e-10 ***
# Var25        0.012544   0.006968   1.800   0.0769 .  
# Var26       -0.001368   0.006968  -0.196   0.8450    
# Var32        0.002644   0.004023   0.657   0.5135    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.01707 on 60 degrees of freedom
# Multiple R-squared:  0.9827,	Adjusted R-squared:  0.9795 
# F-statistic: 309.2 on 11 and 60 DF,  p-value: < 2.2e-16
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
load("rsl.RData")
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
res$hamTest # 0.4477543
res$labelwiseLogLTest # -6.203204
res$accTest # 0.00751708

# Comparison to the non-informative case
load("resEmpty.RData")
f1Final <- c(res$f1Test["f1", ], .weightedF1(res$f1Test["f1", ], propsTest))
#          gait          step         stand            up        center          down      noMotion 
#    0.27879152    0.37817828    0.32164634    0.13516813    0.35291288    0.08477649    0.23858929 
# torsoRotation     rightHand      leftHand        noHand         bulky         handy       utility 
#    0.01566165    0.63073249    0.58277346    0.23575913    0.19312398    0.22125493    0.16106984 
#          cart      computer        noItem               
#    0.14116129    0.06710176    0.13099248    0.37924112 
res$hamTest # 0.6068227
res$labelwiseLogLTest # -7.049255
res$accTest # 0.0007992845
