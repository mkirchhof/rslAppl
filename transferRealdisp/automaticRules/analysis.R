load("../../LaraIMU/data/data.RData")

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

allRes <- matrix(NA, ncol = 18, nrow = 10)
for(i in 1:8){
  load(paste0(i, "_res.RData"))
  allRes[i, 1:17] <- res$f1Val["f1", ] 
  allRes[i,18] <- .weightedF1(res$f1Val["f1", ], props)
}

dat <- data.frame(x = seq(10, 80, 10), y = allRes[1:8, 18])
lm <- lm(y ~ x + I(x^2), data = dat)
plot(lm) # looks good to me
newdat <- data.frame(x = 10:80)
preds <- predict(lm, newdat)
plot(dat)
lines(x = newdat$x, y = preds)
newdat$x[which.max(preds)] # 54 rules is ideal


# Load final benchmark
load("res.RData")
propsTest <- .proportionTable(data$testActual)
propsTest / sum(propsTest) # The weights for computing f1 on test
#            gait            step           stand              up          center 
    # 0.050890184     0.095399369     0.066330825     0.020551105     0.132155863 
    #        down        noMotion torsoRotation_1     rightHand_1      leftHand_1 
    # 0.010775269     0.049138140     0.001658979     0.179258720     0.148369345 
    #    noHand_1           bulky           handy         utility            cart 
    # 0.032851825     0.048899409     0.074463867     0.032402687     0.027199158 
    #    computer          noItem 
    # 0.009043457     0.020611799 
res$f1wVal <- .weightedF1(res$f1Val["f1", ], props)
res$f1wTest <- .weightedF1(res$f1Test["f1", ], propsTest)
res$f1Test
# $f1Test
#                 gait      step     stand  up    center down   noMotion torsoRotation rightHand  leftHand noHand      bulky     handy utility      cart
# precision 0.50072046 0.4815482 0.4104897 NaN 0.6278256  NaN 0.49120235           NaN 0.8430929 0.6978134    NaN 0.27178030 0.3658233     NaN 0.3550914
# recall    0.05525960 0.3287526 0.8779967   0 0.9838645    0 0.05517128             0 1.0000000 1.0000000      0 0.04749690 0.9114275       0 0.1011604
# f1        0.09953455 0.3907443 0.5594294 NaN 0.7665188  NaN 0.09920047           NaN 0.9148675 0.8220142    NaN 0.08086215 0.5220924     NaN 0.1574621
#           computer    noItem
# precision      NaN 0.3314607
# recall           0 0.1737338
# f1             NaN 0.2279753
res$f1wTest # 0.5233966
res$hamTest # 0.3146544
res$labelwiseLogLTest # -4.448446
res$accTest # 0.07642682

# Closer inspection of the learnt rules:
load("rsl.RData")
# So it has not only learned rules between variables, but also to the input data