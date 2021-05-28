source("rsl.R")
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

allRes <- matrix(NA, ncol = 18, nrow = 11)
for(i in 1:11){
  load(paste0(i, "_res.RData"))
  allRes[i, 1:17] <- res$f1Val["f1", ] 
  allRes[i,18] <- .weightedF1(res$f1Val["f1", ], props)
}

dat <- data.frame(x = seq(10, 110, 10), y = allRes[, 18])
lm <- lm(y ~ x + I(x^2), data = dat)
plot(lm) # looks good to me
newdat <- data.frame(x = 10:110)
preds <- predict(lm, newdat)
plot(dat)
lines(x = newdat$x, y = preds)
# That does not look too good
summary(lm)
# Oh no...
# We will use the 90 model as it performed best


# Load final benchmark
load("res.RData")
propsTest <- .proportionTable(data$testActual)
propsTest / sum(propsTest) # The weights for computing f1 on test
    #        gait            step           stand              up          center            down        noMotion torsoRotation_1     rightHand_1 
    # 0.050316780     0.095120422     0.066856196     0.023209263     0.130140870     0.010124947     0.048818318     0.001723795     0.179868030 
    #  leftHand_1        noHand_1           bulky           handy         utility            cart        computer          noItem 
    # 0.147427640     0.034100341     0.046178171     0.075107127     0.032387813     0.028313047     0.008911906     0.021395335 
res$f1wVal <- .weightedF1(res$f1Val["f1", ], props)
res$f1wTest <- .weightedF1(res$f1Test["f1", ], propsTest)
res$f1Test
# $f1Test
#                gait      step     stand         up    center down  noMotion torsoRotation rightHand  leftHand    noHand      bulky     handy utility
# precision 0.5524380 0.5072626 0.5000000 0.70250368 0.7064708  NaN 0.6313920           NaN 0.8532494 0.7192392 0.4514493 0.26110298 0.4118641       0
# recall    0.4820122 0.5735944 0.4549489 0.07718447 0.9089545    0 0.5471190             0 0.9844239 0.8987925 0.5197137 0.08702017 0.8290415       0
# f1        0.5148278 0.5383931 0.4764118 0.13908733 0.7950226  NaN 0.5862424           NaN 0.9141550 0.7990534 0.4831823 0.13053556 0.5503278     NaN
#                cart computer    noItem
# precision 0.2641455      NaN 0.4543877
# recall    0.2427378        0 0.4135510
# f1        0.2529896      NaN 0.4330086
res$f1wTest # 0.6067753
res$hamTest # 0.2950017
res$labelwiseLogLTest # -4.435957
res$accTest # 0.08535593

# Closer inspection of the learnt rules:
load("9_rsl.RData")
# So it has not only learned rules between variables, but also to the input data

set.seed(123)
rules <- getRules(rsl)
rules <- rules$name[sample(90, 8)]
rules <- strsplit(rules, " \\| ")
rules <- lapply(rules, function(x) cbind(gsub("^(.*) .*$", "\\1", x),
                                round(1 - as.numeric(gsub("^.*\\((.*)\\).*$", "\\1", x)), 3)))
