# This script recalibrates the output of the tCNN
# Author: michael.kirchhof@udo.edu
# Date: 12.03.2021

# Dependencies:
source("dirichletCalibration.R")
library(tikzDevice)
source("rsl.R")

load("../../realdispIMU/data/data.RData")
ll <- .labelwiseLogLikelihood(data$test, data$testActual)
quantile(ll, c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
mean(ll)

# Uncalibrated data
tikz('calRealdisp.tex', standAlone = FALSE, width=4.1, height=1.8)
load("../../realdispIMU/data/data.RData")
par(mfcol = c(1, 3), mar = c(0.1, 0.1, 0.1, 0.1), oma = c(4.8, 4.8, 0, 0))
evalCalibration(as.matrix(data$test), data$testActual[, 1], ece = TRUE)
axis(1, at = seq(0, 1, 0.1), labels = c("0", "", "", "0.3", "", "", "", "0.7", "", "", "1"), 
     padj = -0.3, gap.axis = -2)
mtext("Prediction", 1, 2.3, cex = 0.7)
mtext("Uncalibrated", 1, 3.6, cex = 0.7)
axis(2, at = seq(0, 1, 0.1), labels = c("0", "", "", "0.3", "", "", "", "0.7", "", "", "1"), 
     padj = +0.3, gap.axis = -2)
mtext("Positive Labels", 2, 2.3, cex = 0.7)

# Train calibrator on val data
dataBackup <- data
# trainInd <- balanceDataset2(as.matrix(data$val))
# cal <- .trainDirCalibration(as.matrix(data$val[trainInd, ]), data$valActual[trainInd, 1])
# save(cal, file = "cal.RData")

# Do the same without entropy sampling
# trainInd <- seq(nrow(data$val))
# cal <- .trainDirCalibration(as.matrix(data$val[trainInd, ]), data$valActual[trainInd, 1])
# save(cal, file = "cal_2.RData")
load("cal_2.RData")
data$test[] <- .predictDirCalibration(cal, as.matrix(data$test))
evalCalibration(as.matrix(data$test), data$testActual[, 1], ece = TRUE)
axis(1, at = seq(0, 1, 0.1), labels = c("0", "", "", "0.3", "", "", "", "0.7", "", "", "1"), 
     padj = -0.3, gap.axis = -2)
mtext("Prediction", 1, 2.3, cex = 0.7)
mtext("Unbalanced--Recalibrated", 1, 3.6, cex = 0.7)

# Recalibrate
data <- dataBackup
load("cal.RData")
#data$train[] <- .predictDirCalibration(cal, as.matrix(data$train))
#data$val[] <- .predictDirCalibration(cal, as.matrix(data$val))
data$test[] <- .predictDirCalibration(cal, as.matrix(data$test))
evalCalibration(as.matrix(data$test), data$testActual[, 1], ece = TRUE)
axis(1, at = seq(0, 1, 0.1), labels = c("0", "", "", "0.3", "", "", "", "0.7", "", "", "1"), 
     padj = -0.3, gap.axis = -2)
mtext("Prediction", 1, 2.3, cex = 0.7)
mtext("Balanced--Recalibrated", 1, 3.6, cex = 0.7)

# save(data, file = "../realdispIMU/recalibrated/data.RData")

# # Recalibrate LARa data
# load("../../LaraIMU/data/data.RData")
# data$train[] <- .predictDirCalibration(cal, as.matrix(data$train))
# data$val[] <- .predictDirCalibration(cal, as.matrix(data$val))
# data$test[] <- .predictDirCalibration(cal, as.matrix(data$test))
# save(data, file = "../LaraIMU/recalibrated/data.RData")


dev.off()