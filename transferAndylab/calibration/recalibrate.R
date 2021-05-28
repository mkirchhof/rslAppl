# This script recalibrates the output of the tCNN
# Author: michael.kirchhof@udo.edu
# Date: 12.03.2021

# Dependencies:
source("dirichletCalibration.R")
library(tikzDevice)

# Uncalibrated data
load("../../andyLab/data/data.RData")
pdf("calibration.pdf", width = 10, height = 12)
par(mfrow = c(3, 3))
evalCalibration(as.matrix(data$test[, 1:6]), data$testActual[, 1])
evalCalibration(as.matrix(data$test[, 7:11]), data$testActual[, 2])
evalCalibration(as.matrix(data$test[, 12:19]), data$testActual[, 3])

# Train calibrator on val data
trainInd <- balanceDataset2(as.matrix(data$val[, 1:6]))
cal1 <- .trainDirCalibration(as.matrix(data$val[trainInd, c(1,2,4)]), data$valActual[trainInd, 1])
save(cal1, file = "cal1.RData")
trainInd <- balanceDataset2(as.matrix(data$val[, 7:11]))
cal2 <- .trainDirCalibration(as.matrix(data$val[trainInd, 7:11]), data$valActual[trainInd, 2])
save(cal2, file = "cal2.RData")
trainInd <- balanceDataset2(as.matrix(data$val[, 12:19]))
cal3 <- .trainDirCalibration(as.matrix(data$val[trainInd, 12:19]), data$valActual[trainInd, 3])
save(cal3, file = "cal3.RData")

# Recalibrate
load("cal1.RData")
load("cal2.RData")
load("cal3.RData")
dataBackup <- data
data$test[,c(1,2,4)] <- .predictDirCalibration(cal1, as.matrix(data$test[,c(1,2,4)]))
data$test[,7:11] <- .predictDirCalibration(cal2, as.matrix(data$test[,7:11]))
data$test[,12:19] <- .predictDirCalibration(cal3, as.matrix(data$test[,12:19]))
evalCalibration(as.matrix(data$test[, 1:6]), data$testActual[, 1])
evalCalibration(as.matrix(data$test[, 7:11]), data$testActual[, 2])
evalCalibration(as.matrix(data$test[, 12:19]), data$testActual[, 3])

# The same, but without entropy sampling
# Train calibrator on val data
data <- dataBackup
trainInd <- seq(nrow(data$val))
cal1 <- .trainDirCalibration(as.matrix(data$val[trainInd, c(1,2,4)]), data$valActual[trainInd, 1])
save(cal1, file = "cal1_2.RData")
cal2 <- .trainDirCalibration(as.matrix(data$val[trainInd, 7:11]), data$valActual[trainInd, 2])
save(cal2, file = "cal2_2.RData")
cal3 <- .trainDirCalibration(as.matrix(data$val[trainInd, 12:19]), data$valActual[trainInd, 3])
save(cal3, file = "cal3_2.RData")

# Recalibrate
load("cal1_2.RData")
load("cal2_2.RData")
load("cal3_2.RData")
data$test[,c(1,2,4)] <- .predictDirCalibration(cal1, as.matrix(data$test[,c(1,2,4)]))
data$test[,7:11] <- .predictDirCalibration(cal2, as.matrix(data$test[,7:11]))
data$test[,12:19] <- .predictDirCalibration(cal3, as.matrix(data$test[,12:19]))
evalCalibration(as.matrix(data$test[, 1:6]), data$testActual[, 1])
evalCalibration(as.matrix(data$test[, 7:11]), data$testActual[, 2])
evalCalibration(as.matrix(data$test[, 12:19]), data$testActual[, 3])

dev.off()
# save(data, file = "../realdispIMU/recalibrated/data.RData")
# 
# # Recalibrate LARa data
# load("../LaraIMU/data/data.RData")
# data$train[] <- .predictDirCalibration(cal, as.matrix(data$train))
# data$val[] <- .predictDirCalibration(cal, as.matrix(data$val))
# data$test[] <- .predictDirCalibration(cal, as.matrix(data$test))
# save(data, file = "../LaraIMU/recalibrated/data.RData")


# Uncalibrated data
tikz('calAndylab.tex', standAlone = FALSE, width=4.1, height=4.1)
load("../../andyLab/data/data.RData")
par(mfcol = c(3, 3), mar = c(0.1, 0.1, 0.1, 0.1), oma = c(4.8, 4.8, 0, 0))
evalCalibration(as.matrix(data$test[, 1:6]), data$testActual[, 1], ece = TRUE)
axis(2, at = seq(0, 1, 0.1), labels = c("0", "", "", "0.3", "", "", "", "0.7", "", "", "1"), 
     padj = +0.3, gap.axis = -2)
mtext("Positive Labels", 2, 2.3, cex = 0.7)
mtext("General Posture", 2, 3.6, cex = 0.7)
evalCalibration(as.matrix(data$test[, 7:11]), data$testActual[, 2], ece = TRUE)
axis(2, at = seq(0, 1, 0.1), labels = c("0", "", "", "0.3", "", "", "", "0.7", "", "", "1"), 
     padj = +0.3, gap.axis = -2)
mtext("Positive Labels", 2, 2.3, cex = 0.7)
mtext("Detailed Posture", 2, 3.6, cex = 0.7)
evalCalibration(as.matrix(data$test[, 12:19]), data$testActual[, 3], ece = TRUE)
axis(1, at = seq(0, 1, 0.1), labels = c("0", "", "", "0.3", "", "", "", "0.7", "", "", "1"), 
     padj = -0.3, gap.axis = -2)
mtext("Prediction", 1, 2.3, cex = 0.7)
mtext("Uncalibrated", 1, 3.6, cex = 0.7)
axis(2, at = seq(0, 1, 0.1), labels = c("0", "", "", "0.3", "", "", "", "0.7", "", "", "1"), 
     padj = +0.3, gap.axis = -2)
mtext("Positive Labels", 2, 2.3, cex = 0.7)
mtext("Goal--oriented Action", 2, 3.6, cex = 0.7)

# Recalibrate
dataBackup <- data
load("cal1_2.RData")
load("cal2_2.RData")
load("cal3_2.RData")
data$test[,c(1,2,4)] <- .predictDirCalibration(cal1, as.matrix(data$test[,c(1,2,4)]))
data$test[,7:11] <- .predictDirCalibration(cal2, as.matrix(data$test[,7:11]))
data$test[,12:19] <- .predictDirCalibration(cal3, as.matrix(data$test[,12:19]))
evalCalibration(as.matrix(data$test[, 1:6]), data$testActual[, 1])
evalCalibration(as.matrix(data$test[, 7:11]), data$testActual[, 2])
evalCalibration(as.matrix(data$test[, 12:19]), data$testActual[, 3])
axis(1, at = seq(0, 1, 0.1), labels = c("0", "", "", "0.3", "", "", "", "0.7", "", "", "1"), 
     padj = -0.3, gap.axis = -2)
mtext("Prediction", 1, 2.3, cex = 0.7)
mtext("Unbalanced--Recalibrated", 1, 3.6, cex = 0.7)

# Recalibrate
load("cal1.RData")
load("cal2.RData")
load("cal3.RData")
data <- dataBackup
data$test[,c(1,2,4)] <- .predictDirCalibration(cal1, as.matrix(data$test[,c(1,2,4)]))
data$test[,7:11] <- .predictDirCalibration(cal2, as.matrix(data$test[,7:11]))
data$test[,12:19] <- .predictDirCalibration(cal3, as.matrix(data$test[,12:19]))
evalCalibration(as.matrix(data$test[, 1:6]), data$testActual[, 1])
evalCalibration(as.matrix(data$test[, 7:11]), data$testActual[, 2])
evalCalibration(as.matrix(data$test[, 12:19]), data$testActual[, 3])
axis(1, at = seq(0, 1, 0.1), labels = c("0", "", "", "0.3", "", "", "", "0.7", "", "", "1"), 
     padj = -0.3, gap.axis = -2)
mtext("Prediction", 1, 2.3, cex = 0.7)
mtext("Balanced--Recalibrated", 1, 3.6, cex = 0.7)

dev.off()