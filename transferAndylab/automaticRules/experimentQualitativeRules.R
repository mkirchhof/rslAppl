# Get the ID that SLURM hands over to the script as argument
folds <- as.integer(Sys.getenv("PBS_ARRAYID"))
if(is.na(folds)){
  folds <- 1
}
cat("ID = ", folds, "\n")

nRules <- 90

# Dependencies:
library("parallel")
source("rsl_qualitative.R")
library(microbenchmark)

# Initialize cluster
ntasks <- 10
cl <- makeCluster(ntasks)
clusterSetRNGStream(cl, iseed=22122020 - 1)
set.seed(22122020 - 1)
clusterEvalQ(cl, source("rsl_qualitative.R"))

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


# Select the rule
set.seed(123)
rule <- .getAllRules(rsl)[sample(90, 8)][folds]

# evaluate and save
clusterSetRNGStream(cl, iseed=22122020 - 1)
set.seed(22122020 - 1)
pred <- predict(rsl, data$test, cluster = cl, method = "approximate", 
                measureRule = rule)
save(pred, file = paste0(folds, "_pred.RData"))


# stop cluster
stopCluster(cl)