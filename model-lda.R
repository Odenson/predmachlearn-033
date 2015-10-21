#!/usr/bin/R --verbose

# Notes

# Linear discriminant analysis lda MASS
# http://www.statmethods.net/advstats/discriminant.html

# Running Script

# run this script from bash command line using
# R --no-save < model-lda.R | tee data/run-lda.log

require(dplyr, quietly = TRUE)
require(caret, quietly = TRUE)

# load raw data
raw <- read.csv(
    "data/pml-training.csv", header = TRUE,
    na.strings = c("NA", "#DIV/0!"), stringsAsFactors = FALSE
)

# set outcome variable, classe as a factor
raw$classe <- factor(raw$classe)

# order by window num and raw_timestamp_part_2 to mimic a timeseries
raw <- arrange(raw, num_window, raw_timestamp_part_2)

# split into train (70%) and test (30%) on classe
set.seed(033)
rawindex <-
    createDataPartition(raw$classe, p = 0.7, list = FALSE, times = 1)
training <- raw[rawindex,]
testing <- raw[-rawindex,]

# density plot - shows how exercise classe are represented
png(filename = "data/plot-classe-density.png", width = 640, height = 480, units = "px")
qplot(classe, data = training, geom = "density", colour = classe)
dev.off()

# do we have a good classe split?
# table(training$classe)
# table(testing$classe)

# do we have any window overlap between train and test?
# intersect(unique(training$num_window), unique(testing$num_window))

# prepare model formula:

# ignore columns that are more than 95% empty (i.e. NA):
nasPerc <- as.integer(0.95 * nrow(raw))
nas <- sort(apply(raw, 2, function(x) length(which(is.na(x)))), decreasing = TRUE)
badNames <- names(nas[nas >= nasPerc])
# print(badNames)
goodNames <- setdiff(names(training), badNames)
# print(goodNames)
# exclude columns that do not aid in prediction (or are an outcome)
# method: (inner to outer)
# - paste together with OR condition all names to exclude
# - grep returning inverted matched names (i.e. collect those that don't match)
# - sort alphabetically to help us humans
trainNames <- sort(grep(
    paste("classe", "_window", "user_name", "X", "_timestamp", sep = "|"),
    goodNames, value = TRUE, invert = TRUE)
)
# print(trainNames)
# use these names to generate training formula
trainFormula <- as.formula(paste("classe ~ ", paste(trainNames, collapse = "+")))
print(trainFormula)

# model using random forest
if (file.exists("data/model-lda.rds")) {
    print("Restoring model ...")
    model <- readRDS("data/model-lda.rds")
} else {
    print("Building model ...")
    # record start time of model build
    starttime <- proc.time()
    model <- train(trainFormula, data = training, method = "lda")
    # how long did this model take to build?
    print(paste("Total elapsed time is:", (proc.time() - starttime)[["elapsed"]], "secs"))
    # save model
    saveRDS(model, "data/model-lda")
}

# print some information on this model
print(model$finalModel$problemType)
print(model$method)
print(model$finalModel$xNames)
print(model$finalModel$obsLevels)
print(model$finalModel$confusion)
print(model$metric)

# test
testPredict <- predict(model, newdata = testing)
# estimate error (since this is categorical data we are estimating accuracy)
# see page 37 (james2013introduction)
sum(testPredict != testing$classe) / length(testing$classe)

# print Accuracy and Kappa (measure of rating variable(s) agreement)
postResample(testPredict, testing$classe)

# show confusion matrix
confusionMatrix(data = testPredict, reference = testing$classe)

# validate against project test data

# The final model was run against validation data:
validation <- read.csv(
    "data/pml-testing.csv", header = TRUE,
    na.strings = c("NA", "#DIV/0!"), stringsAsFactors = FALSE
)
validationPredict <- predict(model, newdata = validation)
# save answers
saveRDS(validationPredict, "data/predictions-lda.rds")

# test predictions against known results
rfPred <- readRDS("data/predictions-rf.rds")
ldaPred <- readRDS("data/predictions-lda.rds")
print(rfPred)
print(ldaPred)
rfPred == ldaPred
errorRate(rfPred, ldaPred)
