#!/usr/bin/R --verbose

# Method

# Random forest is affected by multi-collinearity but not by outlier problem.
# See http://www.listendata.com/2014/11/random-forest-with-r.html

# A much better way to do this (CPU wise) is directly using the Random Forest package.
# See http://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/

# Running Script

# run this script from bash command line using
# R --no-save < model-rf-cv.R | tee data/run-rf-cv.log

require(dplyr, quietly = TRUE)
require(caret, quietly = TRUE)
require(rfUtilities, quietly = TRUE)

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
rawindex <- createDataPartition(raw$classe, p = 0.7, list = FALSE, times = 1)
training <- raw[rawindex,]
testing <- raw[-rawindex,]


# prepare model formula:

# ignore columns that are more than 95% empty (i.e. NA):
nasPerc <- as.integer(0.95 * nrow(training))
nas <- sort(apply(training, 2, function(x) length(which(is.na(x)))), decreasing = TRUE)
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

# check if any of these columns have problems with multi-collinearity
multi.collinear(dplyr::select(training, one_of(trainNames)))

# model using random forest
if (file.exists("data/model-rf-cv.rds")) {
    print("Restoring model ...")
    model <- readRDS("data/model-rf-cv.rds")
} else {
    print("Building model ...")
    # record start time of model build
    starttime <- proc.time()
    model <- train(trainFormula, data = training, method = "rf",
                   trControl = trainControl(method = "cv", number = 5))
    # how long did this model take to build?
    print(paste("Total elapsed time is:", (proc.time() - starttime)[["elapsed"]], "secs"))
    # save model
    saveRDS(model, "data/model-rf-cv.rds")
}

# show model
model

# cross-validate against test
testPredict <- predict(model, newdata = testing)
# estimate error (since this is categorical data we are estimating accuracy)
errorRate <- function(trueValues, predictValues) {
    sum(trueValues != predictValues) / length(trueValues)
}
errorRate(testing$classe, testPredict)

# print Accuracy and Kappa (measure of rating variable(s) agreement)
postResample(testPredict, testing$classe)

# show confusion matrix
confusionMatrix(data = testPredict, reference = testing$classe)

# Variable Importance
varImp(model)

#EOF
