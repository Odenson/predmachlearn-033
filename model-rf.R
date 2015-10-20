#!/usr/bin/R --verbose

# Method

# Random forest is affected by multi-collinearity but not by outlier problem.
# See http://www.listendata.com/2014/11/random-forest-with-r.html

# A much better way to do this (CPU wise) is directly using the Random Forest package.
# See http://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/
#
# Built a similar model < 10 minutes. :-( I wish I new this earlier!
# Estimated error was also less:
# Caret (1+ hours to run): 0.009345794
# RandomForest (< 10 minutes): 0.007136788
#

# Running Script

# run this script from bash command line using
# R --no-save < model-rf.R | tee data/run-rf.log

require(dplyr, quietly = TRUE)
require(randomForest, quietly = TRUE)
require(rfUtilities, quietly = TRUE)
require(ggplot2, quietly = TRUE)

# load raw data
raw <- read.csv("data/pml-training.csv", header = TRUE, na.strings = c("NA", "#DIV/0!"), stringsAsFactors = FALSE)

# set classe as a factor
raw$classe <- factor(raw$classe)

# split into train (70%) and test (30%)
set.seed(033)
dataSize <- nrow(raw)
sampleSize <- floor(0.70 * dataSize)
rawindex <- sample(seq_len(dataSize), size = sampleSize)
training <- raw[rawindex,]
testing <- raw[-rawindex,]

# do we have a good classe split?
table(training$classe)
table(testing$classe)

# ignore columns that are more than 95% empty (i.e. NA):
nasPerc <- as.integer(0.95 * nrow(raw))
nas <- sort(apply(raw, 2, function(x) length(which(is.na(x)))), decreasing = TRUE)
badNames <- sort(names(nas[nas >= nasPerc]))
goodNames <- setdiff(names(training), badNames)

# exclude columns that do not aid in prediction (or are an outcome)
trainNames <-
    grep(
        paste("classe", "window", "user_name", "X", "_timestamp", sep = "|"),
        goodNames, value = TRUE, invert = TRUE
    )

# use these column names to generate training formula
trainFormula <- as.formula(paste("classe ~ ", paste(trainNames, collapse = "+")))
print(trainFormula)

# model

# model using random forest
if (file.exists("data/model-rf.rds")) {
    print("Restoring model ...")
    model <- readRDS("data/model-rf.rds")
} else {
    print("Building model ...")
    # record start time of model build
    starttime <- proc.time()
    model <- randomForest(trainFormula, data = training)
    # how long did this model take to build?
    print(paste("Total elapsed time is:", (proc.time() - starttime)[["elapsed"]], "secs"))
    # save model
    saveRDS(model, "data/model-rf.rds")
}

# check multi-collinearity
multi.collinear(dplyr::select(training, one_of(trainNames)))

# variable importance
importance(model)

# cross-validation
testPredict <- predict(model, newdata = testing)

# estimate error (since this is categorical data we are estimating accuracy)
errorRate <- function(trueValues, predictValues) {
    sum(trueValues != predictValues) / length(trueValues)
}
errorRate(testing$classe, testPredict)

require(caret, quietly = TRUE)
confusionMatrix(data = testPredict, reference = testing$classe)

#EOF
