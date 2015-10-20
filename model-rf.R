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
require(caret, quietly = TRUE)
require(ggplot2, quietly = TRUE)
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

# show relationship between window and classe
# qplot(num_window, cvtd_timestamp, data = raw[raw$user_name == "carlitos",], colour = classe)
# qplot(num_window, classe, data = raw[raw$user_name == "carlitos",], colour = classe)
png(filename = "data/plot-window-classe.png", width = 640, height = 480, units = "px")
qplot(num_window, classe, data = raw, colour = classe,
      main = "Exercise Window vs Classe",
      xlab = "Window Number", ylab = "Classe") +
    theme_light(base_family = "sans", base_size = 11) +
    theme(legend.key = element_rect(colour = NA))
dev.off()

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

# check if any of these columns have problems with multi-collinearity
multi.collinear(dplyr::select(training, one_of(trainNames)))

# model using random forest
if (file.exists("data/model-rf.rds")) {
    print("Restoring model ...")
    model <- readRDS("data/model-rf.rds")
} else {
    print("Building model ...")
    # record start time of model build
    starttime <- proc.time()
    model <- train(trainFormula, data = training, method = "rf")
    # how long did this model take to build?
    print(paste("Total elapsed time is:", (proc.time() - starttime)[["elapsed"]], "secs"))
    # save model
    saveRDS(model, "data/model-rf.rds")
}

# print some information on this model
print(model$finalModel$problemType)
print(model$method)
print(model$finalModel$xNames)
print(model$finalModel$obsLevels)
print(model$finalModel$confusion)
print(model$metric)
print(model$dots["proximity"])

# problems with outliers?
# this requires proximity measure from model
if (length(model$dots) > 0 && model$dots["proximity"]) {
    model$dots["proximity"]
    outliers.rf <- outlier(model$finalModel)
    png(filename = "data/plot-outliers.png", width = 640, height = 480, units = "px")
    par(family = "sans", cex = 0.7)
    plot(outliers.rf, type = "h", col = training$classe)
    legend(x = 12000, y = 14, legend = levels(training$classe), col = c(1:length(levels(training$classe))), pch = 16)
    dev.off()
    # WARNING: MDS plot takes along time to generate ...
    # MDSplot(model$finalModel, training$classe, main = "MDS Proximity Matrix")
}

png(filename = "data/plot-model-errors.png", width = 640, height = 480, units = "px")
par(family = "sans", cex = 0.7)
plot(model$finalModel, type = "l", main = "Error Rates for Random Forest Model")
legend(x = "topright", title = "Classe", legend = levels(training$classe), col = c(1:length(levels(training$classe))), pch = 16)
dev.off()

# test
testPredict <- predict(model, newdata = testing)
# estimate error (since this is categorical data we are estimating accuracy)
errorRate <- function(trueValues, predictValues) {
    sum(trueValues != predictValues) / length(trueValues)
}
errorRate(testing$classe, testPredict)

# print Accuracy and Kappa (measure of rating variable(s) agreement)
postResample(testPredict, testing$classe)

# show confusion matrix
cm <- confusionMatrix(data = testPredict, reference = testing$classe)
# all
cm
# individually ...
# overall statistics
cm$overall

# Variable Importance
importance(model$finalModel)
varImp(model)
par(mfrow = c(1, 1), family = "sans", cex = 0.7)
varImpPlot(model$finalModel, n.var = 20, main = "Variable Importance")

# reference table
cm$table
# statistics by class
round(cm$byClass, 4)

#EOF