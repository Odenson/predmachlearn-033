#!/usr/bin/R --verbose

# Goal

# The goal of this project is to predict the manner in which people did an
# exercise. This is recorded in the "classe" variable of the training set.
# * describe how model was built
# * why choices made for this model
# * how model was cross validated
# * what the expected out of sample error is

# Notes

# Random forest is affected by multi-collinearity but not by outlier problem.
# See http://www.listendata.com/2014/11/random-forest-with-r.html

# Running Script

# run this script from bash command line using
# R --no-save < model-rf.R | tee run-rf.log

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

# do we have a good classe split?
# table(training$classe)
# table(testing$classe)

# do we have any window overlap between train and test?
# intersect(unique(training$num_window), unique(testing$num_window))

# prepare model formula:

# ignore columns that are more than 95% empty (i.e. NA):
nas.perc <- as.integer(0.95 * nrow(raw))
nas <-
    sort(apply(raw, 2, function(x)
        length(which(is.na(x)))), decreasing = TRUE)
nas.names <- names(nas[nas >= nas.perc])
# print(nas.names)
good.names <- setdiff(names(training), nas.names)
# print(good.names)
# exclude columns that do not aid in prediction (or are an outcome)
# method: (inner to outer)
# - paste together with OR condition all names to exclude
# - grep returning inverted matched names (i.e. collect those that don't match)
# - sort alphabetically to help us humans
train.names <- sort(grep(
    paste("classe", "_window", "user_name", "X", "_timestamp", sep = "|"),
    good.names, value = TRUE, invert = TRUE)
)
# print(train.names)
# use these names to generate training formula
train.formula <- as.formula(paste("classe ~ ", paste(train.names, collapse = "+")))
print(train.formula)

# check if any of these columns have problems with multi-collinearity
multi.collinear(dplyr::select(training, one_of(train.names)))

# model using random forest
if (file.exists("data/model-rf.rds")) {
    print("Restoring model ...")
    model <- readRDS("data/model-rf.rds")
} else {
    print("Building model ...")
    # record start time of model build
    starttime <- proc.time()
    model <- train(train.formula, data = training, method = "rf")
    # how long did this model take to build?
    print(paste("Total elapsed time is:", (proc.time() - starttime)[["elapsed"]], "secs"))
    # save model
    rds.filename <- paste0("data/model-", model$method, ".rds")
    saveRDS(model, rds.filename)
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
test.predict <- predict(model, newdata = testing)
# print Accuracy and Kappa (measure of rating variable(s) agreement)
postResample(test.predict, testing$classe)

# show confusion matrix
cm <- confusionMatrix(data = test.predict, reference = testing$classe)
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

# now lets look at project validation
validation <- read.csv(
    "data/pml-testing.csv", header = TRUE,
    na.strings = c("NA", "#DIV/0!"), stringsAsFactors = FALSE
)
predictions <- predict(model, newdata = validation)
# save answers
saveRDS(predictions, "data/predictions-rf.rds")
# see pred.R to process validation predictions
