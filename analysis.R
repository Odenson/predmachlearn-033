#!/usr/bin/R --verbose --no-save

require(caret, quietly = TRUE)

# load raw data
raw <- read.csv("data/pml-training.csv", header = TRUE,
                na.strings = c("NA", "#DIV/0!"), stringsAsFactors = FALSE)

# set factors classe (for outcome), num_window for partitioning
raw$classe <- factor(raw$classe)
raw$num_window <- factor(raw$num_window)

# split into train (70%) and test (30%) on num_window
set.seed(033)
rawindex <- createDataPartition(raw$num_window, p = 0.7, list = FALSE, times = 1)
training <- raw[rawindex,]
testing <- raw[-rawindex,]

# do we have a good classe split?
levels(training$classe)
levels(testing$classe)

# do we have any window overlap between train and test?
intersect(unique(training$num_window), unique(testing$num_window))

# ignore columns that are more than 95% empty (i.e. NA):
nas.perc <- as.integer(0.95 * nrow(raw))
nas <- sort(apply(raw, 2, function(x) length(which(is.na(x)))), decreasing = TRUE)
bad.names <- names(nas[nas >= 19216])
print(bad.names)
good.names <- setdiff(names(training), bad.names)
print(good.names)

# exclude columns that do not aid in prediction (or are an outcome)
train.names <- sort(
    grep(paste("classe", "window", "user_name", "X", "_timestamp", sep = "|"),
         good.names, value = TRUE, invert = TRUE))
print(train.names)

# use these column names to generate training formula
train.formula <- as.formula(paste("classe ~ ", paste(train.names, collapse = "+")))
print(train.formula)

# model using random forest (windows are grouped so use proximity)
#model <- train(train.formula, data = training, method = "rf", proximity = TRUE)
