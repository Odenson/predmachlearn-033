#!/usr/bin/R --verbose

require(dply, quietly = TRUE)
require(caret, quietly = TRUE)
# require(lubridate, quietly = TRUE)
# require(ggplot2, quietly = TRUE)

starttime <- proc.time()

# load raw data
raw <- read.csv("data/pml-training.csv", header = TRUE,
                na.strings = c("NA", "#DIV/0!"), stringsAsFactors = FALSE)

# set factors classe (for outcome), num_window for partitioning
# raw$cvtd_timestamp <- dmy_hm(raw$cvtd_timestamp)
# raw$num_window <- factor(raw$num_window)
raw$classe <- factor(raw$classe)

# order by window num and raw_timestamp_part_2 to mimic a timeseries
raw <- arrange(raw, num_window, raw_timestamp_part_2)

# qplot(num_window, cvtd_timestamp, data = raw[raw$user_name == "carlitos",], colour = classe)
#qplot(num_window, classe, data = raw[raw$user_name == "carlitos",], colour = classe)
#qplot(num_window, classe, data = raw, colour = classe)

# split into train (70%) and test (30%) on classe
set.seed(033)
rawindex <- createDataPartition(raw$classe, p = 0.7, list = FALSE, times = 1)
training <- raw[rawindex,]
testing <- raw[-rawindex,]

# do we have a good classe split?
# levels(training$classe)
# levels(testing$classe)

# do we have any window overlap between train and test?
# intersect(unique(training$num_window), unique(testing$num_window))

# ignore columns that are more than 95% empty (i.e. NA):
nas.perc <- as.integer(0.95 * nrow(raw))
nas <- sort(apply(raw, 2, function(x) length(which(is.na(x)))), decreasing = TRUE)
bad.names <- names(nas[nas >= 19216])
# print(bad.names)
good.names <- setdiff(names(training), bad.names)
# print(good.names)

# exclude columns that do not aid in prediction (or are an outcome)
train.names <- sort(
    grep(paste("classe", "_window", "user_name", "X", "_timestamp", sep = "|"),
         good.names, value = TRUE, invert = TRUE))
# print(train.names)

# use these column names to generate training formula
train.formula <- as.formula(paste("classe ~ ", paste(train.names, collapse = "+")))
print(train.formula)

# model using random forest
model <- train(train.formula, data = training, method = "rf")

# save model
saveRDS(model, "data/model.rds")

elapsedtime <- proc.time() - starttime

print("Total elapsed time is:")
print(elapsedtime)