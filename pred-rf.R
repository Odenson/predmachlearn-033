#!/usr/bin/R --verbose --no-save

# run model against "validation" to generate "predictions" for submission

require(randomForest, quietly = TRUE)

# restore random forest model
restoreModel <- function() {
    if (!exists("model") & file.exists("data/model-rf.rds")) {
        print("Restoring model ...")
        readRDS("data/model-rf.rds")
    }
}

# restore validation data
restoreData <- function() {
    if (!exists("validation")) {
        print("Restoring validation data ...")
        read.csv(
            "data/pml-testing.csv", header = TRUE,
            na.strings = c("NA", "#DIV/0!"), stringsAsFactors = FALSE
        )
    }
}

# write validation results to separate files
pml_write_files = function(x) {
    n = length(x)
    for (i in 1:n) {
        filename = paste0("data/problem-",i,".txt")
        write.table(
            x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE
        )
    }
}

#
# MAIN
#

# load model and validation data
model <- restoreModel()
validation <- restoreData()

# make predictions on validation data
predictions <- predict(model, newdata = validation)

# save predictions
saveRDS(predictions, "data/predictions-rf.rds")

# separate predictions: 1 per result
pml_write_files(predictions)

#EOF
