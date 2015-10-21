#!/usr/bin/R --verbose --no-save

# run model against "validation" to generate "predictions" for submission

# restore rf model
restoreModel <- function() {
    # restore random forest model
    if (!exists("model") & file.exists("data/model-rf-cv.rds")) {
        print("Restoring model ...")
        readRDS("data/model-rf-cv.rds")
    }
}

# restore validation data
restoreData <- function() {
    if (!exists("validation")) {
        read.csv(
            "data/pml-testing.csv", header = TRUE,
            na.strings = c("NA", "#DIV/0!"), stringsAsFactors = FALSE
        )
    }
}

#
# Function to write validation results to separate files
#
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
saveRDS(predictions, "data/predictions-rf-cv.rds")

# separate predictions: 1 per result
pml_write_files(predictions)

#EOF
