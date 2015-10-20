#!/usr/bin/R --verbose --no-save

# run model against "validation" to generate "predictions" for submission

restore <- function() {
    # restore random forest model
    if (!exists("model") & file.exists("data/model-rf.rds")) {
        print("Restoring model ...")
        model <- readRDS("data/model-rf.rds")
    }

    if (!exists("validation")) {
        validation <- read.csv(
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
restore()

# make predictions on validation data
predictions <- predict(model, newdata = validation)

# save predictions
saveRDS(predictions, "data/predictions-rf.rds")

# separate predictions: 1 per result
pml_write_files(predictions)

#EOF