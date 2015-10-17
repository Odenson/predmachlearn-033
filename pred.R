#!/usr/bin/R --verbose --no-save

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

if (file.exists("data/predictions-rf.rds")) {
    print("Restoring predictions ...")
    predictions <- readRDS("data/predictions-rf.rds")
}

pml_write_files(predictions)