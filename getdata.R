#!/usr/bin/R --verbose --no-save

# Data from Human Activity Recognition
# Source: http://groupware.les.inf.puc-rio.br/har

if (!file.exists("data/pml-training.csv")) {
    print("Downloading training data ...")
    fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    download.file(fileUrl, destfile = "data/pml-training.csv", method = "curl", quiet = TRUE)
}

if (!file.exists("data/pml-testing.csv")) {
    print("Downloading testing data ...")
    fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    download.file(fileUrl, destfile = "data/pml-testing.csv", method = "curl", quiet = TRUE)
}

if (file.exists("data/model.rds")) {
    print("Restoring model ...")
    model <- readRDS("data/model.rds")
}