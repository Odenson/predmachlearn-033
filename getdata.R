#!/usr/bin/R --verbose --quiet

# Data from Human Activity Recognition
# Source: http://groupware.les.inf.puc-rio.br/har

if (!file.exists("pml-training.csv")) {
    print("Downloading training data ...")
    fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    download.file(fileUrl, destfile = "pml-training.csv", method = "curl", quiet = TRUE)
}

if (!file.exists("pml-testing.csv")) {
    print("Downloading testing data ...")
    fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    download.file(fileUrl, destfile = "pml-testing.csv", method = "curl", quiet = TRUE)
}
