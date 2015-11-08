# predmachlearn-033 project

This is the project source code for [Coursera](https://www.coursera.org/) [Data
Science Specialisation](https://www.coursera.org/specializations/jhudatascience)
[Practical Machine Learning](https://www.coursera.org/course/predmachlearn).

## report

The report as a generated HTML page can be found at

* https://frankjungdss.github.io/predmachlearn-033/project.html

## random forest using k-fold cross-validation

| Name | Description |
|------|-------------|
| project.Rmd | project source |
| model-rf-cv.R | train random forest cv model |
| pred-rf-cv.R | prepare predictions for submission |
| getdata.R | download raw CSV data from remote sources |
| data/model-rf-cv.rds | save of random forest cv model |
| data/pml-testing.csv | validation data |
| data/pml-training.csv | training data |
| data/run-rf-cv.log | run log of random forest cv model |

## models used for testing and teaching

I used these models and scripts to get a handle on what to do in this project.

### random forest (reference only)

This was a basic RF model used for experiments.

| Name | Description |
|------|-------------|
| data/model-rf.rds | save of random forest model |
| data/model-rf.R | train random forest model |
| data/run-rf.log | run log of random forest model |
| pred-rf.R | prepare predictions for submission |


### lda model (reference only)

This was a basic LDA model used for experiments.

| Name | Description |
|------|-------------|
| data/model-lda.rds | save of LDA model (not used) |
| data/run-lda.log | run log of LDA model (not used) |
| model-lda.R | train LDA model (not used) |

