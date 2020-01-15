# ucimlr
R interface to UCI's machine learning repository

## Purpose
ucimlr's purpose is to make it easy to download and use data from UCI's machine learning repository (http://archive.ics.uci.edu/ml/index.php) into R for use in benchmarking experiments, machine learning instruction, etc. Given the name of a dataset, it will automatically download, extract, import, and label that dataset for consumption. It uses the fast reading functions from the Tibble package to make the importing process much faster. It can also integrate with the mlr package to create a task from the donwloaded data, including specifying classification vs. regression, including a target variable, and cleaning up column formats to work in mlr.

## How to install
To install from within R, run the following script:

```
install.packages("devtools")
```
```
library(devtools)
```
```
install_github(repo="Prometheus77/ucimlr")
```
```
library(ucimlr)
```
## How to use
To see a list of available datasets, simply type:
```
unlist(lapply(list_datasets(), FUN=function(x) x$short_name))
```

To read in a dataset, simply type:
```
ds <- read_ucimlr("adult") # will load the dataset "Adult" located at https://archive.ics.uci.edu/ml/machine-learning-databases/adult/
```

## How to help
If you'd like to contribute, three are two main ways you can help:
1) Integrate more data sets! The metadata required to integrate a new dataset is in list_datasets.R. Only minimal understanding of R list structure required.
2) Look at the issue list and address any issues you see. This is for those who are confdient, competent coders.
3) Suggest improvements. As this project is still young, I welcome improvement suggestsions in the Issues tracker.
4) Spread the word. 
