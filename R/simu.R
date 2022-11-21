# R code for FIGS based on orginal python coding
# created on 1 Nov. 2022
# author: Haoxue Wang(hw613@cam.ac.uk)
# University of Cambridge


#' simulation for FIGS algoritms
#'
#' simulation for R function in FIGS packages
#'
#'

simulation <- function(X=NULL){

install.packages("rpart")
install.packages("fastshap")
library(rpart)
library(fastshap) # load friedman dataset
source("fit.R")

# getwd() # get the current directory
# setwd("xxxx") # set the path as working environment

# load the data for classification and regression
# X_reg = gen_friedman(100)[,-1]
# Y_reg = data.frame(gen_friedman(100)[,1])
# Y_reg = Y_reg$gen_friedman.100....1.
data_reg = read.csv("~/figs/inst/extdata/data_reg.csv")
data_cls <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"), header = FALSE)
X_cls = data_cls[,3:32] # load breast_cancer dataset
Y_cls = data_cls[,2]
X_reg = data_reg[,1:10]
Y_reg = data_reg[,11]
fit1 <- figs.classifier(X_cls, Y_cls, max_rules=3, sample_weight = rep(1,nrow(X_cls)))
fit2 <-figs.regressor(X_reg, Y_reg, max_rules=11, sample_weight = rep(1,nrow(X_cls)))

}





