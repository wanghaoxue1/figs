## Fast Interpretable Greedy-Tree Sums (FIGS)

**Paper Author : Yan Shuo Tan, Chandan Singh, Keyan Nasseri, Abhineet Agarwal, Bin Yu**

**R Package Author:  Haoxue Wang (hw613@cam.ac.uk)---University of Cambridge**

This package is R version for the FIGS algorithms based on python, hopefully following R version for [imodels](https://github.com/csinva/imodels) will be developed in the furture.

The introduction manual of the package is in [Manual](https://github.com/wanghaoxue1/figs/blob/main/figs_0.8.pdf)

#### Introduction

Fast Interpretable Greedy-Tree Sums (FIGS) is an algorithm for fitting concise rule-based models. Specifically, FIGS generalizes CART to simultaneously grow a flexible number of trees in a summation. The total number of splits across all the trees can be restricted by a prespecified threshold, keeping the model interpretable.
Experiments across real-world datasets show that FIGS achieves state-of-the-art prediction performance when restricted to just a few splits (e.g. less than 20). 'FIGS' is first defined in Tan et al. (2022) )<https://arxiv.org/abs/2201.11931>

#### Key function

**figs.regressor**:  fitting function for fast interpretable greedy-tree sums on regression

**figs.classifier**: fitting function for fast interpretable greedy-tree sums on regression


### **For reproduction of the simulation, you can see the code as the following.**



##### Install all the packages .

```R
install.packages("figs") # not available not as in revison and not published yet
install.packages("rpart") # based on orginal tree split algorithms
install.packages("fastshap") # to generate friedman dataset as required 
library(rpart)
library(fastshap) # load friedman dataset
```



```R
source("fit.R") # please change the working environment as belowed first

# getwd() # get the current directory
# setwd("xxxx") # set the path as working environment

# you can reproduce the simulation conveniently by loading the original data
# load("data_simu.RData") # under the document of figs/inst/extdata

# load the data for classification and regression
# X_reg = gen_friedman(100)[,-1]
# Y_reg = data.frame(gen_friedman(100)[,1])
# Y_reg = Y_reg$gen_friedman.100....1.

data_reg = read.csv("data_reg.csv")  # under the document of figs/inst/extdata
data_cls <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"), header = FALSE)
X_cls = data_cls[,3:32] # load breast_cancer dataset for classification
Y_cls = data_cls[,2]   
fit1 <- figs.classifier(X_cls, Y_cls, max_rules=6, sample_weight = rep(1,nrow(X_cls)))
fit2 <-figs.regressor(X_reg, Y_reg, max_rules=12, sample_weight = rep(1,nrow(X_cls)))

```

```R
 fit1 <- figs.classifier(X_cls, Y_cls, max_rules=3, sample_weight = rep(1,nrow(X_cls)))

 adding node V23 <= 92.5225  # the same result as the python simulation
 adding node V30 <= 0.236422 # for the classification problem, we put y into number for residual caculation
 adding node V25 <= 0.3783622
> fit2 <-figs.regressor(X_reg, Y_reg, max_rules=11, sample_weight = rep(1,nrow(X_cls)))

 adding node x6 <= 0.04461191 # see the output in R for detail structure
 adding node x1 <= 0.07474517 
 adding node x2 <= 0.1974457
 adding node x9 <= 0.111539
 adding node x4 <= 0.07216289
 adding node x2 <= 0.09328108
 adding node x9 <= 0.1909042  
 adding node x5 <= 0.1009846
 adding node x1 <= 0.03279004   # begin to grow another tree to fit the data
 adding node x2 <= 0.1268979
 adding node x10 <= 0.08843308
> 
```
