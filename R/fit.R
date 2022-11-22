# R code for FIGS based on orginal python codes
# created on 1 Nov. 2022
# author: Haoxue Wang(hw613@cam.ac.uk)
# University of Cambridge



#' split the node for regression
#'
#' make the split based on figs regression rule
#'
#'
#' @param X the design matrix
#' @param y the response vector
#' @param idxs the index of samples used for the split(array of bool values. If TRUE, it is used for the split)
#' @param tree_num initial tree_num (default=0)
#' @param sample_weight allocate weight for each individual sampe, array-like of shape n (default=None). If None, then samples are equally weighted.
#' @param max_features the number of features to consider when looking for the best split
#'
#' @return call: the input setting
#' @return node_split: the design matrix for beta after variable splitting
#' node_split$idxs : the index of samples in the original node
#' node_split$value: the value for the split node
#' node_split$impurity: the impurity in the original node
#' node_split$tree_num: the tree number
#' @return node_left: the design matrix for gamma after variable splitting
#' node_split$idxs : the index of samples in the left node
#' node_split$value: the value for the left node after the split
#' node_split$impurity: the impurity in the left node
#' node_split$tree_num: the tree number which the node belongs to
#' @return node_right: the response vector after variable splitting.
#' node_split$idxs : the index of samples in the right node
#' node_split$value: the value for the right node after the split
#' node_split$impurity: the impurity in the right node
#' node_split$tree_num: the tree number which the node belongs to
#' @return n: the number of samples in the original node
#' @return feature: the feature selected to do the split
#' @return threshold: the threshold to make the split
#' @return impurity_reduction: the impurity decreased by the split if the decrease exists


# @max_rules: integer requirement
# @Max total number of rules across all trees
# @min_impurity_decrease:
# @max_features: The number of features to consider when looking for the best split (see https://scikit-learn.org/stable/modules/generated/sklearn.ensemble.RandomForestClassifier.html)


regressor_split <- function(X, y, idxs, tree_num=0, sample_weight=NULL, max_features=NULL){
  # array indices
  SPLIT = 1
  LEFT = 2
  RIGHT = 3
  sweight = NULL
  if(is.null(sample_weight)==FALSE){
    sweight = sample_weight[idxs]
  }
  data = data.frame(X, y)
  stump = rpart(y~. , data = data[idxs,],control = rpart.control(maxdepth = 1))
  frame <- stump$frame
  frame[['gini']] = 1 - (frame[['dev']] / frame[['n']])^2 - (1 - frame[['dev']] / frame[['n']])^2
  result=frame[,c('var','n','dev','gini')]
  feature = result[1,1]
  impurity = result$gini/2  # divide 2 to follow the python result
  # the same as the impurity output from tree.DecisionTreeRegressor
  # (impurity = stump.tree_.impurity)
  n_node_samples = result$n[SPLIT]
  n_node_samples_left = result$n[LEFT]
  n_node_samples_right = result$n[RIGHT]
  threshold = stump$splits[1,4]
  idxs_left=rep(FALSE,nrow(X))
  idxs_right=rep(FALSE,nrow(X))
  idxs_split = idxs
  idxs_left[which(data.frame(stump$where)==2)]=TRUE
  # 2 denote the left 3 denote the right
  idxs_right[which(data.frame(stump$where)==3)]=TRUE
  value = stump$frame$yval  # the proportion between class sample and total node samples
  impurity_reduction = stump$splits[1,3]/2  # divide 2 to follow the python rule result
  if(is.na(feature)==TRUE){
    print("no split found!")
  }
  node_split = list(idxs=idxs, value=value[SPLIT], impurity=impurity[SPLIT], tree_num=tree_num, n=n_node_samples, feature = feature, threshold = threshold, impurity_reduction = impurity_reduction)
  node_left = list(idxs=idxs_left, value=value[LEFT], impurity=impurity[LEFT], tree_num=tree_num,n=n_node_samples_left, feature = feature, threshold = threshold, impurity_reduction = impurity_reduction)
  node_right = list(idxs=idxs_right, value=value[RIGHT], impurity=impurity[RIGHT], tree_num=tree_num, n=n_node_samples_right, feature = feature, threshold = threshold, impurity_reduction = impurity_reduction)
  structure(list(call = match.call(),
                 node_split = node_split,
                 node_left = node_left,
                 node_right = node_right,
                 n=n_node_samples,
                 feature = feature,
                 threshold = threshold,
                 impurity_reduction = impurity_reduction),
                 class = "node")
}





#' split the node for classification
#'
#' make the split based on figs classification rule
#'
#'
#' @param X the design matrix
#' @param y the response vector
#' @param idxs the index of samples used for the split(array of bool values. If TRUE, it is used for the split)
#' @param tree_num initial tree_num (default=0)
#' @param sample_weight allocate weight for each individual sampe, array-like of shape n (default=None). If None, then samples are equally weighted.
#' @param max_features the number of features to consider when looking for the best split
#'
#'
#' @return call: the input setting
#' @return node_split: the design matrix for beta after variable splitting
#' node_split$idxs : the index of samples in the original node
#' node_split$value: the value for the split node
#' node_split$impurity: the impurity in the original node
#' node_split$tree_num: the tree number
#' @return node_left: the design matrix for gamma after variable splitting
#' node_split$idxs : the index of samples in the left node
#' node_split$value: the value for the left node after the split
#' node_split$impurity: the impurity in the left node
#' node_split$tree_num: the tree number which the node belongs to
#' @return node_right: the response vector after variable splitting.
#' node_split$idxs : the index of samples in the right node
#' node_split$value: the value for the right node after the split
#' node_split$impurity: the impurity in the right node
#' node_split$tree_num: the tree number which the node belongs to
#' @return n: the number of samples in the original node
#' @return feature: the feature selected to do the split
#' @return threshold: the threshold to make the split
#' @return impurity_reduction: the impurity decreased by the split if the decrease exists





classifier_split <- function(X, y, idxs, tree_num=0, sample_weight=NULL, max_features=NULL){
    # array indices
    SPLIT = 1
    LEFT = 2
    RIGHT = 3
    sweight = NULL
    if(is.null(sample_weight)==FALSE){
      sweight = sample_weight[idxs]
    }
    data = data.frame(X, y)
    stump = rpart(y~. , data = data[idxs,],control = rpart.control(maxdepth = 1))
    frame <- stump$frame
    frame[['gini']] = 1 - (frame[['dev']] / frame[['n']])^2 - (1 - frame[['dev']] / frame[['n']])^2
    result=frame[,c('var','n','dev','gini')]
    feature = result[1,1]
    impurity = result$gini/2  # divide 2 to follow the python result
    # the same as the impurity output from tree.DecisionTreeRegressor
    # (impurity = stump.tree_.impurity)
    n_node_samples = result$n[SPLIT]
    n_node_samples_left = result$n[LEFT]
    n_node_samples_right = result$n[RIGHT]
    threshold = stump$splits[1,4]
    idxs_left=rep(FALSE,nrow(X))
    idxs_right=rep(FALSE,nrow(X))
    idxs_split = idxs
    idxs_left[which(data.frame(stump$where)==2)]=TRUE
    # 2 denote the left 3 denote the right
    idxs_right[which(data.frame(stump$where)==3)]=TRUE
    value = stump$frame$yval2[,4]  # the proportion between class sample and total node samples
    impurity_reduction = stump$splits[1,3]/2  # divide 2 to follow the python rule result
    if(is.na(feature)==TRUE){
      print("no split found!")
    }
    node_split = list(idxs=idxs, value=value[SPLIT], impurity=impurity[SPLIT], tree_num=tree_num, n=n_node_samples, feature = feature, threshold = threshold, impurity_reduction = impurity_reduction)
    node_left = list(idxs=idxs_left, value=value[LEFT], impurity=impurity[LEFT], tree_num=tree_num,n=n_node_samples_left, feature = feature, threshold = threshold, impurity_reduction = impurity_reduction)
    node_right = list(idxs=idxs_right, value=value[RIGHT], impurity=impurity[RIGHT], tree_num=tree_num, n=n_node_samples_right, feature = feature, threshold = threshold, impurity_reduction = impurity_reduction)
    structure(list(call = match.call(),
                   node_split = node_split,
                   node_left = node_left,
                   node_right = node_right,
                   n=n_node_samples,
                   feature = feature,
                   threshold = threshold,
                   impurity_reduction = impurity_reduction),
              class = "node")
  }




#' figs fit for classification
#'
#' fitting function for fast interpretable greedy-tree sums on classification
#'
#'
#' @param X the design matrix
#' @param y the response vector
#' @param max_rules max total number of rules across all trees
#' @param min_impurity_decrease  A node will be split if this split induces a decrease of the impurity greater than or equal to this value.
#' @param sample_weight allocate weight for each individual sampe, array-like of shape n (default=None). If None, then samples are equally weighted.
#' @param verbose bool value, if TRUE print the split feature and threshold
#' @param max_features the number of features to consider when looking for the best split
#' @param feature_names the name of features in the data
#'
#'
#' @return call: the input setting
#' @return node_split: the design matrix for beta after variable splitting
#' node_split$idxs : the index of samples in the original node
#' node_split$value: the value for the split node
#' node_split$impurity: the impurity in the original node
#' node_split$tree_num: the tree number
#' @return node_left: the design matrix for gamma after variable splitting
#' node_split$idxs : the index of samples in the left node
#' node_split$value: the value for the left node after the split
#' node_split$impurity: the impurity in the left node
#' node_split$tree_num: the tree number which the node belongs to
#' @return node_right: the response vector after variable splitting.
#' node_split$idxs : the index of samples in the right node
#' node_split$value: the value for the right node after the split
#' node_split$impurity: the impurity in the right node
#' node_split$tree_num: the tree number which the node belongs to
#' @return trees: tree struction based on the result
#' @return n: the number of samples in the original node
#' @return feature: the feature selected to do the split
#' @return threshold: the threshold to make the split
#' @return impurity_reduction: the impurity decreased by the split if the decrease exists








figs.classifier <- function(X, y, max_rules = 12, feature_names=NULL, sample_weight=NULL, min_impurity_decrease=0.0, verbose=TRUE,  max_features=NULL){
  if(is.null(feature_names)==TRUE){
    feature_names= seq.int(ncol(X))
  }
  else{
    feature_names=feature_names
  }

  trees = list() # list of the root nodes of added trees
  complexity = 0   # tracks the number of rules in the model
  y_predictions_per_tree = list()  # predictions for each tree
  y_residuals_per_tree = list()  # based on predictions above

  # set up initial potential_splits
  # everything in potential_splits either is_root (so it can be added directly to self.trees_)
  # or it is a child of a root node that has already been added
  idxs = rep(TRUE,nrow(X))
  node_init <- classifier_split(X, y, idxs, tree_num=0, sample_weight=sample_weight, max_features=max_features)
  potential_splits = list(node_init)
  for (i in length(potential_splits)){
    comment(potential_splits[[i]]) <- "is_root"
  }

  finished = FALSE
  complexity =0
  while (length(potential_splits)>0 & finished==FALSE) {
    split_node = potential_splits[[1]]
    if (length(split_node)==1){
      split_node = split_node[[1]]
    }
    potential_splits = potential_splits[-1]
    if (split_node$impurity_reduction<min_impurity_decrease){
      finished = TRUE
      break
    }
    if (verbose==TRUE){
      cat("\n","adding node", split_node$feature,"<=", split_node$threshold)
    }
    complexity = complexity + 1
    # if added a tree root
    if (is.null(comment(split_node))==FALSE){
      # start a new tree
      # add the new trees
      trees = c(trees, list(split_node))
      # update tree_num
      split_node$node_split$tree_num = length(trees)
      split_node$node_left$tree_num = length(trees)
      split_node$node_right$tree_num = length(trees)

      # add new root potential node
      node_new_root = list(idxs = rep(TRUE,nrow(X)), tree_num=0)
      comment(node_new_root) <-"is_root"
      potential_splits <- c(potential_splits,list(node_new_root))
    }
    # add children to potential splits
    # potential_splits
    potential_splits <- c(potential_splits,list(split_node$node_left),list(split_node$node_right))

    # update predictions for altered tree
    for (tree_num in 1:length(trees)){
      prediction = rep(0,nrow(X))
      prediction[trees[[tree_num]]$node_left$idxs] = trees[[tree_num]]$node_left$value
      prediction[trees[[tree_num]]$node_right$idxs] = trees[[tree_num]]$node_right$value
      y_predictions_per_tree[tree_num] = list(prediction)
      y_predictions_per_tree[['new']] = rep(0,nrow(X))
    }

    for (tree_num in list(length(trees), 'new')){
      y_residuals_per_tree[[tree_num]] = ifelse(as.numeric(factor(y))==2,0,1)
      for (tree_num_other in list(length(trees), 'new')){
        if (tree_num_other != tree_num){
          y_residuals_per_tree[[tree_num]]= y_residuals_per_tree[[tree_num]]-y_predictions_per_tree[[tree_num_other]]
        }
      }
    }

    potential_splits_new = list()
    imp = list()
    for(i in 1:length(potential_splits)){
      if (i==1){
        y_target = y_residuals_per_tree[['new']]
      }else{
        y_target = y_residuals_per_tree[[potential_splits[[i]]$tree_num]]
      }
      # re-calculate the best split
      potential_split_updated = regressor_split(X=X,y=y_target,idxs = potential_splits[[i]]$idxs,tree_num = potential_splits[[i]]$tree_num, sample_weight = sample_weight)
      potential_splits_new=c(potential_splits_new,list(potential_split_updated))
    }
    for (i in 1:length(potential_splits_new)){
      imp = c(imp, list(potential_splits_new[[i]]$impurity_reduction))
    }
    new_split=potential_splits_new[which.max(imp)]
    # if(which.max(imp)==1){
    #   new_split[[1]]$tree_num = length(trees)+1
    #   new_split[[1]]$node_split$tree_num = length(trees)+1
    #   new_split[[1]]$node_left$tree_num = length(trees)+1
    #   new_split[[1]]$node_right$tree_num = length(trees)+1
    #   trees = c(trees, new_split)
    # }
    potential_splits=potential_splits[-which.max(imp)]
    # if (verbose==TRUE){
    #   cat("adding node", new_split[[1]]$feature,"<=", new_split[[1]]$impurity_reduction)
    # }
    if (complexity >= max_rules){
      finished = TRUE
      break
    }
    potential_splits=c(list(new_split),potential_splits)
  }

  structure(list(call = match.call(),
                 node_split = potential_splits,
                 complexity = complexity,
                 trees = trees
  ),
  class = "result")
}





#' figs fit for regression
#'
#' fitting function for fast interpretable greedy-tree sums on regression
#'
#'
#' @param X the design matrix
#' @param y the response vector
#' @param max_rules max total number of rules across all trees
#' @param min_impurity_decrease  A node will be split if this split induces a decrease of the impurity greater than or equal to this value.
#' @param sample_weight allocate weight for each individual sampe, array-like of shape n (default=None). If None, then samples are equally weighted.
#' @param verbose bool value, if TRUE print the split feature and threshold
#' @param max_features the number of features to consider when looking for the best split
#' @param feature_names the name of features in the data
#'
#' @return call: the input setting
#' @return node_split: the design matrix for beta after variable splitting
#' node_split$idxs : the index of samples in the original node
#' node_split$value: the value for the split node
#' node_split$impurity: the impurity in the original node
#' node_split$tree_num: the tree number
#'
#' @return node_left: the design matrix for gamma after variable splitting
#' node_split$idxs : the index of samples in the left node
#' node_split$value: the value for the left node after the split
#' node_split$impurity: the impurity in the left node
#' node_split$tree_num: the tree number which the node belongs to
#' @return node_right: the response vector after variable splitting.
#' node_split$idxs : the index of samples in the right node
#' node_split$value: the value for the right node after the split
#' node_split$impurity: the impurity in the right node
#' node_split$tree_num: the tree number which the node belongs to
#' @return trees: tree struction based on the result
#' @return n: the number of samples in the original node
#' @return feature: the feature selected to do the split
#' @return threshold: the threshold to make the split
#' @return impurity_reduction: the impurity decreased by the split if the decrease exists
#'
#'

figs.regressor <- function(X, y, max_rules = 12, feature_names=NULL,verbose=TRUE, sample_weight=NULL, min_impurity_decrease= 0.0, max_features=NULL){
  if(is.null(feature_names)==TRUE){
    feature_names= seq.int(ncol(X))
  }
  else{
    feature_names=feature_names
  }

  trees = list() # list of the root nodes of added trees
  complexity = 0   # tracks the number of rules in the model
  y_predictions_per_tree = list()  # predictions for each tree
  y_residuals_per_tree = list()  # based on predictions above

  # set up initial potential_splits
  # everything in potential_splits either is_root (so it can be added directly to self.trees_)
  # or it is a child of a root node that has already been added
  idxs = rep(TRUE,nrow(X))
  node_init <-regressor_split(X,y,idxs,tree_num=0, sample_weight=sample_weight, max_features=max_features)
  potential_splits = list(node_init)
  for (i in length(potential_splits)){
    comment(potential_splits[[i]]) <- "is_root"
  }

  finished = FALSE
  complexity =0
  a=1 # dummy parameter
  while (length(potential_splits)>0 & finished==FALSE) {
    split_node = potential_splits[[1]]
    if (length(split_node)==1){
      split_node = split_node[[1]]
    }
    potential_splits = potential_splits[-1]
    if (split_node$impurity_reduction<min_impurity_decrease){
      finished = TRUE
      break
    }
    if (verbose==TRUE){
      cat("\n","adding node", split_node$feature,"<=", split_node$threshold)
    }
    complexity = complexity + 1
    # if added a tree root
    if (is.null(comment(split_node))==FALSE){
      # start a new tree
      # add the new trees
      trees = c(trees, list(split_node))
      # update tree_num
      split_node$node_split$tree_num = length(trees)
      split_node$node_left$tree_num = length(trees)
      split_node$node_right$tree_num = length(trees)
      # add new root potential node
      node_new_root = list(idxs = rep(TRUE,nrow(X)), tree_num=0)
      comment(node_new_root) <-"is_root"
      potential_splits <- c(potential_splits,list(node_new_root))
    }
    # add children to potential splits
    # potential_splits
    potential_splits <- c(potential_splits,list(split_node$node_left),list(split_node$node_right))

    # update predictions for altered tree
    for (tree_num in 1:length(trees)){
      prediction = rep(0,nrow(X))
      prediction[trees[[tree_num]]$node_left$idxs] = trees[[tree_num]]$node_left$value
      prediction[trees[[tree_num]]$node_right$idxs] = trees[[tree_num]]$node_right$value
      y_predictions_per_tree[tree_num] = list(prediction)
      y_predictions_per_tree[['new']] = rep(0,nrow(X))
    }

    for (tree_num in list(length(trees), 'new')){
      y_residuals_per_tree[[tree_num]] = y
      for (tree_num_other in list(length(trees), 'new')){
        if (tree_num_other != tree_num){
          y_residuals_per_tree[[tree_num]]= y_residuals_per_tree[[tree_num]]-y_predictions_per_tree[[tree_num_other]]
        }
      }
    }

    potential_splits_new = list()
    imp = list()
    for(i in 1:length(potential_splits)){
      if (i==1){
        y_target = y_residuals_per_tree[['new']]
      }else{
        y_target = y_residuals_per_tree[[potential_splits[[i]]$tree_num]]
      }
      # re-calculate the best split
      potential_split_updated = regressor_split(X=X,y=y_target,idxs = potential_splits[[i]]$idxs,tree_num = potential_splits[[i]]$tree_num,sample_weight = sample_weight)
      potential_splits_new=c(potential_splits_new,list(potential_split_updated))
    }
    for (i in 1:length(potential_splits_new)){
        imp = c(imp, list(potential_splits_new[[i]]$impurity_reduction))
    }
    new_split=potential_splits_new[which.max(imp)]
    if(which.max(imp)==1){
      new_split[[1]]$tree_num = length(trees)+1
      new_split[[1]]$node_split$tree_num = length(trees)+1
      new_split[[1]]$node_left$tree_num = length(trees)+1
      new_split[[1]]$node_right$tree_num = length(trees)+1
      trees = c(trees, new_split)
    }
    potential_splits=potential_splits[-which.max(imp)]
    # if (verbose==TRUE){
    #   cat("adding node", new_split[[1]]$feature,"<=", new_split[[1]]$impurity_reduction)
    # }
    if (complexity >= max_rules){
      finished = TRUE
      break
    }
    potential_splits=c(list(new_split),potential_splits)
  }

  structure(list(call = match.call(),
                 node_split = potential_splits,
                 complexity = complexity,
                 trees=trees
                 ),
            class = "result")
}



figs.predict <- function(X){

}

