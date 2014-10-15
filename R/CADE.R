
# Would it make more sense to have this as generic function and
# dispatch the results based on type, uni.character vs uni.integer
uni <- function(x, len = length(x)) {
  # stopifnot(is.vector(x)) # Does not work for factors, not atomic type.
  if ( is.integer(x) ) {
    sample(min(x):max(x), len, replace = TRUE)
  } else if ( is.numeric(x) ) {
    runif(len, min(x), max(x))
  } else if ( is.factor(x) ) {
    factor(sample(levels(x), len, replace = TRUE))
  } else {
    sample(unique(x), len, replace = TRUE)
  }
}

# TODO: guid could be character, {19f3lt:df2fs42f:e12r53tad:...} a la sure view,
# this will work for most text file type unique identifiers.
is.guid <- function(x) {
  is.integer(x) & length(unique(x)) == length(x)
}

# TODO: Add method to use different classifier
# TODO: Should bounds on uniform be scaled past min/max range
# prop allows you to throttle the 50/50 split, higher means more fake data.


#' The main CADE function
#'
#' This function calls and runs CADE
#' @param df The data frame in question
#' @param prop The proportion of true to false cases
#' @param numTree The number of tree to create in the random forest
#' @param skip Fields that should not be included 
#' @keywords anonaly detection
#' @export
#' @examples
#' cade(cars)

cade <- function(df, prop = 1, numTree = 500, skip = c()) {
  stopifnot(is.data.frame(df))
  
  ids <- df[, skip]
  real <- if (length(skip) > 0) df[, -(which(names(df) %in% skip))] else df
  
  # Take out any id fields
  # real <- df[, !unlist(lapply(df, is.guid))]
  
  # create similar but uniform data
  fake <- as.data.frame(lapply(real, function(x) uni(x, length(x) * prop)))
  
  # label brown dots
  real$y <- 0
  # lable red dots
  fake$y <- 1
  
  # add red dots and brown dots
  data <- rbind(real, fake)
  
  library(randomForest)
  # build classifier
  tree <- randomForest(as.factor(y) ~ ., data = data, ntree = numTree, 
                       importance = TRUE)
  
  vars <- names(attr(tree$terms, 'dataClasses')[-1])
  prop <- tree$importance[, 1] / sum(tree$importance[, 1])
  
  # the classifier probabilities
  # TODO: save time only predict real cases
  df$prob <- predict(tree, newdata = data, type = 'prob')[1:nrow(real), 2]
  df$prob <- df$prob / (1 - df$prob)
  
  prop <- prop * df[, vars]
  names(prop) <- paste('prop.', names(prop), sep = '')
  
  df <- cbind(df, prop)
  df <- df[df$prob > 0, ]
  df
}

#load('data/stats.rda')
#data <- cade(stats, skip = c('player', 'date', 'guid'))

#d <- reshape(data = data, idvar = c('player', 'date', 'guid'), 
#             varying = list(names(data)[c(2:16, 19, 22:38)]), direction = 'long')
#row.names(d) <- NULL

