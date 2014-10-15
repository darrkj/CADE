



# TODO: guid could be character, {19f3lt:df2fs42f:e12r53tad:...} a la sure view,
# this will work for most text file type unique identifiers.
#is.guid <- function(x) {
#  is.integer(x) & length(unique(x)) == length(x)
#}

# TODO: Add method to use different classifier
# TODO: Should bounds on uniform be scaled past min/max range
# prop allows you to throttle the 50/50 split, higher means more fake data.
# TODO: cade add ability to work with data that already has a y variable

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
  #TODO: get better na action
  df <- df[complete.cases(df), ]
  
  ids <- df[, skip]
  real <- if (length(skip) > 0) df[, -(which(names(df) %in% skip))] else df
  
  # Take out any id fields
  # real <- df[, !unlist(lapply(df, is.guid))]
  
  # create similar but uniform data
  fake <- as.data.frame(lapply(real, function(x) uniform(x, length(x) * prop)))
  
  # Label the data you started with as negative outcomes.
  real$y <- 0
  # Label the fake data as the positive outcomes.
  fake$y <- 1
  
  # Combine both the real and the fake data.
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



