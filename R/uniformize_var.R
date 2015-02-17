
#' Create uniform distribution
#' 
#' This function will take an input and create a similar output that
#' is distributed over the same type.
#' 
#' @author Kenny Darrell
#' @param x The vctor of data
#' @keywords cade
#' @export


uniform <- function(x, ...) {
  UseMethod("uniform", x)
}


#' Default version of uniform
#' 
#' @author Kenny Darrell
#' @param x The vctor of data
#' @keywords cade
#' @export

uniform.default <- function(x, len = length(x)) {
  sample(unique(x), len, replace = TRUE)}


#' Integer version of uniform
#' 
#' @author Kenny Darrell
#' @param x The vctor of data
#' @keywords cade
#' @export

uniform.integer <- function(x, len = length(x)) {
  sample(min(x):max(x), len, replace = TRUE)
}

#' Numeric version of uniform
#' 
#' @author Kenny Darrell
#' @param x The vctor of data
#' @keywords cade
#' @export

uniform.numeric <- function(x, len = length(x)) {
  runif(len, min(x), max(x))
}


#' Factor version of uniform
#' 
#' @author Kenny Darrell
#' @param x The vctor of data
#' @keywords cade
#' @export

uniform.factor <- function(x, len = length(x)) {
  factor(sample(levels(x), len, replace = TRUE))
}




