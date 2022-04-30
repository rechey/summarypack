#' @export
#' The goal is to re-create the summary function native to R.
#'
#'
#' Remove NAs

#' @param X
#' @param x

RemoveNA <- function(X, x) {
  x <- X$x
  x <- x[!is.na(x)] #removes any NA's from the column
  return (x)
}
#'
#'
#' @param a
#' @param b
#'
#' @return
#'
#'
#' miniMin - Helper Function to myMin
#' @export
miniMin <- function(a, b) {
  +if (a < b) {
    +return(a)
  }
    else return(b)
}
#' myMin - Finds the Minimum Value
#'
#' @param v
myMin <- function(v) {
  if (length(v) == 1) {
    +return(v)
  }
   return(miniMin(head(v, 1), myMin(tail(v,-1))))
}

#' myMax - Finds the Maximum Value
#'
#' @param v
#'
#' @return
#' @export
#'
#' @examples
#'
myMax <- function(v) {
  tmax<- head(v,1)
  for (i in seq(2,length(v))){
    if (tmax<v[i]) {tmax <-v[i]}}
  return(tmax)}

#' myMean - Find the Mean
#'
#' @param v
#'
#' @return
#' @export
#'
myMean <- function(v) {
  +m <- sum(v) / length(v)
  +     return(m)
}

#' This function is called mySummary. mySummary takes as input a data set
#' expressed as a vector and returns the Min, Mean, and
#' Max of the vector.
#'
#'
#' @export
#' @param v
mySummary <- function(v) {
  print(paste("Min is", as.character(myMin(v))))
  print(paste("Max is", as.character(myMax(v))))
  print(paste("Mean is", as.character(myMean(v))))
}
#'
#'

