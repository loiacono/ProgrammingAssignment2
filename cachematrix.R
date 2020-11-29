## Put comments here that give an overall description of what your
## functions do
#' ------------------------------------------------------
#'
#' Script name: cachematrix.R
#'
#' Purpose of script: Functions that cache potentially time-consuming computations.
#' In particular the inverse of square matrix is calculated and cached.
#' The assignment you will take advantage of the scoping rules of the R language and
#' how they can be manipulated to preserve state inside of an R object.
#'
#' Author: Luca Lo Iacono
#'
#' Date Created: 2020-11-29
#'
#' Email: luca.loiacono@tiscali.it
#'
#' ------------------------------------------------------
#' Notes:
#'
#'
#'
#' ------------------------------------------------------


## Write a short comment describing this function

#' makeCacheMatrix
#' @description This function creates a special "matrix" object that can cache its inverse.
#'
#' @param x is a square invertible matrix
#'
#' @return matrix object
#'
#' @export
#'
#' @examples
#'
makeCacheMatrix <- function(x = matrix()) {
  if (!matrixcalc::is.square.matrix(x))
    return(NULL)
  
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(invx) inverse <<- invx
  
  getinverse <- function() inverse
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

#' cacheSolve
#'
#' @description This function computes the inverse of the special "matrix" returned
#' by makeCacheMatrix above. If the inverse has already been calculated
#' (and the matrix has not changed), then the cachesolve should retrieve the inverse
#' from the cache.
#'
#' @param x is a square invertible matrix
#'
#' @return matrix object
#'
#' @export
#'
#' @examples
#'
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
