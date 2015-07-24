## Progrmming Assignment 2

## The purpose of this function is to create a special matrix. It includes 
## a list containing functions to set and get the value of the matrix, 
## and to set and get the value of the inverted matrix.
## Example
##  a <- makeCacheMatrix(c(2,1,3))
##  a is an object, and it contains functions bound to the environment in which a was created. 
##
## The list function makes it possible to store four functions;
## to call those functions you can subset the main function, by using a format of the type
## <main function>$<function><arguments>
## Example:
## makeCacheMatrix$set(a)
##
## These are the functions:
## get returns the matrix x stored in the makeCacheMatrix function
## set changes the matrix x stored in the makeCacheMatrix function
## and sets to NULL the value of inv_mat stored in the makeCacheMatrix function
## get_inv_mat returns the inv_mat (inverted matrix) stored in the makeCacheMatrix function
## set_inv_mat changes the inv_mat (inverted matrix) stored in the makeCacheMatrix function
##
## Because R uses lexical scoping, if a variable is not defined within the local function, 
## it will search next for a value of the variable in the environment where the function 
## (set, get, etc) was defined.
##
## If the operator <<- is used to assign a value to a variable, a search is made through parent 
## environments for an existing definition of the variable. If such a variable is found
## then its value is redefined, otherwise the assignment takes place in the global environment.
## 

makeCacheMatrix <- function(x = numeric()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  set_inv_mat <- function(inverted_mat) inv_mat <<- inverted_mat
  get_inv_mat <- function() inv_mat
  list(set = set, get = get,
       set_inv_mat = set_inv_mat,
       get_inv_mat = get_inv_mat)
}

## The purpose of this function is to calculate the inverted matrix from 
## the special matrix that was created with makeCacheMatrix.
## The object in which the result of makeCacheMatrix is stored
## can be passed to this function in the form cacheSolve(a)
## This function first checks if the inverted matrix was previously calculated.
## If that is the case, it gets the inverted matrix from the cache.
## Otherwise, it gets the matrix stored in makeCacheMatrix, calculates the 
## inverted matrix and sets the value in the cache via the set_inv_matrix function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$get_inv_mat()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$set_inv_mat(inv_mat)
  inv_mat
}
