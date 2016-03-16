## Matrix inversion can be a costly calculation and caching the inverse of the matrix is a better alternative
## The makeCasheMatrix and cacheSolve functions creates special matrix object
## and caclulates the inverse and stores it to the cache 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m<- NULL

##Set Function to store matrix
set <- function(y) {
  x <<- y
  m <<- NULL
}

##Get Function to pull value from Set Function
get <- function() x

##SetInvMatrix Function to set inverse matrix
SetInvMatrix <- function(solve) m <<- solve

##GetInvMatrix Function to get inverse matrix
GetInvMatrix <- function() m

##List of Functions and return or set value by calling one of the functions in the list
list(set = set, get = get,
     SetInvMatrix = SetInvMatrix,
     GetInvMatrix = GetInvMatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the 
##inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##Call GetInvMatrix from makeCacheMatrix function and see if value is null
  m <- x$GetInvMatrix()
  ##If it is not null then pull cache matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##Else Get the Matrix and calculate the Inverse Matrix
  data <- x$get()
  m <- solve(data, ...)
  x$SetInvMatrix(m)
  ##Return Inverse Matrix
  m
  
}
