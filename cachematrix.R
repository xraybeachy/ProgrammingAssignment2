## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     ## This program calculates the inverse of a given square and 
     ## inversible matrix. The solution is stored in cache, so if 
     ## the same matrix is calculated a second time, the solution will be 
     ## taken directly from cache. 
     ## For this reason, both, makeCacheMatrix.R and cacheSolve.R have
     ## to be sourced into R. 
     
     ## makeCacheMatrix has to be called first, before cacheSolve 
     
     ## a <- makeCacheMatrix(matrix(1:4,2,2))
     
     ## creates a list with the 4 functions (set, get, setinvert, getinvert)
     ## the second call of cacheSolve will solve the defined matrix in a. 
     
     ## cacheSolve(a) 
     
     ## A second call of cacheSolve(a) will give the additional message 
     ## that the data is from the cached memory. 
     
     ## Defining the inverse matrix to NULL
     
     inv <- NULL
     
     ## Defining the function set. Set will set the matrix which has to be 
     ## solved and set the inverse to NULL, indicating that the matrix 
     ## hasn't been solved before. 
     
     set <- function(y =matrix()) {
          x <<- y
          inv <<- NULL
     }
     
     ## Defining the function get. it returns the defined matrix, defined by
     ## set. 
     get <- function() x 
     
     ## Defines the function setinvert, which will save the solved matrix 
     ## in the variable inv 
     setinvert <- function(solve) inv <<- solve
     
     ## Defines the function getinvert. If called, it will give the 
     ## solution of the inverted matrix. 
     getinvert <- function() inv 
     
     ## creates a list with the different functions, which can be called 
     ## by other programms.
     list(set = set, get = get, 
          setinvert = setinvert,
          getinvert = getinvert)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     
     ## This program calculates the inverse of a given square and 
     ## inversible matrix. The solution is stored in cache, so if 
     ## the same matrix is calculated a second time, the solution will be 
     ## taken directly from cache. 
     ## For this reason, both, makeCacheMatrix.R and cacheSolve.R have
     ## to be sourced into R. 
     
     ## makeCacheMatrix has to be called first, before cacheSolve 
     
     ## a <- makeCacheMatrix(matrix(1:4,2,2))
     
     ## creates a list with the 4 functions (set, get, setinvert, getinvert)
     ## the second call of cacheSolve will solve the defined matrix in a. 
     
     ## cacheSolve(a) 
     
     ## A second call of cacheSolve(a) will give the additional message 
     ## that the data is from the cached memory. 
     
     ## Defines the inv variable with the stored value from makeCacheMatrix. 
     inv <- x$getinvert()
     
     ## Checks if inv is defined or NULL. If it is not NULL, it was already 
     ## calculated and the solution can be extracted from cache. The program 
     ## returns the inv variable and exits.
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     
     ## Read the matrix that has to be solved
     data <- x$get()
     
     ## solve/inverse the matrix and write it to inv.  
     inv <- solve(data)
     
     ## save the solution in cache, so it can be called upon. 
     x$setinvert(inv)
     
     ## write solution.
     inv
}
