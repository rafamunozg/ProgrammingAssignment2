## Coursera - R Language - ProgrammingAssignment2 resolution
## Following functions will cache the solution of a matrix and will reuse it
## in future computations.

## Usage: Long path to show better the provided functionality
## 1. Create a matrix >> n <- matrix(round(runif(9,20,30)), 3, 3)
##    n is now a 3x3 matrix with random round numbers between 20 and 30
## 2. Create the cached matrix >> cacheN <- makeCacheMatrix(n)
##    cacheN is now an object that can set/get properties of any matrix, which is now containing n
## 3. Calculate the inverse of n, using the chached object >> cacheSolve(cacheN)
##    First time use, so it'll calculate the value
## 4. Try it again >> cacheSolve(cacheN)
##    Returned value should show the message "Returning data from cache"
##    You save some computation cycles!!!


## makeCacheMatrix: Sets a matrix and its inverse value in memory as a cached object
## Parameters:
##      x: matrix which is assumed to be square and invertible
## Methods:
##      get/set: retrieves/stores the matrix data in the object
##      getinverse/setinverse: retrieves/stores the inverse the matrix

makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  
  set <- function(y){
    x <<- y
    minverse <<- NULL    
  }
  
  get <- function() {x}
  
  setinverse <- function(inverse) {
    minverse <<- inverse
  }
  
  getinverse <- function(){
    minverse
  }

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: Created to interact with the makeCacheMatrix function. 
##    uses a makeCacheMatrix object and checks if the inverse is already cached,
##    if so it'll printed, otherwise it'll calculate it and cach it for the future
## Parameters:
##      x: makeCacheMatrix object
     

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  result <- x$getinverse()
  if (!is.null(result)){
    message("Returning data from cache")
    return(result)
  }
  
  data <- x$get()
  rinverse <- solve(data)
  x$setinverse(rinverse)
  rinverse
}
