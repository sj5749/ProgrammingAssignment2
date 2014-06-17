##makeCacheMatrix function is a list of 4 sub functions 

makeCacheMatrix <- function(x = matrix()) {   #initialize
  m <- NULL
  set <- function(y) {
    x <<- y        # using special assignment operator to store the value in different environment
    m <<- NULL
  }
  get <- function()
    x
  setinv <- function(solve) 
    m <<- solve
  getinv <- function() 
    m
  list(set = set, get = get,   # list of four subfunctions
       setinv = setinv,
       getinv = getinv)
}

## CacheSolve function will check whether the data exist in the cache memory
## or not and proceeds accordingly

cacheSolve <- function(x, ...) {            #initialize 
  m <- x$getinv()
  if(!is.null(m)) {     # checking for null value            
    message("getting cached data")
    return(m)
  }
  data <- x$get()       # getting the data to calculate inverse
  m <- solve(data, ...) # solve function to get the inverse of the matrix  
  x$setinv(m)
  m                     # printing the inverted matrix 
}