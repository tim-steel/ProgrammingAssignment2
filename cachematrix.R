## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#function to create 4 sub functions returned as a list
#sub functions are used to interact with scope variables
makeCacheMatrix <- function(x = matrix()) {
    #Declare inv as Null
    inv <- NULL
    set <- function(y) {
        #Allows new matrix to be set in function
        #clears previous inv value
        x <<- y
        inv <<- NULL
    } 
    #allow retrival of matrix
    get <- function() x
    #enable setting inverse of x
    setInv <- function() inv <<-solve(x)
    #enable retrival of inverse of x
    getInv <- function() inv
    
    #Generate variables accessabe from 
    # a different environment
    list(set=set, get=get,
         setInv=setInv,
         getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInv(m)
    m
}
