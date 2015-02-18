#makeCacheMatrix is a function to create 4 sub functions returned 
##as a list.
##sub functions are used to interact with scope variables
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
    setInv <- function(x) inv <<-solve(x)
    #enable retrival of inverse of x
    getInv <- function() inv
    
    #Generate variables accessabe from 
    # a different environment
    list(set=set, get=get,
         setInv=setInv,
         getInv=getInv)
}


## cacheSolve returns the inverse of the previous functions matrix
## If the inverse has already been set, it is collected from the cache
## If the inverse has not been set, it calculates it
## An inversed matrix is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # if one has been set in cache
    m <- x$getInv()
    
    ##If inverse in cache -> get cached version
    if(!is.null(m)) {
        message("getting cached data")
    } else {
        ## Inverse not in cache!
        ## Set inversion in cache, also allocate to m
        m <- x$setInv(x$get())
    }
    
    # make cache inversion available from this function
    return(m)
}
