## Creates a cached matrix where the inverse is calculated only 1 time.
makeCacheMatrix <- function(m) {
        #inverse variable, used by cache
        i <- NULL
        #function that sets the matrix value.
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
        #gets the matrix
        get <- function() m
        #sets the inverse value
        setinv <- function(inverse) i <<- inverse
        #gets the inverse value
        getinv <- function() i
        #lists the functions...
        list(set=set,  get=get, setinv=setinv, getinv=getinv)
}

## Write a short comment describing this function
cacheSolve <- function(m, ...) {
        #gets the inverse from the matrix
        i <- m$getinv();
        #if the inverse is not null, the inverse is returned
        if(!is.null(i)) {
                message("getting cached data");
                return(i);
        }
        #else, the inverse is calculated using the solve function...
        data <- m$get();
        i <- solve(data, ...);
        #the matrix will receive the inverse value...
        m$setinv(i);
        #the inverse is returned.
        i;
}

