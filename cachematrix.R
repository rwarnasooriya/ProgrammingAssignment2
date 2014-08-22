## makeCacheMatrix :
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
        
        m <- NULL    
        
        ## Set the matrix
        set <- function( matrix ) {    
                x <<- matrix         
                m <<- NULL     
        }        
        
        ## Get the matrix
        get <- function() {
                x        
        }
        
        ## Set the inverse of the matrix
        setInverse <- function(solve) {                                
                m <<- solve                  
        }
        
        ## Get the inverse of the matrix
        getInverse <- function() {
                m
        }
        
        ## Return the object with its Methods
        list(get = get, setInverse = setInverse, getInverse = getInverse) 
}


## cacheSolve :
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Return a matrix that is the inverse of 'x'
        ## if it is already in the cache
        if(!is.null(m)) {                              
                message("getting cached data")  
                return(m)                                     
        }
        
        ## Calculate the inverse of the matrix
        ## as it is not set in the cache
        data <- x$get()        
        m <- solve(data, ...) 
        
        ## Set the inverse of the matrix to the object 'x'
        x$setInverse(m) 
        
        ## Return the inverse of the matrix
        m                    
}