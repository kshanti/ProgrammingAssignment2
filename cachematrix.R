#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
	#initiliaise variable 't'
        t = NULL

	#set the matrix
        set = function(y) {
                
                x <<- y
                t <<- NULL
        }

	#get the matrix
        get = function() x

	#set the inverse
        sett = function(inverse) t <<- inverse

	#get the inverse
        gett = function() t

	#list to serve as imput to the cache solve function
        list(set=set, get=get, sett=sett, gett=gett)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        t = x$gett()
        
        # if the inverse has already been calculated
        if (!is.null(t)){
                # get it from the cache
                message("getting data which has been cached")
                return(t)
        }
        
        # else calculate the inverse 
        data = x$get()
        t = solve(data, ...)
        
        # sets the value of the inverse
        x$sett(t)
        
        t
}

