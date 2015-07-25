## This function creates a special "matrix" object that can cache its inverse

## parameters:
## x: matrix or a R object than may be coerced to matrix
## returns a list with four functions:
##      get():  returns matrix object 
##      set():  overrides matrix object and recompute inverse matrix
##      getSolve(): returns inverse matrix (calculated using solve()) or NA
##      computeSolve(): calculta inverse matrix. If matrix is a singular matrix returns NA
makeCacheMatrix <- function(x = matrix()) {
        
        ##create cache inverse matrix object. Initial NULL value means 'not calculated'
        cacheInvMatrix <- NULL
        
        ##function that returns cached matrix 
        get <- function() x
        
        ##function that overrides cached matrix and recompute inverse matrix
        set <- function(m) 
        {
                ##assign argument to x or try to coerce to matrix
                if( is.matrix(m) )
                {
                        x <<- m    
                }
                else
                {
                        x <<- as.matrix(m)
                }
                
                ##recompute inverse matrix
                computeSolve()
        }
        
        ##function that obtains cached inverse matrix
        getSolve <- function() cacheInvMatrix
        
        ##function that compute inverse of square matrix and cache it
        ##returns NA if can't solve 
        computeSolve <- function()
        {
                ##assign indetermined default value
                cacheInvMatrix <<- NA
                
                ##validate if x object is a matrix
                if( is.matrix(x) )
                {
                        ##validate if matrix is square    
                        if( nrow(x) == ncol(x) )
                        {
                                ##validate if is a numeric matrix
                                if ( length(x[sapply(x, function(y) is.numeric(y))]) > 0 )
                                {
                                        ##if matrix is not singular (determinant is different of zero)
                                        ##can calculate inverse matrix
                                        if( det(x) != 0 )
                                        {
                                                cacheInvMatrix <<- try(solve(x), silent = TRUE) 
                                        }
                                        else
                                        {
                                                message("matrix is singular, cannot compute inverse matrix.")
                                        }
                                }
                                else
                                {
                                        message("matrix is not numeric, cannot compute inverse matrix.")
                                }
                        }
                        else
                        {
                                message("matrix is not square, cannot compute inverse matrix.")
                        }
                }
                else
                {
                        message("object is not a matrix, cannot compute inverse matrix.")
                }
        }
        
        ##return list with functions
        list(get = get, set = set, getSolve = getSolve, computeSolve = computeSolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

## parameters:
## x: special "matrix" object created calling makeCacheMatrix() function
cacheSolve <- function(x, ...) {
        ##get inverse matrix from cache
        iMatrix <- x$getSolve()
        
        ##If not cached, compute solve() for matrix 
        if(is.null(iMatrix))
        {
                x$computeSolve()
                iMatrix <- x$getSolve()
        }
        
        ##return inverse matrix
        iMatrix
}
