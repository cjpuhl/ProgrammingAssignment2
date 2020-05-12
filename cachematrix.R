## Matrix inversion is usually a costly computation and 
##there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matriz = matrix()) {
        invMatriz <- NULL
        set <- function(newMatriz) {
                matriz <<- newMatriz
                invMatriz <<- NULL
        }
        get <- function() matriz
        setInv <- function(inv) invMatriz <<- inv
        getInv <- function() invMatriz
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(matriz, ...) {
        invMatriz <- matriz$getInv()
        data <- matriz$get()
        if(!is.null(invMatriz)) {
                message("getting cached data")
                return(invMatriz)
        }else if (nrow(data) == ncol(data) && det(data)!=0){
                invMatriz = solve(data)
                matriz$setInv(invMatriz)
        }else print("Non invertible Matrix")
}
