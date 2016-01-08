## makeCacheMatrix creates a list of four functions:
## 1/ set()
## 2/ get()
## 3/ setInverse()
## 4/ getInverse()
## creates an object that contains a matrix 
## and may contain its inverse (if it's been calculated)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}

# Some sample output
#
# > mat = matrix(c(1:4, 1:5), nrow=3, ncol=3)
# > matObj = makeCacheMatrix(mat)
# > matObj$get()
# [,1] [,2] [,3]
# [1,]    1    4    3
# [2,]    2    1    4
# [3,]    3    2    5
# > cacheSolve(matObj)
# [,1]  [,2]   [,3]
# [1,] -0.375 -1.75  1.625
# [2,]  0.250 -0.50  0.250
# [3,]  0.125  1.25 -0.875
# > cacheSolve(matObj)
# getting cached data
# [,1]  [,2]   [,3]
# [1,] -0.375 -1.75  1.625
# [2,]  0.250 -0.50  0.250
# [3,]  0.125  1.25 -0.875
#
#Note the message, "getting cached data", on he second run of cacheSolve()
#
#Proving we indeed have the inverse:
# > matObj$getInverse() %*% matObj$get()
# [,1] [,2] [,3]
# [1,]  1.000000e+00    0    0
# [2,] -1.110223e-16    1    0
# [3,]  0.000000e+00    0    1



