#######Peer Graded Programming Assignment####
###Lexical Scoping######
setwd("C:/Users/kthapa.MAGUIRE/Desktop/Coursera-R  Programming")
getwd()

####Creating special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x =is.matrix()) {
        inverse_x <- NULL ###Clears cache
        set <- function(y) {
                x <<- y
                inverse_x <<- NULL
        }
        get <- function() x ###matrix value to get from
        setInverse <- function(inverse) inverse_x <<- inverse ###Setting inverse
        getInverse <- function() inverse_x ####Getting the inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse) 
}

####Computing inverse of the special matrix
cacheSolve <- function(x=is.matrix()) {
        inverse_x <- x$getInverse()
        if(!is.null(inverse_x)) {
                message("getting cached data")
                return(inverse_x)
        }
        data<-x$get()
                inverse_x <- solve(x$get()) ###Solve for inverse of a square Matrix
        x$setInverse(inverse_x)
        inverse_x ####Return the inverse matrix
        }

######TEST########################
test_matrix<-makeCacheMatrix(matrix(1:4, 2,2))
test_matrix$get()
test_matrix$getInverse()
cacheSolve(test_matrix)

