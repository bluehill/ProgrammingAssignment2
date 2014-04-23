#overall Description: A function that creates a special "matrix" object that can cache its inverse. 
#(Matrix inversion is usually a costly computation and there may be some benefit to caching the 
#inverse of a matrix rather than computing it repeatedly)


## computing the inverse of a square matrix with the solve function: 

makeCacheMatrix <- function(x = matrix()) {    #defined by R Peng
        m <- NULL                              #define matrix to NULL
        set <- function(y) {                   #clear any info 
                x <<- y
                m <<- NULL
        }
        get <- function() x                     #return matrix
        setsolve <- function(solve) m <<- solve    #inverse matrix
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        

}


## Check and return the value of the makeCacheMatrix function above:

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' based on cachemean:
        m <- x$getsolve() #query the matrix cache
        if(!is.null(m)) {   #if there is a cache
                message("getting cached data") 
                return(m)        #print the cache, no computation
        }
        data <- x$get()    #if there is no cache
        m <- solve(data, ...) #calculate the inverse matrix
        x$setsolve(m)    #save the result back to x's cache
        m               #print result to screen
        
}

# test code courtesy of Fu Sheng Wang at:
#https://class.coursera.org/rprog-002/forum/thread?thread_id=696
 a <- makeCacheMatrix(matrix(1:4,2))

         a$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4

         a$getsolve()
NULL

         a$set(matrix(5:8,2))
 a$get()
[,1] [,2]
[1,]    5    7
[2,]    6    8
 
         cacheSolve(a)
[,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
 
         cacheSolve(a)
getting cached data
[,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
 
         a$getsolve()
[,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5

        
         #test inverse correctness
         b = a$getsolve()
 a$get() %*% b     #matrix multiplication should show identity matrix
[,1]         [,2]
[1,]    1 3.552714e-15
[2,]    0 1.000000e+00
 