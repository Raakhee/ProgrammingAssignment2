## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     m_inv <- NULL
     setmat <- function(y) {
     	x <<- y
     	m_inv <<- NULL
     }
     stored_mat <<- x
     #print("stored mat is") 
     #print(stored_mat)
      
     getmat <- function() x
     setinv <- function(mat_inv) m_inv <<- mat_inv
     getinv <- function() m_inv
     print("inside makeCacheMatrix")
     list(setmat = setmat,  setinv =setinv, getmat=getmat, getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinv()
        dat_mat <- x$getmat()
        
        #print("stored mat is")
        #print(stored_mat)
        #print("dat mat is")
        #print(dat_mat)
        if ((!is.null(m_inv)) && (dat_mat == stored_mat)){
        	print("getting cached info")
        	return(m_inv)
        }
        
        if (ncol(dat_mat) == nrow(dat_mat)) {
       			 m_inv <- solve(dat_mat) 	
        }
        else{
        		m_inv <- ginv(dat_mat)
        }
        
        x$setinv(m_inv)
        #print(m_inv)
        m_inv 
}

