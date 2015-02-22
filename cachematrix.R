## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
	## Store the inverse
	inv<-NULL
	## set alter the matrix and it invalidates the cache
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	## To return the raw matrix
	get<-function(){
		x
	}
	## setinv sets the inv variable, used only by cacheSolve
	setinv<-function(i){
		inv<<-i
	}
	## And it gets the cached inverse
	getinv<-function(){
		inv
	}
	## Return the special "matrix"
	list(set=set,
		get=get,
		setinv=setinv,
		getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated (and
## the matrix has not changed), then the cacheSolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){
        	##If the inverse us cached, return it
        	message("getting cached inverse")
        	return(inv)
        }
        ## In other circumstances, calculate the inverse and cache it
        matr<-x$get()
        inv<-solve(matr,...)
        x$setinv(inv)
        return(inv)
}

