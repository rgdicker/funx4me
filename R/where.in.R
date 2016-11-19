#' @export
where.in <- function(bigvec,smallvec,fullvec=F){
    # Where in bigvec is smallvec?
    # So e.g. where.in(bigvec[10:12],bigvec) == 10:12
    N <- length(bigvec)
    n <- length(smallvec)
    b <- 1:n
    if(is.integer(bigvec) & !is.integer(smallvec))
        bigvec <- as.numeric(bigvec)
    if(!is.integer(bigvec) & is.integer(smallvec))
        smallvec <- as.numeric(smallvec)
    for(i in 0:(N-n)){
        if(identical(bigvec[b+i],smallvec)){
            if(fullvec) return((i+1):(i+n))
            else return(i+1)
        }
    }
    return(FALSE)
}


