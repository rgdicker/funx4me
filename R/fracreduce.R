#' @export
fracreduce <- function(num,denom){
    gcd <- function(x,y) {
        r <- x%%y;
        return(ifelse(r, gcd(y, r), y))
    }
    div <- gcd(num,denom)
    #to print, do cat(num/div,'/',denom/div,sep='')
    c(num,denom)/div
}
