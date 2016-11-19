#' @export
is.row <- function(vec,mat,rev=T){
    # Is vec a row in mat? What about rev(vec)?
    vec <- as.integer(vec)
    out <- which(as.logical(apply(mat,1,function(x) all.equal(x,vec))))
    if(rev==T){
        out2 <- which(as.logical(apply(mat,1,function(x) all.equal(x,rev(vec)))))
        if(length(out)==0 & length(out2)==0) return(FALSE) else return(TRUE)
    } else if(length(out)==0) return(FALSE) else return(TRUE)
}
