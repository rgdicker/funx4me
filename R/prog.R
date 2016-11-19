#' @export
prog <- function(max=Inf,index=i){
    Sys.sleep(.01)
    # If max was entered show percent complete, else show index
    if(max<Inf) {
        perc <- sprintf("%.1f %%",100*index/max)
        cat(perc, "\r")
        if(index==max) beep(2)
    } else {
        cat(index, "\r")
    }
    flush.console()
}
