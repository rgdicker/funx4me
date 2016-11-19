#' @export
is.divby <- function(x,d){
    # is x divisible by d
    # if d=1, is x a whole number
    !x%%d
}
