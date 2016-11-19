#' @export
facs <- function(x) {
    #all positive factors of a number
    x <- as.integer(x)
    div <- seq_len(abs(x))
    factors <- div[x %% div == 0L]
}
