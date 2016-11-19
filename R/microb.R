



#' @export
#' @import beepr
#' @import microbenchmark
#' @import ggplot2
#' @import plotly
microb <- function (..., list = NULL, times = 100L, unit, check = NULL,
                    control = list())
{
    stopifnot(times == as.integer(times))
    if (!missing(unit))
        stopifnot(is.character("unit"), length(unit) == 1L)
    control[["warmup"]] <- coalesce(control[["warmup"]], 2^18L)
    control[["order"]] <- coalesce(control[["order"]], "random")
    stopifnot(as.integer(control$warmup) == control$warmup)
    exprs <- c(as.list(match.call(expand.dots = FALSE)$...),
               list)
    nm <- names(exprs)
    exprnm <- sapply(exprs, function(e) paste(deparse(e), collapse = " "))
    if (is.null(nm))
        nm <- exprnm
    else nm[nm == ""] <- exprnm[nm == ""]
    names(exprs) <- nm
    if (!is.null(check)) {
        values <- lapply(exprs, eval, parent.frame())
        ok <- check(values)
        if (!isTRUE(ok)) {
            stop("Input expressions are not equivalent.", call. = FALSE)
        }
    }
    gc(FALSE)
    o <- if (control$order == "random")
        sample(rep(seq_along(exprs), times = times))
    else if (control$order == "inorder")
        rep(seq_along(exprs), times = times)
    else if (control$order == "block")
        rep(seq_along(exprs), each = times)
    else stop("Unknown ordering. Must be one of 'random', 'inorder' or 'block'.")
    exprs <- exprs[o]
    res <- .Call(do_microtiming, exprs, parent.frame(), as.integer(control$warmup))
    if (all(is.na(res)))
        .all_na_stop()
    res <- data.frame(expr = factor(nm[o], levels = nm), time = res)
    class(res) <- c("microbenchmark", class(res))
    if (!missing(unit))
        attr(res, "unit") <- unit

    medians <- function(bench){
        print(bench)
        med <- t(as.vector(summary(bench)[5]))
        ratio <- diff <- 0
        for (i in 1:length(med)){
            ratio[i] <- med[i]/med[1]
            diff[i] <- med[i] - med[1]
        }
        beep <- beep(2)
        list(round(rbind(med,ratio,diff),3),autoplot(bench))
    }
    medians(res)
}

environment(microb) <- asNamespace('microbenchmark')

