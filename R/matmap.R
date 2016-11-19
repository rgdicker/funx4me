#' @export
matmap <- function(mat){
    s <- 1:nrow(mat)
    plot_ly(x = s, y = s,
            z = mat[rev(s),], type = "heatmap")
}
