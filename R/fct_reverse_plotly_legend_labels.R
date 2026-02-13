#' reverse_plotly_legend_labels
#'
#' @description A function to reverse legends labels in a plotly plot
#'
#' @return A plotly plot with reversed legend labels
#'
#' @noRd
reverse_plotly_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}
