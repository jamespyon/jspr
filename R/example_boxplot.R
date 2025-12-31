example_boxplot <- function(x = NULL, plot.color = "black", bracket.color = "black", text.color = "black") {

  if(!is.null(x)) {
    stop("x argument is not available at this moment.")
  } else {
    set.seed(04062023)
    x <- rexp(1000, rate = 10)
  }

  quant <- c(quantile(x, probs = c(0.25, 0.5, 0.75)))
  mean <- mean(x)
  iqr <- IQR(x)
  min <- quant[1]-(1.5*iqr)
  if(min <=0) {min <- 0}
  max <- quant[3]+(1.5*iqr)

  data.frame(x = x, y = "Boxplot") |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_boxplot(color = plot.color) +
    ggplot2::stat_summary(fun = "mean",
                          geom = 'point',
                          shape = 13,
                          size = 3,
                          position = ggplot2::position_dodge2(width = 0.75, preserve = "single")) +
    ggplot2::annotate("text", x = quant[1], y = 1.41, label = "Q1", color = text.color) +
    ggplot2::annotate("text", x = quant[3], y = 1.41, label = "Q3", color = text.color) +
    ggplot2::geom_segment(ggplot2::aes(x = mean, xend = mean,
                                       y = 0.58, yend = 0.99),
                          arrow = grid::arrow(angle = 50,
                                              length = grid::unit(7, "points"),
                                              type = "closed"),
                          color = bracket.color) +
    ggplot2::annotate("text", x = quant[2], y = 1.41, label = "Median", color = text.color) +
    ggplot2::annotate("text", x = mean, y = 0.55, label = "Mean", color = text.color) +
    cowplot::theme_cowplot() +
    ggplot2::labs(x = "", y = "",
                  title = "",
                  caption = "")




    ggplot2::geom_segment(ggplot2::aes(x = 1.39, xend = 1.4, y = quant[1]+0.001, yend = quant[1]+0.001), color = bracket.color) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.39, xend = 1.4, y = quant[3]-0.001, yend = quant[3]-0.001), color = bracket.color) +
    ggplot2::annotate("text", x = 1.41, y = 0.09, label = "Interquartile\nRange", hjust = 0, color = text.color) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.4, xend = 1.4, y = quant[1]-0.001, yend = min), color = bracket.color) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.39, xend = 1.4, y = quant[1]-0.001, yend = quant[1]-0.001), color = bracket.color) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.39, xend = 1.4, y = min, yend = min), color = bracket.color) +
    ggplot2::annotate("text", x = 1.41, y = 0.020, label = "Lower\nIQR", hjust = 0, color = text.color) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.4, xend = 1.4, y = 0.142, yend = 0.310), color = bracket.color) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.39, xend = 1.4, y = 0.142, yend = 0.142), color = bracket.color) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.39, xend = 1.4, y = 0.310, yend = 0.310), color = bracket.color) +
    ggplot2::annotate("text", x = 1.41, y = 0.24, label = "Upper\nIQR", hjust = 0, color = text.color) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.4, xend = 1.4, y = 0.312, yend = 0.77), color = bracket.color) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.39, xend = 1.4, y = 0.312, yend = 0.312), color = bracket.color) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.39, xend = 1.4, y = 0.77, yend = 0.77), color = bracket.color) +
    ggplot2::annotate("text", x = 1.41, y = 0.55, label = "Outliers", hjust = 0, color = text.color) +
    cowplot::theme_cowplot() +
    ggplot2::labs(x = "", y = "",
         title = "",
         caption = "")

}
