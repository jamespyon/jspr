#' Create Example Boxplot for Appendices
#'
#' @description Creates a boxplot skeleton which shows all parts of the boxplot with labels naming each part, with options to format the plot as you would like -- useful when you need an example of a boxplot for your appendix.
#' @param x An optional character string denoting the type of distribution to use to generate the boxplot. Default is 'rnorm'.
#' @param add.mean Logical. If you want mean to be added to the boxplot. Default is TRUE.
#' @param mean.shape Numeric. Based on ggplot2 point shape.
#' @param text.size Numeric. Based on ggplot2 text size.
#' @param bracket.size Numeric. Based on ggplot2 linewidth and ggpubr bracket size.
#' @param plot.color A character string denoting a color for the boxplot generated.
#' @param bracket.color A character string denoting a color for the drawn lines generated.
#' @param text.color A character string denoting a color for the text generated.
#' @param title A character string for an optional title.
#' @param caption A character string for an optional caption.
#' @param warnings Logical. Do you want warnings?
#'
#' @returns A ggplot class object.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' example_boxplot()
#'

example_boxplot <- function(x = c("rnorm", "rlnorm", "rexp"), add.mean = TRUE, mean.shape = 10, text.size = 4, bracket.size = 1, plot.color = "black", bracket.color = "darkgray", text.color = "black", title = "", caption = "", warnings = TRUE) {

  # warning
  jspr_warning("This function changes Global Settings.", warnings)

  # vector x setting
  if(identical(x, c("rnorm", "rlnorm", "rexp"))) {x <- "rnorm"}
  if(x == "rnorm") {

    set.seed(04062023)
    example_data <- c(stats::rnorm(20, mean = 5, sd = 1), c(7.5, 9, 10.8, 10.9, 11), c(1, 1.1, 1.2))

  } else if(x == "rlnorm") {

    set.seed(04062023)
    example_data <- stats::rlnorm(20, meanlog = 1, sdlog = 1)

  } else if(x == "rexp") {

    set.seed(04062023)
    example_data <- c(stats::rexp(20, rate = 0.3), c(11, 11.3))

  } else {stop("Please use either 'rnorm' or 'rexp for x argument.")}

  example_data <- stats::rlnorm(20, meanlog = 1, sdlog = 1)

  # stat settings based on vector x
  quant <- c(stats::quantile(example_data, probs = c(0.25, 0.5, 0.75)))
  mean <- mean(example_data)
  iqr <- stats::IQR(example_data)
  if(min(example_data) <= 0) {
    minll <- 0
  } else {
    minll <- quant[1]-(1.5*iqr)
    minll <- sort(example_data[example_data>minll])[1]
  }
  if(max(example_data) <=0) {
    maxul <- 0
  } else {
    maxul <- quant[3]+(1.5*iqr)
    maxul <- sort(example_data[example_data<maxul], decreasing = TRUE)[1]
  }
  min <- min(example_data)
  max <- max(example_data)

  # annotation settings
  lhs_arrow_head <- 0.9
  lhs_arrow_tail_center <- 0.99
  lhs_arrow_tail_box <- 0.92
  lhs_text <- 0.89
  rhs_outlier_text <- 1.03
  rhs_outlier_bracket <- rhs_outlier_text-0.01
  rhs_whisker_text <- 1.04
  rhs_whisker_bracket <- rhs_whisker_text-0.01
  rhs_box_text <- 1.1
  rhs_box_bracket <- rhs_box_text-0.01

  #generate base plot
  example_plot <- data.frame(values = example_data, center = 1) |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data$values, y = .data$center)) +
    ggplot2::geom_boxplot(width = 0.15, color = plot.color) +

    ggplot2::annotate("text", x = max, y = lhs_text, label = "Maximum",
                      color = text.color, size = text.size, hjust = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = max, xend = max,
                                       y = lhs_arrow_tail_center, yend = lhs_arrow_head),
                          arrow = grid::arrow(angle = 50,
                                              length = grid::unit(7, "points"),
                                              type = "closed"),
                          color = bracket.color, linewidth = bracket.size) +

    ggplot2::annotate("text", x = maxul, y = lhs_text, label = "Upper Extreme",
                      color = text.color, size = text.size, hjust = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = maxul, xend = maxul,
                                       y = lhs_arrow_tail_center, yend = lhs_arrow_head),
                          arrow = grid::arrow(angle = 50,
                                              length = grid::unit(7, "points"),
                                              type = "closed"),
                          color = bracket.color, linewidth = bracket.size)+

    ggplot2::annotate("text", x = quant[3], y = lhs_text, label = "75th percentile",
                      color = text.color, size = text.size, hjust = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = quant[3], xend = quant[3],
                                       y = lhs_arrow_tail_box, yend = lhs_arrow_head),
                          arrow = grid::arrow(angle = 50,
                                              length = grid::unit(7, "points"),
                                              type = "closed"),
                          color = bracket.color, linewidth = bracket.size) +

    ggplot2::annotate("text", x = quant[2], y = lhs_text, label = "Median",
                      color = text.color, size = text.size, hjust = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = quant[2], xend = quant[2],
                                       y = lhs_arrow_tail_box, yend = lhs_arrow_head),
                          arrow = grid::arrow(angle = 50,
                                              length = grid::unit(7, "points"),
                                              type = "closed"),
                          color = bracket.color, linewidth = bracket.size) +

    ggplot2::annotate("text", x = quant[1], y = lhs_text, label = "25th percentile",
                      color = text.color, size = text.size, hjust = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = quant[1], xend = quant[1],
                                       y = lhs_arrow_tail_box, yend = lhs_arrow_head),
                          arrow = grid::arrow(angle = 50,
                                              length = grid::unit(7, "points"),
                                              type = "closed"),
                          color = bracket.color, linewidth = bracket.size) +

    ggplot2::annotate("text", x = minll, y = lhs_text, label = "Lower Extreme",
                      color = text.color, size = text.size, hjust = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = minll, xend = minll,
                                       y = lhs_arrow_tail_center, yend = lhs_arrow_head),
                          arrow = grid::arrow(angle = 50,
                                              length = grid::unit(7, "points"),
                                              type = "closed"),
                          color = bracket.color, linewidth = bracket.size) +

    ggplot2::annotate("text", x = quant[1] + (quant[3] - quant[1])/2, y = rhs_box_text, label = "Box",
                      color = text.color, size = text.size, hjust = 0) +
    ggpubr::geom_bracket(xmin = quant[1]+0.02, xmax = quant[3]-0.02, y.position = rhs_box_bracket, label = "",
                         color = bracket.color, size = bracket.size) +

    ggplot2::ylim(0.5, 1.5) +
    ggplot2::coord_flip() +
    ggplot2::theme_void() +
    ggplot2::labs(x = "", y = "",
                  title = title,
                  caption = caption)

  # add mean if TRUE
  if(add.mean == TRUE) {

    example_plot <- example_plot +

      ggplot2::geom_point(ggplot2::aes(x = mean, y = 1), shape = mean.shape, size = 4) +
      ggplot2::annotate("text", x = mean, y = lhs_text, label = "Mean",
                        color = text.color, size = text.size, hjust = 1) +
      ggplot2::geom_segment(ggplot2::aes(x = mean, xend = mean,
                                         y = lhs_arrow_tail_center, yend = lhs_arrow_head),
                            arrow = grid::arrow(angle = 50,
                                                length = grid::unit(7, "points"),
                                                type = "closed"),
                            color = bracket.color, linewidth = bracket.size)

  }

  # add additional items based on distribution
  if(x == "rnorm") {

    example_plot <- example_plot +

      ggplot2::annotate("text", x = min, y = lhs_text, label = "Minimum",
                        color = text.color, size = text.size, hjust = 1) +
      ggplot2::geom_segment(ggplot2::aes(x = min, xend = min,
                                         y = lhs_arrow_tail_center, yend = lhs_arrow_head),
                            arrow = grid::arrow(angle = 50,
                                                length = grid::unit(7, "points"),
                                                type = "closed"),
                            color = bracket.color, linewidth = bracket.size) +

      ggplot2::annotate("text", x = maxul + (max - maxul)/2, y = rhs_outlier_text, label = "Outliers",
                        color = text.color, size = text.size, hjust = 0) +
      ggpubr::geom_bracket(xmin = maxul+0.02, xmax = max+0.1, y.position = rhs_outlier_bracket, label = "",
                           color = bracket.color, size = bracket.size) +

      ggplot2::annotate("text", x = quant[3] + (maxul - quant[3])/2, y = rhs_whisker_text, label = "Whisker",
                        color = text.color, size = text.size, hjust = 0) +
      ggpubr::geom_bracket(xmin = quant[3]+0.02, xmax = maxul-0.02, y.position = rhs_whisker_bracket, label = "",
                           color = bracket.color, size = bracket.size) +

      ggplot2::annotate("text", x = quant[1] + (quant[3] - quant[1])/2, y = rhs_box_text, label = "Box",
                        color = text.color, size = text.size, hjust = 0) +
      ggpubr::geom_bracket(xmin = quant[1]+0.02, xmax = quant[3]-0.02, y.position = rhs_box_bracket, label = "",
                           color = bracket.color, size = bracket.size) +

      ggplot2::annotate("text", x = quant[1] + (minll - quant[1])/2, y = rhs_whisker_text, label = "Whisker",
                        color = text.color, size = text.size, hjust = 0) +
      ggpubr::geom_bracket(xmin = minll+0.02, xmax = quant[1]-0.03, y.position = rhs_whisker_bracket, label = "",
                           color = bracket.color, size = bracket.size) +

      ggplot2::annotate("text", x = minll + (min - minll)/2, y = rhs_outlier_text, label = "Outliers",
                        color = text.color, size = text.size, hjust = 0) +
      ggpubr::geom_bracket(xmin = minll-0.02, xmax = min-0.1, y.position = rhs_outlier_bracket, label = "",
                           color = bracket.color, size = bracket.size)

  } else if(x == "rexp" | x == "rlnorm") {

    example_plot <- example_plot +
      ggplot2::annotate("text", x = maxul + (max - maxul)/2, y = rhs_outlier_text, label = "Outliers",
                        color = text.color, size = text.size, hjust = 0) +
      ggpubr::geom_bracket(xmin = maxul+0.02, xmax = max+0.1, y.position = rhs_outlier_bracket, label = "",
                           color = bracket.color, size = bracket.size) +

      ggplot2::annotate("text", x = quant[3] + (maxul - quant[3])/2, y = rhs_whisker_text, label = "Whisker",
                        color = text.color, size = text.size, hjust = 0) +
      ggpubr::geom_bracket(xmin = quant[3]+0.02, xmax = maxul-0.02, y.position = rhs_whisker_bracket, label = "",
                           color = bracket.color, size = bracket.size) +

      ggplot2::annotate("text", x = quant[1] + (quant[3] - quant[1])/2, y = rhs_box_text, label = "Box",
                        color = text.color, size = text.size, hjust = 0) +
      ggpubr::geom_bracket(xmin = quant[1]+0.02, xmax = quant[3]-0.02, y.position = rhs_box_bracket, label = "",
                           color = bracket.color, size = bracket.size)

  }

  return(example_plot)

}
