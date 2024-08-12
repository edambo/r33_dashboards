gauge_chart <- function(x, n, color =  "#70bfb3", title = NULL) {
  data.frame(x = c(5 * cos(seq(-pi, 0, len = 100)), 
                   3 * cos(seq(0, -pi, len = 100)),
                   5 * cos(seq(-pi, -pi + pi * x/n, len = 100)),
                   3 * cos(seq( -pi + pi * x/n, -pi, len = 100))),
             y = c(5 * -sin(seq(-pi, 0, len = 100)), 
                   3 * -sin(seq(0, -pi, len = 100)),
                   5 * -sin(seq(-pi, -pi + pi * x/n, len = 100)),
                   3 * -sin(seq( -pi + pi * x/n, -pi, len = 100))),
             group = rep(c("off", "on"), each = 200)) |>
    ggplot(aes(x, y, fill = group)) +
    geom_polygon() +
    scale_fill_manual(values = c("#eceaea", color), guide = "none") +
    annotate("text", x = c(-4, 0, 4), y = c(-0.5, 0.6, -0.5),
             label = c(0, round(x, 1), n), size = c(12, 30, 12),
             colour = c("#c8c8c8", "#808080", "#c8c8c8"), 
             fontface = 2) +
    ggtitle(title) +
    coord_equal() +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 40, color = "#808080",
                                    vjust = 2, face = 2))
}

# Source: https://stackoverflow.com/questions/77552994/how-to-edit-plotly-gauge-plot-ticks-in-r