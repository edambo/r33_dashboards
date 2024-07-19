# Create interactive time series plot

time_series <- function(df, y_var, group_var, hovertemplate_text, y_ax_title){
  plot_ly(
    x = ~fct_reorder(df[["month_year"]], df[["visit_date"]]),# Order x-axis labels chronologically
    y = df[[y_var]],
    type = "scatter",
    mode = "lines+markers",
    color = df[[group_var]],
    text = df[[group_var]],
    hovertemplate = hovertemplate_text
  ) %>%
    layout(
      yaxis = list(
        tickmode = "array",
        tickvals = seq(0,100,10),
        title = y_ax_title,
        range = c(-1,101)
      ),
      xaxis = list(
        title = FALSE,
        tickangle = 45
      )
    )
}