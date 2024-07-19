# Create formatted flextables for the dashboard

table_format <- function(df, header_label){
  border_format <- fp_border(color = "#BEC2C6", width = 1.5)
  
  flextable(df) %>%
    delete_part(part = "header") %>%
    add_header_lines(as_paragraph(header_label)) %>%
    bold(j = 1, part = "body") %>%
    bold(part = "header") %>%
    width(j = c(1,2), width = c(3, 1)) %>%
    align(j = 2, align = "right") %>%
    fontsize(size = 14, part = "header") %>%
    align(align = "center", part = "header") %>%
    color(j = c(1,2), color = "#6c757d", part = "body") %>%
    color(i = c(1), color = "#6c757d", part = "header") %>%
    bg(bg = "#f3f3f3", part = "header") %>%
    bg(bg = "#ffffff", part = "body") %>%
    border_outer(part = "all", border = border_format) %>%
    hline_bottom(border = fp_border_default(width = 0), part = "header") %>%
    fix_border_issues() %>%
    height_all(height = 0.5, unit = "in") %>%
    hrule(rule = "exact")
}