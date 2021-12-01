# Colour palette
pltpal <- RColorBrewer::brewer.pal(4, 'Blues')[3:4]
# pltpal <- nhsrtheme::get_nhs_colours(section = 'blues')
line_colour <- pltpal[2]
ribbon_colour <- pltpal[4]
ribbon_alpha <- 0.3


# Common formatting
ggplot2::update_geom_defaults("line", list(size = 1.0))


common_style <- function(panel_spacing = 1, strip_angle = 90, strip_font_size = 12, strip_margin = 1) {
  list(
    xlab('Date'),
    scale_x_date(
      date_breaks = "1 years",
      minor_breaks = NULL,
      date_labels = "%Y"),
    geom_national_covid_lockdowns(TRUE),
    theme(
      # panel.grid.major.x = element_line(color = nhsrtheme::get_nhs_colours('PaleGrey')),
      strip.text = element_text(
        color = 'black',  # nhsrtheme::get_nhs_colours('Black'),
        size = strip_font_size, hjust = 0.5),
      strip.background = element_rect(fill = 'white'),
      panel.background = element_rect(fill = 'grey'),  # nhsrtheme::get_nhs_colour_tints(0.6, 'PaleGrey')),
      strip.placement = "outside",
      panel.spacing = unit(panel_spacing, "lines"),
      strip.text.y = element_text(angle = strip_angle, margin = margin(b = strip_margin, t = strip_margin))
    )
  )
}