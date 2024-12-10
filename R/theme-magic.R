
#' @export
#' @importFrom ggplot2 theme_minimal theme

theme_magic <- function(base_size = 12, base_family = "",
                        .legend_position = 'top') {

  theme_minimal(base_size = base_size, base_family = base_family) %+replace%

    theme(text = element_text(family = 'Helvetica'),
          legend.position = .legend_position,
          legend.title = element_blank(),
          legend.text = element_text(size = base_size),
          title = element_text(size = base_size, margin = margin(b = 8), hjust = 0),
          plot.subtitle = element_text(size = base_size),
          axis.text = element_text(size = base_size),
          axis.text.x = element_text(margin = margin(b = 0)),
          panel.grid = element_line(colour = '#D3D3D3'),
          axis.ticks.x = element_line(linewidth = 0.5, colour = 'black'),
          plot.caption = element_text(size = base_size, hjust = 1),
          plot.title = element_text(face = 'bold'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title.position = 'plot',
          axis.line.x = element_line(linewidth = 1, colour = 'black'))

}

#' @export
#' @importFrom ggplot2 theme

theme_magic_rotated <- function(base_size = 12, base_family = "",
                                .legend_position = 'top') {

  theme_magic(base_size = base_size, base_family = base_family) %+replace%

    theme(text = element_text(family = 'Helvetica'),
          legend.position = .legend_position,
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(colour = '#D3D3D3'))

}
