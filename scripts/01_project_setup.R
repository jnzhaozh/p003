# packages ----------------------------------------------------------------

this_packages <- c(
  "dplyr",
  "forcats",
  "furrr",
  "future",
  "ggplot2",
  "ggraph",
  "ggsci",
  # "ggtext",
  "here",
  "igraph",
  "latex2exp",
  "lhs",
  "mgcv",
  "patchwork",
  "purrr",
  # "randomForest",
  "scales",
  # "sensitivity",
  "stringr",
  "tibble",
  "tidyr",
  "viridis"
)

suppressPackageStartupMessages({
  invisible(lapply(this_packages, library, character.only = TRUE))
})


# folders -----------------------------------------------------------------

this_folders <- c(
  "data",
  "data/raw",
  "docs",
  "figures",
  "figures/archive",
  "results",
  "results/archive",
  "scripts",
  "scripts/archive"
)


invisible(sapply(
  here::here(this_folders),
  dir.create,
  showWarnings = FALSE,
  recursive = TRUE
))


# theme -------------------------------------------------------------------

this_theme <- function(base_size = 11, base_family = "sans") {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      # Plot Titles and Margins
      plot.background = element_blank(),
      plot.title = element_text(
        size = rel(1.0),
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = rel(1.0),
        face = "bold",
        hjust = 0,
        margin = margin(b = 5)
      ),
      plot.margin = ggplot2::margin(t = 5, r = 10, b = 5, l = 5, unit = "pt"),

      # Panels and Grids
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
      panel.spacing = unit(1.25, "lines"),

      # Axes
      axis.title = element_text(size = rel(1.0)),
      axis.text = element_text(size = rel(1.0)),
      axis.ticks = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.title.x = element_text(
        size = rel(1.0),
        # vjust = -0.5,
        margin = margin(t = 5)
      ),
      axis.title.y = element_text(
        size = rel(1.0),
        # hjust = 0.5,
        margin = margin(r = 5),
        angle = 90
      ),
      axis.text.x = element_text(margin = margin(t = 5)),
      axis.text.y = element_text(margin = margin(r = 5)),

      # Legend
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.justification = "center",
      legend.title = element_text(
        size = rel(1.0),
        margin = margin(r = 15, b = 5),
        vjust = 0.5
      ),
      legend.title.position = "left",
      legend.text = element_text(size = rel(1.0)),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.margin = margin(t = 5, r = 15, b = 5, l = 15, unit = "pt"),
      legend.spacing.x = unit(0.5, "cm"),

      # Facet Strips
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(
        size = rel(1.0),
        face = "bold",
        # hjust = 0.5,
        margin = margin(4, 4, 4, 4)
      )
    )
}

# utility functions -------------------------------------------------------

this_saveRDS <- function(x, obj_name = NULL) {
  if (is.null(obj_name)) {
    obj_name <- deparse(substitute(x))
  }

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
  file_name <- paste0(obj_name, "_", timestamp, ".rds")
  file_path <- here::here("results", file_name)

  dir.create(here::here("results"), showWarnings = FALSE, recursive = TRUE)

  saveRDS(x, file = file_path)

  message(">>> Saved to: ", file_path)
}


this_ggsave <- function(x, width = 6.5, height = 4.5) {
  obj_name <- deparse(substitute(x))
  file_name <- paste0(obj_name, ".pdf")
  file_path <- here::here("figures", file_name)
  dir.create(here::here("figures"), showWarnings = FALSE, recursive = TRUE)

  ggsave(
    filename = file_path,
    plot = x,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    device = grDevices::cairo_pdf
  )

  message(">>> Saved to: ", file_path)
}
