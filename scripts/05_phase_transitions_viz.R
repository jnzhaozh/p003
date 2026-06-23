source(here::here("scripts/01_project_setup.R"))

# -------------------------------------------------------------------------

results_sigma_x_gamma <- readRDS(here::here(
  "results/param_sigma_x_gamma_20260611_1620.rds"
))

results_gamma_c <- readRDS(here::here(
  "results/param_gamma_c_20260611_1623.rds"
))


results_mu_p_sigma_x <- readRDS(here::here(
  "results/param_mu_p_sigma_x_20260611_1626.rds"
))


# -------------------------------------------------------------------------

df_sigma_x_gamma <- results_sigma_x_gamma %>%
  pivot_longer(
    cols = c(mean_edge_ratio, mean_lcc_prop, mean_cluster_coeff),
    names_to = "metric",
    values_to = "raw_value"
  ) %>%
  mutate(
    metric = factor(metric),
    z_score = (raw_value - mean(raw_value, na.rm = TRUE)) /
      sd(raw_value, na.rm = TRUE),
    .by = metric
  )


(p_sigma_x_gamma <- df_sigma_x_gamma %>%
  ggplot(aes(x = sigma_x, y = gamma, z = z_score)) +
  geom_contour_filled(
    aes(fill = after_stat(level_mid)),
    breaks = seq(-2, 2, length.out = 9)
  ) +
  facet_wrap(
    ~metric,
    ncol = 3,
    labeller = as_labeller(c(
      "mean_edge_ratio" = "Active Edge Ratio",
      "mean_lcc_prop" = "LCC Proportion",
      "mean_cluster_coeff" = "Clustering Coefficient"
    ))
  ) +
  scale_x_continuous(
    name = TeX("Topic Vector Dispersion ($\\sigma_x$)"),
    limits = c(0.05, 0.2),
    breaks = c(0.05, 0.2),
    labels = c(0.05, 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = TeX("Topic Distance Decay ($\\gamma$)"),
    limits = c(0.1, 10),
    breaks = c(0.1, 10),
    labels = c(0.1, 10),
    expand = c(0, 0)
  ) +
  scale_fill_viridis_b(
    name = "Z-Score",
    option = "magma",
    breaks = seq(-2, 2, length.out = 9),
    limits = c(-2, 2),
    oob = scales::squish,
    guide = guide_colorsteps(
      barwidth = unit(3, "in"),
      barheight = unit(0.15, "in")
    )
  ) +
  coord_cartesian(clip = "off") +
  this_theme(base_size = 9))


this_ggsave(p_sigma_x_gamma, width = 6.5, height = 3.25)


# -------------------------------------------------------------------------

df_gamma_c <- results_gamma_c %>%
  pivot_longer(
    cols = c(mean_edge_ratio, mean_lcc_prop, mean_cluster_coeff),
    names_to = "metric",
    values_to = "raw_value"
  ) %>%
  mutate(
    metric = factor(metric),
    z_score = (raw_value - mean(raw_value, na.rm = TRUE)) /
      sd(raw_value, na.rm = TRUE),
    .by = metric
  )


(p_gamma_c <- df_gamma_c %>%
  ggplot(aes(x = gamma, y = c, z = z_score)) +
  geom_contour_filled(
    aes(fill = after_stat(level_mid)),
    breaks = seq(-2, 2, length.out = 9)
  ) +
  facet_wrap(
    ~metric,
    ncol = 3,
    labeller = as_labeller(c(
      "mean_edge_ratio" = "Active Edge Ratio",
      "mean_lcc_prop" = "LCC Proportion",
      "mean_cluster_coeff" = "Clustering Coefficient"
    ))
  ) +
  scale_x_continuous(
    name = TeX("Topic Distance Decay ($\\gamma$)"),
    limits = c(0.1, 10),
    breaks = c(0.1, 10),
    labels = c(0.1, 10),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = TeX("Mean Degree ($c$)"),
    limits = c(3, 12),
    breaks = c(3, 12),
    labels = c(3, 12),
    expand = c(0, 0)
  ) +
  scale_fill_viridis_b(
    name = "Z-Score",
    option = "magma",
    breaks = seq(-2, 2, length.out = 9),
    limits = c(-2, 2),
    oob = scales::squish,
    guide = guide_colorsteps(
      barwidth = unit(3, "in"),
      barheight = unit(0.15, "in")
    )
  ) +
  coord_cartesian(clip = "off") +
  this_theme(base_size = 9))

this_ggsave(p_gamma_c, width = 6.5, height = 3.25)

# -------------------------------------------------------------------------

df_mu_p_sigma_x <- results_mu_p_sigma_x %>%
  pivot_longer(
    cols = c(mean_edge_ratio, mean_lcc_prop, mean_cluster_coeff),
    names_to = "metric",
    values_to = "raw_value"
  ) %>%
  mutate(
    metric = factor(metric),
    z_score = (raw_value - mean(raw_value, na.rm = TRUE)) /
      sd(raw_value, na.rm = TRUE),
    .by = metric
  )

(p_mu_p_sigma_x <- df_mu_p_sigma_x %>%
  ggplot(aes(x = mu_p, y = sigma_x, z = z_score)) +
  geom_contour_filled(
    aes(fill = after_stat(level_mid)),
    breaks = seq(-2, 2, length.out = 9)
  ) +
  facet_wrap(
    ~metric,
    ncol = 3,
    labeller = as_labeller(c(
      "mean_edge_ratio" = "Active Edge Ratio",
      "mean_lcc_prop" = "LCC Proportion",
      "mean_cluster_coeff" = "Clustering Coefficient"
    ))
  ) +
  scale_x_continuous(
    name = TeX("Topic Prevalence Mean ($\\mu_p$)"),
    limits = c(0.1, 0.9),
    breaks = c(0.1, 0.9),
    labels = c(0.1, 0.9),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = TeX("Topic Vector Dispersion ($\\sigma_x$)"),
    limits = c(0.05, 0.2),
    breaks = c(0.05, 0.2),
    labels = c(0.05, 0.2),
    expand = c(0, 0)
  ) +
  scale_fill_viridis_b(
    name = "Z-Score",
    option = "magma",
    breaks = seq(-2, 2, length.out = 9),
    limits = c(-2, 2),
    oob = scales::squish,
    guide = guide_colorsteps(
      barwidth = unit(3, "in"),
      barheight = unit(0.15, "in")
    )
  ) +
  coord_cartesian(clip = "off") +
  this_theme(base_size = 9))


this_ggsave(p_mu_p_sigma_x, width = 6.5, height = 3.25)

# -------------------------------------------------------------------------

# (p_polarization <- results_polarization %>%
#   ggplot(aes(x = sigma_x, y = gamma, z = mean_edge_ratio)) +
#   geom_contour_filled(
#     aes(fill = after_stat(level_mid)),
#     breaks = with(
#       results_polarization,
#       seq(min(mean_edge_ratio), max(mean_edge_ratio), length.out = 9)
#     )
#   ) +
#   scale_fill_viridis_b(
#     option = "magma",
#     breaks = with(
#       results_polarization,
#       seq(min(mean_edge_ratio), max(mean_edge_ratio), length.out = 9)
#     ),
#     labels = scales::label_scientific(digits = 2)
#   ) +
#   labs(
#     x = TeX("Topic Vector Dispersion ($\\sigma_x$)"),
#     y = TeX("Topic Distance Decay ($\\gamma$)"),
#     fill = "mean_edge_ratio"
#   ) +
#   this_theme() +
#   theme(
#     legend.position = "right",
#     legend.title.position = "top",
#     legend.direction = "vertical",
#     legend.key.height = unit(3, "lines"),
#     legend.key.width = unit(1.5, "lines")
#   ))

# -------------------------------------------------------------------------
#
# (p_scale <- results_scale %>%
#   ggplot(aes(x = mu_p, y = gamma, z = mean_giant_comp)) +
#   geom_contour_filled(
#     aes(fill = after_stat(level_mid)),
#     breaks = with(
#       results_scale,
#       seq(min(mean_giant_comp), max(mean_giant_comp), length.out = 9)
#     )
#   ) +
#   scale_fill_viridis_b(
#     option = "magma",
#     breaks = with(
#       results_scale,
#       seq(min(mean_giant_comp), max(mean_giant_comp), length.out = 9)
#     ),
#     labels = scales::label_scientific(digits = 2)
#   ) +
#   labs(
#     x = TeX("Mean Topic Prevalence ($\\mu_p$)"),
#     y = TeX("Topic Distance Decay ($\\gamma$)"),
#     fill = "Effective Edge Ratio\n(Network Capacity)"
#   ) +
#   this_theme() +
#   theme(
#     legend.position = "right",
#     legend.title.position = "top",
#     legend.direction = "vertical",
#     legend.key.height = unit(3, "lines"),
#     legend.key.width = unit(1.5, "lines")
#   ))
#
#
# # -------------------------------------------------------------------------
#
# (p_echo <- results_echo %>%
#   ggplot(aes(x = mu_p, y = h, z = mean_transitivity)) +
#   geom_contour_filled(
#     aes(fill = after_stat(level_mid)),
#     breaks = with(
#       results_echo,
#       seq(min(mean_transitivity), max(mean_transitivity), length.out = 9)
#     )
#   ) +
#   scale_fill_viridis_b(
#     option = "magma",
#     breaks = with(
#       results_echo,
#       seq(min(mean_transitivity), max(mean_transitivity), length.out = 9)
#     ),
#     labels = scales::label_scientific(digits = 2)
#   ) +
#   labs(
#     x = TeX("Mean Topic Prevalence ($\\mu_p$)"),
#     y = "Degree Heterogeneity (h)",
#     fill = "Transitivity\n(Echo Chambers)"
#   ) +
#   this_theme() +
#   theme(
#     legend.position = "right",
#     legend.title.position = "top",
#     legend.direction = "vertical",
#     legend.key.height = unit(3, "lines"),
#     legend.key.width = unit(1.5, "lines")
#   ))
#
#
# # -------------------------------------------------------------------------
#
# (p_complex <- results_complex %>%
#   mutate(K_label = factor(K, levels = 1:5, labels = paste("K =", 1:5))) %>%
#   ggplot(aes(
#     x = gamma,
#     y = mean_giant_comp,
#     color = K_label,
#     group = K_label
#   )) +
#   geom_line(linewidth = 1.2) +
#   geom_point(size = 2, alpha = 0.8) +
#   scale_color_viridis_d(option = "viridis") +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(
#     x = TeX("Topic Distance Decay ($\\gamma$)"),
#     y = "Giant Component Proportion",
#     color = "Topic Dimensions (K)"
#   ) +
#   this_theme() +
#   theme(
#     legend.position = "right",
#     legend.title.position = "top",
#     legend.direction = "vertical"
#   ))

# (p_complex <- results_complex %>%
#   ggplot(aes(x = gamma, y = factor(K), fill = mean_giant_comp)) +
#   geom_tile() +
#   scale_fill_viridis_c(option = "viridis") +
#   labs(
#     x = TeX("Topic Distance Decay ($\\gamma$)"),
#     y = "Topic Dimensions (K)",
#     fill = "Giant Comp."
#   ) +
#   this_theme())
