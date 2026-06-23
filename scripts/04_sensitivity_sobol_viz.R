source(here::here("scripts/01_project_setup.R"))

# 1. Load the Sobol Object ------------------------------------------------

results_sobol <- readRDS(here::here(
  "results/results_sobol_20260611_1605.rds"
))

# 2. Extract and Format Sobol Indices -------------------------------------
# Extract First-Order (Main) Effects

param_df <- tribble(
  ~Parameter , ~Parameter_Label                            ,
  "mu_p"     , "Topic Prevalence Mean ($\\mu_p$)"          ,
  "sigma_p"  , "Topic Prevalence Dispersion ($\\sigma_p$)" ,
  "K"        , "Topic-Space Dimensionality ($K$)"          ,
  "mu_x"     , "Topic Vector Mean ($\\mu_x$)"              ,
  "sigma_x"  , "Topic Vector Dispersion ($\\sigma_x$)"     ,
  "gamma"    , "Topic Distance Decay ($\\gamma$)"          ,
  "c"        , "Mean Degree ($c$)"                         ,
  "h"        , "Degree Heterogeneity ($h$)"
) %>%
  mutate(
    Parameter_Label = fct_rev(factor(Parameter_Label, levels = Parameter_Label))
  )

metric_df <- tribble(
  ~Metric                   , ~Metric_Label            ,
  "diffusion_edge_ratio"    , "Active Edge Ratio"      ,
  "diffusion_lcc_prop"      , "LCC Proportion"         ,
  "diffusion_cluster_coeff" , "Clustering Coefficient"
) %>%
  mutate(Metric_Label = forcats::fct_inorder(Metric_Label))


# total order ~ all network metrics ---------------------------------------

results_sobol_heatmap <- results_sobol %>%
  inner_join(param_df, by = "Parameter") %>%
  inner_join(metric_df, by = "Metric")


(p_heatmap <- results_sobol_heatmap %>%
  ggplot(aes(
    x = Metric_Label,
    y = Parameter_Label,
    fill = Total_Order
  )) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(
    aes(label = sprintf("%.2f", Total_Order), color = Total_Order >= 0.4),
    size = 3,
    fontface = "plain"
  ) +
  scale_color_manual(values = c("white", "black"), guide = "none") +
  scale_x_discrete(
    name = NULL,
    expand = c(0, 0),
    position = "top",
  ) +
  scale_y_discrete(
    name = NULL,
    expand = c(0, 0),
    labels = TeX
  ) +
  scale_fill_viridis_c(
    name = TeX("Total-Order Sobol Index ($S_{T_i}$)"),
    option = "magma",
    begin = 0.1,
    end = 0.9,
    limits = c(0, 1),
    breaks = c(0, 0.4, 1.0)
  ) +
  coord_cartesian(clip = "off") +
  this_theme(base_size = 9) +
  theme(
    axis.text.x.top = element_text(margin = margin(b = 5)),
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(1, "lines")
  ))

this_ggsave(p_heatmap, width = 6.5, height = 5.5)


# -------------------------------------------------------------------------

results_sobol_bar <- results_sobol_heatmap %>%
  pivot_longer(
    cols = c(First_Order, Total_Order),
    names_to = "Index_Type",
    values_to = "Sobol_Index"
  )

(p_bar_faceted <- results_sobol_bar %>%
  ggplot(aes(
    x = pmax(Sobol_Index, 0),
    y = Parameter_Label,
    fill = factor(Index_Type)
  )) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  facet_wrap(~Metric_Label, ncol = 3) +
  scale_x_continuous(
    name = "Sobol Index",
    limits = c(0, 1),
    labels = seq(0, 1, length.out = 5),
    expand = c(0, 0)
  ) +
  scale_y_discrete(
    name = NULL,
    labels = TeX
  ) +
  scale_fill_manual(
    name = "Index Type",
    values = c(
      "First_Order" = "#ea801c",
      "Total_Order" = "#1a80bb"
    ),
    labels = c(
      "First_Order" = TeX("First-Order ($S_i$)"),
      "Total_Order" = TeX("Total-Order ($S_{T_i}$)")
    ),
    guide = guide_legend(ncol = 2, reverse = TRUE)
  ) +
  coord_cartesian(clip = "off") +
  this_theme(base_size = 9) +
  theme(legend.key.size = unit(1, "lines")))

this_ggsave(p_bar_faceted, width = 6.5, height = 5.5)

# first order + total order ~ edge ratio ----------------------------------
#
# results_sobol_bar <- results_sobol_heatmap %>%
#   filter(Metric_Label == "Active Edge Ratio") %>%
#   arrange(Total_Order) %>%
#   pivot_longer(
#     cols = c(First_Order, Total_Order),
#     names_to = "Effect_Type",
#     values_to = "Variance_Contribution"
#   )
#
# (p_bar <- results_sobol_bar %>%
#   ggplot(aes(
#     x = Variance_Contribution,
#     y = Parameter_Label,
#     fill = factor(Effect_Type)
#   )) +
#   geom_bar(
#     stat = "identity",
#     position = position_dodge(width = 0.8),
#     width = 0.7
#   ) +
#   scale_x_continuous(
#     name = "Variance Contribution",
#     limits = c(NA, 1),
#     labels = scales::percent_format(accuracy = 1)
#   ) +
#   scale_y_discrete(
#     name = "Model Parameter",
#     labels = TeX
#   ) +
#   scale_fill_manual(
#     name = "Sensitivity Index",
#     values = c(
#       "First_Order" = "#ea801c",
#       "Total_Order" = "#1a80bb"
#     ),
#     labels = c(
#       "First_Order" = TeX("First-Order Index ($S_i$)"),
#       "Total_Order" = TeX("Total-Order Index ($S_{T_i}$)")
#     ),
#     guide = guide_legend(ncol = 1, reverse = TRUE)
#   ) +
#   coord_cartesian(clip = "off") +
#   this_theme(base_size = 8) +
#   theme(legend.key.size = unit(1, "lines")))
#
# this_ggsave(p_bar, width = 6.5, height = 5.5)
