source(here::here("scripts/01_project_setup.R"))
source(here::here("scripts/02_model_define.R"))

# Environment & Resource Configuration
Sys.setenv(OPENBLAS_NUM_THREADS = 1)
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(TZ = "Asia/Tokyo")
data.table::setDTthreads(1)


# 1. Execution Function ---------------------------------------------------

simulate_phase_transition <- function(sim_param) {
  sim_name <- deparse(substitute(sim_param))
  cat(sprintf("\n=== Starting Experiment: %s ===\n", sim_name))

  batch_size <- 500L
  batches <- split(sim_param, ceiling(seq_len(nrow(sim_param)) / batch_size))
  results_list <- vector("list", length(batches))

  for (b_idx in seq_along(batches)) {
    cat(sprintf("Processing batch %d / %d\n", b_idx, length(batches)))
    current_batch <- batches[[b_idx]]

    metrics_batch <- furrr::future_pmap(
      current_batch,
      \(condition_id, rep_id, mu_p, sigma_p, K, mu_x, sigma_x, gamma, c, h) {
        set.seed(seed + condition_id)

        p <- draw_topic_prevalence(M, mu_p, sigma_p)

        C_list <- draw_latent_correlation(M, K, mu_x, sigma_x, gamma)
        C <- C_list$latent_correlation_mat
        C_list$latent_correlation_mat <- NULL

        B_diets <- generate_information_diets(M, N, p, C)

        A_edges <- generate_social_network(N, c, h)

        return(c(calculate_diffusion_metrics(N, A_edges, B_diets), C_list))
      },
      .progress = TRUE,
      .options = furrr::furrr_options(
        scheduling = 1,
        globals = c(
          "seed",
          "M",
          "N",
          "draw_topic_prevalence",
          "draw_latent_correlation",
          "generate_information_diets",
          "generate_social_network",
          "calculate_diffusion_metrics"
        )
      )
    )

    results_list[[b_idx]] <- cbind(
      current_batch,
      data.table::rbindlist(metrics_batch)
    )
    rm(current_batch, metrics_batch)
    gc()
  }

  results_summary <- results_list %>%
    data.table::rbindlist() %>%
    .[,
      .(
        mean_edge_ratio = mean(diffusion_edge_ratio),
        mean_lcc_prop = mean(diffusion_lcc_prop),
        mean_cluster_coeff = mean(diffusion_cluster_coeff),
        mean_latent_cor = mean(latent_correlation_mean),
        mean_latent_sd = mean(latent_correlation_sd),
        mean_topic_dist = mean(topic_distance_mean),
        mean_topic_sd = mean(topic_distance_sd)
      ),
      by = .(mu_p, sigma_p, K, mu_x, sigma_x, gamma, c, h)
    ]

  this_saveRDS(results_summary, sim_name)

  return(invisible(results_summary))
}


# 2. Define the Three Theoretical Grids -----------------------------------

seed <- 123L
M <- 10L # Topic cardinality
N <- 10000L # Population size
n_replications <- 30L # Monte Carlo smoothing

param_sigma_x_gamma <- expand.grid(
  mu_p = 0.5,
  sigma_p = 0.125,
  K = 3L,
  mu_x = 0.5,
  sigma_x = seq(0.05, 0.2, length.out = 30), # <- Vary
  gamma = seq(0.1, 10, length.out = 30), # <- Vary
  c = 7.5,
  h = 0.405,
  rep_id = seq_len(n_replications)
) %>%
  data.table::as.data.table() %>%
  .[, condition_id := .I]


param_gamma_c <- expand.grid(
  mu_p = 0.5,
  sigma_p = 0.125,
  K = 3L,
  mu_x = 0.5,
  sigma_x = 0.125,
  gamma = seq(0.1, 10, length.out = 30), # <- Vary
  c = seq(3, 12, length.out = 30), # <- Vary
  h = 0.405,
  rep_id = seq_len(n_replications)
) %>%
  data.table::as.data.table() %>%
  .[, condition_id := .I + max(param_sigma_x_gamma$condition_id)]


param_mu_p_sigma_x <- expand.grid(
  mu_p = seq(0.1, 0.9, length.out = 30), # <- Vary
  sigma_p = 0.125,
  K = 3L,
  mu_x = 0.5,
  sigma_x = seq(0.05, 0.2, length.out = 30), # <- Vary
  gamma = 5.05,
  c = 7.5,
  h = 0.405,
  rep_id = seq_len(n_replications)
) %>%
  data.table::as.data.table() %>%
  .[, condition_id := .I + max(param_gamma_c$condition_id)]

# 3. Execute the Runs -----------------------------------------------------

plan(multisession, workers = 4)

simulate_phase_transition(param_sigma_x_gamma)
simulate_phase_transition(param_gamma_c)
simulate_phase_transition(param_mu_p_sigma_x)

plan(sequential)
