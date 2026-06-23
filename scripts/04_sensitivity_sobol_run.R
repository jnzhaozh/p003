source(here::here("scripts/01_project_setup.R"))
source(here::here("scripts/02_model_define.R"))

# Environment & Resource Configuration
Sys.setenv(OPENBLAS_NUM_THREADS = 1)
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(TZ = "Asia/Tokyo")
data.table::setDTthreads(1)

# 1. Global Simulation Constants ------------------------------------------

seed <- 123L
M <- 10L # Topic inventory
N <- 10000L # Population size

# 2. Saltelli Sampling Setup for Sobol ------------------------------------

# Sobol/Saltelli math: Total runs = base sample * (parameter count + 2)
n_params <- 8L
sample_size <- 10000L
n_runs <- sample_size * (n_params + 2)

# Generate two independent LHS matrices (A and B)
lhs_A <- lhs::randomLHS(sample_size, n_params)
lhs_B <- lhs::randomLHS(sample_size, n_params)

# Helper function to map raw [0,1] space to your exact parameter boundaries
map_parameters <- function(mat) {
  data.frame(
    mu_p = qunif(mat[, 1], 0.1, 0.9),
    sigma_p = qunif(mat[, 2], 0.05, 0.2),
    K = floor(qunif(mat[, 3], 1, 6)),
    mu_x = qunif(mat[, 4], 0.2, 0.8),
    sigma_x = qunif(mat[, 5], 0.05, 0.2),
    gamma = qunif(mat[, 6], 0.1, 10),
    c = qunif(mat[, 7], 3, 12),
    h = qunif(mat[, 8], 0.01, 0.8)
  )
}

X1 <- map_parameters(lhs_A)
X2 <- map_parameters(lhs_B)

# Build the Saltelli design using the Jansen estimator (Highly robust)
sobol_design <- sensitivity::soboljansen(
  model = NULL,
  X1 = X1,
  X2 = X2,
  nboot = 1000
)

# Extract the woven matrix to feed into our simulation loop
simulation_conditions <- sobol_design %>%
  getElement("X") %>%
  data.table::as.data.table() %>%
  .[, condition_id := .I]

batch_size <- 500L
batches <- split(
  simulation_conditions,
  ceiling(seq_len(n_runs) / batch_size)
)

# 3. Parallel Execution Framework -----------------------------------------

results_list <- vector("list", length(batches))

plan(multisession, workers = 4)
tictoc::tic("Total Simulation Time")

for (b_idx in seq_along(batches)) {
  cat(sprintf("Processing batch %d / %d\n", b_idx, length(batches)))

  current_batch <- batches[[b_idx]]

  metrics_batch <- furrr::future_pmap(
    current_batch,
    function(condition_id, mu_p, sigma_p, K, mu_x, sigma_x, gamma, c, h) {
      # 1. Ensure unique randomness for every single run
      set.seed(seed + condition_id)

      # 2. Generate the Information Environment
      p <- draw_topic_prevalence(M, mu_p, sigma_p)

      C_list <- draw_latent_correlation(M, K, mu_x, sigma_x, gamma)
      C <- C_list$latent_correlation_mat
      C_list$latent_correlation_mat <- NULL

      # 3. Generate Binary Information Diets
      B_diets <- generate_information_diets(M, N, p, C)

      # 4. Generate Baseline Social Network
      A_edges <- generate_social_network(N, c, h)

      # 5. Return Diffusion Network Metrics
      return(c(
        calculate_diffusion_metrics(N, A_edges, B_diets),
        list(condition_id = condition_id),
        C_list
      ))
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

  # Bind the results of this batch and store it
  results_list[[b_idx]] <- data.table::rbindlist(metrics_batch)

  # Explicit Memory Cleanup
  rm(current_batch, metrics_batch)
  gc()
}

tictoc::toc()
plan(sequential)

# 4. Aggregation and Save -------------------------------------------------

results_raw <- results_list %>%
  data.table::rbindlist() %>%
  merge(
    .,
    simulation_conditions,
    by = "condition_id",
    all.x = TRUE
  ) %>%
  data.table::setorder(., condition_id)

this_saveRDS(results_raw)

params <- colnames(sobol_design$X)
metrics <- grep("^diffusion_", names(results_raw), value = TRUE)

results_sobol <- lapply(metrics, \(metric) {
  sensitivity::tell(sobol_design, y = results_raw[[metric]])

  data.frame(
    Parameter = params,
    Metric = metric,
    First_Order = sobol_design$S$original,
    Total_Order = sobol_design$T$original
  )
}) %>%
  data.table::rbindlist()

this_saveRDS(results_sobol)
