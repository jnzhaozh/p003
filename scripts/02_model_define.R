source(here::here("scripts/01_project_setup.R"))

# 1. GENERATE TOPIC PREVALENCE (p_i) --------------------------------------

draw_topic_prevalence <- function(
  M, # Topic inventory
  mu_p, # Topic prevalence mean
  sigma_p, # Topic prevalence standard deviation
  eps = 1e-10
) {
  # 1. Validate mathematical boundaries for the Beta distribution
  stopifnot(
    length(mu_p) == 1,
    length(sigma_p) == 1,
    mu_p > 0,
    mu_p < 1,
    sigma_p > 0,
    sigma_p < sqrt(mu_p * (1 - mu_p))
  )

  # 2. Convert Mean and SD into Beta shape parameters (Alpha and Beta)
  concentration <- mu_p * (1 - mu_p) / sigma_p^2 - 1
  alpha <- mu_p * concentration
  beta <- (1 - mu_p) * concentration

  # 3. Draw the random prevalences (vector p)
  topic_prevalence <- rbeta(
    M,
    shape1 = alpha,
    shape2 = beta
  )

  # 4. Enforce boundaries to prevent absolute 0.0s and 1.0s
  return(pmin(pmax(topic_prevalence, eps), 1 - eps))
}


# 2.  GENERATE LATENT CORRELATION (C) -------------------------------------

draw_latent_correlation <- function(
  M, # Topic inventory
  K, # Topic dimension
  mu_x,
  sigma_x,
  gamma, # Topic distance decay
  eps = 1e-10
) {
  stopifnot(
    mu_x > 0,
    mu_x < 1,
    sigma_x > 0,
    sigma_x < sqrt(mu_x * (1 - mu_x))
  )

  # 1. Build the spatial matrix (Topic space vectors x_i)
  # topic_coords <- matrix(runif(M * K), nrow = M)
  # topic_distance_mat <- as.matrix(dist(topic_coords))

  concentration <- (mu_x * (1 - mu_x) / sigma_x^2) - 1
  alpha <- mu_x * concentration
  beta <- (1 - mu_x) * concentration

  topic_coords <- matrix(
    rbeta(
      M * K,
      shape1 = alpha,
      shape2 = beta
    ),
    nrow = M
  )
  topic_distance_mat <- as.matrix(dist(topic_coords))
  off_diag_dist <- topic_distance_mat[upper.tri(topic_distance_mat)]

  # 2. Apply the distance decay equation: C_ij = exp(-\gamma * d(i,j))
  latent_correlation_mat <- exp(-gamma * topic_distance_mat)
  latent_correlation_mat <- pmin(pmax(latent_correlation_mat, eps), 1 - eps)
  diag(latent_correlation_mat) <- 1
  off_diag_cor <- latent_correlation_mat[upper.tri(latent_correlation_mat)]

  return(list(
    latent_correlation_mat = latent_correlation_mat,
    latent_correlation_mean = mean(off_diag_cor),
    latent_correlation_sd = sd(off_diag_cor),
    topic_distance_mean = mean(off_diag_dist),
    topic_distance_sd = sd(off_diag_dist)
  ))
}


# 3. GENERATE BINARY INFORMATION DIETS (B) via Copula ---------------------

generate_information_diets <- function(
  M, # Topic inventory
  N, # Population size
  p, # Topic prevalence vector
  C, # Latent correlation matrix
  eps = 1e-10
) {
  # 1. Add tiny noise to diagonal to ensure strict positive definiteness
  C_adj <- C + diag(eps, M)
  chol_C <- chol(C_adj)

  # 2. Draw continuous latent preferences (Z_v)
  latent_preference <- matrix(
    rnorm(N * M),
    nrow = N,
    ncol = M
  ) %*%
    chol_C

  # 3. Map to binary (B_vi) using the inverse CDF (qnorm) of the prevalences
  # Note: Z <= qnorm(p) is mathematically identical to Phi(Z) <= p
  information_diet <- sweep(latent_preference, 2, qnorm(p), "<=")

  # 4. Compress diets into integer representations using bitwise weights
  topic_weights <- 2L^(seq_len(M) - 1L)

  return(as.integer(information_diet %*% topic_weights))
}


# 4. GENERATE BASELINE SOCIAL NETWORK (A) ---------------------------------

generate_social_network <- function(
  N, # Population Size
  c, # Average connectivity / mean degree
  h # Degree heterogeneity
) {
  # Calculate total undirected edges needed
  n_edges <- round((N * c) / 2)

  # Generate fitness scores from a log-normal distribution
  # Parameterized to maintain a mean of 1 while varying variance (h)
  fitness_scores <- rlnorm(
    N,
    meanlog = -0.5 * h^2,
    sdlog = h
  )

  # Draw the baseline network (Matrix A)
  graph <- igraph::sample_fitness(
    no.of.edges = n_edges,
    fitness.out = fitness_scores,
    fitness.in = NULL,
    loops = TRUE,
    multiple = TRUE
  ) %>%
    igraph::simplify(
      remove.multiple = TRUE,
      remove.loops = TRUE
    )

  # Extract the edge list for fast computation downstream
  edges <- igraph::as_edgelist(graph, names = FALSE)
  storage.mode(edges) <- "integer"
  rm(graph)

  return(list(
    from = edges[, 1],
    to = edges[, 2]
  ))
}


# -------------------------------------------------------------------------

calculate_diffusion_metrics <- function(
  N, # Population size
  A_edges, # Baseline social network edgelist (derived from Matrix A)
  B_diets # Compressed information diets (derived from Matrix B)
) {
  social_from <- A_edges$from
  social_to <- A_edges$to
  total_social <- length(social_from) # Total edges in the baseline network (A)

  # Condition of minimal information congruence: A * I(BB^T > 0)
  # Evaluated instantaneously via bitwise AND operations
  has_overlap <- bitwAnd(
    B_diets[social_from],
    B_diets[social_to]
  ) !=
    0L

  diffusion_from <- social_from[has_overlap]
  diffusion_to <- social_to[has_overlap]
  total_diffusion <- sum(has_overlap) # Total edges in the diffusion network A_diff
  if (total_diffusion == 0) {
    return(list(
      diffusion_edge_ratio = 0,
      diffusion_lcc_prop = 0,
      diffusion_cluster_coeff = 0
    ))
  }

  diffusion_degree <- tabulate(diffusion_from, nbins = N) +
    tabulate(diffusion_to, nbins = N)

  diffusion_ls <- as.vector(rbind(diffusion_from, diffusion_to))
  diffusion_g <- igraph::make_graph(
    edges = diffusion_ls,
    n = N,
    directed = FALSE
  )

  diffusion_csize <- max(igraph::components(diffusion_g)$csize)
  diffusion_trans <- igraph::transitivity(diffusion_g, type = "global")
  if (is.nan(diffusion_trans)) {
    diffusion_trans <- 0
  }

  return(list(
    diffusion_edge_ratio = total_diffusion / total_social,
    diffusion_lcc_prop = diffusion_csize / N,
    diffusion_cluster_coeff = diffusion_trans
  ))
}
