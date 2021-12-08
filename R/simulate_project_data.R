#' Simulate project data
#'
#' @param n_sites `integer` number of sites.
#'
#' @param n_features `integer` number of features.
#'
#' @param n_actions `integer` number of actions.
#'
#' @param parameters `list` object containing configuration parameters.
#'
#' @param prop_locked_out `numeric` proportion of locked out actions. Defaults
#'   to 0.05 (5%).
#'
#' @return `list` object containing the simulated data.
#'
#' @export
simulate_project_data <- function(
  n_sites, n_features, n_actions, parameters, prop_locked_out = 0.05) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.count(n_sites),
    assertthat::is.count(n_features),
    assertthat::is.count(n_actions),
    is.list(parameters),
    assertthat::is.number(prop_locked_out))

  # simulate names
  site_ids <- paste("site", seq_len(n_sites))
  feature_ids <- paste("feature", seq_len(n_features))
  action_ids <- paste("action", seq_len(n_actions))

  # simulate descriptions
  site_descriptions <- paste("Site site", seq_len(n_sites))
  feature_descriptions <- paste("Species feature", seq_len(n_features))
  action_descriptions <- paste("Management action", seq_len(n_actions))

  # simulate sites
  ## data
  site_data <- tibble::tibble(
    name = site_ids,
    x = stats::runif(n_sites),
    y = stats::runif(n_sites),
    status = sample(action_ids, n_sites, replace = TRUE)
  )
  names(site_data) <- c(
    parameters$site_data_sheet$name_header,
    parameters$site_data_sheet$longitude_header,
    parameters$site_data_sheet$latitude_header,
    parameters$site_data_sheet$status_header
  )
  cost_data <- matrix(
    stats::runif(n_sites * n_actions) * 5, nrow = n_sites, ncol = n_actions)
  cost_data <- tibble::as_tibble(as.data.frame(cost_data))
  names(cost_data) <-  as.character(glue::glue(
    parameters$site_data_sheet$action_cost_header, action_ids = action_ids))
  site_data <- dplyr::bind_cols(site_data, cost_data)

  # simulate site feasibility
  ## data
  feasibility_data <- matrix(1, nrow = n_sites, ncol = n_actions)
  zero_idx <- sample.int(
    length(feasibility_data), floor(length(feasibility_data) * prop_locked_out))
  feasibility_data[zero_idx] <- 0
  feasibility_data <- tibble::as_tibble(as.data.frame(feasibility_data))
  names(feasibility_data) <-  as.character(glue::glue(
    parameters$feasibility_data_sheet$action_feasibility_header,
    action_ids = action_ids))
  feasibility_data <- dplyr::bind_cols(
    tibble::tibble(name = site_ids),
    feasibility_data
  )

  # simulate features
  ## data
  feature_data <- tibble::tibble(
    name = feature_ids,
    target = stats::runif(n_features, 0.4, 0.6) * n_sites,
    weight = stats::runif(n_features, 0.5, 1.0))
  names(feature_data) <- c(
    parameters$feature_data_sheet$name_header,
    parameters$feature_data_sheet$target_header,
    parameters$feature_data_sheet$weight_header)

  # simulate action expectation
  ## data
  cn <- as.character(glue::glue(
    parameters$action_expectation_sheet$action_expectation_header,
    feature_ids = feature_ids))
  action_expectation_data <- lapply(seq_along(action_ids), function(i) {
    s <- tibble::tibble(name = site_ids)
    v <- matrix(
      stats::runif(n_sites * n_features),
      nrow = n_sites, ncol = n_features
    )
    v <- tibble::as_tibble(as.data.frame(v))
    names(v) <- cn
    names(s) <- parameters$site_data_sheet$name_header
    dplyr::bind_cols(s, v)
  })

  # return result
  list(
    site_ids = site_ids,
    feature_ids = feature_ids,
    action_ids = action_ids,
    site_descriptions = site_descriptions,
    feature_descriptions = feature_descriptions,
    action_descriptions = action_descriptions,
    site_data = site_data,
    feasibility_data = feasibility_data,
    feature_data = feature_data,
    action_expectation_data = action_expectation_data
  )
}
