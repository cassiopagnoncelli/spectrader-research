# Feature Engineering.

# Helper function to check if a feature is a lagged version
fe_is_lagged_feature <- function(feature_name) {
  grepl("_\\d+$", feature_name)
}

# Helper function to parse lagged feature name
fe_parse_lagged_feature <- function(feature_name) {
  if (!fe_is_lagged_feature(feature_name)) {
    return(NULL)
  }
  parts <- strsplit(feature_name, "_")[[1]]
  lag_value <- as.integer(parts[length(parts)])
  base_name <- paste(parts[-length(parts)], collapse = "_")
  return(list(base = base_name, lag = lag_value))
}

# Base feature definitions
fe_base_feature_definitions <- list(
  S = function(data, params) {
    data$S
  },
  
  sd_short = function(data, params) {
    n <- params$sigma_short
    if (is.null(n)) stop("sigma_short parameter required for sd_short feature")
    RcppRoll::roll_sd(data$r, n = n, fill = NA, align = "right")
  },
  
  sd_long = function(data, params) {
    n <- params$sigma_long
    if (is.null(n)) stop("sigma_long parameter required for sd_long feature")
    RcppRoll::roll_sd(data$r, n = n, fill = NA, align = "right")
  },
  
  sd_ratio = function(data, params) {
    data$sd_short / data$sd_long
  },
  
  h_short = function(data, params) {
    n <- params$ent_short
    if (is.null(n)) stop("ent_short parameter required for h_short feature")
    runH(data$r, n)
  },
  
  h_long = function(data, params) {
    n <- params$ent_long
    if (is.null(n)) stop("ent_long parameter required for h_long feature")
    runH(data$r, n)
  },
  
  h_ratio = function(data, params) {
    data$h_short / data$h_long
  },
  
  vol_vix = function(data, params) {
    data$sd_short / data$vix
  },
  
  cr_short = function(data, params) {
    n <- params$cumret_short
    if (is.null(n)) n <- 3  # default to 3
    RcppRoll::roll_sum(data$r, n = n, fill = NA, align = "right")
  },
  
  cr_long = function(data, params) {
    n <- params$cumret_long
    if (is.null(n)) n <- 8  # default to 8
    RcppRoll::roll_sum(data$r, n = n, fill = NA, align = "right")
  }
)

# Base feature dependencies
fe_base_feature_dependencies <- list(
  S = character(0),
  sd_short = character(0),
  sd_long = character(0),
  sd_ratio = c("sd_short", "sd_long"),
  h_short = character(0),
  h_long = character(0),
  h_ratio = c("h_short", "h_long"),
  vol_vix = c("sd_short"),
  cr_short = character(0),
  cr_long = character(0)
)

# Function to get dependencies for a feature (including lagged features)
fe_get_dependencies <- function(feature_name) {
  lagged_info <- fe_parse_lagged_feature(feature_name)
  if (!is.null(lagged_info)) {
    # Lagged feature depends on its base feature
    return(lagged_info$base)
  }
  # Base feature - look up its dependencies
  deps <- fe_base_feature_dependencies[[feature_name]]
  if (is.null(deps)) return(character(0))
  return(deps)
}

# Function to resolve dependencies and determine computation order
fe_resolve_dependencies <- function(requested_features) {
  all_needed <- character(0)
  to_process <- requested_features
  
  while (length(to_process) > 0) {
    feature <- to_process[1]
    to_process <- to_process[-1]
    
    if (!(feature %in% all_needed)) {
      all_needed <- c(all_needed, feature)
      
      # Add dependencies
      deps <- fe_get_dependencies(feature)
      if (length(deps) > 0) {
        for (dep in deps) {
          if (!(dep %in% all_needed)) {
            to_process <- c(to_process, dep)
          }
        }
      }
    }
  }
  
  # Topological sort to get correct computation order
  sorted <- character(0)
  remaining <- all_needed
  
  while (length(remaining) > 0) {
    # Find features with all dependencies already computed
    ready <- character(0)
    for (f in remaining) {
      deps <- fe_get_dependencies(f)
      if (length(deps) == 0 || all(deps %in% sorted)) {
        ready <- c(ready, f)
      }
    }
    
    if (length(ready) == 0) {
      stop("Circular dependency detected in features")
    }
    
    sorted <- c(sorted, ready)
    remaining <- setdiff(remaining, ready)
  }
  
  return(sorted)
}

fe <- function(data, features, ...) {
  # Extract parameters from ...
  params <- list(...)
  
  # Validate requested features (check base features exist)
  invalid_features <- character(0)
  for (feature in features) {
    lagged_info <- fe_parse_lagged_feature(feature)
    if (!is.null(lagged_info)) {
      # Check if base feature exists
      if (!(lagged_info$base %in% names(fe_base_feature_definitions))) {
        invalid_features <- c(invalid_features, feature)
      }
    } else {
      # Check if base feature exists
      if (!(feature %in% names(fe_base_feature_definitions))) {
        invalid_features <- c(invalid_features, feature)
      }
    }
  }
  if (length(invalid_features) > 0) {
    stop(paste("Unknown features:", paste(invalid_features, collapse = ", ")))
  }
  
  # Resolve dependencies and get computation order
  computation_order <- fe_resolve_dependencies(features)
  
  # Compute features in order
  result <- data
  for (feature_name in computation_order) {
    lagged_info <- fe_parse_lagged_feature(feature_name)
    if (!is.null(lagged_info)) {
      # This is a lagged feature - apply lag to base feature
      result[[feature_name]] <- dplyr::lag(result[[lagged_info$base]], lagged_info$lag)
    } else {
      # This is a base feature - compute it
      result[[feature_name]] <- fe_base_feature_definitions[[feature_name]](result, params)
    }
  }
  
  # Return only original columns plus requested features
  result <- result %>%
    dplyr::select(dplyr::all_of(c(names(data), features)))
  
  return(result)
}

fe_dqr <- function(
  data, sigma_short = 6, sigma_long = 20, ent_short = 9, ent_long = 20, cumret_short = 3, cumret_long = 8
) {
  fe(
    data,
    c("S_1", "S_2", "sd_short", "sd_long", "sd_ratio",
      "h_short", "h_long", "h_ratio",
      "sd_short_1", "sd_long_1", "sd_ratio_1",
      "h_short_1", "h_long_1", "h_ratio_1",
      "vol_vix", "cr_short", "cr_long"
    ),
    sigma_short = sigma_short,
    sigma_long = sigma_long,
    ent_short = ent_short,
    ent_long = ent_long,
    cumret_short = cumret_short,
    cumret_long = cumret_long
  )
}
