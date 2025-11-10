# Feature Engineering.

# Helper function to check if a feature is an acceleration version
fe_is_acceleration_feature <- function(feature_name) {
  grepl("_accel_\\d+$", feature_name)
}

# Helper function to parse acceleration feature name
fe_parse_acceleration_feature <- function(feature_name) {
  if (!fe_is_acceleration_feature(feature_name)) {
    return(NULL)
  }
  # Extract the acceleration lag number
  parts <- strsplit(feature_name, "_")[[1]]
  lag_value <- as.integer(parts[length(parts)])
  
  # Remove "_accel_N" to get base name
  base_name <- sub("_accel_\\d+$", "", feature_name)
  
  return(list(
    base = base_name,
    vel1 = lag_value,      # e.g., 0 in ma_accel_0
    vel2 = lag_value + 1   # e.g., 1 in ma_accel_0
  ))
}

# Helper function to check if a feature is a velocity version
fe_is_velocity_feature <- function(feature_name) {
  grepl("_vel_\\d+$", feature_name)
}

# Helper function to parse velocity feature name
fe_parse_velocity_feature <- function(feature_name) {
  if (!fe_is_velocity_feature(feature_name)) {
    return(NULL)
  }
  # Extract the velocity lag number
  parts <- strsplit(feature_name, "_")[[1]]
  lag_value <- as.integer(parts[length(parts)])
  
  # Remove "_vel_N" to get base name
  base_name <- sub("_vel_\\d+$", "", feature_name)
  
  return(list(
    base = base_name,
    lag1 = lag_value,      # e.g., 0 in ma_vel_0
    lag2 = lag_value + 1   # e.g., 1 in ma_vel_0
  ))
}

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
  
  vix = function(data, params) {
    data$vix
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
  vix = character(0),
  sd_short = character(0),
  sd_long = character(0),
  sd_ratio = c("sd_short", "sd_long"),
  h_short = character(0),
  h_long = character(0),
  h_ratio = c("h_short", "h_long"),
  vol_vix = c("sd_short", "vix"),
  cr_short = character(0),
  cr_long = character(0)
)

# Function to get dependencies for a feature (including acceleration, velocity and lagged features)
fe_get_dependencies <- function(feature_name) {
  # Check acceleration feature first (highest precedence)
  accel_info <- fe_parse_acceleration_feature(feature_name)
  if (!is.null(accel_info)) {
    # Acceleration feature depends on two velocity versions of base feature
    vel1_feature <- paste0(accel_info$base, "_vel_", accel_info$vel1)
    vel2_feature <- paste0(accel_info$base, "_vel_", accel_info$vel2)
    return(c(vel1_feature, vel2_feature))
  }
  
  # Check velocity feature
  velocity_info <- fe_parse_velocity_feature(feature_name)
  if (!is.null(velocity_info)) {
    # Velocity feature depends on two lagged versions of base feature
    lag1_feature <- paste0(velocity_info$base, "_", velocity_info$lag1)
    lag2_feature <- paste0(velocity_info$base, "_", velocity_info$lag2)
    return(c(lag1_feature, lag2_feature))
  }
  
  # Check lagged feature
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
    # Check acceleration feature first (highest precedence)
    accel_info <- fe_parse_acceleration_feature(feature)
    if (!is.null(accel_info)) {
      # Check if base feature exists
      if (!(accel_info$base %in% names(fe_base_feature_definitions))) {
        invalid_features <- c(invalid_features, feature)
      }
    } else {
      # Check velocity feature
      velocity_info <- fe_parse_velocity_feature(feature)
      if (!is.null(velocity_info)) {
        # Check if base feature exists
        if (!(velocity_info$base %in% names(fe_base_feature_definitions))) {
          invalid_features <- c(invalid_features, feature)
        }
      } else {
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
    # Check acceleration feature first (highest precedence)
    accel_info <- fe_parse_acceleration_feature(feature_name)
    if (!is.null(accel_info)) {
      # This is an acceleration feature - compute difference between two velocities
      vel1_col <- paste0(accel_info$base, "_vel_", accel_info$vel1)
      vel2_col <- paste0(accel_info$base, "_vel_", accel_info$vel2)
      result[[feature_name]] <- result[[vel1_col]] - result[[vel2_col]]
    } else {
      # Check velocity feature
      velocity_info <- fe_parse_velocity_feature(feature_name)
      if (!is.null(velocity_info)) {
        # This is a velocity feature - compute difference between two lags
        lag1_col <- paste0(velocity_info$base, "_", velocity_info$lag1)
        lag2_col <- paste0(velocity_info$base, "_", velocity_info$lag2)
        result[[feature_name]] <- result[[lag1_col]] - result[[lag2_col]]
      } else {
        lagged_info <- fe_parse_lagged_feature(feature_name)
        if (!is.null(lagged_info)) {
          # This is a lagged feature - apply lag to base feature
          result[[feature_name]] <- dplyr::lag(result[[lagged_info$base]], lagged_info$lag)
        } else {
          # This is a base feature - compute it
          result[[feature_name]] <- fe_base_feature_definitions[[feature_name]](result, params)
        }
      }
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
      "vol_vix", "vix_vel_0", "vix_accel_0",
      "cr_short", "cr_long"
    ),
    sigma_short = sigma_short,
    sigma_long = sigma_long,
    ent_short = ent_short,
    ent_long = ent_long,
    cumret_short = cumret_short,
    cumret_long = cumret_long
  )
}
