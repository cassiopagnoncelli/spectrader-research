# ChartSmith
renv::install("/Users/cassio/spec/chartsmith", prompt = FALSE)
detach("package:chartsmith", unload = TRUE)
library("chartsmith")

# Spectrader
renv::install("/Users/cassio/spec/spectrader", prompt = FALSE)
detach("package:spectrader", unload = TRUE)
library("spectrader")

# Shipped-in packages: fets, qboost
for (pkg_file in dir("packages")) {
  pkg <- sub("_.*", "", pkg_file)
  pkg_path <- file.path("packages", pkg_file)
  pkg_search <- paste0("package:", pkg)

  # Fully unload everything
  if (pkg %in% loadedNamespaces()) {
    try(unloadNamespace(pkg), silent = TRUE)
  }
  if (pkg_search %in% search()) {
    try(detach(pkg_search, unload = TRUE, character.only = TRUE), silent = TRUE)
  }

  # Remove from renv
  renv::remove(pkg)

  # Install clean
  renv::install(pkg_path, prompt = FALSE)

  # Attach clean version
  library(pkg, character.only = TRUE)
}
