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
  # Extract clean package name (e.g., "fets_1.3.2.9000.tar.gz" -> "fets")
  pkg <- sub("_.*", "", pkg_file)

  renv::remove(pkg)
  renv::install(paste0("packages/", pkg_file), prompt = FALSE)

  if (paste0("package:", pkg) %in% search()) {
    detach(paste0("package:", pkg), unload = TRUE)
  }
  library(pkg, character.only = TRUE)
}
