# Quick install script for susieR
if (!require(remotes, quietly = TRUE)) {
  install.packages("remotes", repos = "https://cran.rstudio.com")
}
remotes::install_github("stephenslab/susieR")
cat("susieR installation complete. Please restart R and run the analysis.\n")
