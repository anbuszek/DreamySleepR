#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_arg <- args[startsWith(args, file_arg)]

root <- if (length(script_arg) > 0) {
  normalizePath(file.path(dirname(sub(file_arg, "", script_arg[[1]])), ".."), winslash = "/")
} else {
  normalizePath(getwd(), winslash = "/")
}

if (!file.exists(file.path(root, "DESCRIPTION"))) {
  stop("Run this script from the package root or via tools/render-docs.R.", call. = FALSE)
}

old_wd <- setwd(root)
on.exit(setwd(old_wd), add = TRUE)

require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required to render documentation.", package), call. = FALSE)
  }
}

require_package("rmarkdown")
require_package("knitr")
require_package("ggplot2")

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(root, quiet = TRUE)
} else if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(root, quiet = TRUE)
} else {
  stop("pkgload or devtools is required to run the doc pipeline.", call. = FALSE)
}

# Clean old plots
dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
old_plots <- list.files("man/figures", pattern = "^README-.*\\.png$", full.names = TRUE)
if (length(old_plots) > 0) {
  file.remove(old_plots)
}

# Source render-site.R to run analysis, save plots to man/figures, and render index.html
message("Sourcing tools/render-site.R...")
source(file.path("tools", "render-site.R"), local = FALSE)

# Render README.Rmd -> README.md (which uses and links plots in man/figures)
message("Rendering README.md...")
rmarkdown::render(
  input = "README.Rmd",
  output_format = "github_document",
  encoding = "UTF-8",
  quiet = FALSE
)

# Render vignettes/poradnik_mcda.Rmd -> doc/poradnik_mcda.html
message("Rendering vignette preview...")
dir.create("doc", showWarnings = FALSE)
rmarkdown::render(
  input = "vignettes/poradnik_mcda.Rmd",
  output_format = rmarkdown::html_vignette(),
  output_dir = "doc",
  encoding = "UTF-8",
  quiet = FALSE
)

message("Pipeline completed successfully!")
