# This R declaration is the source of truth for the reproducible environment.
# Regenerate default.nix from this file instead of editing the generated Nix
# expression manually. The 2026-06-22 rstats-on-nix snapshot contains R 4.6.0
# and fixes the complete R and system-package ecosystem at one date.
if (!requireNamespace("rix", quietly = TRUE)) {
  stop(
    "Package `rix` is required to regenerate default.nix. ",
    "See https://docs.ropensci.org/rix/ for bootstrap instructions.",
    call. = FALSE
  )
}

rix::rix(
  date = "2026-06-22",
  r_pkgs = c(
    "dplyr",
    "forcats",
    "fs",
    "ggplot2",
    "ggrepel",
    "ggridges",
    "knitr",
    "languageserver",
    "patchwork",
    "quarto",
    "ragg",
    "rmarkdown",
    # Keep rix available in the built shell so the environment can regenerate
    # itself after a contributor has completed the one-time bootstrap.
    "rix",
    "scales",
    "stringi",
    "stringr",
    "tibble",
    "tidyr"
  ),
  system_pkgs = c(
    "dejavu_fonts",
    "fontconfig",
    "git",
    "quarto"
  ),
  ide = "none",
  project_path = ".",
  overwrite = TRUE
)
