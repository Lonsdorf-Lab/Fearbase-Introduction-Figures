# FEARBASE publication figure generation ---------------------------------------

# Package imports --------------------------------------------------------------
library(dplyr)
library(forcats)
library(ggplot2)
library(ggrepel)
library(ggridges)
library(grDevices)
library(grid)
library(patchwork)
library(ragg)
library(scales)
library(stats)
library(stringr)
library(tibble)
library(tidyr)
library(utils)


options(
  stringsAsFactors = FALSE,
  warn = 1
)

# Environment setup and and inputs ---------------------------------------------
# The figures were built with R 4.6.0.

# directory where data files (rda) are located
# NOTE: the original data are not supplied and not available upon request as they
#  build the IP of the Fearbase.
input_dir <- file.path("data", "input")

data_long_path <- file.path(input_dir, "data_long.rda") # data in long format
metadata_path <- file.path(input_dir, "metadata.rda") # study and dataset metadata

# Load each .rda file into an isolated environment. This avoids accidentally
# retaining any additional objects that may have been saved in either file.
data_long_environment <- new.env(parent = emptyenv())
data_long_loaded_names <- load(data_long_path, envir = data_long_environment)

data_long <- as_tibble(data_long_environment$data_long)
rm(data_long_environment, data_long_loaded_names)

metadata_environment <- new.env(parent = emptyenv())
metadata_loaded_names <- load(metadata_path, envir = metadata_environment)

metadata <- as_tibble(metadata_environment$metadata)
rm(metadata_environment, metadata_loaded_names)


# Shared visual settings -------------------------------------------------------

publication_font_family <- "Arial"

# Figure font sizes
figure_text <- list(
  title = 11,
  axis_title = 9,
  axis_text = 7,
  label = 5,
  strip = 7,
  legend_title = 9,
  legend_text = 7,
  base = 7
)

# Fearbase colour palette
fearbase_colors <- c(
  blue_100 = "#99C3FF",
  blue_300 = "#267FFF",
  blue_500 = "#0032A0",
  blue_700 = "#002066",
  blue_900 = "#00081A",
  light_blue = "#99C3FF",
  orange = "#FF8800",
  blue = "#0032A0",
  dark_blue = "#00081A",
  light_orange = "#FFECE2"
)

# Fearbase colour palette subset
fearbase_base_palette <- unname(
  fearbase_colors[c("blue_100", "blue_300", "blue_500", "blue_700", "blue_900")]
)

# ggplot2 measures text in millimetres, while the publication specification uses
# points. Store the conversion factor for points (specification) to mm (ggplot2)
# once.
points_to_mm <- 25.4 / 72.27

# Fearbase base theme
theme_fearbase <- theme_classic(
  base_size = figure_text$base,
  base_family = publication_font_family
) +
  theme(
    text = element_text(
      family = publication_font_family,
      color = fearbase_colors[["dark_blue"]]
    ),
    plot.title = element_text(
      size = figure_text$title,
      color = fearbase_colors[["dark_blue"]]
    ),
    plot.tag = element_text(
      size = figure_text$title,
      color = fearbase_colors[["dark_blue"]]
    ),
    axis.title = element_text(size = figure_text$axis_title),
    axis.text = element_text(size = figure_text$axis_text),
    strip.text = element_text(
      size = figure_text$strip,
      face = "bold"
    ),
    legend.title = element_text(size = figure_text$legend_title),
    legend.text = element_text(size = figure_text$legend_text),
    legend.key.size = unit(0.3, "cm"),
    plot.title.position = "plot"
  )

# Fearbase void theme for pie charts
theme_fearbase_void <- theme_void(
  base_size = figure_text$base,
  base_family = publication_font_family
) +
  theme(
    text = element_text(
      family = publication_font_family,
      color = fearbase_colors[["dark_blue"]]
    ),
    plot.title = element_text(
      size = figure_text$title,
      color = fearbase_colors[["dark_blue"]]
    ),
    plot.tag = element_text(size = figure_text$title),
    legend.title = element_text(size = figure_text$legend_title),
    legend.text = element_text(size = figure_text$legend_text),
    plot.title.position = "plot"
  )


# Figure 2: publication year and participant characteristics ------------------

figure_02_dataset_variable <- "study_id"
figure_02_dataset_label <- "Study ID"

## Data preprocessing Figure 2 ------------------------------------------------

### Figure 2 A: Publication Year ----------------------------------------------
figure_02_publication_year <- metadata |>
  select("year_data") |>
  count(.data$year_data, name = "n") |>
  transmute(
    year = as.numeric(as.character(.data$year_data)),
    n = .data$n
  ) |>
  arrange(.data$year)

### Figure 2 B: Sample Size ---------------------------------------------------
figure_02_sample_size <- data_long |>
  select(
    all_of(figure_02_dataset_variable),
    "participant_id"
  ) |>
  filter(
    !is.na(.data[[figure_02_dataset_variable]]),
    !is.na(.data$participant_id)
  ) |>
  distinct() |>
  count(.data[[figure_02_dataset_variable]], name = "n") |>
  arrange(desc(.data$n)) |>
  mutate(
    across(all_of(figure_02_dataset_variable), as.factor)
  )

### Figure 2 C: Age Density ---------------------------------------------------
figure_02_age <- data_long |>
  filter(.data$measure == "age") |>
  transmute(
    condition_id = as.factor(.data$condition_id),
    study_id = as.factor(.data$study_id),
    paper_cond_id = as.factor(.data$paper_cond_id),
    paper_study_id = as.factor(.data$paper_study_id),
    participant_id = .data$participant_id,
    age = as.numeric(as.character(.data$value))
  ) |>
  filter(!is.na(.data$age))

### Figure 2 D: Sex Counts ----------------------------------------------------
figure_02_participant_keys <- data_long |>
  select("condition_id", "study_id", "participant_id") |>
  filter(!is.na(.data$participant_id)) |>
  distinct()

figure_02_reported_sex <- data_long |>
  filter(.data$measure %in% c("sex", "gender")) |>
  transmute(
    condition_id = .data$condition_id,
    study_id = .data$study_id,
    participant_id = .data$participant_id,
    value = str_to_lower(
      str_trim(as.character(.data$value))
    )
  ) |>
  filter(!is.na(.data$participant_id)) |>
  group_by(
    .data$condition_id,
    .data$study_id,
    .data$participant_id
  ) |>
  summarise(value = first(.data$value), .groups = "drop")

figure_02_unreported_sex <- figure_02_participant_keys |>
  anti_join(
    figure_02_reported_sex,
    by = c("condition_id", "study_id", "participant_id")
  ) |>
  mutate(value = "not reported")

figure_02_sex <- bind_rows(
  figure_02_reported_sex,
  figure_02_unreported_sex
) |>
  mutate(
    sex = case_when(
      is.na(.data$value) | .data$value == "" ~ "NA",
      .data$value == "not reported" ~ "not reported",
      str_detect(.data$value, "^m") ~ "male",
      str_detect(.data$value, "^f") ~ "female",
      TRUE ~ "NA"
    ),
    sex = factor(
      .data$sex,
      levels = c("NA", "not reported", "male", "female")
    )
  )

## Plot Generation Figure 2 -----------------------------------------------------

### Panel 2 A: data publication year --------------------------------------------
figure_02_panel_a <- ggplot(
  figure_02_publication_year,
  aes(x = .data$year, y = .data$n)
) +
  geom_col(fill = fearbase_colors[["blue"]]) +
  scale_x_continuous(breaks = seq(2014, 2026, by = 2)) +
  scale_y_continuous(
    breaks = 0:12,
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(2014, 2026), ylim = c(0, 12)) +
  labs(
    title = "Data Publication Year",
    x = "Year of Data Publication",
    y = "Number of Studies"
  ) +
  theme_fearbase +
  theme(plot.title.position = "panel")

### Panel 2 B: participants per study -------------------------------------------
figure_02_panel_b <- ggplot(
  figure_02_sample_size,
  aes(
    x = fct_reorder(
      .data[[figure_02_dataset_variable]],
      .data$n,
      .desc = TRUE
    ),
    y = .data$n
  )
) +
  geom_col(fill = fearbase_colors[["blue"]]) +
  geom_text(
    aes(label = .data$n),
    hjust = -0.25,
    size = figure_text$label * points_to_mm
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.14))
  ) +
  coord_flip(clip = "off") +
  labs(
    title = "Number of Participants",
    x = figure_02_dataset_label,
    y = "Number of Participants"
  ) +
  theme_fearbase +
  theme(
    plot.margin = margin(5.5, 14, 5.5, 5.5),
    plot.title.position = "panel"
  )

### Panel 2 C: age distribution ------------------------------------------------
figure_02_age_order <- figure_02_age |>
  group_by(.data[[figure_02_dataset_variable]]) |>
  summarise(
    median_age = median(.data$age),
    .groups = "drop"
  ) |>
  arrange(desc(.data$median_age)) |>
  pull(.data[[figure_02_dataset_variable]]) |>
  as.character()

figure_02_age_plot_data <- figure_02_age |>
  mutate(
    dataset = factor(
      as.character(.data[[figure_02_dataset_variable]]),
      levels = figure_02_age_order
    )
  )

figure_02_age_color_count <- length(figure_02_age_order)

if (figure_02_age_color_count == 1) {
  figure_02_age_palette_values <- fearbase_base_palette[[3]]
} else if (figure_02_age_color_count == 2) {
  figure_02_age_palette_values <- fearbase_base_palette[c(2, 3)]
} else if (figure_02_age_color_count == 3) {
  figure_02_age_palette_values <- fearbase_base_palette[c(2, 3, 4)]
} else {
  figure_02_age_palette_values <- colorRampPalette(
    fearbase_base_palette,
    space = "Lab",
    interpolate = "spline"
  )(figure_02_age_color_count)
}

figure_02_age_palette <- setNames(
  figure_02_age_palette_values,
  figure_02_age_order
)

figure_02_panel_c <- ggplot(
  figure_02_age_plot_data,
  aes(
    x = .data$age,
    y = .data$dataset,
    group = .data$dataset,
    fill = .data$dataset
  )
) +
  geom_density_ridges(
    alpha = 0.9,
    color = "white",
    linewidth = 0.2,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = figure_02_age_palette, guide = "none") +
  scale_x_continuous(breaks = seq(15, 60, 5)) +
  labs(
    title = "Age Distribution",
    x = "Age",
    y = figure_02_dataset_label
  ) +
  theme_fearbase +
  theme(
    legend.position = "none",
    plot.title.position = "panel"
  )

### Panel 2 D: biological sex --------------------------------------------------
figure_02_sex_plot_data <- figure_02_sex |>
  count(.data$sex, name = "n") |>
  mutate(label = paste0(.data$sex, " (", .data$n, ")")) |>
  arrange(.data$n, as.character(.data$sex))

figure_02_sex_categories <- as.character(figure_02_sex_plot_data$sex)
figure_02_sex_plot_data$sex <- factor(
  as.character(figure_02_sex_plot_data$sex),
  levels = rev(figure_02_sex_categories)
)
figure_02_sex_colors <- setNames(
  tail(
    unname(
      fearbase_colors[c("blue_900", "blue_700", "blue_500", "blue_300")]
    ),
    nrow(figure_02_sex_plot_data)
  ),
  figure_02_sex_categories
)

figure_02_panel_d <- ggplot(
  figure_02_sex_plot_data,
  aes(x = "", y = .data$n, fill = .data$sex)
) +
  geom_col(width = 1, color = "white", linewidth = 0.25) +
  geom_label_repel(
    aes(label = .data$label),
    position = position_stack(vjust = 0.5),
    size = figure_text$label * points_to_mm,
    fill = "white",
    linewidth = 0.15,
    force = 2,
    max.overlaps = Inf,
    show.legend = FALSE,
    family = publication_font_family
  ) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = figure_02_sex_colors, guide = "none") +
  labs(
    title = expression("Biological Sex"^1),
    x = NULL,
    y = NULL
  ) +
  theme_fearbase_void +
  theme(
    legend.position = "none",
    plot.title.position = "panel"
  )

figure_02 <- wrap_plots(
  figure_02_panel_a,
  figure_02_panel_b,
  figure_02_panel_c,
  figure_02_panel_d,
  ncol = 2
) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(
      face = "plain",
      size = figure_text$title
    )
  )

## Data preprocessing Figure 3 ------------------------------------------------

### Figure 3 A: SCR Response Quantification -----------------------------------

figure_03_scr_windows <- metadata |>
  select(
    "paper_study_id",
    "study_id",
    "physio_scr_scoring_approach",
    "physio_scr_baseline_window_start",
    "physio_scr_baseline_window_end",
    "physio_scr_peak_detection_window_min",
    "physio_scr_peak_detection_window_max"
  ) |>
  drop_na(
    "physio_scr_peak_detection_window_max",
    "physio_scr_scoring_approach"
  ) |>
  distinct() |>
  arrange(
    .data$physio_scr_scoring_approach,
    desc(.data$physio_scr_peak_detection_window_min),
    desc(.data$physio_scr_peak_detection_window_max)
  ) |>
  mutate(
    across(
      any_of(c("paper_study_id", "study_id")),
      as.factor
    )
  ) |>
  pivot_longer(
    cols = starts_with("physio_scr_") &
      !matches("scoring_approach"),
    names_to = c("measure", "window", "timepoint"),
    names_pattern = "^physio_(scr)_(.*)_window_(.*)$"
  ) |>
  drop_na("measure") |>
  mutate(
    timepoint = recode(
      .data$timepoint,
      min = "start",
      max = "end"
    )
  ) |>
  pivot_wider(
    names_from = "timepoint",
    values_from = "value"
  ) |>
  drop_na("start", "end") |>
  mutate(
    start = as.numeric(.data$start),
    end = as.numeric(.data$end),
    scoring_approach = recode(
      as.character(.data$physio_scr_scoring_approach),
      baseline_correction = "BLC",
      `trough-to-peak` = "TTP",
      .default = as.character(.data$physio_scr_scoring_approach)
    ),
    window = case_when(
      .data$scoring_approach != "BLC" ~ "Trough Detection",
      .data$window == "peak_detection" ~ "Peak Detection",
      .data$window == "baseline" ~ "Baseline",
      TRUE ~ str_to_title(.data$window)
    ),
    window = factor(
      .data$window,
      levels = c("Baseline", "Peak Detection", "Trough Detection")
    )
  )


### Figure 3 B: Reinforcement Rates -----------------------------------------
figure_03_rate_columns <- grep(
  "^reinforcement_rate_",
  names(metadata),
  value = TRUE
)

figure_03_reinforcement_rates <- metadata |>
  select("study_id", all_of(figure_03_rate_columns)) |>
  pivot_longer(
    cols = all_of(figure_03_rate_columns),
    names_to = "rate_variable",
    values_to = "reinforcement_rate"
  ) |>
  drop_na("reinforcement_rate") |>
  mutate(
    reinforcement_rate = floor(as.numeric(.data$reinforcement_rate))
  ) |>
  distinct(.data$study_id, .data$reinforcement_rate) |>
  count(.data$reinforcement_rate, name = "n") |>
  arrange(.data$reinforcement_rate)


### Figure 3 C: Contingency Instructions --------------------------------------

figure_03_instructions <- metadata |>
  select("study_id", "instruction_contingency") |>
  distinct() |>
  mutate(
    instruction_contingency = if_else(
      is.na(.data$instruction_contingency) |
        str_trim(.data$instruction_contingency) == "",
      "Not reported",
      as.character(.data$instruction_contingency)
    )
  ) |>
  count(.data$instruction_contingency, name = "n") |>
  arrange(.data$n, .data$instruction_contingency) |>
  mutate(
    instruction_contingency = factor(
      .data$instruction_contingency,
      levels = .data$instruction_contingency
    )
  )


### Figure 3 D: CS Modality ---------------------------------------------------

figure_03_cs_modality <- metadata |>
  transmute(
    condition_id = .data$condition_id,
    modality = str_replace(
      string = as.character(.data$cs_type),
      pattern = ",",
      replacement = ", "
    )
  ) |>
  mutate(
    modality = if_else(
      is.na(.data$modality) | str_trim(.data$modality) == "",
      "Not reported",
      .data$modality
    )
  ) |>
  distinct(.data$condition_id, .data$modality) |>
  count(.data$modality, name = "n_studies") |>
  arrange(desc(.data$n_studies)) |>
  mutate(modality = factor(.data$modality, levels = .data$modality))

### Figure 3 E: US Modality ---------------------------------------------------

figure_03_us_modality <- metadata |>
  transmute(
    condition_id = .data$condition_id,
    modality = str_replace(
      string = as.character(.data$us_type),
      pattern = ",",
      replacement = ", "
    )
  ) |>
  mutate(
    modality = if_else(
      is.na(.data$modality) | str_trim(.data$modality) == "",
      "Not reported",
      .data$modality
    )
  ) |>
  distinct(.data$condition_id, .data$modality) |>
  count(.data$modality, name = "n_studies") |>
  arrange(desc(.data$n_studies)) |>
  mutate(modality = factor(.data$modality, levels = .data$modality))


### Plot Generation Figure 3 --------------------------------------------------

### Panel 3 A: SCR Response Quantification Windows ----------------------------
figure_03_study_order <- figure_03_scr_windows |>
  group_by(.data$study_id) |>
  summarise(
    approach_order = first(.data$scoring_approach),
    .groups = "drop"
  ) |>
  arrange(.data$approach_order, .data$study_id) |>
  pull(.data$study_id) |>
  as.character()

figure_03_scr_plot_data <- figure_03_scr_windows |>
  mutate(
    group_id = factor(
      as.character(.data$study_id),
      levels = figure_03_study_order
    )
  )

figure_03_approach_labels <- figure_03_scr_plot_data |>
  distinct(.data$group_id, .data$scoring_approach)
figure_03_onset_label_row <- min(16, max(1, length(figure_03_study_order)))
figure_03_window_colors <- setNames(
  fearbase_base_palette[c(2, 3, 4)],
  c("Baseline", "Peak Detection", "Trough Detection")
)

figure_03_panel_a <- ggplot(
  figure_03_scr_plot_data,
  aes(x = .data$group_id)
) +
  geom_segment(
    aes(
      y = .data$start,
      yend = .data$end,
      xend = .data$group_id,
      color = .data$window,
      group = .data$window
    ),
    linewidth = 2.5
  ) +
  geom_text(
    data = figure_03_approach_labels,
    aes(
      x = .data$group_id,
      y = -2.8,
      label = .data$scoring_approach
    ),
    inherit.aes = FALSE,
    hjust = 0,
    size = 5 * points_to_mm
  ) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  annotate(
    "text",
    x = figure_03_onset_label_row,
    y = 0,
    label = "CS Onset",
    angle = 90,
    vjust = -0.15,
    size = figure_text$label * points_to_mm
  ) +
  scale_color_manual(values = figure_03_window_colors, drop = FALSE) +
  scale_y_continuous(breaks = seq(-2, 8, 2)) +
  coord_flip(ylim = c(-3, 8), clip = "off") +
  labs(
    title = "SCR Response Quantification Approach",
    x = "Study ID",
    y = "Time from Stimulus Onset [s]",
    color = "Window"
  ) +
  theme_fearbase +
  theme(
    axis.text.y = element_text(size = 5),
    legend.position = "top",
    plot.title.position = "panel"
  )


### Panel 3 B: Reinforcement Rates --------------------------------------------
figure_03_panel_b <- ggplot(
  figure_03_reinforcement_rates,
  aes(x = .data$reinforcement_rate, y = .data$n)
) +
  geom_col(fill = fearbase_colors[["blue"]], color = "white") +
  geom_text(
    aes(label = .data$reinforcement_rate),
    vjust = -0.35,
    size = figure_text$label * points_to_mm
  ) +
  scale_x_continuous(breaks = seq(30, 100, 10)) +
  scale_y_continuous(breaks = 0:10) +
  coord_cartesian(ylim = c(0, 8)) +
  labs(
    title = "Reinforcement Rate",
    x = "Reinforcement Rate [%]",
    y = "Number of Studies"
  ) +
  theme_fearbase +
  theme(plot.title.position = "panel")


### Panel 3 C: Contingency Instructions ---------------------------------------
figure_03_panel_c <- ggplot(
  figure_03_instructions,
  aes(x = .data$instruction_contingency, y = .data$n)
) +
  geom_col(fill = fearbase_colors[["blue"]]) +
  scale_y_continuous(breaks = 0:11) +
  coord_flip(ylim = c(0, 11)) +
  labs(
    title = "Contingency Instruction",
    x = NULL,
    y = "Number of Studies"
  ) +
  theme_fearbase +
  theme(
    plot.title = element_text(margin = margin(l = 22))
  )

### Panels 3 D & E: CS und US modality ----------------------------------------
figure_03_cs_plot_data <- figure_03_cs_modality |>
  arrange(.data$n_studies, as.character(.data$modality))
figure_03_cs_categories <- as.character(figure_03_cs_plot_data$modality)
figure_03_cs_plot_data$modality <- factor(
  as.character(figure_03_cs_plot_data$modality),
  levels = rev(figure_03_cs_categories)
)
figure_03_cs_color_count <- nrow(figure_03_cs_plot_data)

if (figure_03_cs_color_count == 1) {
  figure_03_cs_palette_values <- fearbase_base_palette[[3]]
} else if (figure_03_cs_color_count == 2) {
  figure_03_cs_palette_values <- fearbase_base_palette[c(2, 3)]
} else if (figure_03_cs_color_count == 3) {
  figure_03_cs_palette_values <- fearbase_base_palette[c(2, 3, 4)]
} else {
  figure_03_cs_palette_values <- colorRampPalette(
    fearbase_base_palette,
    space = "Lab",
    interpolate = "spline"
  )(figure_03_cs_color_count)
}

figure_03_cs_colors <- setNames(
  figure_03_cs_palette_values,
  figure_03_cs_categories
)

figure_03_panel_d <- ggplot(
  figure_03_cs_plot_data,
  aes(x = "", y = .data$n_studies, fill = .data$modality)
) +
  geom_col(width = 1, color = "white", linewidth = 0.2) +
  geom_label(
    aes(label = .data$n_studies),
    position = position_stack(vjust = 0.5),
    size = figure_text$label * points_to_mm,
    fill = "white",
    linewidth = 0.1,
    show.legend = FALSE
  ) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = figure_03_cs_colors) +
  labs(title = "CS Modality", fill = NULL, x = NULL, y = NULL) +
  theme_fearbase_void +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.3, "cm"),
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(margin = margin(b = 5)),
    plot.margin = margin(b = 10, unit = "pt"),
    plot.tag.position = c(-0.6, 0.95)
  )

figure_03_us_plot_data <- figure_03_us_modality |>
  arrange(.data$n_studies, as.character(.data$modality))
figure_03_us_categories <- as.character(figure_03_us_plot_data$modality)
figure_03_us_plot_data$modality <- factor(
  as.character(figure_03_us_plot_data$modality),
  levels = rev(figure_03_us_categories)
)
figure_03_us_color_count <- nrow(figure_03_us_plot_data)

if (figure_03_us_color_count == 1) {
  figure_03_us_palette_values <- fearbase_base_palette[[3]]
} else if (figure_03_us_color_count == 2) {
  figure_03_us_palette_values <- fearbase_base_palette[c(2, 3)]
} else if (figure_03_us_color_count == 3) {
  figure_03_us_palette_values <- fearbase_base_palette[c(2, 3, 4)]
} else {
  figure_03_us_palette_values <- colorRampPalette(
    fearbase_base_palette,
    space = "Lab",
    interpolate = "spline"
  )(figure_03_us_color_count)
}

figure_03_us_colors <- setNames(
  figure_03_us_palette_values,
  figure_03_us_categories
)

figure_03_panel_e <- ggplot(
  figure_03_us_plot_data,
  aes(x = "", y = .data$n_studies, fill = .data$modality)
) +
  geom_col(width = 1, color = "white", linewidth = 0.2) +
  geom_label(
    aes(label = .data$n_studies),
    position = position_stack(vjust = 0.5),
    size = figure_text$label * points_to_mm,
    fill = "white",
    linewidth = 0.1,
    show.legend = FALSE
  ) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = figure_03_us_colors) +
  labs(title = "US Modality", fill = NULL, x = NULL, y = NULL) +
  theme_fearbase_void +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(
    legend.position = "top",
    legend.key.size = unit(0.3, "cm"),
    legend.box.spacing = unit(0, "pt"),
    plot.title = element_text(margin = margin(b = 5)),
    plot.tag.position = c(-0.6, 1)
  )

figure_03_top_row <- free(figure_03_panel_a) |
  (figure_03_panel_b +
    theme(plot.tag.position = c(0.05, 0.99)))
figure_03_lower_right <- figure_03_panel_d / figure_03_panel_e

figure_03 <- (figure_03_top_row /
  (figure_03_panel_c | figure_03_lower_right)) +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(
      face = "plain",
      size = figure_text$title
    )
  )

### Figure 4: Measure And Phase Co-occurrence Heatmaps ------------------------

figure_04_measure_display_levels <- c(
  "SCR",
  "Pupil Dilation",
  "FPS",
  "Expectancy",
  "Intensity (US)",
  "Fear (CS)",
  "Arousal (CS)",
  "Arousal (US)",
  "Valence (CS)",
  "Valence (US)",
  "Awareness",
  "STAI-T",
  "STAI-S",
  "IUS",
  "ASI"
)
figure_04_questionnaire_measures <- c(
  "stais",
  "stait",
  "neo-ffi",
  "asi",
  "promis",
  "ius",
  "pswq"
)
figure_04_physiological_measures <- c("scr", "scl", "ps", "fps", "hr")
figure_04_rating_measures <- c(
  "anx",
  "arous",
  "aware",
  "expect",
  "fear",
  "val",
  "stress",
  "intrat"
)
figure_04_retained_measures <- c(
  figure_04_questionnaire_measures,
  figure_04_physiological_measures,
  figure_04_rating_measures
)

figure_04_measure_data <- data_long |>
  filter(.data$measure %in% figure_04_retained_measures) |>
  mutate(
    type = case_when(
      .data$measure %in% figure_04_questionnaire_measures ~ "questionnaire",
      .data$measure %in% figure_04_physiological_measures ~ "physiological",
      .data$measure %in% figure_04_rating_measures ~ "rating"
    ),
    stimulus_category = case_when(
      str_detect(.data$stimulus, "^cs") ~ "cs",
      str_detect(.data$stimulus, "^us") ~ "us",
      TRUE ~ NA_character_
    ),
    combined_measure = if_else(
      .data$measure %in%
        c("arous", "val", "fear") &
        !is.na(.data$stimulus_category),
      paste(.data$measure, .data$stimulus_category, sep = "."),
      .data$measure
    ),
    measure = recode(
      .data$combined_measure,
      scr = "SCR",
      ps = "Pupil Dilation",
      fps = "FPS",
      expect = "Expectancy",
      intrat = "Intensity (US)",
      `fear.cs` = "Fear (CS)",
      `arous.cs` = "Arousal (CS)",
      `arous.us` = "Arousal (US)",
      `val.cs` = "Valence (CS)",
      `val.us` = "Valence (US)",
      aware = "Awareness",
      stait = "STAI-T",
      stais = "STAI-S",
      ius = "IUS",
      asi = "ASI",
      .default = NA_character_
    )
  ) |>
  filter(!is.na(.data$measure)) |>
  transmute(
    condition_id = .data$condition_id,
    participant_id = .data$participant_id,
    measure = factor(.data$measure, levels = figure_04_measure_display_levels),
    type = factor(
      .data$type,
      levels = c("physiological", "rating", "questionnaire")
    )
  ) |>
  distinct()

figure_04_measure_totals <- figure_04_measure_data |>
  count(.data$measure, .data$type, name = "used") |>
  mutate(
    measure = factor(
      .data$measure,
      levels = rev(figure_04_measure_display_levels)
    )
  )

figure_04_measure_by_condition <- figure_04_measure_data |>
  count(.data$condition_id, .data$measure, name = "n")

figure_04_measure_wide <- figure_04_measure_by_condition |>
  select("measure", "condition_id", "n") |>
  pivot_wider(
    names_from = "condition_id",
    values_from = "n",
    values_fill = 0,
    values_fn = sum
  )
figure_04_measure_categories <- as.character(figure_04_measure_wide$measure)
figure_04_measure_count_matrix <- figure_04_measure_wide |>
  select(-all_of("measure")) |>
  as.matrix()
figure_04_measure_co_occurrence_matrix <-
  figure_04_measure_count_matrix %*%
  t(figure_04_measure_count_matrix > 0)
dimnames(figure_04_measure_co_occurrence_matrix) <- list(
  figure_04_measure_categories,
  figure_04_measure_categories
)

figure_04_measure_heatmap_data <- figure_04_measure_co_occurrence_matrix |>
  as.table() |>
  as.data.frame(responseName = "value") |>
  as_tibble()

names(figure_04_measure_heatmap_data) <- c("measure", "measure2", "value")

figure_04_measure_heatmap_data <- figure_04_measure_heatmap_data |>
  mutate(
    measure = factor(.data$measure, levels = figure_04_measure_display_levels),
    measure2 = factor(
      .data$measure2,
      levels = rev(figure_04_measure_display_levels)
    ),
    display_value = if_else(
      as.character(.data$measure) == as.character(.data$measure2),
      NA_real_,
      as.numeric(.data$value)
    ),
    is_zero = !is.na(.data$display_value) & .data$display_value == 0,
    label = if_else(
      !is.na(.data$display_value) & .data$display_value > 0,
      as.character(.data$display_value),
      ""
    )
  )

figure_04_phase_by_condition <- data_long |>
  select("condition_id", "participant_id", "phase") |>
  distinct() |>
  filter(
    !is.na(.data$phase),
    !.data$phase %in% c("", "int", "other")
  ) |>
  count(.data$condition_id, .data$phase, name = "used")

figure_04_phase_priority <- c(
  "hab",
  "acq",
  "ext",
  "int",
  "rin",
  "rex",
  "rev",
  "other"
)

figure_04_observed_phases <- unique(as.character(
  figure_04_phase_by_condition$phase
))
figure_04_observed_phases <- figure_04_observed_phases[
  !is.na(figure_04_observed_phases)
]

figure_04_ordered_phases <- figure_04_phase_priority[
  figure_04_phase_priority %in% figure_04_observed_phases
]

figure_04_unrecognized_phases <- setdiff(
  figure_04_observed_phases,
  figure_04_phase_priority
)

figure_04_phase_levels <- c(
  figure_04_ordered_phases,
  figure_04_unrecognized_phases
)

figure_04_phase_totals <- figure_04_phase_by_condition |>
  count(.data$phase, wt = .data$used, name = "used") |>
  mutate(
    phase = factor(.data$phase, levels = figure_04_phase_levels),
    phase = fct_recode(
      .data$phase,
      Habituation = "hab",
      Acquisition = "acq",
      Extinction = "ext",
      `Reinstatement Test` = "rin",
      `Re-Extinction` = "rex",
      Reversal = "rev"
    ),
    phase = fct_rev(.data$phase)
  )

figure_04_phase_wide <- figure_04_phase_by_condition |>
  select("phase", "condition_id", "used") |>
  pivot_wider(
    names_from = "condition_id",
    values_from = "used",
    values_fill = 0,
    values_fn = sum
  )
figure_04_phase_categories <- as.character(figure_04_phase_wide$phase)
figure_04_phase_count_matrix <- figure_04_phase_wide |>
  select(-all_of("phase")) |>
  as.matrix()
figure_04_phase_co_occurrence_matrix <-
  figure_04_phase_count_matrix %*%
  t(figure_04_phase_count_matrix > 0)
dimnames(figure_04_phase_co_occurrence_matrix) <- list(
  figure_04_phase_categories,
  figure_04_phase_categories
)

figure_04_phase_heatmap_data <- figure_04_phase_co_occurrence_matrix |>
  as.table() |>
  as.data.frame(responseName = "value") |>
  as_tibble()
names(figure_04_phase_heatmap_data) <- c("phase", "phase2", "value")
figure_04_phase_heatmap_data <- figure_04_phase_heatmap_data |>
  mutate(
    phase = factor(.data$phase, levels = figure_04_phase_levels),
    phase2 = factor(.data$phase2, levels = figure_04_phase_levels),
    phase = fct_recode(
      .data$phase,
      Habituation = "hab",
      Acquisition = "acq",
      Extinction = "ext",
      `Reinstatement Test` = "rin",
      `Re-Extinction` = "rex",
      Reversal = "rev"
    ),
    phase2 = fct_recode(
      .data$phase2,
      Habituation = "hab",
      Acquisition = "acq",
      Extinction = "ext",
      `Reinstatement Test` = "rin",
      `Re-Extinction` = "rex",
      Reversal = "rev"
    ),
    phase2 = fct_rev(.data$phase2),
    display_value = if_else(
      as.character(.data$phase) == as.character(.data$phase2),
      NA_real_,
      as.numeric(.data$value)
    ),
    is_zero = !is.na(.data$display_value) & .data$display_value == 0,
    label = if_else(
      !is.na(.data$display_value) & .data$display_value > 0,
      as.character(.data$display_value),
      ""
    )
  )

figure_04_heatmap_colors <- fearbase_base_palette[c(2, 3, 4)]

#### Panel 4 A: Measure Co-occurrence Heatmap ---------------------------------
figure_04_panel_a <- ggplot(
  figure_04_measure_heatmap_data,
  aes(
    x = .data$measure,
    y = .data$measure2,
    fill = .data$display_value
  )
) +
  geom_tile(color = NA, linewidth = 0) +
  geom_tile(
    data = filter(figure_04_measure_heatmap_data, .data$is_zero),
    fill = "white",
    color = NA,
    linewidth = 0
  ) +
  geom_text(
    aes(label = .data$label),
    color = "white",
    size = 6.5 * points_to_mm,
    family = publication_font_family,
    fontface = "bold"
  ) +
  scale_fill_gradientn(
    colours = figure_04_heatmap_colors,
    na.value = "grey50",
    limits = c(0, 1000),
    breaks = seq(0, 1000, by = 250),
    labels = seq(0, 1000, by = 250),
    oob = squish,
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(6, "cm"),
      barheight = unit(0.3, "cm")
    )
  ) +
  scale_y_discrete(position = "right") +
  labs(
    title = "Combinations of Measures",
    x = NULL,
    y = NULL,
    fill = "Number of Participants"
  ) +
  theme_fearbase +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.background = element_rect(fill = "white", color = NA)
  )

figure_04_measure_type_levels <- unique(
  as.character(figure_04_measure_totals$type)
)

figure_04_measure_type_colors <- setNames(
  pal_grey()(length(figure_04_measure_type_levels)),
  figure_04_measure_type_levels
)

figure_04_measure_mapping <- aes(x = .data$measure, y = .data$used)

figure_04_measure_totals_plot <- ggplot(
  figure_04_measure_totals,
  figure_04_measure_mapping
) +
  geom_col(aes(fill = .data$type)) +
  scale_fill_manual(
    values = figure_04_measure_type_colors,
    guide = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  geom_text(
    aes(label = .data$used),
    hjust = -0.1,
    size = (figure_text$label + 1) * points_to_mm
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.2))
  ) +
  coord_flip(clip = "off") +
  labs(
    title = "Number of Participants",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_fearbase_void +
  theme(
    plot.title = element_text(
      margin = margin(b = 10, unit = "pt")
    ),
    legend.position = "top",
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(1, "pt"),
    legend.text = element_text(size = 6),
    plot.margin = margin(2, 16, 2, 2)
  )

# Panel 4 B: Phase Co-occurrence Heatmap --------------------------------------
figure_04_panel_b <- ggplot(
  figure_04_phase_heatmap_data,
  aes(
    x = .data$phase,
    y = .data$phase2,
    fill = .data$display_value
  )
) +
  geom_tile(color = NA, linewidth = 0) +
  geom_tile(
    data = filter(figure_04_phase_heatmap_data, .data$is_zero),
    fill = "white",
    color = NA,
    linewidth = 0
  ) +
  geom_text(
    aes(label = .data$label),
    color = "white",
    size = 6.5 * points_to_mm,
    family = publication_font_family,
    fontface = "bold"
  ) +
  scale_fill_gradientn(
    colours = figure_04_heatmap_colors,
    na.value = "grey50",
    limits = c(0, 1250),
    breaks = seq(0, 1250, by = 250),
    labels = seq(0, 1250, by = 250),
    oob = squish,
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(6, "cm"),
      barheight = unit(0.3, "cm")
    )
  ) +
  scale_y_discrete(position = "right") +
  labs(
    title = "Combinations of Phases",
    x = NULL,
    y = NULL,
    fill = "Number of Participants"
  ) +
  theme_fearbase +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.background = element_rect(fill = "white", color = NA)
  )

figure_04_phase_totals_plot <- ggplot(
  figure_04_phase_totals,
  aes(x = .data$phase, y = .data$used)
) +
  geom_col(fill = "grey50") +
  geom_text(
    aes(label = .data$used),
    hjust = -0.1,
    size = (figure_text$label + 1) * points_to_mm
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.2))
  ) +
  coord_flip(clip = "off") +
  labs(
    title = "Number of Participants",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_fearbase_void +
  theme(
    plot.title = element_text(
      margin = margin(b = 10, unit = "pt")
    ),
    legend.position = "top",
    legend.key.size = unit(0.25, "cm"),
    legend.spacing.x = unit(1, "pt"),
    legend.text = element_text(size = 6),
    plot.margin = margin(2, 16, 2, 2)
  )

# Assembly
figure_04 <- wrap_plots(
  figure_04_panel_a,
  figure_04_measure_totals_plot,
  figure_04_panel_b,
  figure_04_phase_totals_plot,
  ncol = 2,
  widths = c(1, 0.6)
) +
  plot_annotation(tag_levels = list(c("a", "", "b", ""))) &
  theme(
    plot.tag = element_text(
      face = "plain",
      size = figure_text$title
    )
  )


## Figure 5: Phase Lengths ----------------------------------------------------

figure_05_phase_lengths <- data_long |>
  mutate(
    stimulus_category = case_when(
      str_detect(.data$stimulus, "^csp") ~ "CS+",
      str_detect(.data$stimulus, "^csm") ~ "CS-",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    !is.na(.data$stimulus_category),
    !is.na(.data$trial),
    .data$measure != "fear",
    !is.na(.data$phase),
    !.data$phase %in% c("", "int", "other")
  ) |>
  group_by(
    .data$study_id,
    .data$phase,
    .data$day,
    .data$measure,
    .data$stimulus_category,
    .data$stimulus
  ) |>
  summarise(
    max_trial_stimulus = max(.data$trial),
    .groups = "drop"
  ) |>
  group_by(
    .data$study_id,
    .data$phase,
    .data$day,
    .data$measure,
    .data$stimulus_category
  ) |>
  summarise(
    phase_length_measure = sum(.data$max_trial_stimulus),
    .groups = "drop"
  ) |>
  group_by(
    .data$study_id,
    .data$phase,
    .data$day,
    .data$stimulus_category
  ) |>
  summarise(
    phase_length = max(.data$phase_length_measure),
    .groups = "drop"
  )

figure_05_plot_data <- figure_05_phase_lengths |>
  count(
    .data$phase,
    .data$phase_length,
    .data$stimulus_category,
    name = "n"
  ) |>
  mutate(
    phase = factor(.data$phase, levels = figure_04_phase_levels),
    phase = fct_recode(
      .data$phase,
      Hab = "hab",
      Acq = "acq",
      Ext = "ext",
      RI = "rin",
      `Re-Ext` = "rex",
      Rev = "rev"
    )
  ) |>
  transmute(
    phase = .data$phase,
    trials = .data$phase_length,
    stimulus = factor(
      .data$stimulus_category,
      levels = c("CS-", "CS+")
    ),
    n = .data$n
  )

figure_05_stimulus_colors <- setNames(
  fearbase_base_palette[c(2, 3)],
  c("CS-", "CS+")
)

figure_05 <- ggplot(
  figure_05_plot_data,
  aes(
    x = .data$trials,
    y = .data$n,
    fill = .data$stimulus,
    group = .data$stimulus
  )
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  facet_grid(
    rows = vars(.data$phase),
    axes = "all",
    axis.labels = "all_x"
  ) +
  scale_x_continuous(
    breaks = 0:27,
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_y_continuous(breaks = seq(0, 6, by = 2)) +
  coord_cartesian(xlim = c(0, 27), ylim = c(0, 6)) +
  scale_fill_manual(
    values = figure_05_stimulus_colors,
    drop = FALSE
  ) +
  labs(
    x = "Number of Trials",
    y = "Number of Studies",
    fill = "Stimulus Category"
  ) +
  theme_fearbase +
  theme(
    legend.position = "top",
    plot.title.position = "panel"
  ) +
  guides(fill = guide_legend(reverse = TRUE))


# Export figures ---------------------------------------------------------------

output_dir <- "figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

publication_figures <- list(
  figure_02 = figure_02,
  figure_03 = figure_03,
  figure_04 = figure_04,
  figure_05 = figure_05
)

publication_figure_dimensions <- tribble(
  ~figure_name , ~width_cm , ~height_cm ,
  "figure_02"  ,        15 ,         15 ,
  "figure_03"  ,        16 ,         15 ,
  "figure_04"  ,        15 ,         21 ,
  "figure_05"  ,        10 ,         10
)

output_paths <- vector("list", length(publication_figures))
names(output_paths) <- names(publication_figures)

for (figure_index in seq_along(publication_figures)) {
  figure_name <- names(publication_figures)[[figure_index]]
  figure_plot <- publication_figures[[figure_index]]
  figure_width_cm <- publication_figure_dimensions$width_cm[
    publication_figure_dimensions$figure_name == figure_name
  ]
  figure_height_cm <- publication_figure_dimensions$height_cm[
    publication_figure_dimensions$figure_name == figure_name
  ]
  pdf_path <- file.path(output_dir, paste0(figure_name, ".pdf"))
  tiff_path <- file.path(output_dir, paste0(figure_name, ".tiff"))

  # PDF export
  cairo_pdf(
    filename = pdf_path,
    width = figure_width_cm / 2.54,
    height = figure_height_cm / 2.54,
    family = publication_font_family,
    onefile = TRUE
  )
  print(figure_plot)
  dev.off()

  # compressed TIFF export
  agg_tiff(
    filename = tiff_path,
    width = figure_width_cm,
    height = figure_height_cm,
    units = "cm",
    res = 600,
    scaling = 1,
    compression = "lzw",
    background = "white"
  )
  print(figure_plot)
  dev.off()

  output_paths[[figure_name]] <- c(pdf = pdf_path, tiff = tiff_path)
}

cat(
  "Generated the following publication figures:\n",
  paste(unlist(output_paths, use.names = FALSE), collapse = "\n"),
  "\n"
)
