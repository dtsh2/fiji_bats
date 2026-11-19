# Publication figures. Run independently or via Fiji_Bat_Human_analysis.R.
required <- c("ggplot2", "dplyr", "tidyr", "patchwork", "ragg", "systemfonts")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install packages: ", paste(missing, collapse = ", "))
library(ggplot2)

input_file <- "Fiji_Bat_Human_data.csv"
output_dir <- "RSOS_figures"
font_family <- "Times New Roman"
# Design dimensions, not asserted journal column widths. Change BEFORE exporting
# to the intended publication size so 9-point text is not reduced afterwards.
width_cm <- 18
height_two_cm <- 9
height_four_cm <- 15
export_dpi <- 600
# Original response colours retained. Check readability in monochrome print.
response_colours <- c("No" = "skyblue", "Will eat" = "grey",
                      "Yes" = "orange", "Missing" = "grey50")

if (!font_family %in% systemfonts::system_fonts()$family) {
  stop("Font unavailable: ", font_family, ". Install it or edit font_family before exporting.")
}
d <- read.csv(input_file, stringsAsFactors = FALSE, fileEncoding = "latin1",
              na.strings = c("", "NA"))
needed <- c("Age", "Gender", "Eat_bat", "Past_eat_bat", "Contact", "Education_yrs")
if (!all(needed %in% names(d))) stop("Missing columns: ",
                                  paste(setdiff(needed, names(d)), collapse = ", "))
for (v in needed) d[[v]] <- trimws(as.character(d[[v]]))
d$Age[d$Age == "25-30" & !is.na(d$Age)] <- "25-29"
d$Gender[d$Gender == "F" & !is.na(d$Gender)] <- "Female"
d$Gender[d$Gender == "M" & !is.na(d$Gender)] <- "Male"
age_levels <- c("18-24", "25-29", "30-39", "40-49", "50-59", "60+")
education_levels <- c(as.character(3:12), "12+", "13", "13+")
check_values <- function(x, allowed, label) {
  unexpected <- setdiff(unique(x[!is.na(x)]), allowed)
  if (length(unexpected)) stop("Unexpected ", label, ": ",
                              paste(unexpected, collapse = ", "))
}
check_values(d$Age, age_levels, "age categories")
check_values(d$Gender, c("Female", "Male"), "gender categories")
check_values(d$Education_yrs, education_levels, "education categories")
check_values(d$Contact, c("No", "Yes"), "contact responses")
check_values(d$Eat_bat, c("No", "Will eat", "Yes"), "current eating responses")
check_values(d$Past_eat_bat, c("No", "Will eat", "Yes"), "past eating responses")
d$Age <- factor(d$Age, levels = age_levels)
# Count survey rows; the original identifiers include repeated and missing values.
message("Rows excluded from age plots: ", sum(is.na(d$Age)))
message("Rows excluded from gender panels (among known ages): ",
        sum(!is.na(d$Age) & is.na(d$Gender)))

journal_theme <- function() {
  theme_bw(base_size = 9, base_family = font_family) +
    theme(text = element_text(size = 9, lineheight = 11/9, colour = "black"),
          axis.text = element_text(size = 9, colour = "black"),
          axis.title = element_text(size = 9),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9),
          plot.title = element_text(size = 9, face = "plain", hjust = 0),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.2),
          legend.key.size = grid::unit(3.5, "mm"),
          plot.margin = margin(5, 5, 5, 5))
}
# plotmath italicizes only the letter; parentheses stay upright. Titles sit on
# white outside the panel, avoiding coloured backgrounds and circle labels.
panel_title <- function(letter, title) {
  bquote("(" * italic(.(letter)) * ")" ~ .(title))
}
integer_labels <- function(x) format(x, trim = TRUE, scientific = FALSE,
                                     big.mark = "\u2009")
maximum_count <- function(x, groups) {
  counts <- dplyr::count(x, dplyr::across(dplyr::all_of(groups)))
  max(c(1, counts$n))
}
make_panel <- function(x, variable, colours, title, ymax) {
  x$Category <- as.character(x[[variable]])
  x$Category[is.na(x$Category)] <- "Missing"
  x$Category <- factor(x$Category, levels = names(colours))
  ggplot(x, aes(x = Age, fill = Category)) +
    geom_bar(width = 0.78, colour = "black", linewidth = 0.15,
             show.legend = TRUE) +
    scale_x_discrete(drop = FALSE,
                     labels = function(x) gsub("-", "\u2013", x, fixed = TRUE)) +
    scale_y_continuous(limits = c(0, ymax * 1.06),
                      breaks = function(lim) {
                        z <- pretty(lim, n = 4)
                        z[z >= 0 & z == floor(z)]
                      }, labels = integer_labels,
                      expand = expansion(mult = c(0, 0))) +
    labs(x = "Age (years)", y = "Count", title = title) + journal_theme()
}
assemble <- function(panels, columns, colours, legend_title = NULL) {
  # Determine categories across the WHOLE figure, not individual panels.
  # Keep their original factor order and colour mapping.
  all_levels <- levels(panels[[1]]$data$Category)
  observed <- unique(unlist(lapply(panels, function(p) {
    as.character(p$data$Category)
  })))
  figure_levels <- all_levels[all_levels %in% observed]
  if (!length(figure_levels)) stop("No categories available for this figure.")

  # Give every panel identical limits and breaks. show.legend = TRUE above
  # ensures a coloured key even for categories absent from the first panel.
  for (i in seq_along(panels)) {
    panels[[i]] <- panels[[i]] + scale_fill_manual(
      name = legend_title, values = colours, limits = figure_levels,
      breaks = figure_levels, drop = FALSE)
    if (i == 1L) {
      panels[[i]] <- panels[[i]] + guides(fill = guide_legend(
        ncol = min(4L, length(figure_levels)), byrow = TRUE,
        title.position = "top", title.hjust = 0.5))
    } else {
      # Only one panel supplies the shared legend: no duplicate guides.
      panels[[i]] <- panels[[i]] + guides(fill = "none")
    }
  }
  patchwork::wrap_plots(panels, ncol = columns, guides = "collect") &
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.justification = "center",
          legend.margin = margin(4, 6, 4, 6),
          legend.box.margin = margin(2, 6, 6, 6),
          legend.key.width = grid::unit(4, "mm"),
          legend.key.height = grid::unit(3.5, "mm"),
          plot.margin = margin(6, 8, 6, 8))
}
save_figure <- function(p, name, height) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  # Explicit plot avoids accidentally saving the last plot displayed.
  ggsave(file.path(output_dir, paste0(name, ".png")), plot = p,
         device = ragg::agg_png, width = width_cm, height = height,
         units = "cm", dpi = export_dpi, bg = "white")
  ggsave(file.path(output_dir, paste0(name, ".tiff")), plot = p,
         device = ragg::agg_tiff, width = width_cm, height = height,
         units = "cm", dpi = export_dpi, bg = "white", compression = "lzw")
}

known_age <- dplyr::filter(d, !is.na(Age))
known_both <- dplyr::filter(known_age, !is.na(Gender))
eating <- tidyr::pivot_longer(known_age, c(Past_eat_bat, Eat_bat),
                              names_to = "Period", values_to = "Response")
eating$Period <- ifelse(eating$Period == "Past_eat_bat", "Past", "Current")

# 1: Contacts by gender.
ymax <- maximum_count(known_both, c("Gender", "Age"))
p <- assemble(lapply(seq_along(c("Female", "Male")), function(i) {
  gender <- c("Female", "Male")[i]
  make_panel(dplyr::filter(known_both, Gender == gender), "Contact",
             response_colours[c("No", "Yes", "Missing")],
             panel_title(letters[i], gender), ymax)
}), 2, response_colours)
save_figure(p, "contacts_gender", height_two_cm)

# 2: Past/current across all participants with known age (including missing gender).
ymax <- maximum_count(eating, c("Period", "Age"))
p <- assemble(lapply(1:2, function(i) {
  period <- c("Past", "Current")[i]
  make_panel(dplyr::filter(eating, Period == period), "Response", response_colours,
             panel_title(letters[i], period), ymax)
}), 2, response_colours)
save_figure(p, "eating_age_current", height_two_cm)

# 3: Current eating only, one response per survey row.
ymax <- maximum_count(known_both, c("Gender", "Age"))
p <- assemble(lapply(1:2, function(i) {
  gender <- c("Female", "Male")[i]
  make_panel(dplyr::filter(known_both, Gender == gender), "Eat_bat", response_colours,
             panel_title(letters[i], paste(gender, "current", sep = ": ")), ymax)
}), 2, response_colours)
save_figure(p, "eating_gender_current", height_two_cm)

# 4: Reading order: female past, female current, male past, male current.
panels <- list()
for (i in 1:4) {
  gender <- c("Female", "Female", "Male", "Male")[i]
  period <- c("Past", "Current", "Past", "Current")[i]
  panels[[i]] <- make_panel(
    dplyr::filter(eating, !is.na(Gender), Gender == gender, Period == period),
    "Response", response_colours,
    panel_title(letters[i], paste(gender, period, sep = ": ")), ymax)
}
save_figure(assemble(panels, 2, response_colours), "eating", height_four_cm)

# 5: Preserve the questionnaire's distinct education categories.
edu_colours <- c(stats::setNames(grDevices::hcl.colors(length(education_levels),
                    palette = "Viridis"), education_levels), "Missing" = "#FFFFFF")
p <- assemble(lapply(1:2, function(i) {
  gender <- c("Female", "Male")[i]
  make_panel(dplyr::filter(known_both, Gender == gender), "Education_yrs",
             edu_colours, panel_title(letters[i], gender), ymax)
}), 2, edu_colours, legend_title = "Education (years)")
save_figure(p, "Education_age", 12)
message("Saved five figures in PNG and TIFF format to: ", normalizePath(output_dir))
