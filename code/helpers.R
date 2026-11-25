# =============================================================================
# helpers.R -- functions and settings for lake_report_v7.Rmd
#
# Sections:
#   1. Packages
#   2. Settings (thresholds, colours, category order)
#   3. Small utilities (guideline direction, unit handling)
#   4. Data loading and preparation
#   5. Trend analysis
#   6. Guideline lookup
#   7. HTML snippets (badges, cards, collapsible table)
#   8. Plots
#   9. Summary statistics table
#  10. Tab rendering and trend dashboard
#
# Conventions: snake_case for functions and objects, UPPER_CASE for constants.
# Data frames are assumed to be in their FINAL units (see scale_units()), so
# nothing downstream rescales.
# =============================================================================


# ---- 1. Packages -------------------------------------------------------------
library(dplyr)      # also supplies tibble()
library(ggplot2)
library(gt)
library(lubridate)
library(httr)


# ---- 2. Settings -------------------------------------------------------------

## Data source
ENMODS_URL <- "https://enmodswr.nrs.gov.bc.ca/api/v1/search/downloadReport"
WQG_URL    <- paste0(
  "https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/",
  "water-quality/water-quality-guidelines/approved-water-quality-guidelines"
)

## Season and depth definitions
SPRING_MONTHS        <- 2:6   # Feb-Jun
SUMMER_MONTHS        <- 7:9   # Jul-Sep
EPI_MAX_UPPER_DEPTH  <- 2     # m; samples starting at or above this depth = epilimnion
HYPO_MIN_LOWER_DEPTH <- 5     # m; samples ending below this depth = hypolimnion

## Data-quality and analysis thresholds
MIN_OBS_PER_PARAM     <- 3     # parameters with fewer observations are dropped
MIN_OBS_FOR_TREND     <- 3     # minimum samples in a Level x Season group to test a trend
MAX_NONDETECT_FRAC    <- 0.5   # skip the trend test if more than this fraction is non-detect
TREND_ALPHA           <- 0.05  # significance level for a trend
DAYS_PER_YEAR         <- 365.25
SMALL_MEAN_THRESHOLD  <- 0.01  # mean below this -> rescale to a smaller unit (see scale_units)
GUIDELINE_PLOT_FACTOR <- 2     # draw the guideline line only if within this factor of the mean result

## Report layout
# Tab order in the report (also the row order of the trend dashboard)
CATEGORIES <- c("Field", "General", "Major Ions", "Nutrients", "Metals - Total")

# Parameters whose plot shows only epilimnion samples (the stats table and
# guideline card still use all samples)
EPI_ONLY_PARAMETERS <- "Chlorophyll A"

# Custom tab order within a category; anything not listed follows alphabetically.
# (Could move to a SortOrder column in the parameter CSV.)
PARAMETER_ORDER <- list(
  Nutrients = c(
    "Total Nitrogen NO2 + NO3",
    "Nitrate (NO3) Dissolved",
    "Nitrate(NO3) + Nitrite(NO2) Dissolved",
    "Nitrogen - Nitrite Dissolved (NO2)",
    "Nitrogen (Kjeldahl) Total Dissolved",
    "Nitrogen Ammonia Dissolved",
    "Nitrogen Ammonia Total",
    "Nitrogen Kjel.Tot(N)",
    "Nitrogen NO2 Total",
    "Nitrogen Organic-Total",
    "Nitrogen Total",
    "Nitrogen Total Dissolved",
    "Phosphorus Ort.Dis-P",
    "Phosphorus Total",
    "Phosphorus Total Dissolved",
    "Carbon Dissolved Inorganic",
    "Carbon Dissolved Organic",
    "Carbon Total",
    "Carbon Total Dissolved",
    "Carbon Total Inorganic",
    "Carbon Total Organic",
    "Silica Reactive Diss"
  )
)

# The four depth/season groups analysed. Order = column order in the dashboard
# and group order in the stats tables.
LEVEL_SEASON_GROUPS <- tibble(
  Level  = c("Epilimnion", "Epilimnion", "Hypolimnion", "Hypolimnion"),
  Season = c("Spring",     "Summer",     "Spring",      "Summer")
)

## Colours used in R plots/tables. (Badge/card colours for the HTML live in the
## {css} chunk of the Rmd; the up/down colours here must match it.)
DETECT_COLOR      <- "#1a1a1a"
NONDETECT_COLOR   <- "#e08214"
TREND_COLOR       <- "#2166ac"   # trend line
TREND_UP_COLOR    <- "#f39c12"   # increasing  (flatly "warning")
TREND_DOWN_COLOR  <- "#3498db"   # decreasing  (flatly "info")
WQG_LINE_COLOR    <- "#8a1c1c"
EXCEED_TEXT_COLOR <- "#e74c3c"   # flatly "danger"
EXCEED_FILL_COLOR <- "#fdedec"
GROUP_FILL_COLOR  <- "#dce8f5"
LEVEL_COLORS      <- c("Epilimnion" = "#2166ac", "Hypolimnion" = "#762a83")

## Shared plot theme
theme_lake <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.border     = element_rect(color = "grey80", fill = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey93"),
      strip.background = element_rect(fill = "grey92", color = "grey80"),
      strip.text       = element_text(face = "bold", size = 11),
      axis.text.x      = element_text(angle = 45, hjust = 1),
      plot.title       = element_text(face = "bold", size = 13),
      plot.caption     = element_text(color = "grey50", size = 9),
      legend.position  = "bottom",
      legend.title     = element_blank()
    )
}


# ---- 3. Small utilities ------------------------------------------------------

#' Direction of a guideline limit.
#' @param x Value from the LimitType column ("upper" or "lower").
#' @return "upper" or "lower"; anything else (including NA) defaults to "upper".
get_limit_dir <- function(x) {
  x <- tolower(trimws(x[1]))
  if (is.na(x) || !x %in% c("upper", "lower")) "upper" else x
}

#' Does a value breach a guideline? Upper limit: value > limit. Lower: value < limit.
exceeds_limit <- function(value, limit, dir) {
  if (dir == "lower") value < limit else value > limit
}

#' Symbol shown before the guideline value: >= for lower limits, <= for upper.
limit_symbol <- function(dir) if (dir == "lower") "≥" else "≤"

# Unit families: factors are relative to ug/L (concentration) and uS/cm (conductance)
CONC_FACTORS <- c("g/L" = 1e6, "mg/L" = 1e3, "ug/L" = 1, "ng/L" = 1e-3, "µg/L" = 1)
COND_FACTORS <- c("S/cm" = 1e6, "mS/cm" = 1e3, "uS/cm" = 1, "µS/cm" = 1)

#' Convert a number between units of the same family.
#'
#' Families: concentration (g/L, mg/L, ug/L, ng/L) or conductance
#' (S/cm, mS/cm, uS/cm).
#' @return The converted value, or NA (with a warning) if the units are unknown
#'   or belong to different families. NA means "no usable guideline" downstream,
#'   so one odd unit no longer aborts the whole report.
convert_units <- function(value, unit_a, unit_b) {
  if (unit_a %in% names(CONC_FACTORS) && unit_b %in% names(CONC_FACTORS)) {
    factors <- CONC_FACTORS
  } else if (unit_a %in% names(COND_FACTORS) && unit_b %in% names(COND_FACTORS)) {
    factors <- COND_FACTORS
  } else {
    warning("Cannot convert '", unit_a, "' to '", unit_b, "': units are unknown or ",
            "in different families. Returning NA.", call. = FALSE)
    return(NA_real_)
  }

  value * factors[[unit_a]] / factors[[unit_b]]
}

#' Put all results for ONE parameter into a single unit.
#'
#' Some parameters (e.g. Chlorophyll A) arrive with a mix of units such as mg/L
#' and ug/L. Every row is converted to the most common unit for that parameter,
#' and the detection-limit values (which live in RESULT) are converted with it.
#' Mixed units that cannot be converted (different families, or unknown units)
#' are left as they are, with a warning.
#' @param dat Data for ONE parameter, with RESULT and UNIT columns.
#' @param param Parameter name, used only in the warning message.
harmonise_units <- function(dat, param = "") {
  units <- dat$UNIT
  known <- units[!is.na(units)]
  if (length(unique(known)) <= 1) return(dat)   # already a single unit

  factors <- if (all(known %in% names(CONC_FACTORS))) {
    CONC_FACTORS
  } else if (all(known %in% names(COND_FACTORS))) {
    COND_FACTORS
  } else {
    warning("Mixed units for '", param, "' (", paste(unique(known), collapse = ", "),
            ") cannot be converted; left unchanged.", call. = FALSE)
    return(dat)
  }

  target <- names(sort(table(known), decreasing = TRUE))[1]   # most common unit

  row_factor <- unname(factors[units])
  row_factor[is.na(row_factor)] <- factors[[target]]          # NA unit: leave value as is

  dat$RESULT <- dat$RESULT * row_factor / factors[[target]]
  dat$UNIT   <- target
  dat
}

#' Rescale a parameter's results to a more readable unit.
#'
#' If every result is in `from` and the mean is tiny, multiply by 1000 and relabel
#' as `to`. Applied twice: mg/L -> ug/L, then ug/L -> ng/L. Data in any other unit
#' (pH, turbidity, conductivity ...) is left alone.
#' @param dat Data for ONE parameter, with RESULT and UNIT columns.
scale_units <- function(dat) {
  rescale_if_small <- function(dat, from, to) {
    units_present <- unique(dat$UNIT[!is.na(dat$UNIT)])
    is_small      <- isTRUE(mean(dat$RESULT, na.rm = TRUE) < SMALL_MEAN_THRESHOLD)
    if (identical(units_present, from) && is_small) {
      dat$RESULT <- dat$RESULT * 1000
      dat$UNIT   <- to
    }
    dat
  }

  dat %>%
    rescale_if_small("mg/L", "ug/L") %>%
    rescale_if_small("ug/L", "ng/L")
}

#' Order a category's parameters for display: custom order first (if one is
#' defined in PARAMETER_ORDER), then the rest alphabetically.
order_parameters <- function(params, category) {
  params <- unique(params)
  custom <- PARAMETER_ORDER[[category]]
  if (is.null(custom)) return(sort(params))
  c(custom[custom %in% params], sort(setdiff(params, custom)))
}


# ---- 4. Data loading and preparation -----------------------------------------

#' Download one lake's BCLMN data from EnMoDS and add derived columns.
#'
#' Adds: COLLECTION_START (Date), RESULT_LETTER ("<" for non-detects),
#' IS_NONDETECT, Season (Spring/Summer/NA) and Level (Epilimnion/Hypolimnion/NA).
#' Vertical profile records are dropped.
load_lake_data <- function(ems_id, from_date, to_date = Sys.Date()) {
  url <- paste0(
    ENMODS_URL,
    "?locationName=", ems_id,
    "&projects=BCLMN",
    "&fromDate=", from_date,
    "&toDate=", to_date,
    "&?media=Water%20-%20Fresh"
  )

  response <- httr::GET(url)
  httr::stop_for_status(response)   # fail clearly if the API is down

  raw <- read.csv(
    textConnection(httr::content(response, as = "text", encoding = "UTF-8")),
    stringsAsFactors = FALSE
  )

  raw %>%
    rename(
      EMS_ID              = location_id,
      MONITORING_LOCATION = location_name,
      COLLECTION_START    = observed_date_time,
      PARAMETER           = observed_property_id,
      RESULT              = result_value,
      UNIT                = result_unit,
      UPPER_DEPTH         = depth_upper,
      LOWER_DEPTH         = depth_lower,
      RESULT_LETTER       = detection_condition,
      FRACTION            = fraction
    ) %>%
    mutate(
      COLLECTION_START = as.Date(ymd_hms(COLLECTION_START, tz = "UTC")),

      RESULT_LETTER = if_else(RESULT_LETTER == "NOT_DETECTED", "<", "", missing = ""),
      IS_NONDETECT  = RESULT_LETTER == "<",

      Season = case_when(
        month(COLLECTION_START) %in% SPRING_MONTHS ~ "Spring",
        month(COLLECTION_START) %in% SUMMER_MONTHS ~ "Summer",
        TRUE ~ NA_character_
      ),

      Level = case_when(
        UPPER_DEPTH <= EPI_MAX_UPPER_DEPTH  ~ "Epilimnion",
        LOWER_DEPTH >  HYPO_MIN_LOWER_DEPTH ~ "Hypolimnion",
        TRUE ~ NA_character_
      ),
      # Field and Physical Properties measurements often have no recorded depth;
      # treat them as surface (epilimnion) readings.
      Level = if_else(
        is.na(Level) &
          grepl("^Field|^Physical Properties", trimws(observed_property_description)),
        "Epilimnion",
        Level
      )
    ) %>%
    filter(data_classification != "VERTICAL_PROFILE")
}

#' Read the parameter CSV: which observed properties to report, their category,
#' and their BC water quality guideline (if any).
#'
#' Required columns: PARAMETER, Include, Category, BCWQG_Limit, BCWQG_Units,
#' BCWQG_LimitType, BCWQG_Use, LimitType, LimitNote.
#' Adds BCWQG_Limit_num; guidelines that are not a simple number become NA.
load_parameter_list <- function(parameter_file) {
  read.csv(parameter_file, stringsAsFactors = FALSE) %>%
    filter(!is.na(PARAMETER), PARAMETER != "", trimws(Include) == "Yes") %>%
    select(PARAMETER, Category, BCWQG_Limit, BCWQG_Units, BCWQG_LimitType,
           BCWQG_Use, LimitType, LimitNote) %>%
    mutate(BCWQG_Limit_num = suppressWarnings(as.numeric(BCWQG_Limit)))
}

#' Final preparation of the lake data:
#'   1. keep only parameters listed in the parameter CSV (and attach their
#'      category and guideline columns),
#'   2. drop parameters with fewer than MIN_OBS_PER_PARAM observations,
#'   3. per parameter: convert mixed units to one unit (harmonise_units), then
#'      rescale tiny values to a smaller unit (scale_units).
#' After this, every function can assume units are final.
prepare_lake_data <- function(lake_data, parameter_list) {
  lake_data %>%
    inner_join(parameter_list, by = "PARAMETER") %>%
    group_by(PARAMETER) %>%
    filter(n() >= MIN_OBS_PER_PARAM) %>%
    group_modify(~ scale_units(harmonise_units(.x, .y$PARAMETER))) %>%
    ungroup()
}


# ---- 5. Trend analysis -------------------------------------------------------

#' TRUE where a p-value indicates a significant trend (NA-safe).
is_significant <- function(p_value) !is.na(p_value) & p_value < TREND_ALPHA

#' Fit a linear trend (RESULT ~ date) for ONE Level x Season group.
#'
#' @param d Rows of one parameter/level/season.
#' @return One-row tibble: n (samples), p_value, slope_per_year (result units
#'   per year) and pct_per_year (slope as % of the group mean). The trend
#'   columns are NA when there are too few samples or too many non-detects.
fit_trend <- function(d) {
  no_trend <- tibble(n = nrow(d), p_value = NA_real_,
                     slope_per_year = NA_real_, pct_per_year = NA_real_)

  if (nrow(d) < MIN_OBS_FOR_TREND || mean(d$IS_NONDETECT) > MAX_NONDETECT_FRAC) {
    return(no_trend)
  }

  coefs <- suppressWarnings(summary(lm(RESULT ~ COLLECTION_START, data = d))$coefficients)
  if (is.null(coefs) || nrow(coefs) < 2) return(no_trend)   # e.g. all dates identical

  slope_per_year <- coefs[2, "Estimate"] * DAYS_PER_YEAR     # lm slope is per day
  tibble(
    n              = nrow(d),
    p_value        = coefs[2, "Pr(>|t|)"],
    slope_per_year = slope_per_year,
    pct_per_year   = slope_per_year / mean(d$RESULT, na.rm = TRUE) * 100
  )
}

#' Trend results for every parameter x Level x Season group.
#'
#' Computed once per report and shared by the plots and the dashboard.
#' @return Tibble with PARAMETER, Level, Season, n, p_value, slope_per_year,
#'   pct_per_year.
calc_all_trends <- function(lake_data) {
  lake_data %>%
    filter(!is.na(Level), !is.na(Season)) %>%
    group_by(PARAMETER, Level, Season) %>%
    group_modify(~ fit_trend(.x)) %>%
    ungroup()
}


# ---- 6. Guideline lookup -----------------------------------------------------

#' Look up the BC water quality guideline for one parameter.
#'
#' @param param Parameter name.
#' @param parameter_list Table from load_parameter_list().
#' @param results_unit Unit of the results. If given, the guideline value is
#'   converted to it; if NULL the value is left in its original unit.
#' @return A list (always, so callers can still show the note when there is no
#'   guideline): limit_raw, unit_raw (as in the CSV), value (converted; NA if
#'   none), has_value, dir, symbol, use, type, note.
get_guideline <- function(param, parameter_list, results_unit = NULL) {
  g    <- parameter_list[parameter_list$PARAMETER == param, , drop = FALSE]
  pick <- function(col) if (nrow(g) == 0) NA else g[[col]][1]
  text <- function(x) if (is.na(x)) "" else as.character(x)

  limit_raw <- pick("BCWQG_Limit_num")
  unit_raw  <- pick("BCWQG_Units")
  dir       <- get_limit_dir(pick("LimitType"))

  value <- limit_raw
  if (!is.na(limit_raw) && !is.null(results_unit)) {
    value <- convert_units(limit_raw, unit_raw, results_unit)
  }

  list(
    limit_raw = limit_raw,
    unit_raw  = unit_raw,
    value     = value,
    has_value = !is.na(value),
    dir       = dir,
    symbol    = limit_symbol(dir),
    use       = text(pick("BCWQG_Use")),
    type      = text(pick("BCWQG_LimitType")),
    note      = text(pick("LimitNote"))
  )
}


# ---- 7. HTML snippets --------------------------------------------------------
# Styles for these classes are in the {css} chunk of the Rmd.

#' Coloured badge for one trend result. Grey "no trend" unless significant.
trend_badge <- function(p_value, pct_per_year) {
  if (!is_significant(p_value) || is.na(pct_per_year)) {
    return('<span class="trend-badge trend-flat">&#8212; no trend</span>')
  }
  css   <- if (pct_per_year >= 0) "trend-up" else "trend-down"
  arrow <- if (pct_per_year >= 0) "&#9650;" else "&#9660;"
  sprintf('<span class="trend-badge %s">%s %.1f%%/yr</span>', css, arrow, abs(pct_per_year))
}

#' Guideline card shown above a parameter's plot.
#' @param g List from get_guideline() with a usable value.
#' @param results_unit Unit shown after the value.
#' @param exceeds TRUE if the most recent sample breaches the guideline
#'   (red card with warning icon); otherwise a green card with a tick.
wqg_card <- function(g, results_unit, exceeds) {
  type_text <- if (nzchar(g$type)) paste0(" &middot; ", g$type) else ""

  sprintf(
    paste0(
      '<div class="%s"><span class="wqg-icon">%s</span><div>',
      '<div class="wqg-title"><a href="%s" target="_blank">BC Aquatic Life Guideline</a>%s %s</div>',
      '<div class="wqg-value">%s%s %s</div>',
      '<div class="wqg-status">%s</div>',
      '</div></div>'
    ),
    if (exceeds) "wqg-card wqg-exceed" else "wqg-card wqg-ok",
    if (exceeds) "&#9888;" else "&#10003;",
    WQG_URL, type_text, g$use,
    g$symbol, signif(g$value, 3), results_unit,
    g$note
  )
}

#' Grey card used when a parameter has no simple numeric guideline.
no_guideline_card <- function(note) {
  sprintf(
    paste0(
      '<div class="wqg-card wqg-none"><span class="wqg-icon">&#8212;</span><div>',
      '<div class="wqg-title">No simple <a href="https://www2.gov.bc.ca/gov/content?id=044DD64C7E24415D83D07430964113C9" target="_blank">BC Aquatic Life Guideline</a> available for this parameter</div>',
      '<div class="wqg-status">%s</div></div></div>'
    ),
    note
  )
}

#' Print a gt table inside a collapsed panel (click the heading to expand).
#' Must be called from a chunk with results = 'asis'.
emit_collapsible_table <- function(param, table_obj) {
  panel_id <- paste0("collapse", gsub("[[:punct:] ]+", "_", param))

  cat("\n")
  cat("<div class='panel panel-default' style='margin-top:10px;border:1px solid #ddd;border-radius:4px;'>")
  cat("<div class='panel-heading' style='background:#f5f5f5;padding:8px 12px;cursor:pointer;'",
      " data-toggle='collapse' href='#", panel_id, "'>", sep = "")
  cat("<strong>&#9654; Summary Statistics Table</strong>")
  cat("</div>")
  cat("<div id='", panel_id, "' class='panel-collapse collapse'>", sep = "")
  cat("<div class='panel-body' style='padding:12px;'>")
  cat(as.character(gt::as_raw_html(table_obj)))   # gt table as inline HTML
  cat("</div></div></div>\n\n")
}


# ---- 8. Plots ----------------------------------------------------------------

#' Sampling history: sample dates vs depth, one vertical line per composite
#' sample with points at the top, middle and bottom depth.
make_sampling_plot <- function(lake_data) {
  depth_data <- lake_data %>%
    select(COLLECTION_START, UPPER_DEPTH, LOWER_DEPTH, Level) %>%
    distinct() %>%
    filter(!is.na(Level)) %>%
    mutate(mid_depth = (UPPER_DEPTH + LOWER_DEPTH) / 2)

  ggplot(depth_data, aes(x = COLLECTION_START)) +
    geom_linerange(aes(ymin = UPPER_DEPTH, ymax = LOWER_DEPTH),
                   color = "grey70", linewidth = 0.6) +
    geom_point(aes(y = UPPER_DEPTH, shape = Level, color = Level), size = 2.5) +
    geom_point(aes(y = mid_depth,   shape = Level, color = Level), size = 2) +
    geom_point(aes(y = LOWER_DEPTH, shape = Level, color = Level), size = 2.5) +
    scale_shape_manual(values = c("Epilimnion" = 17, "Hypolimnion" = 16)) +
    scale_color_manual(values = LEVEL_COLORS) +
    labs(y = "Sample Depth (m)", x = NULL, title = "Sampling History") +
    scale_y_continuous(trans = "reverse") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_lake()
}

#' Layer 1: points faceted by Level (rows) x Season (columns).
#' Non-detects are drawn as open orange triangles at the detection limit.
base_param_plot <- function(dat, title, unit_label, caption) {
  ggplot(dat, aes(x = COLLECTION_START, y = RESULT)) +
    geom_point(aes(shape = IS_NONDETECT, color = IS_NONDETECT, size = IS_NONDETECT)) +
    scale_shape_manual(
      values = c("FALSE" = 16, "TRUE" = 6),
      labels = c("FALSE" = "Detected", "TRUE" = "Non-detect")
    ) +
    scale_color_manual(
      values = c("FALSE" = DETECT_COLOR, "TRUE" = NONDETECT_COLOR),
      labels = c("FALSE" = "Detected", "TRUE" = "Non-detect")
    ) +
    scale_size_manual(values = c("FALSE" = 2.5, "TRUE" = 2.5), guide = "none") +
    scale_x_date(
      date_labels = "%Y",
      breaks = pretty(dat$COLLECTION_START,
                      n = length(unique(year(dat$COLLECTION_START))))
    ) +
    labs(title = title, x = NULL, y = paste0("Result (", unit_label, ")"), caption = caption) +
    facet_grid(rows = vars(Level), cols = vars(Season)) +
    theme_lake() +
    theme(legend.position = "none")
}

#' Layer 2: dashed guideline line with a label.
#' The line is only drawn when the guideline is within GUIDELINE_PLOT_FACTOR of
#' the mean result; otherwise it would squash the data, so a subtitle explains
#' its absence.
add_guideline_layer <- function(p, dat, g) {
  if (!g$has_value) return(p)

  mean_result <- mean(dat$RESULT, na.rm = TRUE)
  show_line <- if (g$dir == "lower") {
    g$value > mean_result / GUIDELINE_PLOT_FACTOR
  } else {
    g$value < mean_result * GUIDELINE_PLOT_FACTOR
  }

  if (!show_line) {
    return(p + labs(subtitle = "Guideline line omitted — it is far from the plotted results."))
  }

  p +
    geom_hline(yintercept = g$value, linetype = "dashed", linewidth = 0.5,
               color = WQG_LINE_COLOR) +
    geom_label(
      data = data.frame(x = min(dat$COLLECTION_START), y = g$value),
      aes(x = x, y = y, label = paste0("Guideline (", g$dir, " limit)")),
      hjust = 0, vjust = -0.3, size = 2.6, fontface = "italic",
      color = WQG_LINE_COLOR, fill = "white", label.size = 0,
      inherit.aes = FALSE
    )
}

#' Layer 3: trend line, confidence band and a coloured badge for each group
#' with a significant trend. `dat` must contain p_value, slope_per_year and
#' pct_per_year (see make_param_plot()).
add_trend_layer <- function(p, dat, unit_label) {
  significant <- dat %>% filter(is_significant(p_value))
  if (nrow(significant) == 0) return(p)

  date_range <- max(dat$COLLECTION_START) - min(dat$COLLECTION_START)

  trend_labels <- significant %>%
    distinct(Level, Season, pct_per_year, slope_per_year) %>%
    mutate(
      direction   = if_else(pct_per_year >= 0, "▲", "▼"),
      badge_color = if_else(pct_per_year >= 0, TREND_UP_COLOR, TREND_DOWN_COLOR),
      label_unit  = paste0(direction, " ", abs(round(slope_per_year, 3)), " ", unit_label,
                           "/yr (", abs(round(pct_per_year, 1)), "%/yr)"),
      x = min(dat$COLLECTION_START) + date_range * 0.015,   # just inside the left edge
      y = Inf                                               # top of each panel
    )

  p +
    geom_smooth(
      data      = significant,
      aes(x = COLLECTION_START, y = RESULT),
      method    = "lm",
      formula   = y ~ x,
      color     = TREND_COLOR,
      fill      = scales::alpha(TREND_COLOR, 0.15),
      linewidth = 0.8
    ) +
    geom_label(
      data = trend_labels,
      aes(x = x, y = y, label = label_unit, fill = badge_color),
      hjust = 0, vjust = 1.4, size = 3, fontface = "bold",
      color = "white", label.size = 0,
      inherit.aes = FALSE
    ) +
    scale_fill_identity()
}

#' Build the full plot for one parameter: points + guideline + trends.
#'
#' @param data Lake data (final units) containing the parameter.
#' @param param Parameter name.
#' @param guideline List from get_guideline().
#' @param trends Table from calc_all_trends().
#' @return A ggplot, or NULL if there is nothing to plot.
make_param_plot <- function(data, param, guideline, trends) {
  dat <- data %>%
    filter(PARAMETER == param, !is.na(Level), !is.na(Season))

  if (nrow(dat) == 0) {
    message("No data for: ", param)
    return(invisible(NULL))
  }

  # Attach each group's trend result to its rows
  dat <- dat %>%
    left_join(
      trends %>% filter(PARAMETER == param) %>% select(-PARAMETER, -n),
      by = c("Level", "Season")
    )

  unit_label <- unique(dat$UNIT)[1]
  has_trend  <- any(is_significant(dat$p_value))

  caption <- if (has_trend) {
    "Trend line & badge = statistically significant trend (p < 0.05). ▽ = non-detect (plotted at detection limit)."
  } else {
    "▽ = non-detect (plotted at detection limit). No statistically significant trends detected in any group."
  }

  base_param_plot(dat, param, unit_label, caption) %>%
    add_guideline_layer(dat, guideline) %>%
    add_trend_layer(dat, unit_label)
}


# ---- 9. Summary statistics table ---------------------------------------------

#' Summary statistics (N, min, percentiles, mean, max) per Level x Season as a
#' gt table. Values at or below the detection limit are shown as "<value".
#' Cells that breach the guideline are highlighted red.
#'
#' @param data Lake data (final units) for the parameter.
#' @param param Parameter name.
#' @param guideline List from get_guideline().
#' @param report_title Title shown in the table header.
make_stats_table <- function(data, param, guideline, report_title) {
  dat <- data %>%
    filter(PARAMETER == param, !is.na(Level)) %>%
    # Chlorophyll is only sampled in the epilimnion
    filter(!(grepl("Chlorophyll", PARAMETER) & Level == "Hypolimnion"))

  if (nrow(dat) == 0) return(invisible(NULL))

  unit    <- unique(dat$UNIT)[1]
  wqg_val <- guideline$value
  wqg_dir <- guideline$dir

  # Detection limit = smallest non-detect result; -9999 if there are none
  dl_candidate <- dat %>%
    filter(RESULT_LETTER == "<") %>%
    summarise(dl = suppressWarnings(min(RESULT, na.rm = TRUE))) %>%
    pull(dl)
  detection_limit <- if (length(dl_candidate) == 0 || is.na(dl_candidate) || is.infinite(dl_candidate)) {
    -9999
  } else {
    dl_candidate
  }

  # 3 significant figures, prefixed "<" if at/below the detection limit
  fmt_val <- function(x) {
    x_sig <- signif(x, 3)
    if_else(x_sig <= detection_limit, paste0("<", x_sig), as.character(x_sig))
  }

  stats <- dat %>%
    group_by(Level, Season) %>%
    summarise(
      `N`        = n(),
      `Min`      = fmt_val(min(RESULT, na.rm = TRUE)),
      `10th Pct` = fmt_val(quantile(RESULT, 0.1, na.rm = TRUE)),
      `Median`   = fmt_val(quantile(RESULT, 0.5, na.rm = TRUE)),
      `Mean`     = fmt_val(mean(RESULT, na.rm = TRUE)),
      `90th Pct` = fmt_val(quantile(RESULT, 0.9, na.rm = TRUE)),
      `Max`      = fmt_val(max(RESULT, na.rm = TRUE)),
      MaxNum     = max(RESULT, na.rm = TRUE),   # numeric copies used for the
      MinNum     = min(RESULT, na.rm = TRUE),   # guideline comparison
      .groups = "drop"
    ) %>%
    mutate(
      Group = paste(Level, Season),
      `N`   = as.character(`N`),
      ExceedsWQG = !is.na(wqg_val) &
        (if (wqg_dir == "lower") MinNum < wqg_val else MaxNum > wqg_val)
    )

  # Upper limit -> highlight the Max column; lower limit -> the Min column
  exceed_col <- if (wqg_dir == "lower") "Min" else "Max"

  tbl <- gt(stats) %>%
    tab_header(
      title    = md(paste0("**", report_title, "**")),
      subtitle = paste0(param, " (", unit, ")")
    )

  # One labelled row group per Level x Season
  for (i in seq_len(nrow(LEVEL_SEASON_GROUPS))) {
    lvl       <- LEVEL_SEASON_GROUPS$Level[i]
    ssn       <- LEVEL_SEASON_GROUPS$Season[i]
    group_key <- paste(lvl, ssn)
    tbl <- tbl %>%
      tab_row_group(label = paste0(lvl, " — ", ssn), rows = Group == group_key)
  }

  tbl <- tbl %>%
    cols_hide(columns = c(Level, Season, Group, MaxNum, MinNum, ExceedsWQG)) %>%
    cols_align(align = "center") %>%
    cols_label(
      N          = "N",
      Min        = "Min",
      `10th Pct` = "10th %ile",
      Median     = "Median",
      Mean       = "Mean",
      `90th Pct` = "90th %ile",
      Max        = "Max"
    ) %>%
    tab_style(
      style     = list(cell_text(weight = "bold"), cell_fill(color = GROUP_FILL_COLOR)),
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style     = list(cell_text(weight = "bold", color = EXCEED_TEXT_COLOR),
                       cell_fill(color = EXCEED_FILL_COLOR)),
      locations = cells_body(columns = all_of(exceed_col), rows = ExceedsWQG)
    ) %>%
    tab_footnote(
      footnote  = "< indicates non-detect; value shown is the detection limit.",
      locations = cells_column_labels(columns = Min)
    )

  # Explain the red highlighting, but only if something is highlighted
  if (any(stats$ExceedsWQG, na.rm = TRUE)) {
    tbl <- tbl %>%
      tab_footnote(
        footnote = if (wqg_dir == "lower") {
          "Red = this group's minimum result is below the BC Aquatic Life guideline."
        } else {
          "Red = this group's maximum result exceeds the BC Aquatic Life guideline."
        },
        locations = cells_column_labels(columns = all_of(exceed_col))
      )
  }

  tbl %>%
    cols_width(everything() ~ px(120)) %>%
    tab_options(
      table.width     = px(900),
      container.width = px(900),
      row.striping.include_table_body = TRUE,
      row_group.font.weight = "bold"
    )
}


# ---- 10. Tab rendering and trend dashboard -----------------------------------

#' Render one parameter's tab: heading, guideline card, plot, collapsible stats
#' table. Must be called from a chunk with results = 'asis'.
#'
#' @param category_data Lake data for the category (final units).
#' @param param Parameter to render.
#' @param parameter_list Table from load_parameter_list().
#' @param trends Table from calc_all_trends().
#' @param report_title Title for the stats table header.
#' @param epi_only If TRUE the plot shows epilimnion samples only. The guideline
#'   card and stats table always use all samples.
render_parameter_tab <- function(category_data, param, parameter_list, trends,
                                 report_title, epi_only = FALSE) {
  cat("##### ", param, "\n\n")

  param_data   <- category_data %>% filter(PARAMETER == param)
  results_unit <- unique(param_data$UNIT)[1]
  guideline    <- get_guideline(param, parameter_list, results_unit)
  plot_data    <- if (epi_only) filter(param_data, Level == "Epilimnion") else param_data

  # Guideline card: red if the most recent sample (any depth/season) breaches it
  cat("\n\n")
  if (guideline$has_value) {
    # latest <- param_data %>%
    #   filter(!is.na(RESULT)) %>%
    #   arrange(desc(COLLECTION_START)) %>%
    #   slice(1)
    max_obs <- param_data %>%
        filter(!is.na(RESULT)) %>%
        arrange(desc(RESULT)) %>%
        slice(1)
    
    exceeds <- nrow(max_obs) > 0 &&
      exceeds_limit(max_obs$RESULT[1], guideline$value, guideline$dir)
    cat(wqg_card(guideline, results_unit, exceeds))
  } else {
    cat(no_guideline_card(guideline$note))
  }
  cat("\n\n")

  # Plot
  p <- make_param_plot(plot_data, param, guideline, trends)
  if (!is.null(p)) suppressWarnings(print(p))

  # Collapsible summary statistics
  tbl <- suppressMessages(suppressWarnings(
    make_stats_table(param_data, param, guideline, report_title)
  ))
  if (!is.null(tbl)) emit_collapsible_table(param, tbl)
}

#' HTML for the site-wide "trends at a glance" table: one row per parameter,
#' one trend badge per Level x Season group, plus the guideline.
#' Rows follow CATEGORIES order, then alphabetical.
#'
#' @return A single HTML string; print it with cat() in an 'asis' chunk.
make_trend_dashboard <- function(lake_data, parameter_list, trends) {

  group_labels <- sprintf(
    "%s<br><span style='font-weight:400'>%s</span>",
    LEVEL_SEASON_GROUPS$Level, LEVEL_SEASON_GROUPS$Season
  )

  # Parameters with at least one sample in a known depth/season, sorted by
  # category (matching tab order) then name. Unknown categories go last.
  params_here <- lake_data %>%
    filter(!is.na(Level), !is.na(Season)) %>%
    distinct(PARAMETER, Category) %>%
    mutate(rank = coalesce(match(Category, CATEGORIES), length(CATEGORIES) + 1L)) %>%
    arrange(rank, PARAMETER)

  # One table cell: trend badge, or "n/a" if the group has too few samples
  trend_cell <- function(param, level, season) {
    t <- trends[trends$PARAMETER == param & trends$Level == level & trends$Season == season, ]
    if (nrow(t) == 0 || t$n[1] < MIN_OBS_FOR_TREND) {
      return('<td class="text-center text-muted">n/a</td>')
    }
    paste0('<td class="text-center">', trend_badge(t$p_value[1], t$pct_per_year[1]), '</td>')
  }

  one_row <- function(i) {
    param <- params_here$PARAMETER[i]

    cells <- vapply(
      seq_len(nrow(LEVEL_SEASON_GROUPS)),
      function(j) trend_cell(param, LEVEL_SEASON_GROUPS$Level[j], LEVEL_SEASON_GROUPS$Season[j]),
      character(1)
    )

    g <- get_guideline(param, parameter_list)   # original units, no conversion
    guideline_text <- if (!is.na(g$limit_raw)) {
      paste0(g$symbol, " ", g$limit_raw, " ", g$unit_raw)
    } else {
      '<span class="text-muted">&#8212;</span>'
    }

    paste0(
      "<tr><td>", params_here$Category[i], "</td>",
      "<td><strong>", param, "</strong></td>",
      paste(cells, collapse = ""),
      "<td class='text-center'>", guideline_text, "</td></tr>"
    )
  }

  rows_html <- vapply(seq_len(nrow(params_here)), one_row, character(1))

  header <- paste0(
    '<div class="table-responsive"><table class="table table-sm table-striped trend-dashboard">',
    '<thead><tr><th>Category</th><th>Parameter</th>',
    paste0('<th class="text-center">', group_labels, '</th>', collapse = ""),
    '<th class="text-center">Guideline</th></tr></thead><tbody>'
  )

  paste0(header, paste(rows_html, collapse = "\n"), "</tbody></table></div>")
}
