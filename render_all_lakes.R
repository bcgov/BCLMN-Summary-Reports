library(rmarkdown)
library(dplyr)

# --- Define the list of lakes to render ---
# Add one row per lake. lakeDepth can be NA if unknown.
lakes <- tibble::tribble(
  ~ems_id,     ~title,                    ~lakeDepth,
  "E206283",   "Lizard Lake",             15,
  "E207466",   "Quamichan Lake",          7,
  "0500615",   "Skaha Lake",      54
)

# --- Path to the report template and parameter CSV ---
rmd_path        <- "lake_report.Rmd"
parameter_file  <- "parameters_with_BCWQG_AquaticLife.csv"   # same categorisation file used for every lake
output_dir      <- "rendered_reports"

if (!dir.exists(output_dir)) dir.create(output_dir)

# --- Loop through each lake and render ---
for (i in seq_len(nrow(lakes))) {

  this_lake <- lakes[i, ]

  # Build a safe file name from the title (spaces -> underscores, strip punctuation)
  safe_name <- gsub("[[:punct:] ]+", "_", this_lake$title)
  out_file  <- paste0(safe_name, "_", this_lake$ems_id, ".html")

  message("Rendering: ", this_lake$title, " (", this_lake$ems_id, ") -> ", out_file)

  tryCatch({
    rmarkdown::render(
      input       = rmd_path,
      intermediates_dir = tempdir(),
      clean = TRUE,
      output_file = out_file,
      output_dir  = output_dir,
      params = list(
        title           = this_lake$title,
        ems_id          = this_lake$ems_id,
        parameter_file  = parameter_file
      ),
      envir = new.env()   # ensures a clean environment for each render
    )
  }, error = function(e) {
    message("  FAILED: ", this_lake$title, " — ", conditionMessage(e))
  })
}

message("Done. Reports saved to: ", output_dir)
