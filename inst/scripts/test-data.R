# Initialization
## load packages
devtools::load_all()

## define variables
site_ids <- c("q", "w", "e")
feature_ids <- c("a", "s", "d", "f")
action_ids <- c("z", "x", "c", "v", "b")
parameters <- read_data_configuration()

# Main processing
wb <- create_template_workbook(
  site_ids = site_ids,
  site_descriptions = paste0("Site ", site_ids),
  feature_ids = feature_ids,
  feature_descriptions = paste0("Feature ", feature_ids),
  action_ids = action_ids,
  action_descriptions = paste0("Action ", action_ids),
  parameters = parameters
)

# Exports
path <- "tests/testthat/testdata/data-template.xlsx"
openxlsx::saveWorkbook(
  wb = wb,
  file = path,
  overwrite = TRUE,
  returnValue = FALSE
)
assertthat::assert_that(file.exists(path), msg = "failed to save workbook")
