library(shiny.i18n)

I18N_DIR <- "./i18n"

translator <- Translator$new(
  translation_csvs_path = I18N_DIR,
  translation_csv_config = file.path(I18N_DIR, "config.yaml")
)
