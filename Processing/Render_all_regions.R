
# List of .qmd files
qmd_files <- list.files(pattern = "\\.qmd$", full.names = TRUE)
qmd_files <- qmd_files[-5] #remove old alaska

library(quarto)
# Render each file one by one
for (file in qmd_files) {
  message("Rendering: ", file)
  tryCatch({
    quarto_render(file)
  }, error = function(e) {
    message("Failed to render ", file, ": ", e$message)
  })
}
