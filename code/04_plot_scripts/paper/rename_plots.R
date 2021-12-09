name_lookup <- read.csv("code/04_plot_scripts/paper/renaming_lookup_table.csv", header = T)
purrr::walk2(
  name_lookup$code_output,
  name_lookup$paper_names,
  function(x, y) {
    file.copy(file.path("plots", x), file.path("plots", "renamed", y), overwrite = T) 
  }
)
