# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Install cmdstanr if not automatically found
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# Set target options:
tar_option_set(
  packages = c("here"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)


# Replace the target list below with your own:
list(
  tar_quarto(models, "Writeups/models.qmd"),
  tar_quarto(figures,"Writeups/figures.qmd")
)
