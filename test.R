#Git/GitHub with R with Vafa Saboori
# Create a Personal Access Token (PAT) on GitHub

# Shorter version
pkg_setup <- function(...) {
      pkgs <- c(...)
      new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
      if (length(new_pkgs)) install.packages(new_pkgs)
      invisible(sapply(pkgs, library, character.only = TRUE))
}

# Usage:
pkg_setup("usethis", "gitcreds")

# Step 1: To check that Git is installed, from the terminal
#which git --version

# Step 2: How to Edit the gitconfig File
usethis::edit_git_config()

# Step 3: Create a Git Project
# File, New Project, New Directory, name the Directory

# This will take you to GitHub where you can create a PAT
# Navigate to the bottom, create, and save the token
usethis::create_github_token()


# Store the PAT to Connect
# This will allow you to synch between RStudio and GitHub
# You may need to update your xCode on MAC in order to enable the gitcreds_set
gitcreds::gitcreds_set()

# Input your token - this will connect RStudio to GitHub, but not a specific project
usethis::use_github()

library(tidyverse)
df <- datasets::airmiles %>% 
      tibble::enframe(name = "year", value = "miles")

# Create histogram
df %>% 
      ggplot(aes(x = miles)) +
      geom_histogram(bins = 30)
datasets::airmiles%>%tibble()