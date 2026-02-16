# ============================================================
# BioR — Class 5 setup (Programming with R)
# Installs missing packages, then loads them.
# ============================================================


pkgs <- c(
  "tm",
  "wordcloud2",
  "scholar",
  "stringr",
  "wordcloud"
)


# Install missing packages
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

# Load packages (grouped, so students see what is for what)
library(tm)
library(wordcloud2)
library(scholar)
library(stringr)
library(wordcloud)

