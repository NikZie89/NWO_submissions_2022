####################################################################
# Auteur: Niklas
# Datum: 01.04.2022
####################################################################

rm(list=ls()); graphics.off()

library(conflicted)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(ivho)
library(rvest)
library(purrr)

# De code hieronder zorgt er voor dat alle functies automatisch beschikbaar worden
base::invisible(
  lapply(
    list.files('code/functies', recursive = TRUE, full.names = TRUE, pattern = '.R'),
    FUN = source
  )
)

settings <- yaml::read_yaml('settings/settings.yml')

#-----------------------------------------------------------------------------------------------------------------------

# Short intro

# The objective is to scrape the details of the submissons (almost 400) for this years (2022) call for the NWO grands.
# https://initiatieven.wetenschapsagenda.nl/initiatieven

# As this is an annual grand, and the website only displays the submissions for the current call, the webscraper will 
# probably be outdated over a few month.
# With a bit of luck, the structure of the web page does not change too much and the scraper can be used next year again.

# With that data it is possible to analyse which Universities submitted the most grand proposals this year,
# which keywords and lines of research were the most popular and if the proposal descriptions tend to use certain language.
# E.g. mentioning words like "innovative", "state of the art" etc.

# Over a few month it might also be possible to compare the winning proposals to those that did not make the cut.

# Biggest limitation: The overview page only give a summary of each proposal, not the full proposal. 
# So the details are limited.


# The scripts
#-----------------------------------------------------------------------------------------------------------------------

#scraper to stroe the html page of each submission and 
source("code/scraping_submission_details.R")