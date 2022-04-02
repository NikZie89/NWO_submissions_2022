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

# I want to scrape the details of all submissons for the NWO grands  of 2022.
# https://initiatieven.wetenschapsagenda.nl/initiatieven



#Getting the links to the individual proposal pages
#-----------------------------------------------------------------------------------------------------------------------

# first I need the links of the overviewpages that contain the links to the proposals
# then I can scrape those
links_overview <- paste0("https://initiatieven.wetenschapsagenda.nl/initiatieven?page=", 0:32) # 33 pages, no new submissions will be added, because deadline is over


# the firewall of my netwok does not let me scrape directly, so instead I download and save all the html pages first


# download html of all 33 overview pages.
# after that I can load them in, and scrape links of each page to the individual proposals (over 300)
# I will name the files simply page_x.html

purrr::map2(.x = links_overview, .y = paste0("page_", 1:33,".html"), 
            ~ {Sys.sleep(5);                                                             # Use anonymous function, include sys.sleep, then print out a message.
              message(Sys.time()," - busy with ", .y); .x} %>%                           # print out a message, next pipe the links (.x) into the download function. The semicolon is used to seperate the different function calls. Could also be done by using a line break instead.
              download.file(destfile = paste0("data/overviewpages/", .y), quite = TRUE)) # download html of all overview pages


# load all stored html pages into a list
# using purrr here as well because it proved to be a bit faster than lapply
html_overview <- base::invisible(
  purrr::map(
      list.files('data/overviewpages', recursive = TRUE, full.names = TRUE, pattern = '.html'),
      rvest::read_html
    )
)


# extract all links
#----------------------------------------------
all_links <- html_overview %>% purrr::map( ~.x %>% html_nodes(".overviewContent") %>% 
                                                        html_elements("a") %>% 
                                                        html_attr("href")) %>%        # all links from the content part of the pages
                                                        unlist()                      # collapse everything

# filter unnecessary links and add domain and 

all_links <- all_links[grepl(pattern = "initiatives", all_links)] %>%
                paste0("https://initiatieven.wetenschapsagenda.nl", .)

all_names <- str_extract(all_links, pattern = "([^/]+$)") # extract the last part of the link so I can that as names for the downloaded html pages


# extract html page for each initiative
#-----------------------------------------------------------------------------------------------------------------------

# now that I have all 393 links I can repeat the earlier step and download the source code of each page
# in case there is a broken link or some other download limitation, I will use purrr::safely this time


download_safely <- purrr::safely( ~ {Sys.sleep(5);                                                     # Use anonymous function, include sys.sleep, then print out a message.
                            message(Sys.time()," - busy with ", .y); .x} %>%                           # print out a message, next pipe the links (.x) into the download function. The semicolon is used to seperate the different function calls. Could also be done by using a line break instead.
                            download.file(destfile = paste0("data/initiatives/", .y), quite = TRUE))


download_log <- purrr::map2(.x = all_links, .y = paste0(all_names,".html"), download_safely)           # downloads all files and also stores potential error messages if the download_safely function should fail

map(download_log, "error") %>% compact() #returns empty list, because all errors are NULL. All links were working.

# load all stored html pages into a list
# using purrr here as well because it proved to be a bit faster than lapply
html_initiatives <- 
  purrr::map(
    list.files('data/initiatives', recursive = TRUE, full.names = TRUE, pattern = '.html'),  # list of all html pages my (.x)
    ~{message(Sys.time()," - reading html from ", .x);
      rvest::read_html(.x)}                                                                  # input .x into the function. Last call of the function, so this will be saved to my object html_initiatives
  )


# extract the actual data and store everything in a data frame
#-----------------------------------------------------------------------------------------------------------------------

# Each page should be build in the same way, which means you can extract
# 1. Name of the project
# 2. Abstract
# 3. Keywords
# 4. Other organizations
# 5. Characteristics (Topic/ Route and cluster question)
# 6. Applicant details if available (applying institution, PI, website of project) #at some pages those are not posted

#First page as a test
test_page <- html_initiatives[[1]]

test_page %>% html_nodes(".articleHead__title") %>% html_text(trim = TRUE)    # Title
test_page %>% html_nodes(".rs_preserve p+ p , p:nth-child(1)") %>% 
              html_text(trim = TRUE) %>% 
              glue::glue_collapse(sep = " ")                                  # Full abstract (collapse multiple paragraphs into one if necessary)
(test_page %>% html_nodes("div.h3+ p") %>% html_text(trim = TRUE))[1]         # Keywords. This one was more complex. Gives me the content of all <p></p> that follow after a (closed) <div> tag with class h3. Usually the first time that occurs is after the keywords header, so I only select the content of the first <p></p>. Important is that the <p> tag is NOT inside of the <div> tag, but follows after the closing brackets </div>

test_page %>% html_elements("p")