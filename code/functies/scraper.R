
#the scraper function to scrape the details from the submissions
#-----------------------------------------------------------------------------------------------------------------------

# #First page as a test
# test_page <- html_initiatives[[100]]
# 
# test_page %>% html_nodes("link[rel=canonical]") %>% html_attr("href")         # Link to page (= node link where attribute is canonical)
# test_page %>% html_nodes(".articleHead__title") %>% html_text(trim = TRUE)    # Title
# test_page %>% html_nodes(".rs_preserve p+ p , p:nth-child(1)") %>% 
#   html_text(trim = TRUE) %>% 
#   glue::glue_collapse(sep = " ")                                  # Full abstract (collapse multiple paragraphs into one if necessary)
# (test_page %>% html_nodes("div.h3+ p") %>% html_text(trim = TRUE))[1]         # Keywords. This one was more complex. Gives me the content of all <p></p> that follow after a (closed) <div> tag with class h3. Usually the first time that occurs is after the keywords header, so I only select the content of the first <p></p>. Important is that the <p> tag is NOT inside of the <div> tag, but follows after the closing brackets </div>
# (test_page %>% html_nodes("div.h3+ p") %>% html_text(trim = TRUE))[2]         # Other organisations
# test_page %>% html_nodes(".sidebar--content p") %>% html_text(trim = TRUE)    # Everything in the side bar. Not every submission has all the information listed so splitting things up later seems easier than trying to use nthchild etc.
# 
# rm(test_page)

scraper <- function(html_page){
  
  if (isFALSE(all(class(html_page) %in% c("xml_document", "xml_node")))){
    stop("Input must have class xml_document and xml_node")}
  
  title    =  html_page %>% rvest::html_nodes(".articleHead__title") %>% rvest::html_text(trim = TRUE)%>%
    if(identical(., character(0))) NA_character_ else .                                       # Title
  
  link     =  html_page %>% html_nodes("link[rel=canonical]") %>% html_attr("href") %>%
    if(identical(., character(0))) NA_character_ else .
  
  abstract =  html_page %>% rvest::html_nodes(".rs_preserve p+ p , p:nth-child(1)") %>% 
    rvest::html_text(trim = TRUE) %>% 
    glue::glue_collapse(sep = " ") %>%
    if(identical(., character(0))) NA_character_ else .                                       # Full abstract (collapse multiple paragraphs into one if necessary)
  
  keywords =  (html_page %>% rvest::html_nodes("div.h3+ p") %>% rvest::html_text(trim = TRUE))[1] %>%
    str_replace_all(pattern = ",  ", replacement = ";") %>%
    stringr::str_squish() %>%  
    if(identical(., character(0))) NA_character_ else .                                       # Keywords. This one was more complex. Gives me the content of all <p></p> that follow after a (closed) <div> tag with class h3. Usually the first time that occurs is after the keywords header, so I only select the content of the first <p></p>. Important is that the <p> tag is NOT inside of the <div> tag, but follows after the closing brackets </div>
  
  other_orgs =  (html_page %>% rvest::html_nodes("div.h3+ p") %>% rvest::html_text(trim = TRUE))[2] %>%
    str_replace_all(pattern = ",  ", replacement = ";") %>%
    stringr::str_squish() %>%
    if(identical(., character(0))) NA_character_ else .        # Other organisations
  
  other_details = html_page %>% rvest::html_nodes(".sidebar--content p") %>% 
    rvest::html_text(trim = TRUE) %>% list()                   # The rest as a list
  
  # note: The if(identical(., character(0))) NA_character_ else . command at the end of each frase was added in case 
  # not all pages include all necessary html_nodes
  # this lead to a character(0) output, that the tibble wouldn't store, which messed everything up.
  # the expression now explicitly assigns a NA value which the tibble can store  
  
  tibble(title, link, abstract, keywords, other_orgs, other_details)
  
}