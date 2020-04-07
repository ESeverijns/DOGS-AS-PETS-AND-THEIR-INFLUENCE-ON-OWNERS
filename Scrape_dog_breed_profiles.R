#scrape dogtime.com
library(tidyverse)
library(rvest)





#function to scrape alle elements also missing elements
scrape_css<-function(css,group,html_page){
  txt<-html_page %>%
    html_nodes(group) %>%
    lapply(.%>% html_nodes(css) %>% html_text() %>% ifelse(identical(.,character(0)),NA,.)) %>%
    unlist()
  return(txt)
}





#function to scrape alle attributes also missing elements
scrape_css_attr<-function(css,group,attribute,html_page){
  txt<-html_page %>%
    html_nodes(group) %>%
    lapply(.%>% html_nodes(css) %>% html_attr(attribute) %>% ifelse(identical(.,character(0)),NA,.)) %>%
    unlist()
  return(txt)
}





#Get data of one element one level deeper
get_element_data<-function(link){
  
  if(!is.na(link)){
    #read the page
    html<-read_html(link)
    
    #delay of 2 seconds between requests
    Sys.sleep(2)
    
    #here I have to change for the one level deeper
    #read the friendly towards stranger
    Friendly_towards_strangers<-html %>%
      html_node(".paws:nth-child(3) .child-characteristic:nth-child(5) .characteristic-star-block") %>%
      html_text()
    
    # #read the dog friendly 
    # Dog_Friendly<-html %>%
    #   html_node(".paws:nth-child(3) .child-characteristic:nth-child(4) .characteristic-star-block") %>%
    #   html_text()
    # 
    # #read the Affectionate_with_family
    # Affectionate_with_family<-html %>%
    #   html_node(".paws:nth-child(3) .parent-characteristic+ .child-characteristic .characteristic-star-block") %>%
    #   html_text()
    # 
    # #read the Potential_to_Mouthiness
    # Potential_to_Mouthiness<-html %>%
    #   html_node(".paws:nth-child(5) .child-characteristic:nth-child(4) .characteristic-star-block") %>%
    #   html_text()
    # 
    # #read the Size
    # Size<-html %>%
    #   html_node(".paws:nth-child(4) .child-characteristic:nth-child(7) .characteristic-star-block") %>%
    #   html_text()
    # 
    
    #add everything to a tibble and return the tibble
    return(tibble(Friendly_towards_strangers=Friendly_towards_strangers))
  }
}





#Get all elements from 1 page
get_elements_from_url<-function(url){
  
  #scrape all elements
  html_page<-read_html(url)
  
  #delay of 2 seconds between requests
  Sys.sleep(2)
  
  #get the house type
  Type_of_Breed<-scrape_css(".list-item-title",".list-item",html_page)
  
  #get all urls to go one level deeper
  element_urls<-scrape_css_attr(".list-item-title",".list-item","href",html_page)
  
  #Get all content of one episode (one level deeper)
  element_data_detail<-element_urls %>%
    # Apply to all URLs
    map(get_element_data) %>%
    # Combine the tibbles into one tibble
    bind_rows()
  
  # Combine into a tibble
  elements_data<-tibble(Type_of_Breed = Type_of_Breed,element_urls=element_urls)
  # Get rid of all the NA's (advertisements, which are links but don't contain data)
  # Complete cases gives FALSE back when the column (in this case column 2), contains a NA
  elements_data_overview <- elements_data[complete.cases(elements_data[,2]), ]
  # Combine the page data en detail data into a tibble and return
  return(bind_cols(elements_data_overview,element_data_detail))
}






#call the function and write the returned tibble to friends
Breeds<-get_elements_from_url("https://dogtime.com/dog-breeds/profiles")





#show the data
View(Breeds)





write_rds(Breeds, "Breeds.rds")

