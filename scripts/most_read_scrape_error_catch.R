# BBC scrape.
# Created 12 October 2023.
# Credit to Danielle Stibbe for the html help!

# Load libraries.
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Define the URL.
myurl <- "https://www.bbc.com/news" 

# Start the repeat.
repeat {

# Maximum number of retries
max_retries <- 10

# Start the repeat.
for (attempt in 1:max_retries) {
  
  
  
  tryCatch({
    # Read html page
    whole_page <- myurl %>% 
      read_html()
    
    # Pull most read element(s).
    most_read_node <- whole_page %>%
      html_element(xpath = "//div[@class = 'nw-c-most-read__items gel-layout gel-layout--no-flex']") %>% 
      html_elements(xpath = ".//a")
    
    # Get the text and make into a table.
    most_read_df <- most_read_node %>%
      html_text() %>% 
      as_tibble() %>% 
      mutate(order_var = 1:10) %>% 
      select(order_var, value)
    
    # Get the attributes associated with each.
    att <- most_read_node %>% 
      html_attrs()
    
    # Pull the links.
    att_links <- lapply(att, function(x) x[[2]]) 
    
    # Add the URLs to our existing tibble.
    url_df <- tibble(
      url = as.character(lapply(att_links, paste))
    ) 
    
    # Stick back to our most read table and add the scrape timestamp.
    most_read_url_df <- most_read_df %>% 
      bind_cols(url_df) %>% 
      mutate(scrape_date = format(Sys.time()))
    
    # Create timestamp for naming the files.
    time_stamp <- unique(most_read_url_df$scrape_date) %>% 
      str_remove_all(pattern = c("-|:")) %>% 
      str_replace_all(pattern = " ", replacement = "_")
    
    # Save.
    write_csv(x = most_read_url_df,
              file = paste0("scrapes/12feb2024_onward/most_read_url_", time_stamp, ".csv"))
    
    # If successful, break out of the loop.
    break
    
   
  }, error = function(e) {
    # Print the error message.
    cat("Error:", conditionMessage(e))
    
    # If not the last attempt, wait for 30 seconds before retrying.
    if (attempt < max_retries) {
      cat(" Retrying in 30 seconds.  \n")
      Sys.sleep(30)
    } else {
      cat(" I give up! Maximum number of retries reached.")
    }
  })
}

# Sleep for 5 minutes before repeat.
Sys.sleep(300)

}


