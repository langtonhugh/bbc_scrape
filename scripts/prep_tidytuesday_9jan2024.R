# Packages.
library(readr)
library(dplyr)
library(lubridate)
library(pbapply)
library(ggplot2)

# List all the scraped files.
scrapes_list <- paste0("scrapes_subset/", list.files("scrapes_subset", pattern = "*.csv"))

# Load them all in.
scrapes_df_list <- pblapply(scrapes_list, read.csv)

# Row bind them together.
scrapes_df <- bind_rows(scrapes_df_list)

# Make sure dates are dates.
scrapes_clean_df <- scrapes_df %>% 
  as_tibble() %>% 
  mutate(scrape_date = ymd_hms(scrape_date)) %>% 
  rename(article_title = value) %>%
  select(-url)

# Save.
write_csv(x = scrapes_clean_df, file = "data/tt_subset.csv")

# End.
