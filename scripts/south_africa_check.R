# Packages.
library(readr)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(stringr)
library(forcats)
library(pbapply)
library(ggplot2)

# List all the scraped files.
scrapes_list <- paste0("scrapes/10jan2024_onward/", list.files("scrapes/10jan2024_onward", pattern = "*.csv"))

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

# Start handling and visual.
scrapes_clean_df %>% 
  ggplot(data = .) +
  geom_point(mapping = aes(x = scrape_date, y = 1))


# Top read orders.
order_value_df <- scrapes_clean_df %>% 
  group_by(order_var, article_title) %>% 
  tally() %>% 
  arrange(desc(n)) 

# What's your keyword(s)?
interest_words <- "genocide|S Africa|court"

# Identify a specific story.
scrapes_clean_df <- scrapes_clean_df %>%
  mutate(article_title_lower = str_to_lower(article_title),
         interest_label = if_else(condition = str_detect(article_title_lower, interest_words),
                                  true      = article_title,
                                  false     = "Other"))

# What have we got?
count(scrapes_clean_df, interest_label)

# scrapes_clean_df for 'other'.
scrapes_clean_df <- scrapes_clean_df %>% 
  mutate(interest_label = fct_relevel(interest_label, "Other"))

# How many stories on the interest flag to we have?
n_stories_vec <- length(unique(scrapes_clean_df$interest_label))-1

# Colour scheme define. Note that 'other' is now always trhe first level.
col_scheme <- c("grey95", brewer.pal(n = n_stories_vec, name = "Set2"))

# Tile plot.
tile_gg <- ggplot(data = scrapes_clean_df) +
  geom_tile(mapping = aes(x = scrape_date, y = order_var, fill = interest_label, colour = interest_label)) +
  scale_fill_manual  (values = col_scheme) +
  scale_colour_manual(values = col_scheme) +
  scale_y_reverse(breaks = c(1:10)) +
  theme_bw() +
  labs(title    = paste("BBC Top 10 'most read' involving the term:", interest_words),
       subtitle = paste("Time range between", min(scrapes_clean_df$scrape_date), "and", max(scrapes_clean_df$scrape_date) ),
       fill = NULL, x = NULL, y = "Top 10 ranking") +
  guides(fill   = guide_legend(nrow = 4),
         colour = "none") +
  theme(legend.position = "bottom") 

# Save.
ggsave(plot = tile_gg, filename = "visuals/interest_tile_genocide.png", width = 20, height = 12, unit = "cm", dpi = 300)
