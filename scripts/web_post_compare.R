# Packages.
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)
library(stringr)
library(forcats)
library(pbapply)
library(ggplot2)

# Useful function(s).
`%nin%` <- Negate(`%in%`)

# List all the scraped files.
scrapes_list <- paste0("scrapes/10jan2024_09feb2024/", list.files("scrapes/10jan2024_09feb2024", pattern = "*.csv"))

# Load them all in.
scrapes_df_list <- pblapply(scrapes_list, read.csv)

# Row bind them together.
scrapes_df <- bind_rows(scrapes_df_list)

# Any missings? No.
sum(is.na(scrapes_df))

# Make sure dates are dates.
scrapes_clean_df <- scrapes_df %>% 
  as_tibble() %>% 
  mutate(scrape_date = ymd_hms(scrape_date)) %>% 
  rename(article_title = value) %>%
  select(-url)

# Top read orders.
order_value_df <- scrapes_clean_df %>% 
  group_by(order_var, article_title) %>% 
  tally() %>% 
  arrange(desc(n)) 

# What about only stories that made number 1?
no1_df <- order_value_df %>% 
  filter(order_var == 1)

# What's your keyword(s)?
topic1 <- "ukraine|russia|putin|zalensky"
topic2 <- "israel|gaza|hamas|netanyahu"
topics <- paste(topic1, topic2, sep =  "|")

# Identify a specific story.
scrapes_clean_df <- scrapes_clean_df %>%
  mutate(article_title_lower = str_to_lower(article_title),
         # topic 1.
         interest_label = case_when(
           str_detect(article_title_lower, topic1) ~ topic1,
           str_detect(article_title_lower, topic2) ~ topic2,
           article_title_lower %nin% topics ~ "Other"
         ))

# What have we got?
count(scrapes_clean_df, interest_label)

# Change factor levels for visual, so that Other is always first.
scrapes_clean_df <- scrapes_clean_df %>% 
  mutate(interest_label = fct_relevel(interest_label, "Other"))

# Defining number of topics, in case more than two.
n_stories_vec <- length(unique(scrapes_clean_df$interest_label))-1

# Colour scheme define. Note that 'other' is now always the first level.
# Warning if topics two, but it is fine.
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
  guides(fill   = guide_legend(nrow = 1),
         colour = "none") +
  theme(legend.position = "bottom") 

# Save.
ggsave(plot = tile_gg, filename = "visuals/interest_tile_compare.png", width = 20, height = 12, unit = "cm", dpi = 300)

# Proportional visual.
props_breakdown_df <- scrapes_clean_df %>% 
  group_by(scrape_date) %>% 
  count(interest_label) %>% 
  ungroup() %>% 
  complete(scrape_date, interest_label, fill = list(n = 0)) %>% 
  mutate(prop_of_top_10 = n*10) %>% 
  arrange(scrape_date)

# Plot.
ggplot(data = props_breakdown_df) +
  geom_area(mapping = aes(x = scrape_date,
                         y = prop_of_top_10,
                         fill = interest_label),
           position = "fill") +
  theme(legend.position = "bottom")

# Save.
ggsave(filename = "visuals/interest_tile_compare_prop.png", width = 20, height = 12, unit = "cm", dpi = 300)

