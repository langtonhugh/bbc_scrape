# Packages.
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(stringr)
library(forcats)
library(pbapply)
library(ggplot2)

# Folder date range of interest.
scrape_folder <- "10jan2024_09feb2024"

# Unzip folder containing scrapes.
unzip(zipfile = paste0("scrapes/", scrape_folder, ".zip"), exdir = "scrapes", overwrite = FALSE)

# Paste together the working directory location, together with all the csv file names.
scrapes_list <- paste0(
  # the working directory folder leading to the csv.
  "scrapes/", scrape_folder, "/", 
  # list all the csvs in that folder.
                       list.files(
                         paste0("scrapes/", scrape_folder),
                         pattern = "*.csv")
                       )

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

# Time spent in top 10 during the period.
top10_time_df <- scrapes_clean_df %>% 
  group_by(article_title) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(mins = n*5,
         hrs  = mins/60) %>% 
  arrange(desc(n)) 

# Save.
write.csv(top10_time_df, file = paste0("output/top10_time_", scrape_folder ,".csv"))

# Time spent in each ranking position during the period.
ranking_time_df <- scrapes_clean_df %>% 
  group_by(order_var, article_title) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(mins = n*5,
         hrs  = mins/60) %>% 
  arrange(desc(n)) 

# Save.
write.csv(ranking_time_df, file = paste0("output/ranking_time_", scrape_folder, ".csv"))

# What about only stories that made number 1?
no1_time_df <- ranking_time_df %>% 
  filter(order_var == 1)

# Save.
write.csv(no1_time_df, file = paste0("output/no1_time_", scrape_folder, ".csv"))

# # Select a 24-hour time period (if wanted).
# scrapes_clean_df <- scrapes_clean_df %>%
#   filter(date(scrape_date) == "2024-01-31")

# What's your keyword(s)?
interest_words <- "putin"

# Identify a specific story.
scrapes_clean_df <- scrapes_clean_df %>%
  mutate(article_title_lower = str_to_lower(article_title),
         interest_label = if_else(condition = str_detect(article_title_lower, interest_words),
                                  true      = article_title,
                                  false     = "Unrelated"))

# What have we got?
count(scrapes_clean_df, interest_label)

# if else for high category topics.
if (length(unique(scrapes_clean_df$interest_label)) > 8 ) {
  scrapes_clean_df <- scrapes_clean_df %>%
    mutate(interest_label = fct_lump_n(interest_label, 7, other_level = "Other"))
} else {
  print("Few than 8 unique stories. No recode done.")
}

# Change factor levels for visual. Unrelated should always be first.
scrapes_clean_df <- scrapes_clean_df %>% 
  mutate(interest_label = fct_relevel(interest_label, "Unrelated"))

# How many stories on the interest flag to we have?
n_stories_vec <- length(unique(scrapes_clean_df$interest_label))-1

# Colour scheme define. Note that 'Unrelated' is now always the first level, so it's grey.
col_scheme <- c("grey95", brewer.pal(n = n_stories_vec, name = "Set2"))

# Tile plot.
tile_gg <- ggplot(data = scrapes_clean_df) +
  geom_tile(mapping = aes(x = scrape_date, y = order_var, fill = interest_label, colour = interest_label)) +
  scale_fill_manual  (values = col_scheme) +
  scale_colour_manual(values = col_scheme) +
  scale_y_reverse(breaks = c(1:10)) +
  theme_bw() +
  labs(title    = paste("BBC Top 10 'most read' involving the term(s):", str_replace_all(interest_words, "\\|", ", ")),
       subtitle = paste("Time range between",
                        format(min(scrapes_clean_df$scrape_date), "%d-%m-%Y %H:%M:%S"),
                        "and",
                        format(max(scrapes_clean_df$scrape_date), "%d-%m-%Y %H:%M:%S") ),
       fill = NULL, x = NULL, y = "Top 10 ranking") +
  guides(fill   = guide_legend(nrow = 4),
         colour = "none") +
  theme(legend.position = "bottom",
        legend.text = element_text (size = 8)) 

# Create format for saving interest word file. We replace spaces and 'or' statements.
interest_words_file <- str_replace_all(interest_words, " |\\|", "_")

# Save.
ggsave(plot = tile_gg,
       filename = paste0("visuals/interest_tile_",
                         interest_words_file,
                         ".png"),
       width = 20, height = 12, unit = "cm", dpi = 300)

# End.