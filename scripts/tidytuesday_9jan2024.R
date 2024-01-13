# Packages.
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(forcats)
library(RColorBrewer)
library(ggplot2)

# Load in the scraped data.
scrapes_df <- read_csv("data/tt_subset.csv")

# Min and max.
min(scrapes_df$scrape_date)
max(scrapes_df$scrape_date)

# How many scrapes?
length(unique(scrapes_df$scrape_date))

# Top read orders.
order_value_df <- scrapes_df %>% 
  group_by(order_var, article_title) %>% 
  tally() %>% 
  arrange(desc(n)) 

# What's your keyword(s)?
interest_words <- "hamburg airport"

# Identify a specific story.
scrapes_df <- scrapes_df %>%
  mutate(article_title_lower = str_to_lower(article_title),
         interest_label = if_else(condition = str_detect(article_title_lower, interest_words),
                                  true      = article_title,
                                  false     = "Other"))

# What have we got?
count(scrapes_df, interest_label)

# Plot.
ggplot(data = scrapes_df) +
  geom_tile(mapping = aes(x = scrape_date, y = order_var, fill = interest_label))

# Reorder for 'other'.
scrapes_df <- scrapes_df %>% 
  mutate(interest_label = fct_relevel(interest_label, "Other"))

# How many stories on the interest flag to we have?
n_stories_vec <- length(unique(scrapes_df$interest_label))-1

# Colour scheme define. Note that 'other' is now always trhe first level.
col_scheme <- c("grey95", brewer.pal(n = n_stories_vec, name = "Set2"))

# Tile plot.
tile_gg <- ggplot(data = scrapes_df) +
  geom_tile(mapping = aes(x = scrape_date, y = order_var, fill = interest_label)) +
  scale_fill_manual(values = col_scheme) +
  scale_y_reverse(breaks = c(1:10)) +
  theme_bw() +
  labs(title    = paste("BBC Top 10 'most read' involving the term:", interest_words),
       subtitle = paste("Time range between", min(scrapes_df$scrape_date), "and", max(scrapes_df$scrape_date) ),
       fill = NULL, x = NULL, y = "Top 10 ranking") +
  guides(fill = guide_legend(nrow = 4)) +
  theme(legend.position = "bottom") 

# Save.
ggsave(plot = tile_gg, filename = "visuals/interest_tile.png", width = 20, height = 12, unit = "cm")

# Test at creating themes.
scrapes_df <- scrapes_df %>% 
  mutate(
    # Main themes.
    theme = ifelse(str_detect(article_title_lower, "rafah|israel|gaza|hamas|palestin*"), "Israel-Gaza"         , " "),
    theme = ifelse(str_detect(article_title_lower, "ukraine|zalenski|russia|russian|ukrainian"), "Ukraine-Russia", theme),
    theme = ifelse(str_detect(article_title_lower, "europe|france|french|germany|sunak|macron") &
                          !str_detect(article_title_lower,
                                      "rafah|israel|gaza|hamas|palestin*"), "European politics", theme),
    theme = ifelse(str_detect(article_title_lower, "trump|biden|us election") &
                          !str_detect(article_title_lower,
                                      "rafah|israel|gaza|hamas|palestin*"), "US politics", theme),
    theme = ifelse(str_detect(article_title_lower, "south korea|chinese|china|taiwan|myanmar|philippines"), "Asia news", theme),
    # One-off others.
    # theme = ifelse(str_detect(article_title_lower, "mushroom"       ), "Mushroom murderer", theme),
    theme = ifelse(str_detect(article_title_lower, "matthew perry"   ), "Matthew Perry"     , theme),
    theme = ifelse(str_detect(article_title_lower, "hamburg airport"), "Hamburg airport hostage"     , theme),
    theme = ifelse(str_detect(article_title_lower, "crypto king|bankman-fried"), "Sam Bankman-Fried", theme),
    # Re-order factors.
    theme = fct_relevel(theme, " ")
    )

# What's the breakdown?
count(scrapes_df, theme)

# Check them.
themes_checks_df <- scrapes_df %>% 
  distinct(article_title_lower, theme)


# Plot.
# How many stories on the interest flag to we have?
n_themes_vec <- length(unique(scrapes_df$theme))-1

# Colour scheme define. Note that 'other' is now always trhe first level.
col_theme_scheme <- c("transparent", brewer.pal(n = n_themes_vec, name = "Set3"))

# Tile plot.
ggplot(data = scrapes_df) +
  geom_tile(mapping = aes(x = scrape_date, y = order_var, fill = theme)) +
  scale_fill_manual(values = col_theme_scheme) +
  scale_y_reverse(breaks = c(1:10)) +
  theme_bw() +
  labs(title    = paste("BBC Top 10 'most read' themes"),
       subtitle = paste("Time range between", min(scrapes_df$scrape_date), "and", max(scrapes_df$scrape_date) ),
       fill = NULL, x = NULL, y = "Top 10 ranking") +
  guides(fill = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom") 

