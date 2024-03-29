# Packages.
library(readr)
library(lubridate)
library(forcats)
library(RColorBrewer)
library(ggplot2)

# Load in the scraped data.
scrapes_df <- read_csv("data/tt_subset.csv")

# Min and max.
min(scrapes_df$scrape_date)
max(scrapes_df$scrape_date)

# Create new time units.
scrapes_df <- scrapes_df %>% 
  mutate(date_round = round_date(scrape_date, "hour"),
         date_lub   = date(scrape_date),
         wday_lub   = wday(scrape_date))

# Check scrape counts per day.
scrapes_df %>%
  distinct(scrape_date, .keep_all = TRUE) %>% 
  group_by(date_lub) %>% 
  tally()

# Top read orders.
order_value_df <- scrapes_df %>% 
  group_by(order_var, article_title) %>% 
  tally() %>% 
  arrange(desc(n)) 

# Most read by hour of the week.
most_pop_df <- scrapes_df %>% 
  group_by(date_round) %>% 
  arrange(order_var) %>% 
  slice(1)

# Identify a specific story.
scrapes_df <- scrapes_df %>% 
  mutate(article_title_lower = str_to_lower(article_title),
         interest_flag  = if_else(condition = str_detect(article_title_lower, "hamburg airport"),
                                 true  = "yes",
                                 false = "no"),
         interest_label = if_else(condition = str_detect(article_title_lower, "hamburg airport"),
                                 true = article_title,
                                 false = "Other"))
         # interest_label = fct_relevel(interest_label, "Other"))

# Plot.
scrapes_df %>% 
  ggplot(data = .) +
  geom_point(mapping = aes(x = date_round, y = order_var, colour = interest_label)) +
  theme(legend.position = "bottom") 

# How many stories on the interest flag to we have?
n_stories_vec <- length(unique(scrapes_df$interest_label))-1

# Colour scheme define.
col_scheme <- c(brewer.pal(n = n_stories_vec, name = "Set2"), "grey95")

# Tile plot.
ggplot(data = scrapes_df) +
  geom_tile(mapping = aes(x = date_round, y = order_var, fill = interest_label), colour = "grey20") +
  scale_fill_manual(values = col_scheme) +
  scale_y_reverse(breaks = c(1:10)) +
  theme_bw() +
  labs(fill = NULL) +
  guides(fill = guide_legend(nrow = 3)) +
  theme(legend.position = "bottom")

# Save.
ggsave(filename = "cisuals/tile_interest.png", height = 6, width = 12)



# # Plot.
# ggplot(data = scrapes_df) +
#   geom_line (mapping = aes(x = date_round, y = order_var, colour = interest_label, linewidth= interest_flag)) +
#   geom_point(mapping = aes(x = date_round, y = order_var, colour = interest_label, size = interest_flag)) +
#   scale_colour_manual(values = col_scheme) +
#   scale_y_reverse(breaks = c(1:10)) +
#   scale_linewidth_manual(values = c(0.1, 3)) +
#   guides(size = "none",
#          colour = guide_legend(nrow = 2)) +
#   theme_bw() +
#   labs(colour = NULL) +
#   theme(legend.position = "bottom")