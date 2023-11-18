# Packages.
library(readr)
library(openxlsx)
library(dplyr)
library(purrr)
library(forcats)
library(pbapply)
library(lubridate)
library(ggplot2)

# List all the scraped files.
scrapes_list <- paste("scrapes/", list.files("scrapes", pattern = glob2rx("*.csv"),  recursive=TRUE), sep = "")

# Load them all in.
scrapes_df_list <- pblapply(scrapes_list, read_csv)

# Row bind them together.
scrapes_df <- bind_rows(scrapes_df_list)

# Save.
# write_csv(x = scrapes_df, "data/scrapes_26oct2023.csv")

# Resolve the change in the order variable.
scrapes_clean_df <- scrapes_df %>% 
  mutate(order = ifelse(is.na(order), order_var, order)) %>% 
  select(-order_var)

# Frequency table of most read.
freq_df <- scrapes_clean_df %>% 
  group_by(order, value) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(minutes_in_position = 5*n)

# What's the timespan?
scrapes_clean_df %>% 
  distinct(scrape_date) %>%
  ggplot(data = .) +
  geom_point(mapping = aes(x = scrape_date, y = 1)) +
  scale_x_datetime(date_breaks = '1 day') +
  theme(axis.text.x = element_text(angle = 90) )

# Create the different time scales.
scrapes_clean_df <- scrapes_clean_df %>% 
  mutate(day_date_lub = lubridate::date(scrape_date),
         day_lub      = day(scrape_date),
         hour_lub     = hour(scrape_date))

# Check the scrape completion.
scrapes_date_df <- scrapes_clean_df %>%
  group_by(day_date_lub) %>% 
  tally() %>% 
  arrange(day_date_lub)

# Subset out a few complete days.
example_days_df <- scrapes_clean_df %>% 
  filter(day_date_lub %in% c("2023-10-18",
                             # "2023-10-19",
                             # "2023-10-20",
                             # "2023-10-21"
                             ))

# Frequency plot just for this day, and calculate minutes at each rank.
story_freq_df <- example_days_df %>% 
  group_by(order, value) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(minutes_in_position = 5*n)

# Plot.
ggplot(data = example_days_df) +
  geom_point(mapping = aes(x = scrape_date, y = order, colour = value)) +
  geom_line (mapping = aes(x = scrape_date, y = order, colour = value)) +
  theme(legend.position = "none")

# Identify the number ones for coding.
number_ones_df <- example_days_df %>% 
  filter(order == 1) %>% 
  distinct(value)

# Save them for giving broad cats in Excel.
# write_csv(x = number_ones_df, file = "data/number_ones_scrapes_26oct2023.csv")

# Load pack in after thematic coding.
number_ones_themed_df <- read.xlsx("data/number_ones_scrapes_26oct2023.xlsx")

# Identify only those stories that reached number 1.
number_ones_vec <- example_days_df %>% 
  filter(order == 1) %>% 
  distinct(value) %>% 
  pluck("value")

# Flag these in our data.
example_days_df <- example_days_df %>% 
  mutate(number_ones         = if_else(value %in% number_ones_vec, value, "Never hit number 1"),
         numer_ones_fac      = fct_relevel(number_ones, "Never hit number 1"),
         numer_ones_flag     = if_else(numer_ones_fac != "Never hit number 1", "yes", "no"),
         numer_ones_flag_fac = as.factor(numer_ones_flag),
         order_fac = fct_rev(as.factor(order)) ) %>% 
  left_join(number_ones_themed_df) %>% 
  mutate(theme = ifelse(is.na(theme), "Never hit number 1", theme),
         theme = fct_relevel(theme, "Never hit number 1"))

# First, auto specify how many number ones we actually have.
n_number_ones_vec <- length(unique(example_days_df$numer_ones_fac))-1

# Colour scheme define.
col_scheme <- c("grey90", viridis::viridis(n_number_ones_vec))

# Plot.
ggplot(data = example_days_df) +
  # geom_point(mapping = aes(x = scrape_date, y = order_fac, colour = numer_ones_fac, group = numer_ones_fac)) +
  geom_line (mapping = aes(x = scrape_date, y = order_fac, colour = numer_ones_fac, group = numer_ones_fac,
                           linewidth = numer_ones_flag_fac)) +
  scale_colour_manual(values = col_scheme) +
  scale_discrete_manual("linewidth", values = c(0.1, 3), guide = "none") +
  labs(title = "How do 'most read' article rise and fall?", y = NULL, x = NULL, colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom") 



