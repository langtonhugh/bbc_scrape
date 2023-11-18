# Packages.
library(readr)
library(openxlsx)
library(dplyr)
library(purrr)
library(tidyr)
library(forcats)
library(pbapply)
library(lubridate)
library(stringr)
library(ggplot2)

# List all the scraped files.
scrapes_list <- paste("scrapes/", list.files("scrapes", pattern = glob2rx("*.csv"),  recursive=TRUE), sep = "")

# Load them all in.
scrapes_df_list <- pblapply(scrapes_list, read.csv)

# Row bind them together.
scrapes_df <- bind_rows(scrapes_df_list)

# Make sure dates are dates.
scrapes_df <- scrapes_df %>% 
  mutate(scrape_date_lub = ymd_hms(scrape_date)) %>% 
  as_tibble()

# Plot the date range.
scrapes_df %>% 
  distinct(scrape_date_lub) %>%
  ggplot(data = .) +
  geom_point(mapping = aes(x = scrape_date_lub, y = 1))

# Calculate time different between scrapes.
time_diff_df <- scrapes_df %>% 
  arrange(scrape_date_lub) %>% 
  distinct(scrape_date_lub) %>% 
  mutate(scrape_date_lagged = lag(scrape_date_lub),
         time_diff = as.numeric(scrape_date_lub-scrape_date_lagged)/60) %>% 
  drop_na(scrape_date_lagged)

# Calculate how long continious scrapes have lasted for. We ask for 5 minutes
# lag but we set a more relaxed boundary of 8 minutes to flag it as 'failed'.
scrape_lengths_df <- time_diff_df %>%
  mutate(cont_scr_group    = cumsum(time_diff > 8),
         cont_scr_group_na = ifelse(time_diff > 8, NA, cont_scr_group)) %>% 
  group_by(cont_scr_group_na) %>% 
  summarize(first_scrape        = min(scrape_date_lagged),
            last_scrape         = max(scrape_date_lagged),
            scrape_length_hours = difftime(last_scrape, first_scrape, units = "days")) %>% 
  drop_na(cont_scr_group_na) %>% 
  arrange(desc(scrape_length_hours))

# Id the dates.
start_date <- date(scrape_lengths_df$first_scrape[[1]])
end_date   <- date(scrape_lengths_df$last_scrape[[1]])

# Filter our main data between the dates of our longest scrape.
case_scrape_df <- scrapes_df %>% 
  filter(scrape_date >= start_date & scrape_date <= end_date) %>% 
  mutate()
  
# Check the timespan.
case_scrape_df %>% 
  distinct(scrape_date_lub) %>%
  ggplot(data = .) +
  geom_point(mapping = aes(x = scrape_date_lub, y = 1)) +
  scale_x_datetime(date_breaks = '1 day') +
  theme(axis.text.x = element_text(angle = 90) )

# Did any of the URL scrapes actually work? No.
sum(is.na(case_scrape_df$url)) 
sum(is.na(case_scrape_df$url)) == nrow(case_scrape_df) # TRUE

# Resolve the change in the order variable.
case_scrape_clean_df <- case_scrape_df %>% 
  mutate(order = ifelse(is.na(order), order_var, order)) %>% 
  select(order, value, scrape_date_lub)

# Fixing non-Latin format issues (introduced during scrape, somehow).
case_scrape_clean_df <- case_scrape_clean_df %>% 
  mutate(value_clean = iconv(value, "latin1", "ASCII", sub=""))

# Each scrape is 5 minutes (we assume no change inbetween), so each record
# is 5 minutes in that position.

# Top read orders.
order_value_df <- case_scrape_clean_df %>% 
  group_by(order, value_clean) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(est_minutes = 5*n) 

# Just top 10 in general.
top10_value_df <- case_scrape_clean_df %>% 
  group_by(value_clean) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  mutate(est_minutes = 5*n) %>% 
  slice(1:10)

# Create the different time scales.
case_scrape_clean_df <- case_scrape_clean_df %>% 
  mutate(day_date_lub  = lubridate::date(scrape_date_lub),
         day_label_lub  = wday(scrape_date_lub,
                               label = TRUE,
                               abbr = FALSE),
         day_numbr_lub  = wday(scrape_date_lub, week_start = "Monday"),
         hour_lub       = hour(scrape_date_lub),
         week_numbr_lub = (day_numbr_lub-1)*24+(hour_lub))

# Get the most popular per hour of the week.
weekly_hour_pop_df <- case_scrape_clean_df %>% 
  group_by(week_numbr_lub, value_clean) %>% 
  tally() 
  



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



