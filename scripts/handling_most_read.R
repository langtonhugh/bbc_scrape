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
scrapes_list <- paste0("scrapes/", list.files("scrapes", pattern = "*.csv"))

# Load them all in (base-like).
scrapes_df_list <- pblapply(scrapes_list, read.csv)

# # Load them in and row bind (mostly tidyverse).
# scrapes_df <- map_dfr(scrapes_list, read.csv)

# Row bind and initial handling.
scrapes_df <- scrapes_df_list %>% 
  bind_rows() %>% 
  mutate(scrape_date_lub = ymd_hms(scrape_date)) %>% 
  as_tibble()

# Plot the date range.
scrapes_df %>% 
  distinct(scrape_date_lub) %>%
  ggplot() +
  geom_point(mapping = aes(x = scrape_date_lub, y = 1))

# Is every scrape a unique date?
length(unique(scrapes_df$scrape_date)) 

# Calculate time different between scrapes.
time_diff_df <- scrapes_df %>% 
  arrange(scrape_date_lub) %>% 
  distinct(scrape_date_lub) %>% 
  mutate(scrape_date_lagged = lag(scrape_date_lub),
         time_diff          = as.numeric(scrape_date_lub-scrape_date_lagged)/60) %>% 
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

# Now we define 7-days. Nice and round, but it helps with weekly
# analysis.
case_scrape_week_df <- case_scrape_df %>% 
  filter(scrape_date >= "2023-11-03" & scrape_date <= "2023-11-10")

# Did any of the URL scrapes actually work?
sum(is.na(case_scrape_week_df$url)) 
sum(is.na(case_scrape_week_df$url)) == nrow(case_scrape_week_df) # TRUE

# Resolve the change in the order variable.
case_scrape_clean_df <- case_scrape_week_df %>% 
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

# Check the combos.
combos_df <- case_scrape_clean_df %>% 
  distinct(week_numbr_lub, day_label_lub)

# Are there actually the same number of scrapes per hour?
# Pretty much. A small number have 11 due to time shifts.
hourly_scrapes_df <- case_scrape_clean_df %>% 
  group_by(week_numbr_lub) %>% 
  summarise(total_scaprs = n()/10)

# Identify an example topic.
matt_vec <- case_scrape_clean_df %>% 
  filter(str_detect(value_clean, "Israel|Gaza")) %>% 
  distinct(value_clean) %>% 
  pluck("value_clean")

# Flag these in our data.
case_scrape_clean_df <- case_scrape_clean_df %>% 
  mutate(matt_flag = ifelse(value %in% matt_vec, "perry_flag", "other"))

# Plot.
ggplot(data = case_scrape_clean_df) +
  geom_point(mapping = aes(x = week_numbr_lub, y = order, colour = matt_flag)) +
  geom_line(mapping = aes(x = week_numbr_lub, y = order, colour = matt_flag))



s# First, auto specify how many number ones we actually have.
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



