
# ==============================================================================
# === Alternative visuals ======================================================
# ==============================================================================

# Handling to create the flag.
# mutate(
#   interest_flag  = if_else(condition = str_detect(article_title_lower, interest_words),
#                          true  = "yes",
#                          false = "no")
# )

# # Plot.
# scrapes_df %>% 
#   ggplot(data = .) +
#   geom_point(mapping = aes(x = date_round, y = order_var, colour = interest_label)) +
#   theme(legend.position = "bottom") 

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