df_rolling_average_and_error <- read.csv("data/data-rolling-average-and-error.csv", stringsAsFactors = F, sep=",", encoding ="utf-8")

load("C:/Users/Moritz/Desktop/ElectionForecasting/Processed/poll.RData")

# format forecasts
w.shares %<>% 
  select(vote_nextelection_de, shares) %>% 
  rename(partei = vote_nextelection_de) %>%
  arrange(desc(shares))
  
w.shares$shares <- w.shares$shares/100
w.shares$datum <- ymd(rep("2017-03-20", nrow(w.shares)))
w.shares$pct <- round(w.shares$shares*100, 1)

# set formats
df_rolling_average_and_error$partei %<>%  
  str_replace("CDU/CSU", "Union") %>%
  str_replace("Grüne", "Gruene")

df_rolling_average_and_error$datum <- as.Date(df_rolling_average_and_error$datum, "%Y-%m-%d")
df_rolling_average_and_error$ci_lower <- as.numeric(df_rolling_average_and_error$ci_lower)
df_rolling_average_and_error$ci_higher <- as.numeric(df_rolling_average_and_error$ci_higher)

df_rolling_average_and_error_party <- unique(df_rolling_average_and_error$partei)

latest_values <- arrange(df_rolling_average_and_error, desc(datum)) %>% filter(datum == datum[1])
hidden_chars <- c("\U200C","\u200D","\u200E","\u200F","\U200C","\u200D")
latest_values <- arrange(latest_values, desc(rolling_average))
latest_values <- cbind(latest_values, hidden_chars)
startDatum <- "2015-06-01"
df_rolling_average_and_error <- filter(df_rolling_average_and_error, datum > startDatum)

# andere Namen für die Linien als das Standardlabel
get_label_value <- function (partei){
  index = match(partei, latest_values$partei)
  label = paste0(round(latest_values$ci_lower[index]*100, digits = 0), "-", round(latest_values$ci_higher[index]*100, digits = 0), "%",latest_values$hidden_chars[index] )
  label = as.character(label)
}



### Plot ######################################################################

# Diagramm zusammen bauen
basechart <- ggplot() +
  geom_ribbon(data = df_rolling_average_and_error, 
              mapping = aes(x = datum, ymin = ci_lower, ymax = ci_higher, fill = partei, group = partei, color = partei), alpha = .6, size = .1) +
  geom_line(data = df_rolling_average_and_error,
            mapping = aes(x = datum, y = rolling_average, color = partei), size = .2) +
  geom_dl(data = df_rolling_average_and_error,
          mapping = aes(x = datum, y = rolling_average, 
          label = as.character(get_label_value(partei))), 
          color = farben[df_rolling_average_and_error$partei], 
          method = list(dl.trans(x = x + .1, cex = 1.5, fontfamily="SZoSansCond-Light"),"calc.boxes", "last.bumpup")) +
  geom_point(data = w.shares, 
             mapping = aes(x = datum, y = shares),
             color = farben[w.shares$partei], size = 3) +
  geom_text_repel(data = w.shares, 
                  mapping = aes(x = datum, y = shares, label = pct), 
                  size = 4, fontface = 'bold', color = farben[w.shares$partei],
                  box.padding = unit(0.35, "lines"),
                  point.padding = unit(0.5, "lines"))
  # geom_text(data = w.shares,
  #           aes(x = datum, y = shares, label = pct))

basechart <- basechart + 
  scale_fill_manual(values = farben_ci[mlabels], labels = plabels) + 
  guides(fill = guide_legend(override.aes = list(alpha = 1, fill = farben), nrow = 1)) +
  scale_colour_manual(values = farben[mlabels], labels = NULL, breaks = NULL) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA))

article_chart <- basechart + sztheme_lines +
  scale_x_date(date_labels = "%B %y", limits = as.Date(c(startDatum, NA)), expand = c(0, 0))

article_chart <- ggplotGrob(article_chart)
article_chart$layout$clip[article_chart$layout$name == "panel"] <- "off"

ggsave(file="data/assets/longterm-poll-article2.png", plot=article_chart, dpi = 144, units = "in", width = 8.89, height = 5)
# ggsave(file="data/assets/longterm-poll-hp.png", plot=article_chart, dpi = 144, units = "in", width = 7.78, height = 4.38)


### Table #####################################################################

