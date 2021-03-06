df_rolling_average_and_error <- 
  read.csv("Benchmarking/data/data-rolling-average-and-error.csv",
           stringsAsFactors = F, 
           sep=",", 
           encoding ="utf-8")

load("Processed/polls.RData")

# format forecasts
Polls %<>% filter(!str_detect(method ,".+count")) %>% 
  gather("partei", "shares", -method, -date) %>%
  dplyr::rename(datum = date, pct = shares)

Polls$shares <- as.numeric(Polls$pct)/100

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
startDatum <- "2016-01-01"
df_rolling_average_and_error <- filter(df_rolling_average_and_error, datum > startDatum)

# andere Namen für die Linien als das Standardlabel
get_label_value <- function (partei){
  index = match(partei, latest_values$partei)
  label = paste0(round(latest_values$ci_lower[index]*100, digits = 0), "-", round(latest_values$ci_higher[index]*100, digits = 0), "%",latest_values$hidden_chars[index] )
  label = as.character(label)
}

### Table #####################################################################

rolling.average.dates <- df_rolling_average_and_error %>%
  filter(datum %in% ymd(c("2016-12-10", "2017-3-18", "2017-3-22"))) %>%
  mutate(year = year(datum),
         shares = rolling_average,
         pct = shares*100) %>%
  group_by(year, partei) %>%
  summarize_all(mean) %>%
  ungroup() %>%
  select(-year, -sz_err, -rolling_average, -matches("ci_"))

rolling.average.dates$method = rep("sz.rolling.av", nrow(rolling.average.dates))

Polls <- rbind(Polls, rolling.average.dates)
Polls$pct <- round(as.numeric(Polls$pct), 1)
Polls$datum <- lubridate::month(Polls$datum, label = TRUE, abbr = FALSE)
PollsTable <- Polls %>% select(-shares) %>% spread(partei, pct)

a <- PollsTable %>% 
  filter(is.na(Other)) %>%
 select(-method, -datum, -Other)

PollsTable[is.na(PollsTable)] <- 100-rowSums(a) 

# rmse computation
partynames <- names(PollsTable[,-c(1,2)])
PollsTable$rmse <- rep(0, nrow(PollsTable)) 
#df <- PollsTable

# rmse computation
PollsTable <- rmse.func(PollsTable) 

# rename PollsTable methods to fit paper style
PollsTable %>%
  rename(Method = method, Date = datum, RMSE = rmse) %>%
  mutate(Method = str_replace(Method, "GAR.+", "GAR Census Data"),
         Method = str_replace(Method, "sz.rolling.av", "SZ Rolling Average"),
         Method = str_replace(Method, "GAV.uw.+", "Dalia Unweighted"),
         Method = str_replace(Method, "GAV.w.+", "GAV Exit Polls"),
         Method = as.factor(Method)
         ) %>%
  select(Method, Date, Union, SPD, FDP, Gruene, Linke, AfD, Other, RMSE) %>%
  arrange(desc(Date), desc(Method)) %>%
  xtable()


### Plot ######################################################################

# filter forecast of best method for March and December
bestMethod <- c("GAV.w.DMar.pct", "GAV.w.DDec.pct") 

ForecastBest <- PollsTable %>% filter(method %in% bestMethod) %>%
  mutate(datum = str_replace(datum, "Mar", "2017-03-20")) %>%
  mutate(datum = str_replace(datum, "Dec", "2016-12-10")) %>%
  select(-rmse, -method)

ForecastBest$datum <- ymd(ForecastBest$datum)

ForecastBest %<>% gather(partei, pct, -datum) %>%
  mutate(shares = pct/100) %>% filter(!partei == "Other")

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
          method = list(dl.trans(x = x + .1, cex = 1.5, fontfamily="Times"), "calc.boxes", "last.bumpup")) +
  geom_point(data = ForecastBest, 
             mapping = aes(x = datum, y = shares),
             color = farben[ForecastBest$partei], size = 2.8) +
  geom_text_repel(data = ForecastBest, 
                  mapping = aes(x = datum, y = shares, label = pct), 
                  size = 4,family = "Times", fontface = 'bold', color = farben[ForecastBest$partei],
                  box.padding = unit(0.3, "lines"),
                  point.padding = unit(0.4, "lines"))

basechart <- basechart + 
  scale_fill_manual(values = farben_ci[mlabels], labels = plabels) + 
  guides(fill = guide_legend(override.aes = list(alpha = 1), nrow = 1)) +
  scale_colour_manual(values = farben[mlabels], labels = NULL, breaks = NULL) +
  scale_y_continuous(labels = scales::percent, limits = c(0, NA))

article_chart <- basechart + sztheme_lines +
  scale_x_date(date_labels = "%b %y", limits = as.Date(c(startDatum, NA)), expand = c(0, 0))

article_chart <- ggplotGrob(article_chart)
article_chart$layout$clip[article_chart$layout$name == "panel"] <- "off"

ggsave(file="Benchmarking/data/assets/longterm-poll-article2.png", plot=article_chart, dpi = 144, units = "in", width = 10, height = 5)
# ggsave(file="data/assets/longterm-poll-hp.png", plot=article_chart, dpi = 144, units = "in", width = 7.78, height = 4.38)



