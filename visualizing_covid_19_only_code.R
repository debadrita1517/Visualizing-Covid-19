library(readr)
library(ggplot2)
library(dplyr)
confirmed_cases_worldwide <- read_csv("https://raw.githubusercontent.com/debadrita1517/Visualizing-Covid-19/main/confirmed_cases_worldwide.csv")
confirmed_cases_worldwide
ggplot(confirmed_cases_worldwide, aes(date, cum_cases)) +
  geom_line() +
  ylab("Cumulative confirmed cases")
confirmed_cases_china_vs_world <- read_csv("https://raw.githubusercontent.com/debadrita1517/Visualizing-Covid-19/main/confirmed_cases_china_vs_world.csv")
glimpse(confirmed_cases_china_vs_world)
plt_cum_confirmed_cases_china_vs_world <- ggplot(confirmed_cases_china_vs_world) +
  geom_line(aes(date, cum_cases, group = is_china, color = is_china)) +
  ylab("Cumulative confirmed cases")
plt_cum_confirmed_cases_china_vs_world
who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting\nchange"
) %>%
  mutate(date = as.Date(date))
plt_cum_confirmed_cases_china_vs_world +
  geom_vline(aes(xintercept = date), data = who_events, linetype = "dashed") +
  geom_text(aes(date, label = event), data = who_events, y = 1e5)
china_after_feb15 <- confirmed_cases_china_vs_world %>%
  filter(is_china == "China", date >= "2020-02-15")
ggplot(china_after_feb15, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases")
not_china <- confirmed_cases_china_vs_world %>%
  filter(is_china == "Not China")
plt_not_china_trend_lin <- ggplot(not_china, aes(date, cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Cumulative confirmed cases")
plt_not_china_trend_lin
plt_not_china_trend_lin + 
  scale_y_log10()
confirmed_cases_by_country <- read_csv("https://raw.githubusercontent.com/debadrita1517/Visualizing-Covid-19/main/confirmed_cases_by_country.csv")
glimpse(confirmed_cases_by_country)
top_countries_by_total_cases <- confirmed_cases_by_country %>%
  group_by(country) %>%
  summarize(total_cases = max(cum_cases)) %>%
  top_n(7, total_cases)
top_countries_by_total_cases
confirmed_cases_top7_outside_china <- read_csv("https://raw.githubusercontent.com/debadrita1517/Visualizing-Covid-19/main/confirmed_cases_top7_outside_china.csv")
glimpse(confirmed_cases_top7_outside_china)
ggplot(confirmed_cases_top7_outside_china, aes(date, cum_cases, color = country, group = country)) +
  geom_line() +
  ylab("Cumulative confirmed cases")
