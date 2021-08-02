library(tidyverse)
library(dplyr)
library(forcats)
library(lubridate)
library(fable)
library(tsibble)
library(ggdark)

# theme setup
black <- "#292c35"
red <- "#ee1d52"
yellow <- "#f2d803"
blue <- "#63a3be"
palette <- c(blue, red, yellow, black)
xvzf_base <- ggdark::dark_theme_gray()
xvzf_theme <- list(xvzf_base, scale_color_manual(values = palette),scale_fill_manual(values = palette))

# data prep
df_raw <- readr::read_csv("https://storage.googleapis.com/data_xvzf/m5_state_sales.csv")
df_ts <- df_raw %>% as_tsibble(index=date,key=state_id)
df_ts_train <- df_ts %>% filter(date <= (max(date) - weeks(3)))
df_ts_test <- df_ts %>% filter(date > (max(date) - weeks(3))) %>% as_tibble()
DAYS_OUT <- length(unique(df_ts_test$date))

# model
df_fcast <- df_ts_train %>% 
  model(
    snaivef = SNAIVE(sales ~ lag("week")),
    meanf = MEAN(sales),
    naivef = NAIVE(sales),
    ) %>%
generate(h = DAYS_OUT, times = 200, bootstrap = TRUE) %>%
as_tibble()

# plotting
p <- df_ts_train %>% 
  as_tibble() %>%
  filter(date > "2016-03-01") %>%
  ggplot2::ggplot(aes(x = date)) +
  ggplot2::geom_line(aes(y = sales)) +
  ggplot2::geom_line(aes(y = .sim, group = .rep, color = .model), alpha = 0.1, data = df_fcast) +
  ggplot2::geom_line(aes(y = sales), data = df_ts_test) +
  ggplot2::facet_wrap(~state_id+.model) +
  xvzf_theme
p

ggsave(p, filename = "states_and_models.jpg", width = 14, height = 6)

df_snaive <- df_fcast %>% filter(.model == "snaivef")
p <- df_ts_train %>% 
  as_tibble() %>%
  filter(date > "2016-03-01") %>%
  ggplot2::ggplot(aes(x = date)) +
  ggplot2::geom_line(aes(y = sales)) +
  ggplot2::geom_line(aes(y = .sim, group = .rep), color = palette[3], alpha = 0.1, data = df_snaive) +
  ggplot2::geom_line(aes(y = sales), data = df_ts_test) +
  ggplot2::facet_wrap(~state_id+.model, ncol=1) +
  xvzf_theme
p
ggsave(p, filename = "snaive.jpg", width = 14, height = 6)

# error metric calculation - credit to tidyverts tools
# https://github.com/tidyverts/fabletools/blob/25c1cd89f0dcde5fd906c4996d4392885e51b633/R/accuracy.R#L265-L269

crps <- function(y,vec_forecast){
        x <- sort(vec_forecast)
        m <- length(x)
        (2/m) * mean((x-y)*(m*(y<x) - seq_len(m) + 0.5))
}

df_af <- df_ts_test %>% dplyr::left_join(df_fcast, by = c("date", "state_id"))
df_crps <- df_af %>% group_by(date,state_id,.model) %>% summarize(score = crps(sales,.sim)) 
df_avg_crps <- df_crps %>% group_by(state_id,.model) %>% summarize(mean_crps = mean(score)) %>% arrange(state_id,mean_crps)
df_avg_crps %>% readr::write_csv("df_avg_crps.csv")

