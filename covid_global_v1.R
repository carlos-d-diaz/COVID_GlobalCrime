#' ---
#' title: "The Heterogeneous Effects of COVID-19 Lockdowns on Crime across the World"
#' subtitle: "MC results with US cities excluded after May 25th"
#' author: 
#'   - Trajtenberg et al. (2024)
#' 
#' date: "`r format(Sys.time(), 'This version: %B %d, %Y')`"
#' output:
#'   pdf_document:
#'     latex_engine: xelatex
#'     keep_tex: false
#' fontsize: 11pt
#' geometry: margin = 2.5cm
#' 
#' ---

#+ echo=FALSE
# rmarkdown::render("covid_global_v1.R")


#+ echo=FALSE
# preliminaries

# clear workspace
rm(list = ls(all.names = TRUE))

# set file path
filepath <- '/Users/sfossati/Dropbox/Research/COVID Global/02_Analysis/REPLICATION FILES'
setwd(filepath)

# packages
library(pacman)
p_load(
  tidyverse, lubridate, cowplot, ggforce, RColorBrewer, kableExtra,
  fixest, modelsummary, panelView, fect, tsibble, fable
)

# load functions
source("covid_global_functions_v1.r")

# no scientific numbers
options(scipen=999, digits=3)

# plot theme
theme_set(theme_light())
theme_update(
  plot.background = element_rect(fill = "transparent", color = NA),
  legend.background = element_rect(fill = "transparent", color = NA)
)

# grey, blue, orange, green, yellow
my_colors <- c("#606060", "#3C78B0", "#D55E00", "#64B4C2", "#E69F00")

# settings
set.seed(1234)


#+ echo=FALSE, cache=TRUE, include = FALSE, message = FALSE
# load data
# crime data
crime_data <-read_csv("data/new_database.csv", col_names=TRUE)

# fix Boston NAs coded as 0s
crime_data_1 <- crime_data %>% 
  filter(
    City == "Boston", 
    Date > "2019-09-28" & Date < "2020-01-01"
  ) %>%
  mutate(
    Assault = NA, 
    Burglary = NA, 
    Robbery = NA, 
    Theft = NA, 
    AutoTheft = NA, 
    Homicide = NA, 
    DomViolence = NA, 
    Rape = NA
  )
# join and rename
crime_data_2 <- 
  rbind(
    crime_data_1, 
    anti_join(
      crime_data, 
      crime_data_1, 
      by = c("City", "Date")
    )
  ) %>% 
  rename(Vehicle = AutoTheft) %>% 
  arrange(City, Date)

# get cities, countries, fix Guatemala name
cities <- crime_data_2 %>% 
  filter(Date == "2018-01-01") %>% 
  mutate(
    country_code = ifelse(Country == "Guatemala", "GTM", as.character(Country))
  ) %>% 
  select(city = City, country_code)

# number of cities
n_cities <- nrow(cities)


####################
# load other data

# Oxford stringency data
stringency_data <-read_csv("data/OxCGRT_latest.csv", col_names=TRUE)

# Google mobility data
mobility_data <-read_csv("data/Global_Mobility_Report.csv", col_names=TRUE)

# covariates
temperature <-read_csv("data/temperature.csv", col_names=TRUE)
rainfall <-read_csv("data/rainfall.csv", col_names=TRUE)


####################
# stay-at-home indexes
stay_at_home_w <- get_stay_at_home_indexes(stringency_data, cities, weekly=T)
stay_at_home_m <- get_stay_at_home_indexes(stringency_data, cities, weekly=F)


####################
# mobility indexes
mobility_data_w <- get_mobility_indexes(mobility_data, cities, stay_at_home_w)


####################
# mobility regressions
# residential
fit1.1 <- feols(
  value ~ treat_0 + treat_2 | city, 
  data = filter(mobility_data_w, index == "residential")
)
# retail
fit1.2 <- feols(
  value ~ treat_0 + treat_2 | city, 
  data = filter(mobility_data_w, index == "retail")
)
# transit
fit1.3 <- feols(
  value ~ treat_0 + treat_2 | city, 
  data = filter(mobility_data_w, index == "transit")
)
# workplace
fit1.4 <- feols(
  value ~ treat_0 + treat_2 | city, 
  data = filter(mobility_data_w, index == "workplace")
)


####################
# set data frequency
# TRUE for weekly, FALSE for monthly
# weekly = TRUE
weekly = FALSE
# data and periods for plots
if (weekly == T) {
  stay_at_home <- stay_at_home_w
  tmp_index <- c(-39,10)
} else {
  stay_at_home <- stay_at_home_m
  tmp_index <- c(-11,4)
}


####################
# get crime data
crimes_all <- c("Assault","Burglary","Robbery","Theft","Vehicle","Homicide")
# 
data_all <- 
  map(
    crimes_all, 
    ~ get_crime_indexes(
      as.data.frame(crime_data_2), 
      .x, 
      stay_at_home, 
      weekly, 
      temperature, 
      rainfall
    )
  )
# list names
names(data_all) <- crimes_all

# remove cities/crime with few reports
# remove US cities after May 25, 2020

# series not available or removed due to very small number of crimes
to_be_removed <- list(
  # removed: Boston, Buenos Aires, Cali, Ljubljana, Muzaffarpur, Santiago, Vancouver
  Assault <- c(5,7,8,18,27,36,44),
  
  # removed: Buenos Aires, Cali, Fortaleza, Guatemala City, Guayaquil, 
  # removed: Hannover, Helsinki, Lima, Montevideo, Rio de Janeiro, 
  # removed: San Salvador, Sao Paulo, Seoul, Tallinn
  Burglary <- c(7,8,11,12,13,14,15,17,26,31,35,37,39,41),
  
  # removed: Cali, Hannover, Vancouver
  Robbery <- c(8,14,44),
  
  # removed: Cali, Hannover, Lima, Zurich
  Theft <- c(8,14,17,45),
  
  # removed: Buenos Aires, Cali, Fortaleza, Guayaquil, Hannover, Kansas City,  
  # removed: Lima, Montevideo, Muzaffarpur, Seoul, Tallinn, Zurich
  Vehicle <- c(7,8,11,13,14,16,17,26,27,39,41,45),
  
  # removed: Auckland, Austin, Barcelona, Brisbane, Dallas, Hannover,
  # removed: Helsinki, Kansas City, Lima, Ljubljana, Malmo, Sacramento,
  # removed: Saint Louis, San Francisco, Seattle, Stockholm, Tallinn, 
  # removed: Tel Aviv, Vancouver, Zurich
  Homicide <- c(2,3,4,6,10,14,15,16,17,18,22,32,33,34,38,40,41,42,44,45)
  
)
names(to_be_removed)<- crimes_all


####################
# remove cities/crime with few reports
data_all_with_us <- 
  data_all %>% 
  map2(to_be_removed, ~ filter(.x, city_id %in% .y)) %>%
  map(~ mutate(.x, y = NA, y_indx = NA)) %>%
  map2(
    data_all, 
    ~ rbind(.x, anti_join(.y, .x, by = c("city_id", "date")))
  ) %>%
  map(~ mutate(.x, treat_2_plot = ifelse(is.na(y), NA, treat_2))) %>% 
  map(~ arrange(.x, city_id, date))


####################
# remove US cities after May 25, 2020
data_all_without_us <- 
  data_all_with_us %>% 
  map(~ filter(.x, country == "USA", date >= as.Date("2020-05-01"))) %>%
  map(~ mutate(.x, y = NA, y_indx = NA)) %>%
  map2(
    data_all_with_us, 
    ~ rbind(.x, anti_join(.y, .x, by = c("city_id", "date")))
  ) %>%
  map(~ mutate(.x, treat_2_plot = ifelse(is.na(y), NA, treat_2))) %>% 
  map(~ arrange(.x, city_id, date))

# data for models below
data_for_models <- data_all_without_us


####################
# event studies
tmp_dates <- filter(stay_at_home, city == "Amsterdam")[,"date"]

# post dummy
fit_event_1 <- map(data_for_models, ~ event_est_1(.x))

# table 
models.2 <- 
  list(
    "Assault"   = fit_event_1$Assault, 
    "Burglary"  = fit_event_1$Burglary, 
    "Robbery"   = fit_event_1$Robbery, 
    "Theft"     = fit_event_1$Theft, 
    "Vehicle"   = fit_event_1$Vehicle, 
    "Homicide"  = fit_event_1$Homicide
  )

# 
cm <- c('treat_0' = 'Treatment 1', 'temp' = 'Temperature', 'rain' = 'Rain')
gm <- 'BIC|AIC|R2 Within|R2 Pseudo|Log.Lik.|FE:city'
# 
table_post_1 <- 
  modelsummary(
    models.2,
    statistic = c("{std.error}", "{conf.low}", "{conf.high}", "{p.value}"),
    coef_map = cm,
    gof_omit = gm,
    fmt = '%.2f', 
    booktabs = TRUE, 
    title = 'Estimated effects March/2020 - December/2020.'
  )

# 2020 monthly dummies
fit_event_2 <- map(data_for_models, ~ event_est_2(.x))

# clean event study estimates
event_est <- fit_event_2 %>% 
  map(~ clean_event_est(.x, tmp_dates)) %>% 
  map2(names(.), ~ add_column(.x, crime = rep(.y, nrow(.x)))) %>% 
  bind_rows() %>%
  as_tibble()


####################
# estimate ATT (MC)
fit_mc <- 
  map(
    data_for_models, 
    ~ fit_model_mc(.x, y_indx ~ D + temp + rain)
  )

# nice summary table
table_fit_mc <- get_estimates_table(fit_mc, placebo=FALSE)

# std errors of monthly estimates
j_estimates_mc <- data_for_models %>% 
  map(~ leave_one_out_est(.x, n_cities, "mc"))

# clean monthly estimates
monthly_estimates_mc <- j_estimates_mc %>% 
  map(~ clean_monthly_estimates(.x)) %>% 
  map2(names(.), ~ add_column(.x, crime = rep(.y, nrow(.x)))) %>% 
  bind_rows() %>%
  as_tibble()

# clean relative estimates
rel_effect_mc <- clean_relative_estimates(fit_mc, tmp_index)


####################
# estimate placebo effect: 4 periods
placebo_mc_4 <- 
  pmap(
    list(data_for_models, fit_mc),
    ~ fit_model_mc_p(.x, y_indx ~ D + temp + rain, .y$lambda.cv, c(-3,0))
  )

# nice summary table
table_placebo_mc_4 <- get_estimates_table(placebo_mc_4, placebo=TRUE)

# clean results for plots
effect_placebo_mc_4 <- clean_relative_estimates(placebo_mc_4, tmp_index)



#' \newpage
#' # Main results
#' ## Crime and stay-at-home indexes

#+ r time_series_plot_index, echo = FALSE, warning = FALSE, message = FALSE, fig.height = 4.8, fig.cap = "Crime indexes (monthly). "
# time series plots, indexes
# long data set
data_all_3 <- data_for_models %>% 
  map2(names(.), ~ add_column(.x, crime = rep(.y, nrow(.x)))) %>% 
  bind_rows() 
#
time_series_plot_index_2 <- time_series_plot_2(data_all_3)
time_series_plot_index_2

#' \clearpage
#' \newpage

#+ r plot_stay_at_home_index, echo = FALSE, warning = FALSE, message = FALSE, fig.height = 6.8, fig.cap = "Stay-at-home index (weekly). "
# plot stay_at_home_index
plot_stay_at_home_index <- 
  ggplot(
    stay_at_home_w, 
    aes(x = date, y = city, fill = as.factor(value))
  ) +
  geom_tile(color = "grey40", alpha = .95) +
  scale_fill_manual(
    values = c("grey60","#fecc5c","#fd8d3c","#fc4e2a"), 
    na.value = "grey90"
  ) +
  coord_fixed() +
  scale_y_discrete(name = "", limits = rev) + 
  scale_x_date(name = "", date_breaks = "1 year", date_labels = "%Y") +
  coord_cartesian(expand = FALSE) +
  theme_light(base_size = 10) +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    panel.background = element_blank()
  ) 
plot_stay_at_home_index


#' \clearpage
#' \newpage
#' ## Impact of COVID-19 on crime

#+ r table_post_1, echo = FALSE, warning = FALSE, message = FALSE
# table 1
table_post_1

#' \clearpage
#' \newpage

#+ r event_plot, echo = FALSE, warning = FALSE, message = FALSE, fig.height = 4.8, fig.cap = "Average post effects, by month.  "
# plot
event_plot <- event_est_plot(event_est)
event_plot


#' \clearpage
#' \newpage
#' ## Impact of strict lockdowns on crime

#+ r plot_treat_2, echo = FALSE, warning = FALSE, message = FALSE, fig.height = 6.8, fig.cap = "Stay-at-home index (weekly): require not leaving house with exceptions for daily exercise, grocery shopping, and 'essential' trips. "
# plot treat_2
plot_treat_2 <- panel_plot(stay_at_home_w)
plot_treat_2


#' \clearpage
#' \newpage
#' ### MC results

#+ r monthly_estimates_plot_mc, echo = FALSE, warning = FALSE, message = FALSE, fig.height = 5.4, fig.cap = "Average post effects for each group of cities, by month.  "
# plot effects
monthly_estimates_plot_mc <- monthly_est_plot(monthly_estimates_mc)
monthly_estimates_plot_mc

#' \clearpage
#' \newpage

#+ r monthly_treatment_plot_mc, echo = FALSE, warning = FALSE, message = FALSE, fig.height = 4.8, fig.cap = "Average treatment effects, by month.  "
# plot treatment effects
monthly_treatment_plot_mc <- monthly_trt_plot(monthly_estimates_mc)
monthly_treatment_plot_mc

#' \clearpage
#' \newpage

#+ r rel_effect_plot_mc, echo = FALSE, warning = FALSE, message = FALSE, fig.height = 4.8, fig.cap = "Average treatment effects relative to beginning of lockdown.  "
# plot results
rel_effect_plot_mc <- relative_effect_plot(rel_effect_mc, tmp_index, weekly)
rel_effect_plot_mc

#' \clearpage
#' \newpage

#+ r table_2_mc, echo = FALSE, warning = FALSE, message = FALSE
# ATT effect
table_2_mc <- kbl(table_fit_mc, booktabs=TRUE, digits=2) 
table_2_mc


#' \clearpage
#' \newpage
#' ### MC placebo tests

#+ r table_3_mc, echo = FALSE, warning = FALSE, message = FALSE
# placebo test
table_3_mc <- kbl(table_placebo_mc_4, booktabs=TRUE, digits=2) 
table_3_mc

#' \clearpage
#' \newpage

#+ r placebo_plot_mc, echo = FALSE, warning = FALSE, message = FALSE, fig.height = 4.8, fig.cap = "Placebo plot.  "
# plot placebo tests
placebo_mc_4_plot <- 
  placebo_plot(
    effect_placebo_mc_4,
    tmp_index,
    plac = 4,
    weekly
  )
# plot
placebo_mc_4_plot


#' \clearpage
#' \newpage

#' # Additional results
#' ## Mobility indexes

#+ r plot_mobility_1, echo = FALSE, warning = FALSE, message = FALSE, fig.height = 4.8, fig.cap = "Google mobility indexes (weekly). "
plot_mobility_1 <- 
  ggplot(
    mobility_data_w, 
    aes(x = date, y = value, color = city)
  ) + 
  scale_color_grey(start = 0.4, end = 0.6) +
  geom_line(size = .5) + 
  scale_y_continuous(
    name = "% change",
    breaks = c(50,0,-50,-100)
  ) + 
  expand_limits(y = 50) + 
  scale_x_date(
    name = "year 2020", 
    limits = as.Date(c("2020-01-01","2020-12-31")),
    date_breaks = "3 month",
    date_labels = "%b"
  ) +
  theme(legend.position = "none") +
  facet_wrap( ~ index)
# plot
plot_mobility_1

#' \clearpage
#' \newpage

#+ r, echo = FALSE, warning = FALSE, message = FALSE
# mobility regressions table 
models.1 <- 
  list(
    "Residential" = fit1.1, 
    "Retail" = fit1.2, 
    "Transit" = fit1.3, 
    "Workplace" = fit1.4
  )
# 
cm <- c('treat_0' = 'Treatment 1', 'treat_2' = 'Treatment 2')
gm <- 'BIC|AIC|R2 Within|R2 Pseudo|Log.Lik.|FE:city'
# 
table_mobility_1 <- 
  modelsummary(
    models.1,
    statistic = c("{std.error}", "{p.value}"),
    coef_map = cm,
    gof_omit = gm,
    fmt = '%.2f', 
    booktabs = TRUE, 
    title = 'Estimated effects of treatment on mobility.'
  )
# table
table_mobility_1


#' \clearpage
#' \newpage

# number of observations for each (dynamic) ATT
number_of_obs <- rel_effect_mc |> 
  select(count, time, crime) |> 
  pivot_wider(names_from = crime, values_from = count) 

# show results
number_of_obs

