# Global COVID-19
# 2022/03
# Diaz, Fossati, Trajtenberg
# functions
# 

# 
# [1] "Amsterdam"      "Auckland"       "Austin"         "Barcelona"      "Boston"        
# [6] "Brisbane"       "Buenos Aires"   "Cali"           "Chicago"        "Dallas"        
# [11] "Fortaleza"      "Guatemala City" "Guayaquil"      "Hannover"       "Helsinki"      
# [16] "Kansas City"    "Lima"           "Ljubljana"      "London"         "Los Angeles"   
# [21] "Louisville"     "Malmo"          "Memphis"        "Mendoza"        "Mexico City"   
# [26] "Montevideo"     "Muzaffarpur"    "Nashville"      "New York City"  "Oakland"       
# [31] "Rio de Janeiro" "Sacramento"     "Saint Louis"    "San Francisco"  "San Salvador"  
# [36] "Santiago"       "Sao Paulo"      "Seattle"        "Seoul"          "Stockholm"     
# [41] "Tallinn"        "Tel Aviv"       "Toronto"        "Vancouver"      "Zurich"  


# 
nrow <- function(x) dim(x)[1]
ncol <- function(x) dim(x)[2]


# get stay-at-home indexes
get_stay_at_home_indexes <- function(data0, cities, weekly = TRUE){

  # country codes
  code <- c(
    'NLD', 'NZL', 'USA', 'ESP', 'USA', 'AUS', 'ARG', 'COL', 'USA',
    'USA', 'BRA', 'GTM', 'ECU', 'DEU', 'FIN', 'USA', 'PER', 'SVN',
    'GBR', 'USA', 'USA', 'SWE', 'USA', 'ARG', 'MEX', 'URY', 'IND',
    'USA', 'USA', 'USA', 'BRA', 'USA', 'USA', 'SLV', 'CHL', 'BRA',
    'USA', 'KOR', 'USA', 'SWE', 'EST', 'ISR', 'CAN', 'CAN', 'CHE'
  )
  
  # region codes
  region_code <- c(
    '', '', 'US_TX', '', 'US_MA', '', '', '', 'US_IL',
    'US_TX', 'BR_CE', '', '', '', '', 'US_KS', '', '',
    'UK_ENG', 'US_CA', 'US_KY', '', 'US_TN', '', '', '', '',
    'US_TN', 'US_NY', 'US_CA', 'BR_RJ', 'US_CA', 'US_CA', '', '', 'BR_SP',
    'US_WA', '', 'US_MO', '', '', '', 'CAN_ON', 'CAN_BC', ''
  )
  
  # clean data set
  n_cities <- nrow(cities)
  tmp_index <- matrix(0, 366, n_cities)
  # 
  for(i in 1:n_cities){
    # 
    tmp_1 <- data0 %>% 
      filter(CountryCode == code[i], Date < 20210101)
    #
    if (code[i] %in% c('USA','BRA','GBR','CAN')){
      tmp_1 <- tmp_1 %>% 
        filter(RegionCode == region_code[i])
    }
    # select index
    tmp_index[,i] <- tmp_1$`C6_Stay at home requirements`
  }
  # add dates
  tmp_index_1 <- 
    tibble(
      cbind(
        as.data.frame(tmp_index),
        as.Date(as.character(tmp_1$Date),"%Y%m%d")
      )
    )
  colnames(tmp_index_1) <- c(cities$city, "date")
  
  # add 2018 & 2019 values (0s)
  tmp_date <- seq.Date(as.Date("2018-01-01"), as.Date("2019-12-31"), by="day")
  tmp_index_2 <- tibble(
    cbind(
      data.frame(matrix(0, ncol = n_cities, nrow = length(tmp_date))), 
      tmp_date
    )
  )
  colnames(tmp_index_2) <- c(cities$city, "date")

  # join 2018, 2019, and 2020 indexes
  stay_at_home_1 <- tibble(rbind(tmp_index_2, tmp_index_1))
  
  # tidy
  stay_at_home_2 <- stay_at_home_1 %>% 
    pivot_longer(-date, names_to = "city", values_to = "value") 

  # aggregate data
  if (weekly == TRUE) {
    # make weekly
    data_out <- stay_at_home_2 %>%
      mutate(
        year = year(date), 
        month = month(date), 
        y_week = yearweek(date)
      ) %>%
      group_by(city, y_week) %>%
      summarise(
        city = first(city), 
        date = first(date), 
        value = max(value, na.rm = TRUE), 
        year = first(year),
        month = first(month)
      ) %>%
      arrange(city, y_week)
    
    # remove incomplete weeks (ie, week 53)
    data_out <- filter(data_out, y_week != yearweek("2020 W53"))
    
  } else {
    # make monthly
    data_out <- stay_at_home_2 %>%
      mutate(
        year = year(date), 
        month = month(date), 
        y_month = yearmonth(date)
      ) %>%
      group_by(city, y_month) %>%
      summarise(
        city = first(city), 
        date = first(date), 
        value = max(value, na.rm = TRUE), 
        year = first(year),
        month = first(month)
      ) %>%
      arrange(city, y_month)
    
  }
  
  # treatment variable
  data_out$treat_0 <- as.numeric(data_out$date > "2020-02-28")
  data_out$treat_1 <- as.numeric(data_out$value > 0)
  data_out$treat_2 <- as.numeric(data_out$value > 1)
  data_out$treat_2_plot <- as.numeric(data_out$value > 1)
  # 
  return(data_out)
}

 
# get mobility indexes
get_mobility_indexes <- function(data0, cities, stay_at_home_w){
  
  # sub-region codes
  region_code <- c(
    'NL-NH','NZ-AUK','US-01','ES-CT','US-02','AU-QLD','AR-C','CO-VAC','US-03',
    'US-04','BR-CE','GT-GU','EC-G','DE-NI','FI-18','US-05','PE-LIM','SI-061',
    'GB-LND','US-06','US-07','SE-M','US-08','AR-M','MX-CMX','UY-MO','IN-BR',
    'US-09','US-10','US-11','BR-RJ','US-12','US-13','SV-SS','CL-RM','BR-SP',
    'US-14','KR-SL','US-15','SE-AB','EE-37','IL-TA','CA-ON','CA-BC','CH-ZH'
  )
  
  # clean coding issues with Seoul
  data_1 <- mutate(
    data0, 
    iso_3166_2_code = replace(
      iso_3166_2_code, 
      metro_area == "Seoul Metropolitan Area",
      "KR-SL"
    )
  ) 
  
  # clean coding issues with US cities 
  us_city_codes <- region_code[c(3,5,9,10,16,20,21,23,28,29,30,32,33,37,39)]
  us_city_counties <- c(
    "Travis County","Suffolk County","Cook County","Dallas County",
    "Jackson County","Los Angeles County","Jefferson County","Shelby County",
    "Davidson County","New York County","Alameda County","Sacramento County",
    "San Francisco County","King County","St. Louis"
  )
  # replace region names
  for(i in 1:15){
    data_1 <- mutate(
      data_1, 
      iso_3166_2_code = replace(
        iso_3166_2_code, 
        sub_region_2 == us_city_counties[i], 
        us_city_codes[i]
      )
    )
  }
  
  # clean data set
  data_1 <- data_1 %>% 
    filter(iso_3166_2_code %in% region_code, date < "2021-01-01") %>%
    mutate(date = as.Date(date)) %>% 
    select(
      date,
      city = iso_3166_2_code,
      retail = retail_and_recreation_percent_change_from_baseline,
      transit = transit_stations_percent_change_from_baseline,
      workplace = workplaces_percent_change_from_baseline,
      residential = residential_percent_change_from_baseline
    ) %>%
    as_tibble()
  
  # replace region names
  for(i in 1:n_cities){
    data_1 <- mutate(
      data_1, 
      city = replace(city, city == region_code[i], cities[i,"city"])
    )
  }
  
  # tidy
  data_2 <- data_1 %>% 
    pivot_longer(
      c("retail","transit","workplace","residential"), 
      names_to = "index", 
      values_to = "value"
    ) %>% 
    filter(date >= "2020-02-17")
  
  # aggregate data
  data_w <- data_2 %>%
    mutate(
      year = year(date), 
      month = month(date), 
      y_week = yearweek(date)
    ) %>%
    group_by(city, index, y_week) %>%
    summarise(
      city = first(city), 
      date = first(date), 
      index = first(index), 
      value = mean(value, na.rm = TRUE), 
      year = first(year),
      month = first(month)
    ) %>%
    arrange(city, index, y_week) 
  
  # remove incomplete weeks (ie, week 53)
  data_w <- filter(data_w, y_week != yearweek("2020 W53"))
  
  # treatment variable
  tmp <- stay_at_home_w %>% 
    filter(date >= "2020-02-17") %>% 
    select(city, y_week, treat_0, treat_1, treat_2)
  # 
  data_out <- data_w %>% 
    left_join(tmp, by = c("city", "y_week")) 

  # 
  return(data_out)
}


# get crime data
get_crime_indexes <- 
  function(
    crime_data_0, 
    crime, 
    stay_at_home, 
    weekly = TRUE, 
    temp_0, 
    rain_0
  ){
    
    # select crime and remove 2021
    data_0 <- crime_data_0 %>% 
      select(
        date = Date,
        city = City,
        country = Country,
        value = crime
      ) %>% 
      filter(date < "2021-01-01")
    
    # aggregate
    if (weekly == TRUE) {
      # make weekly
      data_1 <- data_0 %>%
        mutate(
          city = as.factor(city),
          city_id = as.numeric(city),
          year = year(date), 
          month = month(date), 
          y_week = yearweek(date)
        ) %>%
        group_by(city_id, y_week) %>%
        summarise(
          city = first(city), 
          city_id = first(city_id), 
          country = first(country),
          date = first(date), 
          y = sum(value), 
          year = first(year),
          month = first(month),
        ) %>%
        rowwise() %>%
        mutate(
          temp = as.numeric(temp_0[city_id,month+1]),
          rain = as.numeric(rain_0[city_id,month+1])
        ) %>%
        arrange(city, y_week)
      
      # remove incomplete weeks (ie, week 53)
      data_1 <- filter(data_1, y_week != yearweek("2020 W53"))
      
      # construct index
      tmp0 <- data_1 %>% 
        filter(year==2019) %>% 
        group_by(city) %>%
        summarize(y_base = mean(y, na.rm=T))
      # 
      data_out <- data_1 %>% 
        left_join(tmp0) %>% 
        group_by(city_id) %>%
        mutate(y_indx = 100*y/y_base)
      
    } else {
      # make monthly
      data_1 <- data_0 %>%
        mutate(
          city = as.factor(city),
          city_id = as.numeric(city),
          year = year(date), 
          month = month(date), 
          y_month = yearmonth(date)
        ) %>%
        group_by(city_id, y_month) %>%
        summarise(
          city = first(city), 
          city_id = first(city_id), 
          country = first(country),
          date = first(date), 
          y = sum(value), 
          year = first(year),
          month = first(month)
        ) %>%
        rowwise() %>%
        mutate(
          temp = as.numeric(temp_0[city_id,month+1]),
          rain = as.numeric(rain_0[city_id,month+1])
        ) %>%
        arrange(city, y_month)
      
      # construct index
      tmp0 <- data_1 %>% 
        filter(year==2019) %>% 
        group_by(city) %>%
        summarize(y_base = mean(y, na.rm=T))
      # 
      data_out <- data_1 %>% 
        left_join(tmp0) %>% 
        group_by(city_id) %>%
        mutate(y_indx = 100*y/y_base)
    }
    
    # stay-at-home treatment variables
    data_out$treat_0 <- stay_at_home$treat_0
    data_out$treat_1 <- stay_at_home$treat_1
    data_out$treat_2 <- stay_at_home$treat_2
    
    # treatment variable w/ missing values
    data_out2 <- data_out %>% 
      mutate(D = ifelse(is.na(y_indx), NA, as.numeric(treat_2)))
    
    # 
    return(data_out2)
  }


# time series plot
time_series_plot <- 
  function(data_0) {
    ggplot(data_0, aes(x = date, y = y)) + 
      geom_line(size = .5, color = my_colors[3]) + 
      geom_blank(aes(x = as.Date("2019-01-01"), y = 1.25)) + 
      scale_y_continuous(name = "", breaks = 0) + 
      expand_limits(y = 0) + 
      scale_x_date(
        name = "", 
        limits = as.Date(c("2018-01-01","2020-12-31")),
        breaks = as.Date(c("2018-01-01","2019-01-01","2020-01-01","2021-01-01")),
        date_labels = "%Y"
      ) +
      theme(
        legend.position = "none",
        legend.title = element_blank(),
        axis.text.y = element_blank()
      ) +
      facet_wrap( ~ city, scales = "free_y", ncol = 5)
  }


# time series plot 2, indexes
time_series_plot_2 <- 
  function(data_0) {
    # mean indexes
    mean_index <- data_0 %>% 
      group_by(crime, date) %>% 
      summarize(
        date = first(date),
        mean_indx = mean(y_indx, na.rm = TRUE),
        crime = first(crime)
      )
    # plot
    ggplot(data_0, aes(x = date, y = y_indx, color = city)) + 
      scale_color_grey(start = 0.5, end = 0.7) +
      geom_line(size = .5) + 
      geom_line(
        data = mean_index, 
        aes(x = date, y = mean_indx), 
        size = 1,
        color = my_colors[3]
      ) + 
      scale_y_continuous(name = "") + 
      expand_limits(y = c(0,400)) + 
      scale_x_date(
        name = "", 
        limits = as.Date(c("2018-01-01","2020-12-31")),
        breaks = as.Date(c("2018-01-01","2019-01-01","2020-01-01")),
        date_labels = "%Y"
      ) +
      theme(legend.position = "none") +
      facet_wrap(
        ~ factor(
          crime, 
          levels = c("Assault","Burglary","Robbery","Theft","Vehicle","Homicide")
        )
      )
  }


# panel plot
panel_plot <- 
  function(data_0) {
    # 
    ggplot(data_0, aes(x = date, y = city, fill = as.factor(treat_2_plot))) +
      geom_tile(color = "grey40", alpha = .95) +
      scale_fill_manual(
        values = c("grey60", "#fd8d3c"), 
        labels = c("Untreated", "Treated"),
        na.value = "grey90"
      ) +
      scale_y_discrete(name = "", limits = rev) + 
      scale_x_date(name = "", date_breaks = "1 year", date_labels = "%Y") +
      coord_fixed() +
      coord_cartesian(expand = FALSE) +
      theme_light(base_size = 10) +
      theme(
        legend.position = "bottom", 
        legend.title = element_blank(),
        panel.background = element_blank()
      ) 
  }


# plot covariates
covariates_plot <- 
  function(data, var, color_0) {
    ggplot(
      filter(data, year == 2018), 
      aes_string(x = "date", y = var)
      ) + 
      geom_point(size = 1, color = color_0) + 
      scale_y_continuous(name = "") + 
      scale_x_date(name = "",
                   date_breaks = "3 month",
                   date_minor_breaks = "1 month",
                   date_labels = "%b"
      ) +
      theme(legend.position = "none") +
      facet_wrap( ~ city, ncol = 5)
  }



# 
event_est_1 <- function(data_0){
  #
  data_1 <- data_0 %>% mutate(period = seq(1:36))
  # 
  fit1 <- feols(
    y_indx ~ treat_0 + temp + rain | city + year + month, 
    data = data_1
  )
  # 
  return(fit1)
} 

# 
event_est_2 <- function(data_0){
  #
  data_1 <- data_0 %>% mutate(period = seq(1:36))
  # 
  fit1 <- feols(
    y_indx ~ i(period, keep = 27:36) + temp + rain | city + year + month, 
    data = data_1
  )
  # 
  return(fit1)
} 

# 
clean_event_est <- function(fit, tmp_dates){
  #
  results_1 <- tibble(
    filter(tmp_dates, date > "2019-12-01"),
    month = seq(12),
    year = 2020,
    coefficient = c(0,0,fit$coeftable[1:10,"Estimate"]),
    se = c(0,0,fit$coeftable[1:10,"Std. Error"]),
    treatment = "Post"
  )
} 

# plot event study estimates
event_est_plot <- 
  function(data) {
    #
    interval_95 <- -qnorm((1-0.95)/2)  # 95% multiplier
    interval_90 <- -qnorm((1-0.90)/2)  # 90% multiplier
    # 
    ggplot(data, aes(x = date, y = coefficient)) + 
      geom_hline(yintercept = 0, color = "grey50", size = .5) +
      geom_vline(
        xintercept = as.Date("2020-03-01"), 
        color = "grey25", 
        linetype = "dashed", 
        size = .75
      ) +
      geom_line(size = .5, color = my_colors[3]) + 
      geom_point(size = .5, color = my_colors[3]) + 
      geom_pointrange(
        aes(
          x = date, 
          y = coefficient, 
          ymin = coefficient - se*interval_90,
          ymax = coefficient + se*interval_90
        ),
        color = my_colors[3],
        lwd = .5, 
        fatten = 1
      ) + 
      scale_y_continuous(
        name = "average effect (% change)", 
        breaks = c(-75,-50,-25,0,25,50)
      ) + 
      scale_x_date(
        name = "year 2020", 
        date_breaks = "3 months",
        date_labels = "%b"
      ) +
      coord_cartesian(
        xlim = as.Date(c("2019-12-31","2020-12-31")),
        ylim = c(-80,55)
      ) +
      theme(legend.position = "none") +
      facet_wrap(
        ~ factor(
          crime, 
          levels = c("Assault","Burglary","Robbery","Theft","Vehicle","Homicide")
        )
      )
  }


# fixed effect (FE) estimator
fit_model_fe <-
  function(data, formula, se = TRUE, ...) {
    fect(
      formula  = formula,
      data     = data,
      method   = "fe",
      index    = c("city","date"), 
      force    = "two-way", 
      se       = se,
      nboots   = 200,
      alpha    = .1,
      seed     = 1234,
      na.rm    = TRUE
    )
  }


# interactive fixed effect (IFE) estimator
fit_model_ife <-
  function(data, formula, se = TRUE, ...) {
    fect(
      formula  = formula,
      data     = data,
      method   = "ife",
      index    = c("city","date"), 
      force    = "two-way", 
      CV       = TRUE,
      r        = c(0,2),
      parallel = TRUE,
      se       = se,
      nboots   = 1000,
      alpha    = .1,
      seed     = 1234,
      na.rm    = TRUE
    )
  }

# placebo model
fit_model_ife_p <-
  function(data, formula, r.cv, period, ...) {
    fect(
      formula  = formula,
      data     = data,
      method   = "ife",
      index    = c("city","date"), 
      force    = "two-way", 
      CV       = 0,
      r        = r.cv, 
      parallel = TRUE,
      se       = TRUE,
      nboots   = 1000,
      alpha    = .1,
      seed     = 1234,
      na.rm    = TRUE, 
      placeboTest    = TRUE, 
      placebo.period = period
    )
  }


# matrix completion (MC) estimator
fit_model_mc <-
  function(data, formula, se = TRUE, ...) {
    fect(
      formula  = formula,
      data     = data,
      method   = "mc",
      index    = c("city","date"), 
      force    = "two-way", 
      CV       = TRUE,
      nlambda  = 10,
      parallel = TRUE,
      se       = se,
      nboots   = 1000,
      alpha    = .1,
      seed     = 1234,
      na.rm    = TRUE
    )
  }

# placebo model
fit_model_mc_p <-
  function(data, formula, lambda.cv, period, ...) {
    fect(
      formula  = formula,
      data     = data,
      method   = "mc",
      index    = c("city","date"), 
      force    = "two-way", 
      CV       = 0,
      lambda   = lambda.cv,
      parallel = TRUE,
      se       = TRUE,
      nboots   = 1000,
      alpha    = .1,
      seed     = 1234,
      na.rm    = TRUE, 
      placeboTest    = TRUE, 
      placebo.period = period
    )
  }


# get table
get_estimates_table <- 
  function(fit, placebo = FALSE) {
    if (placebo == TRUE) {
      fit %>% 
        map( ~ cbind(
          as.data.frame(.x$est.placebo)[1:5],
          Cities = as.integer(.x$N), 
          Months = as.integer(.x$T)
        )
        ) %>%
        do.call(what = rbind.data.frame) %>%
        t() 
    } else {
      fit %>% 
        map( ~ cbind(
          as.data.frame(.x$est.avg), 
          Cities = as.integer(.x$N), 
          Months = as.integer(.x$T)
        )
        ) %>%
        do.call(what = rbind.data.frame) %>%
        t() %>% 
        as.data.frame()
    }
  }


# jackknife bias-adjusted mean
j_mean <- function(x_i, n){
  apply(x_i, 1, function(x) mean(x, na.rm=T))
}

# jackknife s.e.
j_se <- function(x_i, n){
  apply(x_i, 1, function(x) sqrt(((n-1)^2)/n)*sd(x, na.rm=T))
}

# leave-one-out estimates
leave_one_out_est <- function(data0, n_units, estimator){
  #
  if(estimator == "mc"){
    mod0 <- 
      data0 %>% 
      fit_model_mc(formula = y_indx ~ D + temp + rain, se = FALSE)
  } else {
    mod0 <- 
      data0 %>% 
      fit_model_ife(formula = y_indx ~ D + temp + rain, se = FALSE)
  }
  # 2020 values
  tmp1 <- ifelse(mod0$obs.missing > 1, NA, mod0$obs.missing)
  tmp2 <- tmp1*mod0$eff
  # ATT
  att_n <- as.numeric(sum(tmp2, na.rm=T)/sum(tmp1, na.rm=T))
  # monthly ATTs
  mtt_n <- as.numeric(rowSums(tmp2, na.rm=T)/rowSums(tmp1, na.rm=T))
  # centered fixed effects for 2020
  fxd_n <- mod0$xi - mean(mod0$xi[1:24]) 
  # aggregate effect
  agg_n <- fxd_n + mtt_n
  
  # storage matrices 
  store_1 <- matrix(0,  1, n_units)
  store_2 <- matrix(0, 36, n_units)
  store_3 <- matrix(0, 36, n_units)
  store_4 <- matrix(0, 36, n_units)
  # leave-one-out estimates
  for(i in 1:n_units){
    #
    if(estimator == "mc"){
      mod_i <- 
        data0 %>% 
        filter(city_id != i) %>% 
        fit_model_mc(formula = y_indx ~ D + temp + rain, se = FALSE)
    } else {
      mod_i <- 
        data0 %>% 
        filter(city_id != i) %>% 
        fit_model_ife(formula = y_indx ~ D + temp + rain, se = FALSE)
    }
    # 2020 values
    tmp1 <- ifelse(mod_i$obs.missing > 1, NA, mod_i$obs.missing)
    tmp2 <- tmp1*mod_i$eff
    # 
    att_i <- as.numeric(sum(tmp2, na.rm=T)/sum(tmp1, na.rm=T))
    mtt_i <- as.numeric(rowSums(tmp2, na.rm=T)/rowSums(tmp1, na.rm=T))
    fxd_i <- mod_i$xi - mean(mod_i$xi[1:24]) 
    agg_i <- fxd_i + mtt_i
    # 
    store_1[,i] <- att_i
    store_2[,i] <- mtt_i
    store_3[,i] <- fxd_i
    store_4[,i] <- agg_i
  }
  # 
  att <- cbind(att_n, j_mean(store_1, n_units), j_se(store_1, n_units))
  mtt <- cbind(mtt_n, j_mean(store_2, n_units), j_se(store_2, n_units))
  fxd <- cbind(fxd_n, j_mean(store_3, n_units), j_se(store_3, n_units))
  agg <- cbind(agg_n, j_mean(store_4, n_units), j_se(store_4, n_units))
  # 
  
  # return
  return(
    list(
      "att" = att, 
      "mtt" = mtt[25:36,], 
      "fxd" = fxd[25:36,], 
      "agg" = agg[25:36,]
      )
    )
}


# clean results
clean_monthly_estimates <- function(loo_est){
  #
  results_1 <- tibble(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-01"), by="month"),
    month = seq(12),
    year = 2020,
    coefficient = c(0,0,loo_est$agg[3:12,2]),
    se = c(0,0,loo_est$agg[3:12,3]),
    treatment = "Treated"
  )
  #
  results_2 <- tibble(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-01"), by="month"),
    month = seq(12),
    year = 2020,
    coefficient = loo_est$fxd[1:12,2],
    se = loo_est$fxd[1:12,3],
    treatment = "Untreated"
  )
  #
  results_3 <- tibble(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-01"), by="month"),
    month = seq(12),
    year = 2020,
    coefficient = c(0,0,loo_est$mtt[3:12,2]),
    se = c(0,0,loo_est$mtt[3:12,3]),
    treatment = "Treatment"
  )
  # return
  return(rbind(results_1,results_2,results_3))
}


# plot monthly estimates
monthly_est_plot <- 
  function(monthly_est) {
    # 
    monthly_est <- monthly_est %>% 
      filter(treatment != "Treatment")
    # 
    interval_95 <- -qnorm((1-0.95)/2)  # 95% multiplier
    interval_90 <- -qnorm((1-0.90)/2)  # 90% multiplier
    # 
    ggplot(monthly_est, aes(x = date, y = coefficient, color = treatment)) + 
      scale_color_manual(
        name = "", 
        values = c("Treated" = my_colors[3], "Untreated" = my_colors[1])
      ) +
      geom_hline(yintercept = 0, color = "grey50", size = .5) +
      geom_vline(
        xintercept = as.Date("2020-03-01"), 
        color = "grey25", 
        linetype = "dashed", 
        size = .75
      ) +
      geom_line(size = .5) + 
      geom_point(size = .5) + 
      geom_pointrange(
        aes(
          x = date, 
          y = coefficient, 
          ymin = coefficient - se*interval_90,
          ymax = coefficient + se*interval_90
        ),
        lwd = .5, 
        fatten = 1
      ) + 
      scale_y_continuous(
        name = "average effect (% change)", 
        breaks = c(-75,-50,-25,0,25,50)
      ) + 
      scale_x_date(
        name = "year 2020", 
        date_breaks = "3 months",
        date_labels = "%b"
      ) +
      coord_cartesian(
        xlim = as.Date(c("2019-12-31","2020-12-31")),
        ylim = c(-80,55)
      ) +
      theme(legend.position = "bottom") +
      facet_wrap(
        ~ factor(
          crime, 
          levels = c("Assault","Burglary","Robbery","Theft","Vehicle","Homicide")
        )
      )
  }

# plot monthly estimates
monthly_trt_plot <- 
  function(monthly_est) {
    # 
    monthly_est <- monthly_est %>% 
      filter(treatment == "Treatment")
    # 
    interval_95 <- -qnorm((1-0.95)/2)  # 95% multiplier
    interval_90 <- -qnorm((1-0.90)/2)  # 90% multiplier
    # 
    ggplot(monthly_est, aes(x = date, y = coefficient)) + 
      geom_hline(yintercept = 0, color = "grey50", size = .5) +
      geom_vline(
        xintercept = as.Date("2020-03-01"), 
        color = "grey25", 
        linetype = "dashed", 
        size = .75
      ) +
      geom_line(size = .5, color = my_colors[3]) + 
      geom_point(size = .5, color = my_colors[3]) + 
      geom_pointrange(
        aes(
          x = date, 
          y = coefficient, 
          ymin = coefficient - se*interval_90,
          ymax = coefficient + se*interval_90
        ), 
        color = my_colors[3],
        lwd = .5, 
        fatten = 1
      ) + 
      scale_y_continuous(
        name = "average effect (% change)", 
        breaks = c(-75,-50,-25,0,25,50)
      ) + 
      scale_x_date(
        name = "year 2020", 
        date_breaks = "3 months",
        date_labels = "%b"
      ) +
      coord_cartesian(
        xlim = as.Date(c("2019-12-31","2020-12-31")),
        ylim = c(-80,55)
      ) +
      theme(legend.position = "bottom") +
      facet_wrap(
        ~ factor(
          crime, 
          levels = c("Assault","Burglary","Robbery","Theft","Vehicle","Homicide")
        )
      )
  }


# clean relative results
clean_relative_estimates <- 
  function(fit, tmp_index) {
    tmp_index_2 <- seq(tmp_index[1],tmp_index[2])
    #
    fit %>% 
      map( 
        ~ cbind(as.data.frame(.x$est.att[row.names(.x$est.att) %in% tmp_index_2,]))
      ) %>%
      map( ~ add_column(.x, time = tmp_index_2)) %>% 
      map2(names(.), ~ add_column(.x, crime = rep(.y, nrow(.x)))) %>% 
      bind_rows() %>%
      as_tibble()
  }

# effect plot
relative_effect_plot <- 
  function(data_1, tmp_index, weekly) {
    # 
    if (weekly == TRUE) {
      lim_x <- c(tmp_index[1]-1, tmp_index[2])
      lim_y <- c(-80,40)
    } else {
      lim_x <- c(tmp_index[1]-1, tmp_index[2])
      lim_y <- c(-80,40)
    }
    # 
    ggplot(data_1, aes(x = time, y = ATT)) + 
      geom_hline(yintercept = 0, color = "grey50", size = .5) +
      geom_vline(
        xintercept = 0, 
        color = "grey25", 
        linetype = "dashed", 
        size = .75
      ) +
      geom_line(size = .5, color = my_colors[3]) + 
      geom_point(size = .5, color = my_colors[3]) + 
      geom_pointrange(
        aes(x = time, y = ATT, ymin = CI.lower, ymax = CI.upper),
        color = my_colors[3],
        lwd = .5, 
        fatten = 1
      ) + 
      scale_y_continuous(
        name = "average effect (% change)", 
        breaks = c(-75,-50,-25,0,25,50)
      ) + 
      scale_x_continuous(name = "months since treatment began") +
      coord_cartesian(
        xlim = c(tmp_index[1]-1, tmp_index[2]),
        ylim = c(-80,55)
      ) +
      theme(legend.position = "none") +
      facet_wrap(
        ~ factor(
          crime, 
          levels = c("Assault","Burglary","Robbery","Theft","Vehicle","Homicide")
        )
      )
  }

# placebo plot
placebo_plot <- 
  function(data_1, tmp_index, plac, weekly) {
    # 
    if (weekly == TRUE) {
      lim_x <- c(tmp_index[1]-1, tmp_index[2])
      lim_y <- c(-80,40)
    } else {
      lim_x <- c(tmp_index[1]-1, tmp_index[2])
      lim_y <- c(-80,40)
    }
    #
    ggplot(data_1, aes(x = time, y = ATT)) + 
      geom_hline(yintercept = 0, color = "grey50", size = .5) +
      geom_vline(
        xintercept = 0, 
        color = "grey25", 
        linetype = "dashed", 
        size = .75
      ) +
      geom_line(size = .5, color = my_colors[1]) + 
      geom_point(size = .5, color = my_colors[1]) + 
      geom_pointrange(
        aes(x = time, y = ATT, ymin = CI.lower, ymax = CI.upper),
        color = my_colors[1],
        lwd = .5, 
        fatten = 1
      ) + 
      geom_line(
        data = filter(data_1, time %in% seq(-plac+1,0)), 
        size = .5, 
        color = my_colors[3]
      ) + 
      geom_point(
        data = filter(data_1, time %in% seq(-plac+1,0)), 
        size = 1, 
        color = my_colors[3]
      ) + 
      geom_pointrange(
        data = filter(data_1, time %in% seq(-plac+1,0)), 
        aes(x = time, y = ATT, ymin = CI.lower, ymax = CI.upper),
        color = my_colors[3],
        lwd = .5, 
        fatten = 1
      ) + 
      scale_y_continuous(
        name = "average effect (% change)", 
        breaks = c(-75,-50,-25,0,25,50)
      ) + 
      scale_x_continuous(name = "months since treatment began") +
      coord_cartesian(
        xlim = c(tmp_index[1]-1, tmp_index[2]),
        ylim = c(-80,55)
      ) +
      theme(legend.position = "none") +
      facet_wrap(
        ~ factor(
          crime, 
          levels = c("Assault","Burglary","Robbery","Theft","Vehicle","Homicide")
        )
      )
  }


