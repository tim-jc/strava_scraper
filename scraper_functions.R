
# Define values -----------------------------------------------------------

# Manual activities to exclude (Strava API call fails if these aren't removed)
activities_to_remove <- c(6317473692, 5686021341, 1358368751, 1222988207, 574040145, 543631692)

# Peaks units / conversions
peaks_units <- tribble(
  ~metric, ~display_name, ~multiplier, ~units,
  "cadence", "cadence", 1, "rpm",
  "watts", "power", 1, "w",
  "heartrate", "heart rate", 1, "bpm",
  "velocity_smooth", "speed", 2.23694, "mph",
  "distance", "distance", 0.000621371, "miles"
)

# Assemble YTD stats
ytd_stats <- tbl(con, "activities") %>%
  select(id, start_date_local, distance, moving_time, kilojoules) %>% 
  collect() %>% 
  filter(start_date_local >= floor_date(Sys.Date() - years(1), "year")) %>% 
  mutate(yr = year(start_date_local),
         yr_day = yday(start_date_local),
         distance_mi = distance * 0.000621371,
         ton = distance_mi >= 100,
         moving_time_hr = moving_time / 3600) %>% 
  group_by(yr) %>% 
  arrange(start_date_local) %>% 
  mutate(ytd_distance_mi = cumsum(distance_mi),
         ytd_tons = cumsum(ton),
         ytd_time_hr = cumsum(moving_time_hr),
         ytd_energy_kcal = cumsum(replace_na(kilojoules,0)),
         ytd_longest_ride = max(distance_mi[yr_day <= yday(Sys.Date())], na.rm = T),
         yr_longest_ride = max(distance_mi, na.rm = T)) %>% 
  select(id, distance_mi, start_date_local, ton, matches("^yr|^ytd")) %>% 
  mutate(start_date_local = as.Date(start_date_local)) %>% 
  group_by(start_date_local, yr, yr_day) %>% 
  summarise(ytd_distance_mi = max(ytd_distance_mi),
            ytd_tons = max(ytd_tons),
            ton_day = any(ton),
            ytd_time_hr = max(ytd_time_hr),
            ytd_energy_kcal = max(ytd_energy_kcal),
            ytd_longest_ride = max(ytd_longest_ride, na.rm = T),
            yr_longest_ride = max(yr_longest_ride, na.rm = T),
            activity_day = TRUE,
            activity_id = id[distance_mi == max(distance_mi)],
            .groups = "drop") %>% 
  group_by(yr) %>% 
  mutate(yr_distance_mi = max(ytd_distance_mi),
         yr_tons = max(ytd_tons),
         yr_time_hr = max(ytd_time_hr),
         yr_energy_kcal = max(ytd_energy_kcal)) %>% 
  right_join(tibble(start_date_local = seq.Date(floor_date(Sys.Date() - years(1), "year"),
                                                ceiling_date(Sys.Date(), "year") - days(1),
                                                "days"))) %>% 
  mutate(yr = year(start_date_local),
         yr_day = yday(start_date_local),
         activity_day = if_else(is.na(activity_day), F, activity_day),
         ton_day = if_else(is.na(ton_day), F, ton_day)) %>% 
  arrange(yr, yr_day) %>% 
  filter(!(yr == year(Sys.Date()) & yr_day > yday(Sys.Date()))) %>% 
  fill(names(.)[names(.) != "activity_ids"], .direction = "down") %>% 
  replace(is.na(.), 0) %>%  # in case no riding on first day of year.
  mutate(ytd_val = yr_day == yday(Sys.Date()),
         yr_lbl = if_else(yr == year(Sys.Date()), "ytd", "pytd")) %>% 
  ungroup()


# Scraper functions -------------------------------------------------------

calculate_activity_peaks <- function(activity_id,
                                     peaks_for = c("cadence", "heartrate", "watts", "velocity_smooth"),
                                     peak_time_ranges = c(5, 10, 12, 20, 30, 60, 120, 300, 360, 600, 720, 1200, 1800, 3600)) {
  # Get actvity data
  activities <- tbl(con, "activities") %>% collect()
  
  # Get stream for activity
  stream_sql <- tbl(con, "streams") %>% 
    filter(id == activity_id) %>%
    collect() %>%
    left_join(activities %>% select(id, sport_type), by = "id") %>% 
    select(time, sport_type, all_of(peaks_for)) 
  
  peak_time_ranges <- peak_time_ranges[peak_time_ranges <= max(stream_sql$time)]
  
  # Get all time peaks
  # Calculate best ever and best this year
  peaks_sql <- tbl(con, "peaks") %>% 
    filter(peak > 0) %>% 
    left_join(tbl(con, "activities") %>% select(id, start_date_local, sport_type), by = "id") %>% 
    collect()
  
  if(activity_id %in% peaks_sql$id) {
    stop(str_glue("Activity ID {activity_id} has already had peaks calculated. Remove from SQL table if re-calculation needed."))
  }
  
  best_peaks <- peaks_sql %>%
    filter(year(start_date_local) == year(Sys.Date())) %>% 
    mutate(peak_period = "Current year") %>% 
    bind_rows(peaks_sql %>% mutate(peak_period = "All time")) %>% 
    filter(!(sport_type == "VirtualRide" & metric == "velocity_smooth")) %>% # exclude speed metrics from virtual rides
    group_by(peak_period, metric, time_range) %>% 
    slice_max(peak, n = 3, with_ties = F) %>% 
    mutate(rank = rank(-peak))
  
  # Create distance "peaks", combine with other peaks (exclude current activity so it isn't compared
  # to itself)
  distance_peaks <- bind_rows(activities %>% filter(id != activity_id) %>% 
                                slice_max(distance, n = 3) %>%
                                mutate(peak_period = "All time", 
                                       rank = row_number()),
                              activities %>% filter(id != activity_id,
                                                    year(start_date_local) == year(Sys.Date())) %>%
                                slice_max(distance, n = 3) %>%
                                mutate(peak_period = "Current year",
                                       rank = row_number())) %>% 
    mutate(metric = "distance") %>% 
    select(id, metric, peak = distance, start_date_local, sport_type, peak_period, rank)
  
  best_peaks <- bind_rows(best_peaks,
                          distance_peaks)
  
  # Ensure a row present for every second of the entire activity (no data recorded when stopped)
  # Then establish the width of any data gaps for later smoothing
  # Large data gaps will mean bike stopped - values should be set to zero - but small gaps can be filled
  peaks <- tibble(time = seq(0,max(stream_sql$time),1)) %>% 
    left_join(stream_sql, by = "time") %>% 
    pivot_longer(-c(time, sport_type), names_to = "metric") %>% 
    filter(!(sport_type == "VirtualRide" & metric == "velocity_smooth")) %>% # exclude speed metrics from virtual rides
    arrange(metric, time) %>% 
    mutate(has_data = !is.na(value),
           has_data_group_id = row_number(),
           prev_has_data = lag(has_data),
           has_data_group_id = case_when(is.na(prev_has_data) ~ has_data_group_id,
                                         has_data != prev_has_data ~ has_data_group_id,
                                         T ~ NA_integer_)) %>% 
    fill(has_data_group_id, .direction = "down") %>% 
    group_by(has_data_group_id) %>% 
    mutate(gap_size = n(),
           val2 = value) %>% 
    ungroup() %>% 
    fill(val2, .direction = "down") %>% 
    mutate(value = case_when(is.na(value) & gap_size <= 10 ~ val2,
                             is.na(value) ~ 0,
                             T ~ value)) %>% 
    select(-matches("has_data"), -gap_size, -val2) %>% 
    nest(data = !metric) %>% 
    crossing(time_range = peak_time_ranges) %>% 
    mutate(id = activity_id,
           time_range_means = map2(data, time_range, ~slide_dbl(.x$value, .f = mean, .after = (.y - 1), .complete = T)),
           peak = map_dbl(time_range_means, ~max(., na.rm = T))) %>% 
    select(id, metric, time_range, peak)
  
  # Bind on distance "peak" for activity in question
  distance_peak <- activities %>% 
    filter(id == activity_id) %>% 
    mutate(metric = "distance") %>% 
    select(id, metric, peak = distance)
  
  peaks <- bind_rows(peaks,
                     distance_peak)
  
  # Compare peaks
  peaks_compare <- best_peaks %>% 
    left_join(peaks %>% select(metric, time_range, current_peak = peak),
              by = c("metric", "time_range")) %>% 
    filter(current_peak > peak) %>% 
    slice_min(rank) %>% 
    left_join(peaks_units, by = "metric") %>% 
    mutate(msg = str_glue("{peak_period}: {if_else(rank == 1,'B',str_c(scales::ordinal(rank), ' b'))}est {if_else(time_range < 60, str_c(time_range,'s'), str_c(time_range/60,'min'))} {display_name} - {round(current_peak*multiplier,1)}{units}"),
           msg = str_remove(msg, "NA "))
  
  if(nrow(peaks_compare) > 0) {
    walk(peaks_compare$msg, print)
  }
  
  # Append to peaks table
  dbWriteTable(con, "peaks", peaks %>% filter(metric != "distance"), append = T)
  
  str_glue("Peak efforts for activity {activity_id} appended to database.") %>% print()
  
}

draw_bbox_map <- function(activity_bbox, draw_bbox = F, activity_types = "Ride") {
  
  activity_id <- activity_bbox[[1]]
  bbox <- activity_bbox[[2]]
  
  # Limit activity ids to those of the selected types
  all_type_id <- tbl(con, "activities") %>% 
    filter(type %in% activity_types) %>% 
    pull(id)
  
  activity_id <- activity_id[activity_id %in% all_type_id]
  
  
  streams <- tbl(con, "streams") %>%
    select(id, lat, lng) %>% 
    filter(id %in% activity_id) %>% 
    collect() %>% 
    filter(row_number() %% 10 == 1) # select every 10th row -> speed up the plotting of the map
  
  # create base map
  leaflet_map <- leaflet::leaflet() %>% 
    leaflet::addTiles(urlTemplate = "https://tiles.stadiamaps.com/tiles/outdoors/{z}/{x}/{y}{r}.png")
  
  # Add on each ride
  for(ride_id in unique(streams$id)) {
    
    ride_stream <- streams %>% filter(id == ride_id)
    
    leaflet_map <- leaflet_map %>% 
      leaflet::addPolylines(ride_stream$lng, ride_stream$lat,
                            opacity = 1,
                            weight = 2,
                            color = "#48494B")
  }
  
  # Draw on bounding boxes, if draw_bbox set to TRUE
  if(draw_bbox & !is.na(bbox)) {
    
    for(i in 1:length(bbox)) {
      leaflet_map <- leaflet_map %>% 
        leaflet::addRectangles(
          lng1=bbox[[i]][4], lat1=bbox[[i]][2],
          lng2=bbox[[i]][3], lat2=bbox[[i]][1],
          fillColor = "transparent", 
          color = "#FF0000",
          weight = 1
        )
    }
    
  }
  
  print(leaflet_map)
  print(activity_id)
}

# Data quality functions --------------------------------------------------

check_data_quality <- function() {
  
  activities_loaded <- tbl(con, "activities") %>% select(id) %>% distinct() %>% pull(id)
  streams_loaded <- tbl(con, "streams") %>% select(id) %>% distinct() %>% pull(id)
  peaks_loaded <- tbl(con, "peaks") %>% select(id) %>% distinct() %>% pull(id)
  
  # Check for activities without peaks
  awp <- activities_loaded[!activities_loaded %in% peaks_loaded]
  if(length(awp > 0)) {awp <- str_glue("Activities without peaks: {str_flatten(awp, collapse = ',')}\n\n")} else {awp = ""}
  
  # Check for activities without streams
  aws <- activities_loaded[!activities_loaded %in% streams_loaded]
  if(length(aws > 0)) {aws <- str_glue("Activities without streams: {str_flatten(aws, collapse = ',')}\n\n")} else {aws = ""}
  
  # Check for orphaned peaks (i.e. not in activities)
  op <- peaks_loaded[!peaks_loaded %in% activities_loaded]
  if(length(op > 0)) {op <- str_glue("Orphaned peaks: {str_flatten(op, collapse = ',')}\n\n")} else {op = ""}
  
  # Check for orphaned streams (i.e. not in activities)
  os <- streams_loaded[!streams_loaded %in% activities_loaded]
  if(length(os > 0)) {os <- str_glue("Orphaned streams: {str_flatten(os, collapse = ',')}\n\n")} else {os = ""}
  
  dq_msg <-  str_c(awp, aws, op, os) 
  
  if(nchar(dq_msg > 0)) {
    send_ntfy_message(msg_body = dq_msg,
                      msg_title = "Strava data quality issue",
                      msg_tags = "x",
                      msg_link_url = NA_character_)
  }
  
  # Check date of most recent database backup
  
}

# Ride finder functions ---------------------------------------------------

# A set of functions for finding rides, all consistently named. Each takes a vector of activity ids and returns a vector of
# activity ids so they can be daisy chained together. Simpler than having one mega function, and easier to build extra
# filtering functionality later

find_rides_starting <- function(activity_bbox = list(NA, NA), start_dates = NA, start_years = NA, start_location = NA, min_distance = NA_real_) {
  
  # start_dates - vector of one or more dates
  # start_years - vector of one or more years
  activity_id <- activity_bbox[[1]]
  prev_bbox <- activity_bbox[[2]]
  
  # start_location - bounding box passed as NSEW vector, e.g. c(51, 50, 1, 0))
  
  if(all(is.na(activity_id))) {
    activity_id <- tbl(con, "activities") %>% pull(id) %>% unique()
  }
  
  # If bounding box supplied (i.e. 4 element list)
  if(length(start_location) == 4) {
    
    bbox_north <- start_location[1]
    bbox_south <- start_location[2]
    bbox_east <- start_location[3]
    bbox_west <- start_location[4]
    
    if(bbox_north <= bbox_south) {stop(str_glue("North value supplied ({bbox_north}) must be greater that South value ({bbox_south})"))}
    
    if(bbox_east <= bbox_west) {stop(str_glue("East value supplied ({bbox_east}) must be greater that West value ({bbox_west})"))}
    
    activity_id <- tbl(con, "activities") %>%
      filter(start_lat <= bbox_north,
             start_lat >= bbox_south,
             start_lng <= bbox_east,
             start_lng >= bbox_west,
             id %in% activity_id) %>%
      pull(id) %>% unique()
    
  }
  
  # If vector of dates supplied
  if(all(!is.na(start_dates))) {
    activity_id <- tbl(con, "activities") %>% 
      collect() %>% 
      mutate(start_date_local = as.Date(start_date_local)) %>% 
      filter(start_date_local %in% start_dates,
             id %in% activity_id) %>%
      pull(id) %>% unique()
  }
  
  # If vector of years supplied
  if(!is.na(start_years)) {
    activity_id <- tbl(con, "activities") %>% 
      collect() %>% 
      mutate(start_yr = year(start_date_local)) %>% 
      filter(start_yr %in% start_years,
             id %in% activity_id) %>%
      pull(id) %>% unique()
  }
  
  # If minimum distance supplied
  if(!is.na(min_distance)) {
    activity_id <- tbl(con, "activities") %>% 
      collect() %>% 
      mutate(distance_miles = distance / 1609.34) %>% 
      filter(distance_miles >=  min_distance,
             id %in% activity_id) %>%
      pull(id) %>% unique()
  }
  
  if(all(!is.na(prev_bbox))) {
    if(is.list(prev_bbox)) {
      prev_bbox <- append(prev_bbox, list(start_location))
    } else {
      prev_bbox <- list(prev_bbox, start_location)  
    }
  } else {
    prev_bbox <- list(start_location)
  }
  
  
  
  activity_bbox <- list(activity_id, prev_bbox)
  
  return(activity_bbox)
  
}

find_rides_visiting <- function(activity_bbox = list(NA, NA), visiting_location = NA) {
  
  activity_id <- activity_bbox[[1]]
  prev_bbox <- activity_bbox[[2]]
  
  # visiting_location - bounding box passed as NSEW vector, e.g. c(51, 50, 1, 0))
  
  if(all(is.na(activity_id))) {
    activity_id <- tbl(con, "activities") %>% pull(id) %>% unique()
  }
  
  # If bounding box supplied (i.e. 4 element list)
  if(length(visiting_location) == 4) {
    
    bbox_north <- visiting_location[1]
    bbox_south <- visiting_location[2]
    bbox_east <- visiting_location[3]
    bbox_west <- visiting_location[4]
    
    if(bbox_north <= bbox_south) {stop(str_glue("North value supplied ({bbox_north}) must be greater that South value ({bbox_south})"))}
    
    if(bbox_east <= bbox_west) {stop(str_glue("East value supplied ({bbox_east}) must be greater that West value ({bbox_west})"))}
    
    activity_id <- tbl(con, "streams") %>%
      filter(lat <= bbox_north,
             lat >= bbox_south,
             lng <= bbox_east,
             lng >= bbox_west,
             id %in% activity_id) %>%
      pull(id) %>% unique()
    
  }
  
  if(all(!is.na(prev_bbox))) {
    if(is.list(prev_bbox)) {
      prev_bbox <- append(prev_bbox, list(visiting_location))
    } else {
      prev_bbox <- list(prev_bbox, visiting_location)  
    }
  } else {
    prev_bbox <- list(visiting_location)
  }
  
  activity_bbox <- list(activity_id, prev_bbox)
  
  return(activity_bbox)
}

# Need to build a nice map plotting function



# Visualisation functions -------------------------------------------------

draw_critical_metric_curve <- function(metric_to_plot) {
  
  if(!metric_to_plot %in% peaks_units$display_name) {
    stop(str_glue("Invalid metric supplied; allowed values are '{str_flatten(peaks_units$display_name, collapse = \"', '\")}'"))
  }
  
  units <- peaks_units %>% filter(display_name == metric_to_plot)
  
  # Get all time peaks
  # Calculate best ever and best this year
  peaks_sql <- tbl(con, "peaks") %>% 
    filter(peak > 0,
           metric == local(units$metric)) %>% 
    left_join(tbl(con, "activities") %>% select(id, start_date_local, sport_type), by = "id") %>% 
    collect()
  
  best_peaks <- peaks_sql %>%
    filter(year(start_date_local) == year(Sys.Date())) %>% 
    mutate(peak_period = "Current year") %>% 
    bind_rows(peaks_sql %>% mutate(peak_period = "All time")) %>% 
    filter(!(sport_type == "VirtualRide" & metric == "velocity_smooth")) %>% # exclude speed metrics from virtual rides
    group_by(peak_period, metric, time_range) %>% 
    slice_max(peak, n = 3, with_ties = F) %>% 
    mutate(rank = rank(-peak),
           peak = peak * local(units$multiplier)) %>% 
    left_join(peaks_units, by = "metric") %>% 
    mutate(time_range_fct = if_else(time_range < 60, str_c(time_range,'s'), str_c(time_range/60,'min')),
           time_range_fct = factor(time_range_fct),
           time_range_fct = fct_reorder(time_range_fct, time_range),
           activity_url = str_glue("https://www.strava.com/activities/{id}"),
           hover_lbl = str_glue("Best {time_range_fct} {display_name} - {peak_period}
                              {round(peak, digits = 1)}{units}"))
  
  peaks_plot <- best_peaks %>% 
    filter(display_name == metric_to_plot, 
           rank == 1) %>% 
    ggplot(aes(x = time_range_fct, y = peak, colour = peak_period,
               fill = peak_period, group = peak_period)) +
    geom_point(aes(text = hover_lbl, customdata = activity_url)) +
    geom_area(position = "identity", alpha = 0.2) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(x = "",
         y = str_glue("{str_to_title(metric_to_plot)} /{local(units$units)}\n"))
  
  peaks_plot <- plotly::ggplotly(peaks_plot, tooltip = "text")
  
  # Render custom JS
  peaks_plot <- peaks_plot %>% htmlwidgets::onRender("
       function(el, x) {
       
         el.on('plotly_click', function(data) {
           // retrieve url from the customdata field passed to ggplot
           var url = data.points[0].customdata;
           // open this url in the same window
           window.open(url, \"_blank\");
         });
       
       }")
  
  return(peaks_plot)
  
}

draw_ytd_curve <- function(metric_to_plot) {
  
  ytd_tbl <- ytd_stats %>% 
    pivot_longer(c(matches("^ytd"),-ytd_val)) %>% 
    filter(name == metric_to_plot) %>% 
    mutate(hover_lbl = str_glue("{start_date_local}
                                 N = {round(value, 1)}"),
           activity_url = str_glue("https://www.strava.com/activities/{activity_id}"))
  
  ytd_curve <- ytd_tbl %>% 
    ggplot(aes(x = yr_day, y = value, colour = yr_lbl)) +
    geom_step(alpha = 0.5) +
    theme_minimal() +
    scale_colour_manual(values = c("pytd" = "grey85", "ytd" = "#0C2340")) +
    theme(legend.position = "none",
          axis.text.x = element_blank()) +
    labs(x = "", y = "")
  
  if(metric_to_plot == "ytd_tons") {
    ytd_curve <- ytd_curve +
      geom_point(data = ytd_tbl %>% filter(ton_day),
                 aes(text = hover_lbl, customdata = activity_url), size = 1)
    
  } else {
    ytd_curve <- ytd_curve +
      geom_point(data = ytd_tbl %>% filter(ytd_val),
                 aes(text = hover_lbl, customdata = activity_url), size = 1)
    
  }
  
  ytd_curve <- plotly::ggplotly(ytd_curve, tooltip = "text")
  
  # Render custom JS
  ytd_curve <- ytd_curve %>% htmlwidgets::onRender("
       function(el, x) {
       
         el.on('plotly_click', function(data) {
           // retrieve url from the customdata field passed to ggplot
           var url = data.points[0].customdata;
           // open this url in the same window
           window.open(url, \"_blank\");
         });
       
       }")
  
  
  return(ytd_curve)
}

get_ytd_values <- function(metric_to_display) {
  
  vals <- ytd_stats %>%
    filter(ytd_val) %>% 
    select(-c(yr_day, ytd_val, yr, start_date_local)) %>% 
    pivot_longer(-yr_lbl, names_to = "metric") %>% 
    filter(str_detect(metric, metric_to_display)) %>% 
    mutate(value = round(value,1),
           yr_lbl = if_else(str_detect(metric, "^yr_"), str_replace(yr_lbl, "td", "r"), yr_lbl)) %>% 
    select(-metric) %>% 
    deframe()
  
  return(vals)
  
}

get_ytd_valuebox <- function(...) {
  
  vals <- get_ytd_values(...)

  icon_str <- case_when(vals["ytd"] > vals["pytd"] ~ "fa-arrow-up",
                        vals["ytd"] < vals["pytd"] ~ "fa-arrow-down",
                        vals["ytd"] == vals["pytd"] ~ "fa-arrows-left-right")
  
  valueBox(vals["ytd"], icon = icon_str, color = "#EDF0F1")
}

draw_map <- function(streams_tbl) {
  
  map <- leaflet() %>% 
    addTiles('https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png',
             attribution = paste(
               '&copy; <a href="https://openstreetmap.org">OpenStreetMap</a> contributors',
               '&copy; <a href="https://cartodb.com/attributions">CartoDB</a>'
             )) 
  
  for(i in unique(streams_tbl$id)) {
    
    map <- map %>% add_track(streams_tbl %>% filter(id == i))
    
  }
  
  return(map)
  
}

draw_ytd_map <- function() {
  
  ytd_activities <- tbl(con, "activities") %>% 
    collect() %>% 
    filter(type == "Ride",
           start_date_local >= floor_date(Sys.Date(), "year"))
  
  ytd_streams <- tbl(con, "streams") %>% 
    filter(id %in% local(ytd_activities$id)) %>% 
    collect()
  
  map <- draw_map(ytd_streams)
  
  return(map)
  
}


# Publish / notify functions ----------------------------------------------

send_ntfy_message <- function(msg_body,
                              msg_url = "ntfy.sh/strava_stats_dashboard",
                              msg_title = "Strava dashboard updated",
                              msg_tags = "bike,chart_with_upwards_trend",
                              msg_priority = "default",
                              msg_link_url = "https://tim-jc.github.io/strava_scraper") {
  
  # Allowed priorities
  allowed_priorites <- c("urgent", "high", "default", "low", "min")
  
  if(!msg_priority %in% allowed_priorites) {
    stop(str_glue("Invalid priority level supplied; allowed values are '{str_flatten(allowed_priorites, collapse = \"', '\")}'"))
  }
  
  # tags - if they match an emoji short code they'll be rendered as such, otherwise will appear as string
  # tags should be supplied as a single string value with commas separating tags
  response <- httr::POST(url = msg_url,
                         body = msg_body,
                         httr::add_headers(c("Title" = msg_title,
                                             "Tags" = msg_tags,
                                             "Priority" = msg_priority,
                                             "Click" = msg_link_url)))
  
  return(response)
  
}

publish_to_git <- function(git_path = here::here(),
                           file_to_publish = "docs/index.html",
                           commit_msg = "auto commit from cron / Rscript") {
  
  command_str <- str_glue("cd {git_path} &&
                           git add {file_to_publish} &&
                           git commit -m '{commit_msg}' &&
                           git push -u origin main")
  
  system(command_str)
  
}
