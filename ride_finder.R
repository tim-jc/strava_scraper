xmas_dates <- crossing(d = seq(24,31),y = seq(11,22)) %>% 
  mutate(date_vec = str_glue("{d}12{y}") %>% dmy()) %>% 
  pull(date_vec)

find_rides_starting(start_dates = xmas_dates) %>% 
  draw_map(draw_bbox = T)



find_rides_starting(min_distance = 100) %>% 
  draw_map(draw_bbox = T)



find_rides_starting(start_location = c(51.95, 51.94, 1.083, 1.078)) %>% 
  draw_map(draw_bbox = T)


find_rides_visiting(visiting_location = c(51.95, 51.94, 1.083, 1.078))
