library(tidyverse)
library(janitor)
library(lubridate)
library(silgelib)
library(ggmap)
library(zoo)
library(geosphere)


setwd("~/Library/CloudStorage/OneDrive-BDFGroup/Documents/@Projects/Flight LH790")

file_names <- list.files(pattern = "\\.csv$")

all_data <- map_dfr(file_names, function(file) {
  read_csv(file) %>% 
    mutate(FileName = file)
}) %>% 
  clean_names() %>% 
  separate(position,into = c("lat","lon"), sep = ",", convert =  TRUE) %>% 
  mutate(weekday = wday(utc, week_start = 1, label = TRUE),
         weekday = as.factor(weekday),
         file_name = if_else(file_name == "LH790_32e79440.csv",
                             "LH790_32e5d438.csv", file_name)) %>% 
  #filter(altitude = 0) %>% 
  arrange(timestamp) %>% 
  group_by(file_name) %>% 
  mutate(weekday_start = wday(utc[1], week_start = 1, label = TRUE),
         date_start = as.Date(utc[1]), 
         label = paste("LH790", date_start)) 


all_data %>% 
  ggplot(aes(x = speed, y = altitude)) +
  geom_point()


all_data <- all_data %>% 
  filter(!(altitude > 100 & speed < 100)) %>% 
  filter(!(altitude > 30000 & speed < 400)) %>% 
  group_by(file_name) %>% 
  arrange(timestamp) %>%
  mutate(altitude_before = lag(altitude, default = 0),
         altitude_after = lead(altitude))  %>% 
  filter(!(altitude == 0 & altitude_before == 0 & altitude_after == 0)) 
  
  


all_data <- all_data %>% 
  group_by(file_name) %>% 
  arrange(utc, .by_group = TRUE) %>% 
  mutate(startzeit = utc[1], 
         flugdauer = difftime(utc, startzeit, units = "mins"),
         flugdauer = as.numeric(flugdauer))


takeoff_threshold = 10    
landing_threshold = -10

all_data <- all_data %>% 
  mutate(gradient = c(NA, diff(altitude) / diff(flugdauer)),
         smooth_gradient = rollmean(gradient, 5, fill = NA)) %>% 
  mutate(phase = case_when(
    smooth_gradient > takeoff_threshold ~ 'Climb',
    smooth_gradient < landing_threshold ~ 'Descend',
    TRUE ~ 'Cruise'
  ))


all_data %>% 
  #filter(!file_name == "LH790_32e5d438.csv") %>% 
  ggplot(aes(x = flugdauer, y = speed, color = phase)) +
  geom_point(shape = "circle", size = 0.5, alpha= 0.8) +
  #geom_line(size = 1.5, alpha= 0.8) +
  facet_wrap(vars(label)) +
  theme_plex() +
  labs(x = "Speed in ln", y = "Altitude in ft")

all_data %>% 
  #filter(!file_name == "LH790_32e5d438.csv") %>% 
  ggplot(aes(x = flugdauer, y = altitude, color = phase)) +
  geom_point(shape = "circle", size = 0.5, alpha= 0.8) +
  #geom_line(size = 1.5, alpha= 0.8) +
  facet_wrap(vars(label)) +
  theme_plex() +
  labs(x = "Flighttime in min", y = "Altitude in ft")


all_data %>% 
  ggplot(aes(x = speed, y = altitude, color = phase, group = phase)) +
  geom_point(alpha = 0.4) +
  theme_plex()
  

# Anflug bestimmen --------------------------------------------------------


all_data %>%
  group_by(label) %>% 
  filter(phase == "Descend" & altitude < 1000) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_point(shape = "circle", colour = "#0032a0", size = 0.5) +
  facet_wrap(vars(label), scales = "free") +
  geom_point(aes(x = 103.989441, y = 1.359167), color = "#45a13f", size = 2) +
  theme_plex() +
  theme(strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5))


flights_direction <- all_data %>%
  group_by(label) %>% 
  filter(phase == "Descend" & altitude < 1000) %>% 
  group_by(label) %>%
  summarize(approach_direction = if_else(mean(lat) > 1.359167, 'north-east', 'south-west'))

all_data %>%
  left_join(flights_direction) %>%
  group_by(label) %>% 
  filter(phase == "Descend" & altitude < 1000) %>% 
  ggplot(.) +
  aes(x = lon, y = lat, colour = approach_direction) +
  geom_point(shape = "circle", size = 0.5) +
  facet_wrap(vars(label), scales = "free") +
  geom_point(aes(x = 103.989441, y = 1.359167), color = "#45a13f", size = 2) +
  theme_plex() +
  theme(strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5))



all_data %>%
  left_join(flights_direction) %>%
  select(label, approach_direction) %>% 
  distinct() %>% 
  ungroup() %>% 
  count(approach_direction) %>% 
  ggplot(aes(x = approach_direction, y = n)) +
  geom_col(fill = "#0032a0") +
  theme_plex() +
  labs(x = "Direction of approach (from)", y = "Occurence")



# Schleifen bestimmen -----------------------------------------------------


# Plot der letzten 30 min um grafisch Schleifen zu sehen

all_data %>%
  group_by(label) %>%
  mutate(remain_time = max(flugdauer) - flugdauer) %>% 
  filter(remain_time <= 30) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_point(shape = "circle", colour = "#0032a0", size = 0.5) +
  facet_wrap(vars(label), scales = "free") +
  geom_point(aes(x = 103.989441, y = 1.359167), color = "#45a13f", size = 2) +
  theme_plex()



# Schleifen berechnen

all_data <- all_data %>%
  arrange(label, utc) %>% #
  mutate(lead_lat = lead(lat),
         lead_lon = lead(lon)) %>%
  rowwise() %>% 
  mutate(bearing = if_else(is.na(lead_lat) | is.na(lead_lon), NA, bearing(c(lon, lat), c(lead_lon, lead_lat)))) %>%
  ungroup() 


# Neuer Plot mit Punktfarbe >1 = TRUE <=1 FALSE
# Markiert also Richtungsänderung pro Punkt

all_data %>%
  group_by(label) %>%
  mutate(
    remain_time = max(flugdauer) - flugdauer,
    change_in_bearing = c(NA, diff(bearing))) %>%
  filter(remain_time <= 30) %>%
  mutate(in_holding_pattern = abs(change_in_bearing) > 1) %>% 
  ggplot(aes(x = lon, y = lat, color = in_holding_pattern)) +
  geom_point(shape = "circle", size = 0.5) +
  facet_wrap(vars(label), scales = "free") +
  geom_point(aes(x = 103.989441, y = 1.359167), color = "#45a13f", size = 2) +
  theme_plex()



# Gedanke: Eine Schleife zieht mehrere Richtungsänderungen in einer _Folge_ von Punkten nach sich

# Berechung pro Flug in den letzten 30 min, wieviele Punkte in folge es maximal gibt, mit einer Richtungsänderung
holding_pattern <- all_data %>%
  group_by(label) %>%
  mutate(
    remain_time = max(flugdauer) - flugdauer,
    change_in_bearing = c(NA, diff(bearing))) %>%
  filter(remain_time <= 30) %>%
  mutate(in_holding_pattern = abs(change_in_bearing) > 1) %>% 
  group_by(label) %>% 
  arrange(utc, .by_group = TRUE) %>% 
  mutate(row_id = row_number()) %>%
  group_by(label, grp = with(rle(in_holding_pattern), rep(seq_along(lengths), lengths))) %>%
  mutate(consecutive_count = n()) %>%
  ungroup() %>% 
  select(-row_id, -grp) %>% 
  group_by(label) %>% 
  summarise(max_consecutive_count_last_30_min = max(consecutive_count))

# Plotte mit Anzahl consecutiver Folgen als Farbe

all_data %>%
  group_by(label) %>%
  mutate(
    remain_time = max(flugdauer) - flugdauer,
    change_in_bearing = c(NA, diff(bearing))) %>%
  filter(remain_time <= 30) %>%
  mutate(in_holding_pattern = abs(change_in_bearing) > 1) %>% 
  group_by(label) %>% 
  arrange(utc, .by_group = TRUE) %>% 
  mutate(row_id = row_number()) %>%
  group_by(label, grp = with(rle(in_holding_pattern), rep(seq_along(lengths), lengths))) %>%
  mutate(consecutive_count = n()) %>%
  ungroup() %>% 
  select(-row_id, -grp) %>% 
  ggplot(aes(x = lon, y = lat, color = consecutive_count)) +
  geom_point(shape = "circle", size = 0.5) +
  facet_wrap(vars(label), scales = "free") +
  geom_point(aes(x = 103.989441, y = 1.359167), color = "#45a13f", size = 2) +
  theme_plex()




# Bilde Grenzwert der konsekutiven Punkte > 30 = Hold, darunter Non-Hold

all_data %>%
  group_by(label) %>%
  mutate(remain_time = max(flugdauer) - flugdauer) %>% 
  filter(remain_time <= 30) %>% 
  left_join(holding_pattern) %>% 
  mutate(holding_pattern = if_else(max_consecutive_count_last_30_min > 30, "Hold", "Non-Hold")) %>% 
  ggplot(aes(x = lon, y = lat, color = holding_pattern)) +
  geom_point(shape = "circle", size = 0.5) +
  facet_wrap(vars(label), scales = "free") +
  geom_point(aes(x = 103.989441, y = 1.359167), color = "#45a13f", size = 2) +
  theme_plex() +
  theme(strip.text.x = element_text(size = 8),
        strip.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5))



# Bringe beide Ableitungen - approach und holds zusammen ------------------

all_data %>%
  group_by(label) %>%
  mutate(remain_time = max(flugdauer) - flugdauer) %>% 
  filter(remain_time <= 30) %>% 
  left_join(holding_pattern) %>% 
  left_join(flights_direction) %>% 
  mutate(holding_pattern = if_else(max_consecutive_count_last_30_min > 30, "Hold", "Non-Hold")) %>% 
  select(label, holding_pattern, approach_direction) %>% 
  distinct() %>% 
  ungroup() %>% 
  tabyl(approach_direction, holding_pattern) %>% 
  pivot_longer(-approach_direction, names_to = "holding_pattern", values_to = "count") %>% 
  mutate(share = count / sum(count))
  
