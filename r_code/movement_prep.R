rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
nflreadr::.clear_cache()

library(tidyverse) # Data Cleaning, manipulation, summarization, plotting
library(scales)
library(signs)
library(nflreadr)

setwd("~/BDB24")

#number of games, currently at 6270 after TNF
games <- nflreadr::load_schedules(seasons = 2022) |>
  dplyr::filter(week < 10) |>
  dplyr::select(gameId = old_game_id, new_gameId = game_id, week) |>
  dplyr::mutate(gameId = as.integer(gameId))

# Data Cleaning
info <- week |>
  rename(team = club) |>
  filter(team == 'football') |>
  select(gameId, playId, frame = frameId, event, fbx = x, fby = y) |>
  distinct() |>
  arrange(gameId, playId, frame) |>
  group_by(gameId, playId) |>
  mutate(new_event = ifelse(event %in% c("line_set", "man_in_motion", "shift"), NA, event),
         start = first(na.omit(new_event)),
         is_start = ifelse(start == event, 1, 0),
         end = last(na.omit(new_event)),
         is_end = ifelse(end == event, 1, 0),
         any_start = any(is_start == 1),
         any_end = any(is_end == 1)) |>
  filter(any_start, any_end) |>
  summarize(start_frame = frame[which(is_start == 1)[1]],
            end_frame = frame[which(is_end == 1 & frame > start_frame)[1]], .groups = "drop") |>
  ungroup() |>
  left_join(games, by = 'gameId')

carrier <- week |>
  filter(nflId == ballCarrierId) |>
  select(gameId, playId, frame = frameId, car_x = x_std, car_y = y_std, car_s = s, car_a = a,
         car_dir = dir_std, car_o = o_std, ballCarrierId, playDirection) |>
  left_join(info, by = c('gameId', 'playId')) |>
  filter(!is.na(start_frame), !is.na(end_frame),
         frame >= start_frame, frame <= end_frame) |>
  distinct() |>
  group_by(gameId, playId, ballCarrierId) |>
  mutate(lag_dir = lag(car_dir),
         lag_dir = ifelse(is.na(lag_dir), car_dir, lag_dir)) |>
  ungroup() |>
  mutate(change_dir = 180 - abs(abs(car_dir - lag_dir) - 180)) |>
  arrange(gameId, playId, frame) |>
  group_by(ballCarrierId) |>
  mutate(max_s = max(car_s),
         max_a = max(car_a),
         max_change_dir = max(change_dir)) |>
  ungroup() |>
  mutate(change_dir = ifelse(change_dir>60, 60, change_dir)) |>
  select(gameId, playId, frame, ballCarrierId, car_x, car_y, 
         car_s, car_a, max_s, max_a, car_dir, car_o, change_dir, max_change_dir)

write.csv(carrier, "data/carrier.csv", row.names = FALSE)

team <- week |>
  rename(team = club) |>
  filter(nflId != ballCarrierId) |>
  mutate(side = ifelse(team == defensiveTeam, "DEF", "OFF")) |>
  select(gameId, playId, frame = frameId, side, nflId, 
         x_std, y_std, s, a, dir_std, playDirection) |>
  left_join(info, by = c('gameId', 'playId')) |>
  filter(!is.na(start_frame), !is.na(end_frame),
         frame >= start_frame, frame <= end_frame) |>
  distinct() |>
  select(gameId, new_gameId, week, playId, frame, side, nflId, x_std, y_std, s, a, dir_std)

rm(week, plays0, prep_players, games)
gc()

total <- team |>
  inner_join(carrier, by = c("gameId", "playId", "frame")) |>
  mutate(dist_to_carrier = sqrt((x_std - car_x)^2 + (y_std - car_y)^2)) |>
  select(gameId, new_gameId, week, playId, everything())

rm(info, carrier, team)
gc()

write.csv(total, "data/influence_prep.csv", row.names = FALSE)