rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
nflreadr::.clear_cache()

library(tidyverse) # Data Cleaning, manipulation, summarization, plotting
library(nflplotR)
library(ggrepel) # better labels
library(nflfastR)
library(ggthemes) # custom pre-built themes
library(scales)
library(signs)
library(ggrepel) # better labels
library(ggtext)
library(ggimage)
library(viridis)
library(gtExtras)
library(nflreadr)

setwd("~/BDB24")

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
  ungroup()

carrier <- week |>
  filter(nflId == ballCarrierId) |>
  select(gameId, playId, frame = frameId, car_x = x_std, car_y = y_std, car_s = s, car_dir = dir_std,
         ballCarrierId, playDirection) |>
  left_join(info, by = c('gameId', 'playId')) |>
  filter(!is.na(start_frame), !is.na(end_frame),
         frame >= start_frame, frame <= end_frame) |>
  distinct() |>
  mutate(car_s = car_s * 2.04545) |>
  select(gameId, playId, frame, ballCarrierId, car_x, car_y, car_s, car_dir)

def <- week |>
  rename(team = club) |>
  filter(team == defensiveTeam, nflId != ballCarrierId) |>
  select(gameId, playId, frame = frameId, defId = nflId, def_x = x_std, def_y = y_std, def_s = s,
         def_dir = dir_std, playDirection) |>
  left_join(info, by = c('gameId', 'playId')) |>
  filter(!is.na(start_frame), !is.na(end_frame),
         frame >= start_frame, frame <= end_frame) |>
  distinct() |>
  mutate(def_s = def_s * 2.04545) |>
  select(gameId, playId, frame, defId, def_x, def_y, def_s, def_dir)

off <- week |>
  rename(team = club) |>
  filter(team == possessionTeam, nflId != ballCarrierId) |>
  select(gameId, playId, frame = frameId, offId = nflId, off_x = x_std, off_y = y_std, off_s = s,
         off_dir = dir_std, playDirection) |>
  left_join(info, by = c('gameId', 'playId')) |>
  filter(!is.na(start_frame), !is.na(end_frame),
         frame >= start_frame, frame <= end_frame) |>
  distinct() |>
  mutate(off_s = off_s * 2.04545) |>
  select(gameId, playId, frame, offId, off_x, off_y, off_s, off_dir) |>
  group_by(gameId, playId, frame) |>
  nest() |>
  ungroup()

rm(week, plays0, prep_players)

lev <- def |>
  inner_join(carrier, by = c("gameId", "playId", "frame")) |>
  inner_join(off, by = c('gameId', 'playId', 'frame'))

rm(info, carrier, def, off)
gc()

lev1 <- lev |>
  #Mapping function
  mutate(dist_to_carrier = sqrt((def_x - car_x)^2 + (def_y - car_y)^2)) |>
  unnest(data) |>
  rowwise() |>
  mutate(dist = sqrt((def_x - off_x)^2 + (def_y - off_y)^2)) |>
  ungroup() |>
  group_by(gameId, playId, frame, defId) |>
  slice(which.min(dist)) |>
  ungroup()

gc()

write.csv(lev1, "data/distance.csv", row.names = FALSE)


lev1 <- read.csv("data/distance.csv")
# Find Expected Position of QB in 0.5 Seconds
#Code is copied and modified from this repository:
#https://github.com/ritchi12/punt_returns_using_the_math_to_find_the_path/blob/main/Models/Code/model_data_prep.R

lev0 <- lev1 |>
  mutate(blocked = ifelse(dist < 2, 1, 0),
         #why am I dividing here??
         #cars = car_s/2.04545
         ) |>
  mutate(angle = ifelse(car_dir < 90, 90-car_dir, 
                        ifelse(car_dir > 90 & car_dir < 180, car_dir-90, 
                               ifelse(car_dir > 180 & car_dir < 270, 270-car_dir, car_dir-270)))) |>
  mutate(x_change = ifelse(car_dir < 180, sin((angle*pi)/180)*(car_s/2), -sin((angle*pi)/180)*(car_s/2))) |>
  mutate(y_change = ifelse(car_dir > 90 & car_dir < 270, -cos((angle*pi)/180)*(car_s/2), cos((angle*pi)/180)*(car_s/2))) |>
  mutate(x_car_exp = car_x+x_change) |>
  mutate(y_car_exp = car_y+y_change)

# Blocker Leverage
pRB = sqrt((lev0$x_car_exp - lev0$off_x)^2 + (lev0$y_car_exp - lev0$off_y)^2)
pRD = sqrt((lev0$x_car_exp - lev0$def_x)^2 + (lev0$y_car_exp - lev0$def_y)^2)
pBD = sqrt((lev0$def_x - lev0$off_x)^2 + (lev0$def_y - lev0$off_y)^2)

block_leverage = acos((pRB^2 + pRD^2 - pBD^2)/(2*pRB*pRD))
lev0$block_lev_deg = block_leverage*180/pi

# Defender Leverage
DRp = sqrt((lev0$x_car_exp - lev0$def_x)^2 + (lev0$y_car_exp - lev0$def_y)^2)
DR = sqrt((lev0$car_x - lev0$def_x)^2 + (lev0$car_y - lev0$def_y)^2)
DB = sqrt((lev0$def_x - lev0$off_x)^2 + (lev0$def_y - lev0$off_y)^2)
RB = sqrt((lev0$car_x - lev0$off_x)^2 + (lev0$car_y - lev0$off_y)^2)
RR = sqrt((lev0$car_x - lev0$x_car_exp)^2 + (lev0$car_y - lev0$y_car_exp)^2)

theta1 = acos((DB^2 + DR^2 - RB^2)/(2*DB*DR))
theta2 = acos((DR^2 + DRp^2 - RR^2)/(2*DR*DRp))

delta = theta2-theta1
lev0$def_lev_deg = delta*180/pi

lev0$block_wt = lev0$block_lev_deg * lev0$blocked
lev0$def_wt = lev0$def_lev_deg * lev0$blocked

final <- lev0 |>
  select(gameId, playId, frame, defId, offId, dist_to_carrier, dist_def_block = dist, 
         blocked, block_lev_deg, def_lev_deg, block_wt, def_wt)

write.csv(final, "data/leverage.csv", row.names = FALSE)