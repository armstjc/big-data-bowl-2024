rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
nflreadr::.clear_cache()

library(tidyverse) # Data Cleaning, manipulation, summarization, plotting
library(nflplotR)
library(ggrepel) # better labels
library(ggthemes) # custom pre-built themes
library(scales)
library(signs)
library(ggrepel) # better labels
library(ggtext)
library(ggimage)
library(viridis)
library(gtExtras)
library(nflreadr)
library(gganimate)
library(cowplot)

setwd("~/BDB24")

#Clean DWA results
# multmerge = function(path){
#   filenames=list.files(path=path, full.names=TRUE)
#   data.table::rbindlist(lapply(filenames, data.table::fread))
# }
# 
# paths <- c(
#   "~/BDB24/DWA/wk1",
#   "~/BDB24/DWA/wk2",
#   "~/BDB24/DWA/wk3",
#   "~/BDB24/DWA/wk4",
#   "~/BDB24/DWA/wk5",
#   "~/BDB24/DWA/wk6",
#   "~/BDB24/DWA/wk7",
#   "~/BDB24/DWA/wk8",
#   "~/BDB24/DWA/wk9"
# )
# 
# # Use lapply to read and convert data frames
# data_frames <- lapply(paths, function(path) {
#   df <- multmerge(path)
#   as.data.frame(df)
# })
# 
# # Combine all data frames into one
# DWA <- do.call(rbind, data_frames)
# 
# DWA0 <- DWA |>
#   mutate(gameId = as.integer(gameId)) |>
#   group_by(gameId, playId) |>
#   arrange(frame) |>
#   mutate(lag_traj_x = lag(traj_x),
#          lag_traj_y = lag(traj_y),
#          lag_traj_x = ifelse(is.na(lag_traj_x), car_x, lag_traj_x),
#          lag_traj_y = ifelse(is.na(lag_traj_y), car_y, lag_traj_y),
#          dev = sqrt((car_x - lag_traj_x)^2 + (car_y - lag_traj_y)^2),
#          cum_dev = cumsum(dev)) |>
#   ungroup() |>
#   select(gameId, playId, frame, traj_x = lag_traj_x, traj_y = lag_traj_y, dev, cum_dev)
# 
# write.csv(DWA0, "C:/Users/Owner/Documents/BDB24/data/DWA.csv", row.names = FALSE)

carrier <- read.csv("data/carrier.csv")
DWA <- read.csv("data/DWA.csv")
players <- read.csv("data/players.csv")

#Team & EPA
nfl <- nflreadr::load_pbp(2022) |>
  #filter first week to condense data
  dplyr::filter(season_type=='REG', week<10, !is.na(epa)) |>
  dplyr::select(new_gameId = game_id, gameId = old_game_id, playId = play_id, week, 
                defteam, posteam, ep, epa, pass, qb_scramble, yards_gained, yards_after_catch) |>
  dplyr::mutate(gameId = as.integer(gameId),
                playId = as.integer(playId),
                yds_gain = ifelse(pass==1 & qb_scramble==0, yards_after_catch, yards_gained))

carrier0 <- carrier |>
  inner_join(nfl, by = c("gameId", "playId")) |>
  left_join(DWA0, by = c("new_gameId" = "gameId","playId", "frame"))

optimal <- carrier0 |>
  group_by(gameId, playId, ballCarrierId) |>
  summarize(
    dev = mean(dev, na.rm = TRUE),
    total_dev = sum(dev, na.rm = TRUE)
  ) |>
  ungroup() |>
  inner_join(nfl, by = c("gameId", "playId")) |>
  inner_join(players, by = c ("ballCarrierId" = "nflId")) |>
  group_by(ballCarrierId) |>
  summarise(
    name = last(displayName),
    team = last(posteam),
    pos = last(position),
    plays = n(),
    passes = sum(pass, na.rm = TRUE),
    runs = plays - passes,
    avg_dev = mean(dev, na.rm = TRUE),
    avg_dev_pass = mean(dev[pass==1 & qb_scramble==0], na.rm = TRUE),
    avg_dev_run = mean(dev[pass==0 | qb_scramble==1], na.rm = TRUE),
    avg_yds = mean(yds_gain, na.rm = TRUE),
    avg_yds_pass = mean(yds_gain[pass==1 & qb_scramble==0], na.rm = TRUE),
    avg_yds_run = mean(yds_gain[pass==0 | qb_scramble==1], na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(plays > 24)

write.csv(optimal, "data/deviation.csv", row.names = FALSE)

#Interactive Scatterplot Data for Optimal Path Deviation
optimal <- read.csv("data/deviation.csv")


#Random Forest Data Cleaning

# #Team & EPA
# nfl <- nflreadr::load_pbp(2022) |>
#   #filter first week to condense data
#   dplyr::filter(season_type=='REG', week<10, !is.na(epa)) |>
#   dplyr::select(new_gameId = game_id, gameId = old_game_id, playId = play_id, week,
#                 defteam, posteam, epa, pass, qb_scramble, yards_gained) |>
#   dplyr::mutate(gameId = as.integer(gameId),
#                 playId = as.integer(playId),
#                 true_pass = ifelse(pass==1 & qb_scramble==0, 1, 0))
# 
# #Players
# players <- read.csv("data/players.csv")
# #BDB Data
# leverage <- read.csv('data/leverage.csv')
# rel_vel <- read.csv('data/rel_vel.csv')
# imp_vel <- read.csv('data/imp_vel.csv')
# tackle <- read.csv("data/tackles.csv")
# tackle_frame <- read.csv("data/tackle_frame.csv")
# total <- leverage |>
#   left_join(tackle_frame, by = c("gameId", "playId", "frame")) |>
#   left_join(tackle, by = c("gameId", "playId", "defId" = "nflId")) |>
#   mutate_at(c(31:34), ~replace(., is.na(.), 0)) |>
#   arrange(gameId, playId, frame) |>
#   group_by(gameId, playId) |>
#   mutate(adj_frame = frame - min(frame),
#          seconds = adj_frame/10) |>
#   ungroup()
# rm(leverage, tackle_frame)
# gc()
# #Predictions
# predict <- read.csv("data/predictions.csv")
# 
# #off = c("RB", "WR", "FB")
# 
# tackler <- predict |>
#   inner_join(total, by = c("gameId", "playId", "defId", "adj_frame")) |>
#   inner_join(rel_vel, by = c("gameId", "playId", "defId", "frame" = "frameId")) |>
#   inner_join(imp_vel, by = c("gameId", "playId", "defId", "frame" = "frameId")) |>
#   inner_join(nfl, by = c("gameId", "playId")) |>
#   inner_join(players, by = c ("defId" = "nflId")) |>
#   mutate(true_tackle = ifelse(test_actual == is_tackle & is_tackle == 1, 1, 0)) |>
#   group_by(gameId, playId, frame, defteam) |>
#   mutate(sum_prob = 1 - prod(1 - test_pred_probs),
#          adj_sum_prob = map_dbl(row_number(), ~{
#            all_probs <- test_pred_probs
#            current_prob <- all_probs[.x]
#            other_probs <- all_probs[-.x]
#            1 - prod(1 - other_probs) })
#   ) |>
#   ungroup() |>
#   mutate(change_prob = ((sum_prob*100) - (adj_sum_prob*100))/(adj_sum_prob*100),
#          top = test_actual - test_pred_probs) |>
#   distinct() |>
#   filter(position!="FB", position!="RB", position!="WR")
# 
# rm(predict, total, players, rel_vel, imp_vel, tackle)
# gc()
# 
# write.csv(tackler, "data/tackler.csv", row.names = FALSE)

tackler <- read.csv("data/tackler.csv")

#Team & EPA
nfl <- nflreadr::load_pbp(2022) |>
  #filter first week to condense data
  dplyr::filter(season_type=='REG', week<10, !is.na(epa)) |>
  dplyr::mutate(gameId = as.integer(old_game_id),
                playId = as.integer(play_id),
                yds_gain = ifelse(pass==1 & qb_scramble==0, yards_after_catch, yards_gained)) |>
  dplyr::select(gameId, playId, desc, yrdln, yardline_100, yds_gain, pass, qb_scramble, air_yards, penalty) 

#Optimal Path Deviation
DWA <- read.csv("data/DWA.csv")


rf <- tackler |>
  inner_join(nfl, by = c("gameId", "playId")) |>
  select(new_gameId, gameId, playId, desc, event, frame, game_fold, ballCarrierId, car_x, yrdln, 
         posteam, yardline_100, yds_gain, sum_prob, adj_sum_prob, change_prob, penalty) |>
  distinct() |>
  filter(penalty!=1) |>
  inner_join(DWA, by = c("new_gameId" = "gameId", "playId", "frame")) |>
  group_by(gameId, playId) |>
  arrange(frame) |>
  #Account for LOS
  mutate(diff = car_x - lag(car_x),
         diff = ifelse(is.na(diff), 0, diff),
         yardage_gain = cumsum(diff),
         tot_yd_gain = last(yardage_gain)) |>
  ungroup() |>
  mutate(yds_remaining = tot_yd_gain - yardage_gain,
         game_play = paste0(new_gameId, "_", playId))

final <- rf |>
  select(gameId, playId, frame, ballCarrierId, yds_remaining, sum_prob, cum_dev) |>
  distinct()

set.seed(2005)
game_fold_table <- tibble(gameId = unique(final$gameId)) |>
  mutate(game_fold = sample(rep(1:10, length.out = n()), n()))

model <- final |>
  dplyr::inner_join(game_fold_table, by = "gameId")

rm(nfl, DWA, tackler)

#Data used for Random Forest model
write.csv(model, "C:/Users/Owner/Documents/BDB24/data/rf_data.csv", row.names = FALSE)



#Test Results from Model

results <- read.csv("data/rf_results.csv")

sqrt(sum((results$cond_median - results$yds_remaining)^2) / nrow(results))

players <- read.csv("data/players.csv")

#Team & EPA
nfl <- nflreadr::load_pbp(2022) |>
  #filter first week to condense data
  dplyr::filter(season_type=='REG', week<10, !is.na(epa)) |>
  dplyr::select(new_gameId = game_id, gameId = old_game_id, playId = play_id, week, 
                defteam, posteam, pass, qb_scramble, epa, yac_epa, yards_gained, yards_after_catch) |>
  dplyr::mutate(gameId = as.integer(gameId),
                playId = as.integer(playId),
                true_pass = ifelse(pass==1 & qb_scramble==0, 1, 0),
                yds_gain = ifelse(true_pass==1, yards_after_catch, yards_gained),
                new_epa = ifelse(true_pass==1, yac_epa, epa)) 

mae_test <- results |>
  inner_join(nfl, by=c("gameId", "playId")) |>
  #yds_gain<=5
  #yds_gain>5, yds_gain<=20
  #yds_gain>20
  filter(yds_gain>20)

sqrt(sum((mae_test$cond_median - mae_test$yds_remaining)^2) / nrow(mae_test))


#yoe for Defenses
team <- results |>
  group_by(gameId, playId, ballCarrierId) |>
  summarize(
    cum_dev = last(cum_dev),
    yds_remaining = mean(yds_remaining, na.rm=TRUE),
    cond_median = mean(cond_median, na.rm=TRUE)
  ) |>
  ungroup() |>
  inner_join(nfl, by=c("gameId", "playId")) |>
  group_by(defteam, true_pass) |>
  summarize(
    plays = n(),
    epa = mean(new_epa, na.rm=TRUE),
    cum_dev = mean(cum_dev),
    yds_gain = mean(yds_gain, na.rm=TRUE),
    yds_remaining = mean(yds_remaining, na.rm=TRUE),
    cond_median = mean(cond_median, na.rm=TRUE),
    yoe = yds_remaining - cond_median
  ) |>
  ungroup()

# write.csv(team, "C:/Users/Owner/Documents/BDB24/yoe_split.csv", row.names = FALSE)

run <- team |>
  filter(true_pass==0) |>
  ggplot(aes(x=yoe, y=epa)) +
  nflplotR::geom_mean_lines(aes(x0 = yoe, y0 = epa)) +
  #Linear line, 95% CI displayed
  ggpmisc::stat_poly_line(color="#5C53A5") +
  # #Pearson's R
  # ggpmisc::stat_correlation(label.x = "right", label.y = "bottom") +
  nflplotR::geom_nfl_logos(aes(team_abbr = defteam), width = 0.06, alpha = 0.8) +
  labs(x = "Yards Over Expected (YOE)",
       y = "Expected Points Added (EPA)",
       title = "Defense's YOE correlation with EPA",
       subtitle = "First nine weeks for run plays (2022)") +
  #Themes
  theme_classic()+
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.ticks.y = element_line(color = "#0e0e0e"),
    axis.ticks.x = element_line(color = "#0e0e0e"),
    axis.text = element_text(face = "bold", color = "#0e0e0e"),
    axis.line.y = element_line(lineend = "round", color = "#0e0e0e"),
    axis.line.x = element_line(lineend = "round", color = "#0e0e0e"),
    panel.grid.major.y = element_line(color = "#c1c1c1",
                                      linetype = "solid"),
    panel.grid.major.x = element_line(color = "#c1c1c1",
                                      linetype = "solid"),
    legend.position = "none",
    plot.background = element_rect(fill = "#f1f1f1"),
    panel.background = element_rect(fill = "#f1f1f1"),
    plot.margin = margin(5,15,10,10,"pt"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size=12)
  )+
  #Axis Ticks
  scale_y_reverse(breaks = scales::pretty_breaks(n = 8), 
                  labels = scales::label_number(accuracy = 0.01)) +
  scale_x_reverse(breaks = scales::pretty_breaks(n = 8), 
                  labels = scales::label_number(accuracy = 0.1))


pass <- team |>
  filter(true_pass==1) |>
  ggplot(aes(x=yoe, y=epa)) +
  nflplotR::geom_mean_lines(aes(x0 = yoe, y0 = epa)) +
  #Linear line, 95% CI displayed
  ggpmisc::stat_poly_line(color="#5C53A5") +
  # #Pearson's R
  # ggpmisc::stat_correlation(label.x = "right", label.y = "bottom") +
  nflplotR::geom_nfl_logos(aes(team_abbr = defteam), width = 0.06, alpha = 0.8) +
  labs(x = "Yards Over Expected (YOE)",
       y = "Expected Points Added (EPA)",
       title = "Defense's YOE correlation with EPA",
       subtitle = "First nine weeks for pass plays (2022)") +
  #Themes
  theme_classic()+
  theme(
    axis.title = element_text(size = 12, face = "bold"),
    axis.ticks.y = element_line(color = "#0e0e0e"),
    axis.ticks.x = element_line(color = "#0e0e0e"),
    axis.text = element_text(face = "bold", color = "#0e0e0e"),
    axis.line.y = element_line(lineend = "round", color = "#0e0e0e"),
    axis.line.x = element_line(lineend = "round", color = "#0e0e0e"),
    panel.grid.major.y = element_line(color = "#c1c1c1",
                                      linetype = "solid"),
    panel.grid.major.x = element_line(color = "#c1c1c1",
                                      linetype = "solid"),
    legend.position = "none",
    plot.background = element_rect(fill = "#f1f1f1"),
    panel.background = element_rect(fill = "#f1f1f1"),
    plot.margin = margin(5,15,10,10,"pt"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size=12)
  )+
  #Axis Ticks
  scale_y_reverse(breaks = scales::pretty_breaks(n = 8), 
                  labels = scales::label_number(accuracy = 0.01)) +
  scale_x_reverse(breaks = scales::pretty_breaks(n = 8), 
                  labels = scales::label_number(accuracy = 0.1))

cowplot::plot_grid(run, pass)


#yoe for Ball Carriers
library(gt)
library(gtExtras)

car <- results |>
  group_by(gameId, playId, ballCarrierId) |>
  summarize(
    # yoe = mean(yds_remaining - cond_median, na.rm=TRUE),
    frames = n(),
    avg_dev = last(cum_dev)/frames,
    yds_remaining = mean(yds_remaining, na.rm=TRUE),
    cond_median = mean(cond_median, na.rm=TRUE)
  ) |>
  ungroup() |>
  left_join(nfl, by=c("gameId", "playId")) |>
  inner_join(players, by = c("ballCarrierId" = "nflId")) |>
  group_by(ballCarrierId, true_pass) |>
  summarize(
    name = last(displayName),
    team = last(posteam),
    pos = last(position),
    plays = n(),
    epa = mean(new_epa, na.rm=TRUE),
    yds_gain = mean(yds_gain, na.rm=TRUE),
    avg_dev = mean(avg_dev),
    yds_remaining = mean(yds_remaining, na.rm=TRUE),
    cond_median = mean(cond_median, na.rm=TRUE),
    yoe = yds_remaining - cond_median
  ) |>
  ungroup() |>
  filter(plays>24)


car |>
  ggplot(aes(x = yoe, y = epa)) +
  geom_point() +
  facet_wrap(~true_pass)

car |>
  ggplot(aes(x = yoe, y = avg_dev)) +
  geom_point() +
  facet_wrap(~true_pass)

# write.csv(car, "C:/Users/Owner/Documents/BDB24/data/Shiny/carrier.csv", row.names = FALSE)

# Function to make names pretty with html
first_last <- function(name) {
  first <- paste0(stringr::word(name, 1), " ", sep = "")
  last <- stringr::str_trim(stringr::str_extract(name, " .*"))
  glue::glue("<div style='line-height:16px'><span style ='font-weight:bold;color:grey;font-size:14px'>{first}</span><span style='font-weight:bold;font-variant:small-caps;font-size:16px'>{last}</div>")
}

run <- car |>
  filter(true_pass==0) |> 
  arrange(-yoe) |>
  dplyr::slice(1:15) |>
  mutate(rank = dplyr::dense_rank(dplyr::desc(yoe))) |>
  select(rank, name, team, pos, plays, yds_gain, avg_dev, yoe)

run$name <- first_last(run$name)



run |>
  mutate(name = purrr::map(name, gt::html)) |>
  gt::gt() |>
  tab_header(title = md("**Ranking Ball Carriers on Run Plays**"),
             subtitle = "Minimum of 25 touches (2022)") |>
  cols_label(
    rank = md(""),
    name = md("**Player**"),
    team = md("**Team**"),
    pos = md("**Pos**"),
    plays = md("**Touches**"),
    yds_gain = md("**Yards**"),
    avg_dev = md("**DEV**"),
    yoe = md("**YOE**")
  ) |>
  fmt_number(columns = c(yds_gain), decimals = 1) |>
  fmt_number(columns = c(avg_dev, yoe), decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"),locations = cells_body(columns = c(rank, pos, plays, yds_gain, avg_dev, yoe))) |> 
  cols_align(align = "center", columns = c(rank, name, team, pos, plays, yds_gain, avg_dev, yoe)) |>
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size = "large"), 
            locations = cells_title(groups = "title")) |>
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size='small'),
            locations = list(cells_column_labels(everything()))) |>
  tab_style(style = cell_text(align = "center", size = "medium"), locations = cells_body()) |>
  tab_style(style = cell_text(font = c(google_font(name = "Times"),
                                       default_fonts())), locations = cells_body(columns = everything())) |>
  text_transform(locations = cells_body(c(team)),
                 fn = function(x) web_image(url = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", x, ".png"))) |>
  cols_width(c(rank, team) ~ px(50)) |>
  cols_width(c(pos, plays, yds_gain, avg_dev, yoe) ~ px(55)) |>
  cols_width(c(name) ~ px(155)) |>
  tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
            locations = list(cells_column_labels(columns = everything()))) |>
  gt::data_color(columns = vars(yoe),
                 colors = scales::col_numeric(palette = rcartocolor::carto_pal(n = 10, name = "Sunset"),
                                              domain = range(run$yoe)), alpha = 0.8) |>
  tab_options(data_row.padding = px(0.5), source_notes.font.size = 10) |>
  gtsave(filename = "run_bc.png", vwidth = 600, vheight = 1100)


pass <- car |>
  filter(true_pass==1) |> 
  arrange(-yoe) |>
  dplyr::slice(1:15) |>
  mutate(rank = dplyr::dense_rank(dplyr::desc(yoe))) |>
  select(rank, name, team, pos, plays, yds_gain, avg_dev, yoe)

pass$name <- first_last(pass$name)

pass |>
  mutate(name = purrr::map(name, gt::html)) |>
  gt::gt() |>
  tab_header(title = md("**Ranking Ball Carriers on Pass Plays**"),
             subtitle = "Minimum of 25 touches (2022)") |>
  cols_label(
    rank = md(""),
    name = md("**Player**"),
    team = md("**Team**"),
    pos = md("**Pos**"),
    plays = md("**Touches**"),
    yds_gain = md("**Yards**"),
    avg_dev = md("**DEV**"),
    yoe = md("**YOE**")
  ) |>
  fmt_number(columns = c(yds_gain), decimals = 1) |>
  fmt_number(columns = c(avg_dev, yoe), decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"),locations = cells_body(columns = c(rank, pos, plays, yds_gain, avg_dev, yoe))) |> 
  cols_align(align = "center", columns = c(rank, name, team, pos, plays, yds_gain, avg_dev, yoe)) |>
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size = "large"), 
            locations = cells_title(groups = "title")) |>
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size='small'),
            locations = list(cells_column_labels(everything()))) |>
  tab_style(style = cell_text(align = "center", size = "medium"), locations = cells_body()) |>
  tab_style(style = cell_text(font = c(google_font(name = "Times"),
                                       default_fonts())), locations = cells_body(columns = everything())) |>
  text_transform(locations = cells_body(c(team)),
                 fn = function(x) web_image(url = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", x, ".png"))) |>
  cols_width(c(rank, team) ~ px(50)) |>
  cols_width(c(pos, plays, yds_gain, avg_dev, yoe) ~ px(55)) |>
  cols_width(c(name) ~ px(155)) |>
  tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
            locations = list(cells_column_labels(columns = everything()))) |>
  gt::data_color(columns = vars(yoe),
                 colors = scales::col_numeric(palette = rcartocolor::carto_pal(n = 10, name = "Sunset"),
                 domain = range(pass$yoe)), alpha = 0.8) |>
  tab_options(data_row.padding = px(0.5), source_notes.font.size = 10) |>
  gtsave(filename = "pass_bc.png", vwidth = 600, vheight = 1100)


library(magick)
combined <- image_append(c(image_read("run_bc.png"), image_read("pass_bc.png")))

image_write(combined, path = "bc_total.png", format = "png")

#yoe for defenders
yoe <- results |>
  select(-c("sum_prob", "ballCarrierId", "game_fold"))

tackler_new <- tackler |>
  inner_join(yoe, by=c("gameId", "playId", "frame")) 
#Bring in team, EPA, etc

rm(tackler)
gc()

tackler0 <- tackler_new |>
  group_by(gameId, playId, defId) |>
  summarize(
    name = last(displayName),
    position = last(position),
    plays = n(),
    pass = mean(true_pass, na.rm=TRUE),
    yds_gain = mean(yards_gained, na.rm=TRUE),
    lev = sum(def_wt),
    pff_solo = mean(tackle, na.rm = TRUE),
    pff_assist = mean(assist, na.rm = TRUE),
    pff_miss = mean(pff_missedTackle, na.rm = TRUE),
    xtackle = mean(test_pred_probs),
    tackle = mean(test_actual),
    top = mean(top, na.rm=TRUE),
    contribution_pct = mean(change_prob, na.rm = TRUE),
    yds_remaining = mean(yds_remaining, na.rm=TRUE),
    cond_median = mean(cond_median, na.rm=TRUE),
    maxspeed = max(def_s),
    team = last(defteam)
  ) |>
  ungroup() |>
  group_by(defId) |>
  summarize(
    name = last(name),
    pos = last(position),
    plays = n(),
    team = last(team),
    maxspeed = mean(maxspeed),
    lev = mean(lev, na.rm=TRUE),
    Solo = sum(pff_solo),
    Assist = sum(pff_assist),
    Miss = sum(pff_miss, na.rm=TRUE),
    Tackle = sum(tackle),
    xTackle = sum(xtackle),
    TOP = sum(top, na.rm=TRUE),
    tackles_pass = sum(tackle[pass==1], na.rm=TRUE),
    tackles_run = sum(tackle[pass==0], na.rm=TRUE),
    con_pct_pass = mean(contribution_pct[pass==1], na.rm=TRUE),
    con_pct_run = mean(contribution_pct[pass==0], na.rm=TRUE),
    yoe_pass = mean(yds_remaining[pass==1 & tackle==1], na.rm=TRUE) - mean(cond_median[pass==1 & tackle==1], na.rm=TRUE),
    yoe_run = mean(yds_remaining[pass==0 & tackle==1], na.rm=TRUE) - mean(cond_median[pass==0 & tackle==1], na.rm=TRUE),
    doT_pass = mean(yds_gain[pass==1 & tackle==1], na.rm=TRUE),
    doT_run = mean(yds_gain[pass==0 & tackle==1], na.rm=TRUE),
    tackle_rate = Tackle/plays
  ) |>
  ungroup() |>
  group_by(pos) |>
  mutate(mlev = mean(lev)) |>
  ungroup() |>
  mutate(resistance = mlev - (lev)) |>
  select(name, pos, plays, team, Solo, Assist, Miss, xTackle, Tackle, TOP, 
         tackles_pass, tackles_run, doT_pass, doT_run, con_pct_pass, con_pct_run, yoe_pass, yoe_run
  ) |>
  filter(plays>24)

# write.csv(tackler0, "C:/Users/Owner/Documents/BDB24/tackler_final.csv", row.names = FALSE)
tackler0 <- read.csv("tackler_final.csv")

tackler0 |>
  filter(plays>124) |>
  ggplot(aes(x = TOP, y = yoe_run + yoe_pass)) +
  #Linear line, 95% CI displayed
  ggpmisc::stat_poly_line() +
  #Pearson's R
  ggpmisc::stat_correlation(label.x = "right", label.y = "bottom") +
  geom_point() +
  scale_y_reverse() 

tackler0 |>
  filter(plays>124) |>
  ggplot(aes(x = con_pct_run, y = yoe_run)) +
  #Linear line, 95% CI displayed
  ggpmisc::stat_poly_line() +
  #Pearson's R
  ggpmisc::stat_correlation(label.x = "right", label.y = "bottom") +
  geom_point() +
  scale_y_reverse() 

tackler0 |>
  filter(plays>124) |>
  ggplot(aes(x = con_pct_pass, y = yoe_pass)) +
  #Linear line, 95% CI displayed
  ggpmisc::stat_poly_line() +
  #Pearson's R
  ggpmisc::stat_correlation(label.x = "right", label.y = "bottom") +
  geom_point() +
  scale_y_reverse()

run <- tackler0 |>
  filter(tackles_run>9) |>
  arrange(yoe_run) |>
  dplyr::slice(1:15) |>
  mutate(rank = dplyr::dense_rank(yoe_run)) |>
  select(rank, name, team, pos, tackles_run, doT_run, con_pct_run, yoe_run)

run$name <- first_last(run$name)

run |>
  mutate(name = purrr::map(name, gt::html)) |>
  gt::gt() |>
  tab_header(title = md("**Ranking Defenders on Run Plays**"),
             subtitle = "Minimum of 10 tackles (2022)") |>
  cols_label(
    rank = md(""),
    name = md("**Player**"),
    team = md("**Team**"),
    pos = md("**Pos**"),
    tackles_run = md("**Tackles**"),
    doT_run = md("**doT**"),
    con_pct_run = md("**CON%**"),
    yoe_run = md("**YOE**")
  ) |>
  fmt_percent(columns = c(con_pct_run), decimals = 1) |>
  fmt_number(columns = c(doT_run), decimals = 1) |>
  fmt_number(columns = c(yoe_run), decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"),locations = cells_body(columns = c(rank, pos, tackles_run, doT_run, con_pct_run, yoe_run))) |> 
  cols_align(align = "center", columns = c(rank, name, team, pos, tackles_run, doT_run, con_pct_run, yoe_run)) |>
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size = "large"), 
            locations = cells_title(groups = "title")) |>
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size='small'),
            locations = list(cells_column_labels(everything()))) |>
  tab_style(style = cell_text(align = "center", size = "medium"), locations = cells_body()) |>
  tab_style(style = cell_text(font = c(google_font(name = "Times"),
                                       default_fonts())), locations = cells_body(columns = everything())) |>
  text_transform(locations = cells_body(c(team)),
                 fn = function(x) web_image(url = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", x, ".png"))) |>
  cols_width(c(rank, team) ~ px(50)) |>
  cols_width(c(pos, tackles_run, doT_run, con_pct_run, yoe_run) ~ px(55)) |>
  cols_width(c(name) ~ px(165)) |>
  tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
            locations = list(cells_column_labels(columns = everything()))) |>
  gt::data_color(columns = c(yoe_run),
                 colors = scales::col_numeric(palette = rcartocolor::carto_pal(n = 10, name = "Sunset"),
                                              domain = range(run$yoe_run)), alpha = 0.8) |>
  tab_options(data_row.padding = px(0.5), source_notes.font.size = 10) |>
  gtsave(filename = "run_def.png", vwidth = 600, vheight = 1100)


pass <- tackler0 |>
  filter(tackles_pass>9) |>
  arrange(yoe_pass) |>
  dplyr::slice(1:15) |>
  mutate(rank = dplyr::dense_rank(yoe_pass)) |>
  select(rank, name, team, pos, tackles_pass, doT_pass, con_pct_pass, yoe_pass)

pass$name <- first_last(pass$name)

pass |>
  mutate(name = purrr::map(name, gt::html)) |>
  gt::gt() |>
  tab_header(title = md("**Ranking Defenders on Pass Plays**"),
             subtitle = "Minimum of 10 tackles (2022)") |>
  cols_label(
    rank = md(""),
    name = md("**Player**"),
    team = md("**Team**"),
    pos = md("**Pos**"),
    tackles_pass = md("**Tackles**"),
    doT_pass = md("**doT**"),
    con_pct_pass = md("**CON%**"),
    yoe_pass = md("**YOE**")
  ) |>
  fmt_percent(columns = c(con_pct_pass), decimals = 1) |>
  fmt_number(columns = c(doT_pass), decimals = 1) |>
  fmt_number(columns = c(yoe_pass), decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"),locations = cells_body(columns = c(rank, pos, tackles_pass, doT_pass, con_pct_pass, yoe_pass))) |> 
  cols_align(align = "center", columns = c(rank, name, team, pos, tackles_pass, doT_pass, con_pct_pass, yoe_pass)) |>
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size = "large"), 
            locations = cells_title(groups = "title")) |>
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size='small'),
            locations = list(cells_column_labels(everything()))) |>
  tab_style(style = cell_text(align = "center", size = "medium"), locations = cells_body()) |>
  tab_style(style = cell_text(font = c(google_font(name = "Times"),
                                       default_fonts())), locations = cells_body(columns = everything())) |>
  text_transform(locations = cells_body(c(team)),
                 fn = function(x) web_image(url = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", x, ".png"))) |>
  cols_width(c(rank, team) ~ px(50)) |>
  cols_width(c(pos, tackles_pass, doT_pass, con_pct_pass, yoe_pass) ~ px(55)) |>
  cols_width(c(name) ~ px(165)) |>
  tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
            locations = list(cells_column_labels(columns = everything()))) |>
  gt::data_color(columns = c(yoe_pass),
                 colors = scales::col_numeric(palette = rcartocolor::carto_pal(n = 10, name = "Sunset"),
                                              domain = range(pass$yoe_pass)), alpha = 0.8) |>
  tab_options(data_row.padding = px(0.5), source_notes.font.size = 10) |>
  gtsave(filename = "pass_def.png", vwidth = 600, vheight = 1100)

combined <- image_append(c(image_read("run_def.png"), image_read("pass_def.png")))
image_write(combined, path = "def_total.png", format = "png")
 