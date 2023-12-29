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

#nflfastR
seasons <- 2022
nfl <- nflfastR::load_pbp(seasons) |>
  #filter first week to condense data
  dplyr::filter(season_type=='REG', week<10, !is.na(epa)) |>
  dplyr::select(new_gameId = game_id, gameId = old_game_id, playId = play_id, week, ep, epa, 
                air_epa, yac_epa, wp, wpa, pass,
                secstogo = half_seconds_remaining, yardline_100, posteam_score, defteam_score) |>
  dplyr::mutate(gameId = as.integer(gameId),
                playId = as.integer(playId))

#Tackle Data -> clean file to be merged with tracking and play data
tackle <- read.csv("data/tackles.csv")

#Players
players <- read.csv("data/players.csv")
#Smaller Player File
prep_players <- players |>
  select(nflId, height, weight)

#Plays
plays <- read.csv("data/plays.csv")
#Games
games <- read.csv("data/games.csv") |>
  select(gameId, homeTM = homeTeamAbbr, awayTM = visitorTeamAbbr)

plays0 <- plays |>
  inner_join(nfl, by = c('gameId', 'playId')) |>
  inner_join(games, by = c('gameId'))

rm(nfl, games, players, plays)
gc()

multmerge = function(path){
  filenames=list.files(path=path, full.names=TRUE)
  data.table::rbindlist(lapply(filenames, data.table::fread))
}


path <- "~/BDB24/week"
DF <- multmerge(path)
DF <- as.data.frame(DF)

#Filter pass and run plays separately??
week <- DF |>
  left_join(tackle, by = c('gameId', 'playId', 'nflId')) |>
  inner_join(plays0, by = c('gameId', 'playId')) |>
  mutate(x_std = ifelse(playDirection == "left", 120-x, x),
         y_std = ifelse(playDirection == "left", 160/3 - y, y),
         dir_s1 = ifelse(playDirection == "left" & dir < 180, dir+180,
                          ifelse(playDirection == "left" & dir > 180,dir-180, dir)),
         dir_s2 = abs(dir_s1 - 180),
         dir_std = ifelse(dir_s2<90, dir_s2 + 270, dir_s2 - 90),
         o_s1 = ifelse(playDirection == "left" & o < 180, o+180,
                          ifelse(playDirection == "left" & o > 180,o-180, o)),
         o_s2 = abs(o_s1 - 180),
         o_std = ifelse(o_s2<90, o_s2 + 270, o_s2 - 90)
         )


#add tackle once you figure out how to merge
rm(DF, tackle)
gc()
