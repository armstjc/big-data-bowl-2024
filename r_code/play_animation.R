rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
nflreadr::.clear_cache()

library(tidyverse)
library(gganimate)
library(cowplot)
library(nflplotR)
library(nflfastR)
library(lubridate)
# library(ggvoronoi)
library(sf)

team <- week |>
  filter(possessionTeam=='CIN') 

rm(week)
gc()

example.play <- team |>
  rename(team = club) |>
  #game: 2022091103, play: 253
  filter(gameId==2022092506, playId==3467, possessionTeam=='CIN') |>
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))  |>
  mutate(snap = ifelse(event=='ball_snap', frameId, frameId[event=='ball_snap']),
         #ADD goal to go situations!!
         dnd = ifelse(down==1, paste("1ST &", yardsToGo, sep = " "), 
                      ifelse(down==2, paste("2ND &", yardsToGo, sep = " "), 
                             ifelse(down==3, paste("3RD &", yardsToGo, sep = " "), 
                                    ifelse(down==4, paste("4TH &", yardsToGo, sep = " "), NA)))),
         gameClock0 = ms(paste(gameClock, '.00', sep=''))
  ) |>
  fill(snap, .direction = "downup") |>
  group_by(nflId) |>
  mutate(ms0 = ifelse(row_number() - snap < 0, 0, (row_number() - snap)/1000)) |>
  ungroup()

ex <- example.play |>
  select(timebin=frameId, quarter, gameClock0, ms0) |>
  distinct() |>
  mutate(ms0 = trunc(ms(ms0)/10),
         #Difference between snap and current time
         diff = as.character(gameClock0 - ms0),
         diff = gsub(" ", ":", diff),
         diff = gsub("[MS]", "", diff),
         #Game Time (Qtr: Time)
         gt = ifelse(quarter==1, paste("1ST:", diff, sep = " "), 
                     ifelse(quarter==2, paste("2ND:", diff, sep = " "), 
                            ifelse(quarter==3, paste("3RD:", diff, sep = " "), 
                                   ifelse(quarter==4, paste("4TH:", diff, sep = " "), paste("OT:", diff, sep = " ")))))) |>
  select(timebin, gt)


mintm = min(example.play$team)
maxtm = max(example.play$team)

frames = sort(unique(example.play$frameId))

#Don't think this is needed
# library(sp)

carrier <- example.play |>
  filter(nflId == ballCarrierId) |>
  select(timebin = frameId, x_carrier = x)

#Functions
player_position1 <- function(df, frames){
  dfall <- df %>% filter(frameId == frames)  %>% 
    filter(team!="football") %>% select (team, x, y, jerseyNumber)
  colnames(dfall) <- c('ID','x','y', 'jerseyNumber')
  return(dfall)
}

ball_position1 <- function(df, frames){
  dfall <- df %>% filter(frameId == frames)  %>% 
    filter(team=="football") %>% select (team, x, y, jerseyNumber)
  colnames(dfall) <- c('ID','x','y', 'jerseyNumber')
  return(dfall)
}

fulldf=list()

for(i in seq_along(frames)){
  
  dplayer <- player_position1(df=example.play, frames[i]) #Gets positions of players
  ballpos <- ball_position1(df=example.play, frames[i])  #Gets position of ball
  dplayer$valx = 'player'
  ballpos$valx  = 'football'
  fulldf[[i]] = rbind(dplayer,ballpos)
}

fulldf = Map(cbind,fulldf,timebin=1:length(fulldf))
playdf = data.table::rbindlist(fulldf) |>
  left_join(ex, by = ('timebin'))

voronoidf <- playdf |>
  left_join(carrier, by = ('timebin')) |>
  filter(x_carrier - x >= -5 & x_carrier - x <= 25)

#Football Field
source("code/gg_field.R")

#Home Team
home_team = unique(example.play$homeTM)
home_score = unique(example.play$preSnapHomeScore)
home_col0 = unique(example.play$team_color[example.play$team==example.play$homeTM])
home_col = unique(example.play$team_color2[example.play$team==example.play$homeTM])
#Away Team
away_team = unique(example.play$awayTM)
away_score = unique(example.play$preSnapVisitorScore)
away_col0 = unique(example.play$team_color[example.play$team==example.play$awayTM])
away_col = unique(example.play$team_color2[example.play$team==example.play$awayTM])
#Possession Team
pos_team = unique(example.play$possessionTeam[example.play$team==example.play$possessionTeam])
pos_side = unique(example.play$side[example.play$team==example.play$possessionTeam])
pos_col0 = unique(example.play$team_color[example.play$team==example.play$possessionTeam])
pos_col = unique(example.play$team_color2[example.play$team==example.play$possessionTeam])
#Defensive Team
def_team = unique(example.play$defensiveTeam[example.play$team==example.play$defensiveTeam])
def_side = unique(example.play$side[example.play$team==example.play$defensiveTeam])
def_col0 = unique(example.play$team_color[example.play$team==example.play$defensiveTeam])
def_col = unique(example.play$team_color2[example.play$team==example.play$defensiveTeam])
play_direction = unique(example.play$playDirection)
#Fix LOS

#Hull Factor -> Character Team Value
playdf$ID <- ifelse(playdf$ID==1, mintm, 
                    ifelse(playdf$ID==2, maxtm, playdf$ID))
#Play Description
desc <-unique(example.play$playDescription)
#Line of Scrimmage (x, y)
losx <- unique(example.play$x[example.play$frameId==1 & example.play$team=='football'])
losy <- unique(example.play$y[example.play$frameId==1 & example.play$team=='football'])
#Yards to Go
ytg <- unique(example.play$yardsToGo)
#First Down Line
fd <- ifelse(play_direction=="left", losx - ytg, losx + ytg)
#Down and Distance
dnd <- unique(example.play$dnd)
x0 <- ifelse(play_direction=="left", losx - 2.25, losx + 2.25)
xend0 <- ifelse(play_direction=="left", losx + 2.125, losx - 2.125)
arrowx <- ifelse(play_direction=="left", losx - 2, losx + 2)
adjust <- ifelse(play_direction=="left", 0, 0.01)
yend0 <- ifelse(losy>26.665, 15.33, 38)
#Field Parameters
ydmax <- ifelse(play_direction=="left", (round(max(playdf$x)) + 5), (losx + 20))
ydmin <- ifelse(play_direction=="left", (losx - 20), (round(min(playdf$x)) - 5))
hw <- ydmin + (ydmax - ydmin)/2

# remotes::install_github("cran/rgeos", dependencies = TRUE, build_opts = c("--no-resave-data"))
# remotes::install_github("garretrc/ggvoronoi", dependencies = TRUE, build_opts = c("--no-resave-data"))

#Field Dimensions for Voronoi
outline.df <- data.frame(x = c(-10, 10, 10, -10),
                         y = c(-10, -10, 10, 10))


bound <- c(range(playdf$x), range(playdf$y))

#Animation
animate.play <- ggplot() + gg_field(yardmin = ydmin, yardmax = ydmax, endzone_color = "forestgreen") +
  #Voronoi
  ggforce::geom_voronoi_tile(data=voronoidf %>% filter(valx=="player"),
                             aes(x = x, y = y, fill = ID, group = -1), alpha = 0.4) +
  ggforce::geom_voronoi_segment(data=voronoidf %>% filter(valx=="player"),
                                aes(x = x, y = y)) +
  #Endzone Wordmarks
  nflplotR::geom_nfl_wordmarks(aes(x=5, y=53.33/2, team_abbr = home_team), angle = 90, width = 0.7) +
  nflplotR::geom_nfl_wordmarks(aes(x=115, y=53.33/2, team_abbr = home_team), angle = 270, width = 0.7) + 
  nflplotR::geom_nfl_logos(aes(x=60, y=53.33/2, team_abbr = home_team), width = 0.225) +
  #Line of Scrimmage & First Down 
  annotate("segment",x=c(losx, fd), y=0, xend=c(losx, fd), yend=53.33,
           lwd=1.3, col=c('blue4', 'yellow'), alpha = .7) +
  #Down & Distance
  annotate("rect", xmin = x0, xmax = xend0, ymin = yend0 - 1.5, ymax = yend0 + 1.5, fill = pos_col0,
           alpha=0.6) +
  geom_segment(aes(x = arrowx - adjust, xend = arrowx, y = yend0, yend = yend0), size = 1.75,
               alpha=0.6, color = pos_col,
               arrow = arrow(length = unit(0.6,"cm"))) +
  geom_segment(aes(x = losx - adjust, xend = losx, y = yend0, yend = yend0), size = 1.75,
               alpha=0.6, color = pos_col,
               arrow = arrow(length = unit(0.6,"cm"))) +
  geom_text(aes(x=losx, y = yend0 + 0.125), label = dnd, size = 3, fontface = 'bold', color='white') +
  #Background Color
  annotate("rect", xmin = ydmin + 1.5, xmax = ydmin + 9.43, ymin = 53.43, ymax = 58.33, fill = home_col0, color = home_col) +
  annotate("rect", xmin = ydmin + 9.57, xmax = ydmin + 17.5, ymin = 53.43, ymax = 58.33, fill = away_col0, col = away_col) +
  #Home Team
  nflplotR::geom_nfl_logos(aes(x=ydmin + 4, y=56, team_abbr = home_team), width = 0.075) +
  #Home Score
  geom_text(aes(x=ydmin + 8, y =56), label = home_score, size = 8, color='white') +
  #Away Team
  nflplotR::geom_nfl_logos(aes(x=ydmin + 12, y=56, team_abbr = away_team), width = 0.075) +
  #Away Score
  geom_text(aes(x=ydmin + 16, y = 56), label = away_score, size = 8, color="white") +
  #Time Remaining
  geom_text(data=playdf %>% filter(valx=="football"), aes(x=hw + 9, y = 56, label = gt), size = 8, color = 'white') +
  
  # #Convex Hulls for the Pass Protection and the Pass Rush
  # geom_polygon(data=playdf %>% filter(valx=="hull", ID!=def_team),
  #              aes(x = x, y = y, group = ID, fill = ID, color = ID), alpha = 0.5) + 
  #Player (OFF)
  geom_point(data=playdf %>% filter(valx=="player", ID!=def_team),
             aes(x = x, y = y,
                 fill = ID, color = ID), shape = 21, alpha = 0.7, size = 8) +
  #Player (DEF)
  geom_point(data=playdf %>% filter(valx=="player", ID==def_team),
             aes(x = x, y = y), 
             fill = def_col, color = def_col0, shape = 21, alpha = 0.7, size = 8) +
  #Jersey Number (OFF)
  geom_text(data=playdf %>% filter(valx=="player", ID!=def_team),
            aes(x = x, y = y, group = ID, label = jerseyNumber), color = 'white', 
            vjust = 0.36, size = 3.5) +
  #Jersey Number (DEF)
  geom_text(data=playdf %>% filter(valx=="player", ID==def_team),
            aes(x = x, y = y, group = ID, label = jerseyNumber), color = def_col0, 
            vjust = 0.36, size = 3.5) +
  #Play Description
  geom_text(aes(x=hw, y =-2), label = desc, size = 3, color = 'white') +
  #Team Colors
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl(type = "primary") +
  #Football
  geom_point(data=playdf %>% filter(valx=="football"),
             aes(x = x, y = y), color='chocolate4', size=3) +
  theme_nothing() + 
  transition_time(as.integer(timebin))  +
  ease_aes('linear') + 
  NULL

#Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(playdf$timebin))
animate(animate.play, fps = 10, nframe = play.length.ex)

#Save
anim_save("ex_pressure.gif", animation = last_animation())