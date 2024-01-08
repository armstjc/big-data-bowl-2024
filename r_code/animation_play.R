# packages ----------------------------------------------------------------

library(arrow)
library(dplyr)
library(ggplot2)
library(sportyR)
library(nflplotR)
library(ggalt)
library(ggstar)
library(readr)
library(nflfastR)
library(purrr)
library(grid)
library(ggpmisc)
library(ggnewscale)
library(tidyr)
library(ggforce)
library(lubridate)

# custom shape ------------------------------------------------------------

custom_shape <- function(angle = 0, fill_circle = "yellow") {
  vp <- viewport(angle = angle)
  grobTree(
    rectGrob(
      x = unit(0.5, "npc"),
      y = unit(0.5, "npc"),
      width = unit(3, "mm"),
      height = unit(3 / 2, "mm"),
      gp = gpar(fill = "black"),
      vjust = 0,
      vp = vp
    ),
    circleGrob(
      x = unit(0.5, "npc"),
      y = unit(0.5, "npc"),
      r = unit(3 / 2, "mm"),
      gp = gpar(fill = fill_circle),
      vp = vp
    )
  )
}

# data --------------------------------------------------------------------

teams_colors2 <- nflfastR::teams_colors_logos %>%
  mutate(bw_text = coloratio::cr_choose_bw(team_color))

schedule <- nflreadr::load_schedules() %>%
  select(game_id, week, old_game_id,home_team, away_team)

play_desc <-  read_parquet("data_processed/plays.parquet")  %>%
  mutate(gameId = as.character(gameId)) %>%
  left_join(schedule, by = c("gameId" = "old_game_id")) %>%
  filter(game_id == "2022_02_MIA_BAL",
         playId == 2801,
  ) %>%
  mutate(down_text =
           case_when(
             down == 1 ~ "1st",
             down == 2 ~ "2nd",
             down == 3 ~ "3rd",
             down == 4 ~ "4th"),
         down_all = paste(down_text,"&",yardsToGo),
         quarter_text =
           case_when(
             quarter == 1 ~ "1st",
             quarter == 2 ~ "2nd",
             quarter == 3 ~ "3rd",
             quarter == 4 ~ "4th",
             quarter == 5 ~ "OT"),
         clock_min = stringr::str_extract(gameClock, "^[:digit:]{2}"),
         clock_sec = stringr::str_extract(gameClock, paste0("(?<=^",clock_min,"\\:)[:digit:]{2}")),
         clock_text = paste(clock_min, clock_sec, sep = ":"),
         new_clock = lubridate::ms(paste(clock_text, '.00', sep='')))


tracking <- read_parquet("data_processed/tracking_week_2.parquet")


play_track <- tracking %>%
  mutate(gameId = as.character(gameId)) %>%
  left_join(schedule, by = c("gameId" = "old_game_id")) %>%
  filter(game_id == "2022_02_MIA_BAL",
         playId == 2801
  ) %>%
  mutate(
    x = 120-x+1,
    y = 53-y+1,
    o = ifelse(o < 180, o+180, o-180),
    dir = ifelse(dir < 180, dir+180, dir-180)
  )


time_snap <- play_track %>%
  filter(event =='ball_snap') %>%
  slice_head(n = 1) %>%
  pull(time)

play_track2 <- play_track %>%
  mutate(new_time = round(difftime(time_snap, time)),
         new_clock = as.numeric(as.numeric(play_desc$new_clock)+new_time),
         min = (new_clock / 60) |> floor(),
         min = formatC(min, width = 2, format = "d", flag = "0"),
         sec = new_clock %% 60,
         new_clock = paste(min,sec,sep = ":")
  ) %>%
  select(-c(min, sec, new_time)) %>%
  filter(is.na(nflId))

tackle <- read_parquet("data_processed/tackles.parquet")%>%
  mutate(gameId = as.character(gameId)) %>%
  left_join(schedule, by = c("gameId" = "old_game_id")) %>%
  filter(game_id == "2022_02_MIA_BAL",
         playId == 2801
  )



table <- tibble(
  xmin = min(play_track$x)-5,
  xmax = max(play_track$x)+5,
  home_nick = teams_colors2 %>%
    filter(team_abbr %in% play_desc$home_team)%>%
    pull(team_nick),
  away_nick = teams_colors2 %>%
    filter(team_abbr %in% play_desc$away_team)%>%
    pull(team_nick),
  home_color = teams_colors2 %>%
    filter(team_abbr %in% play_desc$home_team)%>%
    pull(team_color),
  home_color_bw = teams_colors2 %>%
    filter(team_abbr %in% play_desc$home_team)%>%
    pull(bw_text),
  away_color = teams_colors2 %>%
    filter(team_abbr %in% play_desc$away_team)%>%
    pull(team_color),
  away_color_bw = teams_colors2 %>%
    filter(team_abbr %in% play_desc$away_team)%>%
    pull(bw_text),
  off_color = teams_colors2 %>%
    filter(team_abbr %in% play_desc$possessionTeam)%>%
    pull(team_color),
  off_bw = teams_colors2 %>%
    filter(team_abbr %in% play_desc$possessionTeam)%>%
    pull(bw_text),
  def_color = teams_colors2 %>%
    filter(team_abbr %in% play_desc$defensiveTeam)%>%
    pull(team_color),
  def_bw = teams_colors2 %>%
    filter(team_abbr %in% play_desc$defensiveTeam)%>%
    pull(bw_text)
)



limits_view <- play_track2 %>%
  filter(is.na(nflId)) %>%
  group_by(frameId) %>%
  summarise(
    xmin = x+10,
    xmax = x-25,
  ) %>%
  mutate(
    xmin = ifelse(xmin < 34, 34, xmin),
    xmax = ifelse(xmax < -1, -1, xmax)
  )

# plot data --------------------------------------------------------------------


off <- play_track %>%
  filter(!is.na(nflId),
         club %in% play_desc$possessionTeam) %>%
  mutate(
    shape = purrr::map2(
      -o,
      table$off_color,
      custom_shape
    )
  )

def <- play_track %>%
  filter(!is.na(nflId),
         club %in% play_desc$defensiveTeam)  %>%
  mutate(
    shape = purrr::map2(
      -o,
      table$def_color,
      custom_shape
    )
  )

ball <- play_track %>%
  filter(is.na(nflId))

ball_carrier <-  play_track %>%
  filter(nflId %in% play_desc$ballCarrierId)

frame_carrier <- ball_carrier %>%
  filter(event == "handoff") %>%
  pull(frameId)


# influence area ----------------------------------------------------------

influence_area <- readr::read_csv("data/influence_df_2022_02_MIA_BAL.csv")

influence_area2 <- influence_area %>%
  mutate(x = 120-x+1,
         y = 53-y+1) %>%
  filter(value > 0.4)


# optimal path arrow ------------------------------------------------------

arrow_path <- readr::read_csv("data/arrow.csv")

arrow_path2 <- arrow_path %>%
  pivot_longer(
    cols = starts_with("arrow_"),
    names_to = c(".value", "set"),
    names_pattern = "arrow_(.)(\\d+)"
  ) %>%
  mutate(x = 120-x+1,
         y = 53-y+1
  )



# plot --------------------------------------------------------------------
# save all frames of play

for (i in 1:122){


  cli::cli_alert_info("Frame {i}")

  limits_view_frame <- limits_view %>%
    filter(frameId == i)

  plot_test <- geom_football("NFL",
                             display_range = "in_bounds_only",
                             x_trans = 60,
                             y_trans = 26.65,
                             color_updates = list(
                               field_apron = "#21ae5f",
                               offensive_half = "#21ae5f",
                               defensive_half = "#21ae5f",
                               offensive_endzone = "#177b43",
                               defensive_endzone = "#177b43"
                             ),
                             xlims = c(
                               limits_view_frame$xmax,
                               limits_view_frame$xmin+2)
  )+
    #logo middle
    nflplotR::geom_nfl_logos(
      data = play_desc,
      aes(x = 60, y = 26.65,
          team_abbr = home_team),
      width = 0.2
    )+
    #logo endzone left
    nflplotR:: geom_nfl_wordmarks(
      data = play_desc,
      aes(x = 5, y = 26.65,
          team_abbr = home_team),
      angle = 90,
      width = 0.8
    )+
  #path ball_carrier
  geom_bspline2(data = ball_carrier %>%
                  rename(frame = frameId) %>%
                  filter(frame <= i),
                aes(x = x, y = y,
                    color = s
                ),
                linewidth = 1.7,
                n = 200,
                lineend = "round",
                alpha = 1)+
    #influence area
    geom_contour_filled(
      data = influence_area2 %>%
        filter(frame == i),
      aes(x=x,y=y,
          z = value),
      binwidth = 0.05,
      alpha = 0.5,
      na.rm = TRUE)+
    scale_fill_manual(limits = factor(c(  "(0.70, 0.75]",
                                          "(0.75, 0.80]",
                                          "(0.80, 0.85]",
                                          "(0.85, 0.90]",
                                          "(0.90, 0.95]",
                                          "(0.95, 1.00]")),
                      values = c(
                        "(0.70, 0.75]" = "#440154",
                        "(0.75, 0.80]" = "#414487",
                        "(0.80, 0.85]" = "#2A788E",
                        "(0.85, 0.90]" = "#22A884",
                        "(0.90, 0.95]" = "#7AD151",
                        "(0.95, 1.00]" = "#FDE725"
                      ),
                      na.value = NA)+
    scale_color_distiller(palette = "YlOrRd",direction = 1)+
    ggnewscale::new_scale_color()+
    ggnewscale::new_scale_fill()+
    #LOS
    geom_segment(
      data = play_desc,
      aes(x = 100-yardlineNumber+10,
          xend = 100-yardlineNumber+10,
          y = 0.05,
          yend = 53.25),
      color = "#000aa0"
    )+
    #1stdown
    geom_segment(
      data = play_desc,
      aes(x = 100-yardlineNumber+10-yardsToGo,
          xend = 100-yardlineNumber+10-yardsToGo,
          y = 0.05,
          yend = 53.25),
      color = "#FDE725"
    )+
  #offense
  geom_segment(
    data = ball_carrier%>%
      filter(frameId %in% i),
    aes(x = x,
        xend = x+(s * cos((-dir+90)*pi/ 180)),
        y = y,
        yend = y+(s * sin((-dir+90)*pi/ 180))),
    arrow = arrow(type = "closed", length = unit(0.05, "inches")),
    color = "white"
  )+
    #optimal path
    geom_line(
      data = arrow_path2%>%
        filter(frame %in% i),
      aes(x = x,
          y = y),
      color = "#7a0177",
      inherit.aes = FALSE
    )+
    #optimal path point
    geom_point(
      data = arrow_path2%>%
        filter(frame %in% i,
               set == 7),
      aes(x = x,
          y = y),
      color = "#7a0177",inherit.aes = FALSE
    )+
    geom_grob(data = off %>%
                filter(frameId %in% i),
              aes(x, y, label = shape))+
    geom_text(data = off %>%
                filter(frameId %in% i),
              color = table$off_bw[1],
              size = 2,
              aes(x = x, y = y,
                  angle = 90,
                  label = jerseyNumber
              ))+
    #defense
    geom_grob(data = def %>%
                filter(frameId %in% i),
              aes(x, y, label = shape))+
    geom_text(data = def %>%
                filter(frameId %in% i),
              color = table$def_bw[1],
              size = 2,
              aes(x = x, y = y,
                  angle = 90,
                  label = jerseyNumber
              ))+
    #ball
    ggstar::geom_star(data = ball %>%
                        filter(frameId %in% i),
                      aes(x = x, y = y),
                      starshape = 25,
                      angle = 90,
                      fill = "#825736",
                      color = "black"
    )+
  ##boxscore
  #away box name
  annotate(
    "rect",
    xmin = limits_view_frame$xmin, xmax = limits_view_frame$xmin+2,
    ymin = 13+1.5, ymax = 17+1.5,
    fill = table$away_color, color = NA
  )+
    nflplotR::geom_nfl_logos(
      data = play_desc,
      aes(x = limits_view_frame$xmin+1,
          y = 15.5+1.5,
          team_abbr = "MIA"),
      angle = 90,
      height = 0.05,
      alpha = 0.3
    )+
    #away box score
    annotate(
      "rect",
      xmin = limits_view_frame$xmin, xmax = limits_view_frame$xmin+2,
      ymin = 17+1.5, ymax = 19+1.5,
      fill = colorspace::darken(table$away_color,0.2), color = NA
    )+
    #away nick
    annotate(
      "text",
      x = limits_view_frame$xmin+1, y = 13.5+1.5,
      hjust = 0,
      label = play_desc$away_team,
      angle = 90,
      color = table$away_color_bw,
      size = 2.5,
      family = "Product Sans"
    )+
    #away score
    annotate(
      "text",
      x = limits_view_frame$xmin+1, y = 18+1.5,
      label = toupper(play_desc$preSnapVisitorScore),
      angle = 90,
      color = table$away_color_bw,
      size = 2.5,
      family = "Product Sans"
    )+
    #home box name
    annotate(
      "rect",
      xmin = limits_view_frame$xmin, xmax = limits_view_frame$xmin+2,
      ymin = 19+1.5, ymax = 23+1.5,
      fill = table$home_color, color = NA
    )+
    nflplotR::geom_nfl_logos(
      data = play_desc,
      aes(x = limits_view_frame$xmin+1,
          y = 21.5+1.5,
          team_abbr = "BAL"),
      angle = 90,
      height = 0.05,
      alpha = 0.3
    )+
    #home box score
    annotate(
      "rect",
      xmin = limits_view_frame$xmin, xmax = limits_view_frame$xmin+2,
      ymin = 23+1.5, ymax = 25+1.5,
      fill = colorspace::darken(table$home_color,0.2), color = NA
    )+
    #home nick
    annotate(
      "text",
      x = limits_view_frame$xmin+1, y = 19.5+1.5,
      hjust = 0,
      label = play_desc$home_team,
      angle = 90,
      color = table$home_color_bw,
      size = 2.5,
      family = "Product Sans"
    )+
    #home score
    annotate(
      "text",
      x = limits_view_frame$xmin+1, y = 24+1.5,
      label = toupper(play_desc$preSnapHomeScore),
      angle = 90,
      color = table$home_color_bw,
      size = 2.5,
      family = "Product Sans"
    )+
    #box time
    annotate(
      "rect",
      xmin = limits_view_frame$xmin, xmax = limits_view_frame$xmin+2,
      ymin = 25+1.5, ymax = 35-1.5,
      fill = "black", color = NA,
      size = 2.5
    )+
    #text time
    annotate(
      "text",
      x = limits_view_frame$xmin+1, y = 30,
      label = paste(play_desc$quarter_text, play_track2 %>%
                      filter(frameId == i) %>%
                      pull(new_clock)),
      angle = 90,
      color = "white",
      size = 2.5,
      family = "Product Sans"
    )+
    #box down
    annotate(
      "rect",
      xmin = limits_view_frame$xmin, xmax = limits_view_frame$xmin+2,
      ymin = 35-1.5, ymax = 40-1.5,
      fill = "grey", color = NA
    )+
    #down text
    annotate(
      "text",
      x = limits_view_frame$xmin+1, y = 37.5-1.5,
      label = play_desc$down_all,
      angle = 90,
      color = "black",
      size = 2.5,
      family = "Product Sans"
    )+
    scale_fill_nfl(type = "primary",guide = "none")+
    scale_color_nfl(type = "secondary")+
    scale_linetype_identity()+
    theme(
      legend.position = "none"
    )+
    NULL

  cli::cli_alert_info("Saving")

  ggsave(plot = plot_test, filename = paste0("figures/kaggle_animation/MIA_BAL_frame_",i,".png"),
         width = 2.95, height = 4.3, dpi = 600)
}


# create gif --------------------------------------------------------------

# get all figures from folder and combine into a gif

filenames <- paste0("figures/kaggle_animation/MIA_BAL_frame_",6:122,".png")
m <- magick::image_rotate(magick::image_read(filenames[1]),90)

for (i in 2:length(filenames)){
  m <- c(m, magick::image_rotate(magick::image_read(filenames[i]),90))
}
m_final <- magick::image_animate(m, fps = 10,optimize = TRUE, dispose = "background")
magick::image_write(m_final, "figures/animation_kaggle.gif")
