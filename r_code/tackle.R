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

# #Team & EPA
# nfl <- nflreadr::load_pbp(2022) |>
#   #filter first week to condense data
#   dplyr::filter(season_type=='REG', week<10, !is.na(epa)) |>
#   dplyr::mutate(gameId = as.integer(old_game_id),
#                 playId = as.integer(play_id),
#                 play_type = ifelse(pass==1 & qb_scramble==0, 1, 
#                                    ifelse(pass==1 & qb_scramble==1, 2, 3))) |>
#   dplyr::select(gameId, playId, play_type)
# 
# leverage <- read.csv('data/leverage.csv')
# rel_vel <- read.csv('data/rel_vel.csv')
# imp_vel <- read.csv('data/imp_vel.csv')
# tackle <- read.csv("data/tackles.csv")
# tackle_frame <- read.csv("data/tackle_frame.csv")
# 
# tackle0 <- leverage |>
#   left_join(tackle_frame, by = c("gameId", "playId", "frame")) |>
#   left_join(tackle, by = c("gameId", "playId", "defId" = "nflId")) |>
#   mutate_at(c(31:34), ~replace(., is.na(.), 0)) |>
#   group_by(gameId, playId, defId) |>
#   slice(1) |>
#   ungroup() |>
#   arrange(gameId, playId) |>
#   group_by(defId) |>
#   mutate(lag_tackle = lag(tackle),
#          lag_tackle = ifelse(is.na(lag_tackle), 0 , lag_tackle),
#          cumtackle = cumsum(lag_tackle)) |>
#   ungroup() |>
#   group_by(gameId, playId) |>
#   arrange(desc(cumtackle), dist_to_carrier) |>
#   mutate(tackle_rank = 1:n()) |>
#   ungroup() |>
#   select(gameId, playId, defId, tackle, assist, forcedFumble,
#          pff_missedTackle, cumtackle, tackle_rank) |>
#   distinct()
# 
# #switch to leverage
# final <- leverage |>
#   inner_join(rel_vel, by = c("gameId", "playId", "defId", "frame" = "frameId")) |>
#   inner_join(imp_vel, by = c("gameId", "playId", "defId", "frame" = "frameId")) |>
#   left_join(tackle_frame, by = c("gameId", "playId", "frame")) |>
#   left_join(tackle0, by = c("gameId", "playId", "defId")) |>
#   inner_join(nfl, by = c("gameId", "playId")) |>
#   #Frames Converted to Seconds
#   arrange(gameId, playId, frame) |>
#   group_by(gameId, playId) |>
#   mutate(car_impact = ifelse(dist_to_carrier<=2, bc_impact, 0),
#          tot_tackle = ifelse(tackle == 1 | assist == 1, 1, 0),
#          adj_frame = frame - min(frame),
#          seconds = adj_frame/10) |>
#   ungroup()
# 
# final0 <- final |>
#   select(gameId, playId, defId, adj_frame, tackle = tot_tackle, dist_to_carrier, tackle_rank, play_type,
#          def_x, def_y, def_joules, car_x, car_y, car_joules, car_impact, seconds, block_wt, relative_velo)
# 
# rm(nfl, leverage, rel_vel, imp_vel, tackle, tackle_frame, tackle0)
# gc()

library(caTools)
library(xgboost)
library(caret)
library(SHAPforxgboost)

# set.seed(1985)
# game_fold_table <- tibble(gameId = unique(final0$gameId)) |>
#   mutate(game_fold = sample(rep(1:10, length.out = n()), n()))
# 
# model <- final0 |>
#   dplyr::inner_join(game_fold_table, by = "gameId")
# 
# #Test Statistical Significance and Multicollinearity with simple Logit Model
# logit_model <- glm(tackle ~ dist_to_carrier + seconds + play_type + def_joules + car_joules +
#                      car_impact + tackle_rank + block_wt + relative_velo,
#                    data = model, family = "binomial")
# 
# summary(logit_model)
# with(summary(logit_model), 1 - deviance/null.deviance)
# car::vif(logit_model)
# 
# write.csv(model, "data/model.csv", row.names = FALSE)

model <- read.csv("data/model.csv")

xgb_prob_preds <- 
  map_dfr(unique(model$game_fold), 
          function(test_fold) {
            test_data <- model %>% filter(game_fold == test_fold)
            train_data <- model %>% filter(game_fold != test_fold)
            
            y_train <- as.integer(train_data$tackle)
            y_test <- as.integer(test_data$tackle)
            X_train <- train_data |> select(-tackle, -gameId, -playId, -defId, - adj_frame, -game_fold)
            X_test <- test_data |> select(-tackle, -gameId, -playId, -defId, -adj_frame, -game_fold)
            
            xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
            xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
            
            params <- list(
              objective = "binary:logistic",
              learning_rate = 0.03,
              subsample = 0.7,
              reg_lambda = 2,
              max_depth = 3
            )
            
            fit_xgb <- xgb.train(
              params,
              data = xgb_train,
              watchlist = list(valid = xgb_test),
              eval_metric = "aucpr",
              early_stopping_rounds = 100,
              print_every_n = 250,
              nrounds = 250 # early stopping
            )
            
            xgb_preds <- as.numeric(predict(fit_xgb, as.matrix(X_test), reshape = TRUE))
            
            # Return tibble of holdout results:
            tibble(test_pred_probs = xgb_preds,
                   test_actual = test_data$tackle,
                   adj_frame = test_data$adj_frame,
                   game_fold = test_fold,
                   gameId = test_data$gameId,
                   playId = test_data$playId,
                   defId = test_data$defId)
          }) 

gc() 
library(MLmetrics)

f1_scores <- sapply(seq(0.01, 0.50, .01), function(thresh) F1_Score(xgb_prob_preds$test_actual, ifelse(xgb_prob_preds$test_pred_probs >= thresh, 1, 0), positive = 1))
which.max(f1_scores) #20
max_f1 = which.max(f1_scores)/100 #20

predict <- xgb_prob_preds |>
  mutate(pred_thresh = ifelse(test_pred_probs>max_f1, 1, 0))

write.csv(predict, "data/predictions.csv", row.names = FALSE)

predict <- read.csv("data/predictions.csv")

#Accuracy
confusionMatrix(as.factor(predict$pred_thresh), as.factor(predict$test_actual), mode="prec_recall")

recall <- 248630/(248630 + 316084) #A ?/ (A + C), C being False Negative (TPR)
precision <- 248630/(248630 + 268163) #A / (A + B), A being True Positive and B being False Positive (PPV)

f1 <- 2 * (precision * recall)/(precision + recall)
f1
#0.4597844

# MCC
tp <- 248630
tn <- 3925140
fp <- 268163
fn <- 316084

MCC <- (tn * tp - fn * fp)/sqrt((tp + fp)*(tp + fn)*(tn + fp)*(tn + fn))
MCC
#0.3911581

normMCC <- (MCC + 1)/2
normMCC
#0.695579

#Brier Score
brier <- sum((predict$test_pred_probs - predict$test_actual)**2)/nrow(predict)
brier
#0.08092262

#Shap Values
fold <- unique(model$game_fold)

xgb_shap <- function(fold){
  test_data <- model %>% filter(game_fold == fold)
  train_data <- model %>% filter(game_fold != fold)

  y_train <- as.integer(train_data$tackle)
  y_test <- as.integer(test_data$tackle)
  X_train <- train_data |> select(-tackle, -gameId, -playId, -defId, -adj_frame, -game_fold)
  X_test <- test_data |> select(-tackle, -gameId, -playId, -defId, -adj_frame, -game_fold)

  xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

  params <- list(
    objective = "binary:logistic",
    learning_rate = 0.03,
    subsample = 0.7,
    reg_lambda = 2,
    max_depth = 3
  )

  fit_xgb <- xgb.train(
    params,
    data = xgb_train,
    watchlist = list(valid = xgb_test),
    eval_metric = "aucpr",
    #c("error", "auc")
    early_stopping_rounds = 100,
    print_every_n = 250,
    nrounds = 250 # early stopping
  )

  shap_long <- shap.prep(xgb_model = fit_xgb, X_train = as.matrix(X_train))

  meanshap <- as.data.frame(shap_long) |>
    select(c(variable, mean_value)) |>
    distinct()

  meanshap$game_fold <- fold
  return(meanshap)

}
gc()

shap0=list()

for(i in seq_along(fold)){
  shap1 <- xgb_shap(fold[i])
  shap0[[i]] = shap1
}
gc()

data = data.table::rbindlist(shap0, fill = TRUE)
data = as.data.frame(data)

write.csv(data, "data/shap.csv", row.names = FALSE)


shap <- read.csv("data/shap.csv")

shap0 <- shap |>
  group_by(variable) |>
  summarize(
    mshap = mean(mean_value)
  ) |>
  ungroup()

# dist_to_carrier, tackle_rank,
# def_x, def_y, def_joules, car_x, car_y, car_joules, car_impact, seconds, block_wt, relative_velo
#Rename variables as needed
# Barplot
shap0 |>
  mutate(dependent = ifelse(variable=='dist_to_carrier', 'Distance to Carrier', 
                            ifelse(variable=='def_joules', 'Defender Kinectic Energy', 
                                   ifelse(variable=='tackle_rank', 'Relative Tackle Rank', 
                                          ifelse(variable=='block_wt', 'Blocker Leverage',
                                                 ifelse(variable=='seconds', 'Seconds Elapsed',
                                                        ifelse(variable=='relative_velo', 'Relative Velocity', 
                                                               ifelse(variable=='def_x', 'Defender X', 
                                                                      ifelse(variable=='def_y', 'Defender Y', 
                                                                             ifelse(variable=='car_joules', 'Carrier Kinectic Energy',
                                                                                    ifelse(variable=='car_x', 'Carrier X', 
                                                                                           ifelse(variable=='car_y', 'Carrier Y', 
                                                                                                  ifelse(variable=='car_impact', 'Collision Velocity', 
                                                                                                         ifelse(variable=='play_type', 'Play Type', NA)))))))))))))) |>
  ggplot(aes(x=reorder(dependent, mshap), y=mshap)) + 
  coord_flip() +
  geom_bar(aes(alpha = as.factor(mshap)), stat = "identity", fill = "forestgreen") +
  scale_alpha_manual(values = c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1)) +
  #titles and caption
  labs(x = "",
       y = "Mean SHAP Value",
       title = "Tackle Probability Model Feature Importance") +
  theme_fivethirtyeight() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold')) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        #plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8)) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))