#### July 2024 
## Collection of helper functions for processing data
##


read_player <- function(path, file){
  dat <- read_csv(file = paste0(path, file),
                  col_types = cols(UTCDate = col_date("%Y.%m.%d"),
                                   WhiteTitle = col_character(),
                                   BlackTitle = col_character(),
                                   WhiteElo = col_character(),
                                   BlackElo = col_character(),
                                   FEN = col_character())) |> 
    dplyr::select(Username, Event, White, Black, Result, UTCDate, UTCTime, 
           WhiteElo, BlackElo, Variant, TimeControl, Termination) |> 
    mutate(WhiteElo = parse_number(if_else(WhiteElo == "?", NA, WhiteElo)),
           BlackElo = parse_number(if_else(BlackElo == "?", NA, BlackElo)))
  dat
}

get_hist <- function(user, games, prev_n) {
  if(prev_n == 1){
    hist_games <- games |> 
      filter(Username == user) |> 
      arrange(UTCDate, UTCTime) |> 
      mutate(focal_white = ifelse(Username == White, 1, 0)) |> 
      select(White:BlackElo, focal_white) |> 
      mutate(focal_result = case_when(
        (focal_white == 1 & Result == "1-0") ~ 1,
        (focal_white == 0 & Result == "0-1") ~ 1,
        (Result == "1/2-1/2") ~ 0.5,
        .default = 0
      )) |> 
      mutate(focal_win_prop = focal_result)
    
  }
  else{
    hist_games <- games |> 
      filter(Username == user) |> 
      arrange(UTCDate, UTCTime) |>
      mutate(focal_white = ifelse(Username == White, 1, 0),
             UTCDateTime = ymd_hms(paste0(UTCDate, "_", UTCTime)),
             time_diff = UTCDateTime - lag(UTCDateTime, default = NA)) |> 
      select(White:BlackElo, focal_white, time_diff) |> 
      mutate(focal_result = case_when(
        (focal_white == 1 & Result == "1-0") ~ 1,
        (focal_white == 0 & Result == "0-1") ~ 1,
        (Result == "1/2-1/2") ~ 0.5,
        .default = 0)
      ) |> 
      #updating so n = min(prev_n, number of games since first game in session)
      mutate(n_session = (!(time_diff > 300 | is.na(time_diff))), #TRUE indicates game in a session, FALSE indicates first game in new session
             n_session = 1 + accumulate(n_session, ~if (.y) {.x + 1} else {0}, .init = 0)[-1]) |> #the current number of games in a session
      rowwise() |> 
      mutate(n_hist = min(n_session, prev_n)) |> #the n to use for calculating history (ave_prop)
      ungroup() |> #remove rowwise calculations
      mutate(focal_win_prop = map_dbl(row_number(), function(i) { #rolling mean calculation (mean of past n_hist games)
        start <- max(1, i - n_hist[i] + 1)
        mean(focal_result[start:i])
      }))
  }
  hist_games
}


#RDs/Volatility calculation

#returns dataframe with RDs and volatilities for some user
#meant to be run on init_data only
get_RDs = function(user, games) { 
  focal_games = games %>% filter(focal_user == user) 
  
  #storage
  RDs = numeric(length = nrow(focal_games))
  vols = numeric(length = nrow(focal_games))
  
  #loop through each game to use true ratings and get estimated RD/volatility
  for (i in 1: nrow(focal_games)) {
    curr_row = focal_games[i,]
    
    focal_name = curr_row$focal_user #focal players name
    focal_rating = curr_row$focal_rating #focal players rating
    opp_name = curr_row$opp #opponent name
    opp_rating = curr_row$opp_rating #opponent rating
    
    result = curr_row$focal_result
    colour_ind = ifelse(curr_row$focal_white == 1, 1, -1)
    
    #these are for the focal player
    if (i == 1) { #guess parameters
      curr_RD = 100
      curr_volatility = 0.05
    } else { #use the previously estimated parameters
      curr_RD = RDs[i - 1]
      curr_volatility = vols[i - 1]
    }
    
    games = data.frame("Time" = i,
                       "Player1" = focal_name,
                       "Player2" = opp_name,
                       "Result" = result)
    initstate = data.frame("Player" = c(focal_name, opp_name),
                           "Rating" = c(focal_rating, opp_rating),
                           "Deviation" = c(curr_RD, 200), #doesnt matter what opponents RD/volatility is
                           "Volatility" = c(curr_volatility, 0.05))
    
    #get updated rating, RDs, volatility
    glicko2 = glicko2(games, status = initstate, history = TRUE, gamma = colour_ind, tau = 0.5)
    
    #extract the rating of focal player and store it
    focal_glick = glicko2$history[focal_name, ,]
    
    #store them
    RDs[i] = as.numeric(focal_glick)[2]
    vols[i] = as.numeric(focal_glick)[3]
  }
  #put results back into dataframe
  focal_games = focal_games %>% 
    mutate(RD = RDs,
           volatility = vols)
}




## extract the sessions containing more than 1 game
get_true_sequences <- function(x) {
  rle_x <- rle(x)
  true_lengths <- rle_x$lengths[rle_x$values == TRUE]
  return(true_lengths)
}



## plotting defaults
## setup for the plots
theme_set(theme_bw())
axis_title <- 16
title_size <- 18
axis_text_size <- 14
legend_text <- 14
line_size <- 1