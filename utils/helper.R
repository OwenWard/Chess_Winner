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