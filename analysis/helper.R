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
    select(Username, Event, White, Black, Result, UTCDate, UTCTime, 
           WhiteElo, BlackElo, Variant, TimeControl, Termination) |> 
    mutate(WhiteElo = parse_number(if_else(WhiteElo == "?", NA, WhiteElo)),
           BlackElo = parse_number(if_else(BlackElo == "?", NA, BlackElo)))
  dat
}

get_hist <- function(user, games, prev_n) {
  if(prev_n == 1){
    hist_games <- games |> 
      # filter(White == user | Black == user) |>
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
      # filter(White == user | Black == user) |> 
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
      mutate(focal_win_prop = c(cumsum(focal_result[1:(prev_n - 1)])/(1:(prev_n -1)), 
                                roll_mean(focal_result, n = prev_n)))
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