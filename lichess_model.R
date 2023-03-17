# lichess model
library(tidyverse)
setwd("/Users/mia/Desktop/lichess")

ldata <- readRDS("rdata/lichess_pilot.Rdata")

# is the focal player white or black?
ldata$w <- ifelse(ldata$Username == ldata$White, 1, 0)

# result column
ldata$result <- ifelse(ldata$w == 1 & ldata$Result == "1-0" |
                         ldata$w == 0 & ldata$Result == "0-1", 1, 0)



# white/black elo adjustments
ldata$elof <- ifelse(ldata$w == 1, ldata$WhiteElo, ldata$BlackElo)
ldata$elonf <- ifelse(ldata$w == 0, ldata$WhiteElo, ldata$BlackElo)

## focal and non focal, if focal is black then nonfocal is white


# building a regression model
plot_data <- ldata[c(1:500), ]

ggplot(plot_data, aes(x = UTCDate, y = result)) +
  geom_point()

ldata$elof <- as.numeric(ldata$elof)
ldata$elonf <- as.numeric(ldata$elonf)
ldata$w <- as.numeric(ldata$w)
ldata1 <- ldata[c(1:1000), ]
m1 <- glm(result ~ elof + elonf + w + Event, family = binomial(), data = ldata1)
summary(m1)
plot(m1)


# testing the model
options(scipen = 999)
probabilities <- m1 %>% predict(ldata1, type = "response")
probabilities <- as.data.frame(probabilities)

## are these used again then?

# fish data WL Effect model fit to this data
l.data <- data.frame(
  winner = ifelse(ldata$Result == "1-0", ldata$White, ldata$Black),
  loser = ifelse(ldata$Result == "0-1", ldata$White, ldata$Black)
)

l.data$date <- ldata$UTCDate
l.data$type <- 0
l.data$observation <- 0
l.data$group <- 0


## will fill these in based on other stuff below

l.data <- l.data[, c(6, 3, 5, 1, 2, 4)] 
l.data <- l.data[c(1:1000), ]


## what does this function do?
## for specific values of the coefficients, does something?
## need to see a specific example below


get_previous.outcome.predictions <- function(model, parameter.indexes) {
  n.poutcmes <- length(parameter.indexes)
  prediction.dfs <- vector(n.poutcmes, mode = "list")
  coef.range <- seq(from = 0, to = 1, by = 0.01)
  coefs <- as.numeric(fixef(model))
  for (i in 1:n.poutcmes) {
    param.ests <- vector(length(coefs), mode = "list")
    param.ests[[1]] <- coefs[1]
    for (j in 2:length(param.ests)) {
      if (j == parameter.indexes[i]) {
        param.ests[[j]] <- coefs[j] * coef.range
      } else {
        param.ests[[j]] <- coefs[j] * 0
      }
    }
    x.logits <- Reduce("+", param.ests)
    x.probs <- exp(x.logits) / (1 + exp(x.logits))
    prediction.dfs[[i]] <- data.frame(x = coef.range,
                                      predicted = x.probs,
                                      nth.previous.outcome = 
                                        rep.int(i, length(coef.range))) 
    # this assumes that nth previous is always sequential from 1
  }
  prediction.dfs <- do.call(rbind, prediction.dfs)
  prediction.dfs$nth.previous.outcome <- as.factor(prediction.dfs$nth.previous.outcome)
  return(prediction.dfs)
}

## this doesn't do anything here, all group 0?
## except make the row numbers all start 0.whatever, 
## because all in group 0

l.data$group <- as.factor(l.data$group)
l.data <- split(l.data, f = l.data$group)
l.data <- do.call(rbind, l.data)
l.data$index <- seq(from = 1, to = nrow(l.data), by = 1)
l.data

# choose either the winner or loser to be the focal individual in a particular interaction
l.data$assigned.focal <- character(nrow(l.data))
for (i in 1:nrow(l.data)) {
  random <- runif(n = 1, min = 0, max = 1)
  if (random == 0.5) {
    random <- runif(n = 1, min = 0, max = 1)
  }
  l.data$assigned.focal[i] <- ifelse(random > 0.5, "w", "l")
}

## choosing the focal to be the winner or the loser at random here

R <- nrow(l.data)
glmm.l.data <- vector(R, mode = "list")
for (i in 1:R) {
  l.data$seq <- seq(from = 1, to = nrow(l.data), by = 1)
  previous.interactions <- filter(l.data, seq < i)
  
  if (l.data$assigned.focal[i] == "w") {
    index <- l.data$index[i]
    group <- l.data$group[i]
    pwin <- 1
    ## if focal is winner then pwin = 1
    ## type still won't do anything here
    focal <- l.data$winner[i]
    opponent <- l.data$loser[i]
    type <- l.data$type[i]
  } else {
    index <- l.data$index[i]
    group <- l.data$group[i]
    pwin <- 0
    focal <- l.data$loser[i]
    opponent <- l.data$winner[i]
    type <- l.data$type[i]
  }
  
  glmm.l.data[[i]] <- data.frame(index, group, pwin, focal, opponent, type)
}


glmm.l.data <- do.call(rbind, glmm.l.data)
glmm.l.data <- as_tibble(glmm.l.data)
glmm.l.data

### Function to extract data for the previous nth interaction
previous_outcome <- function(glmm.df, nth.previous) {
  R <- nrow(glmm.df)
  previousoutcome.f <- numeric(R)
  previousoutcome.o <- numeric(R)
  previoustype.f <- numeric(R)
  previoustype.o <- numeric(R)
  for (i in 1:R) {
    fish.f <- as.character(glmm.df$focal[i])
    fish.o <- as.character(glmm.df$opponent[i])
    earlier.times <- filter(glmm.df, index < glmm.df$index[i])
    previous.fights.f <- filter(earlier.times, focal == fish.f | opponent == fish.f)
    previous.fights.o <- filter(earlier.times, focal == fish.o | opponent == fish.o)
    
    if (nrow(previous.fights.f) >= nth.previous) {
      nth.last.interaction <- 
        previous.fights.f[nrow(previous.fights.f) - (nth.previous - 1), ] 
      # select the previous nth fight
      ## this selects just one fight, not multiple
      
      ## see if current focal one the previous interaction
      previousoutcome.f[i] <- 
        ifelse(nth.last.interaction$focal == fish.f & 
                 nth.last.interaction$pwin == 1 | nth.last.interaction$opponent == fish.f & 
                 nth.last.interaction$pwin == 0, 1, 0)
      
      ## can have different types of interactions potentially also
      previoustype.f[i] <- nth.last.interaction$type
    } else {
      previousoutcome.f[i] <- NA
      previoustype.f[i] <- NA
    }
    ## this is all previous fights for opponent, not necessarily same pair
    if (nrow(previous.fights.o) >= nth.previous) {
      nth.last.interaction <- 
        previous.fights.o[nrow(previous.fights.o) - (nth.previous - 1), ] # select the previous nth fight
      previousoutcome.o[i] <- 
        ifelse(nth.last.interaction$focal == fish.o & 
                 nth.last.interaction$pwin == 1 | nth.last.interaction$opponent == fish.o &
                 nth.last.interaction$pwin == 0, 1, 0)
      previoustype.o[i] <- nth.last.interaction$type
    } else {
      previousoutcome.o[i] <- NA
      previoustype.o[i] <- NA
    }
  }
  new.glmm.df <- data.frame(glmm.df, 
                            previousoutcome.f, 
                            previousoutcome.o, 
                            previoustype.f, 
                            previoustype.o)
  names(new.glmm.df)[ncol(glmm.df) + 1] <- paste("previous", nth.previous,
                                                 "outcome.f", sep = ".")
  names(new.glmm.df)[ncol(glmm.df) + 2] <- paste("previous", nth.previous,
                                                 "outcome.o", sep = ".")
  names(new.glmm.df)[ncol(glmm.df) + 3] <- paste("previous", nth.previous,
                                                 "type.f", sep = ".")
  names(new.glmm.df)[ncol(glmm.df) + 4] <- paste("previous", nth.previous,
                                                 "type.o", sep = ".")
  return(new.glmm.df)
}

## check this function to see what the output actually looks like then!

glmm.l.data$group <- as.factor(glmm.l.data$group)
glmm.l.data <- split(glmm.l.data, f = glmm.l.data$group)
glmm.l.data <- lapply(glmm.l.data, previous_outcome, nth.previous = 1)
glmm.l.data <- do.call(rbind, glmm.l.data)
glmm.l.data <- as_tibble(glmm.l.data)
glmm.l.data

# function to calculate centred win frequencies for previous.nth.interaction
WUC_previous.outcome <- function(onegroup.glmmdata, nth.previous.interaction) {
  outcome.name.f <- paste("previous", nth.previous.interaction, "outcome", "f", sep = ".")
  outcome.name.o <- paste("previous", nth.previous.interaction, "outcome", "o", sep = ".")
  ## this gets the columns with the same names
  outcome.f.index <- which(names(onegroup.glmmdata) == outcome.name.f)
  outcome.o.index <- which(names(onegroup.glmmdata) == outcome.name.o)
  
  current.ncol <- ncol(onegroup.glmmdata)
  
  all.fish <- unique(c(as.character(onegroup.glmmdata$focal), as.character(onegroup.glmmdata$opponent)))
  R <- length(all.fish)
  fish.mean <- numeric(R)
  for (i in 1:R) {
    fish <- all.fish[i]
    all.interactions.f <- filter(onegroup.glmmdata, focal == fish)
    all.interactions.o <- filter(onegroup.glmmdata, opponent == fish)
    
    all.interactions <- data.frame(
      index = c(all.interactions.f$index, all.interactions.o$index),
      previous.outcomes = unlist(c(all.interactions.f[, outcome.f.index],
                                   all.interactions.o[, outcome.o.index]))
    )
    ## mean number of wins from all interactions with than node
    fish.mean[i] <- mean(all.interactions$previous.outcomes, na.rm = TRUE)
  }
  print(hist(fish.mean))
  
  ## then store this data, to join with the original data
  fish.mean <- data.frame(all.fish, fish.win.freq.f = fish.mean)
  
  onegroup.glmmdata <- left_join(onegroup.glmmdata, fish.mean, 
                                 by = c("focal" = "all.fish"))
  
  names(fish.mean)[2] <- "fish.win.freq.o"
  onegroup.glmmdata <- left_join(onegroup.glmmdata, fish.mean, 
                                 by = c("opponent" = "all.fish"))
  
  ## subtracting the prop of fights they win?
  win.deviation.f <- (unlist(onegroup.glmmdata[, outcome.f.index] - onegroup.glmmdata$fish.win.freq.f))
  win.deviation.o <- (unlist(onegroup.glmmdata[, outcome.o.index] - onegroup.glmmdata$fish.win.freq.o))
  win.deviation.f <- as.numeric(win.deviation.f)
  win.deviation.o <- as.numeric(win.deviation.o)
  
  onegroup.glmmdata <- data.frame(onegroup.glmmdata, win.deviation.f, win.deviation.o)
  
  
  new.col.names <- c(
    paste("fish.win.", "freq.", nth.previous.interaction, ".f", sep = ""),
    paste("fish.win.", "freq.", nth.previous.interaction, ".o", sep = ""),
    paste("win.", "deviation.", nth.previous.interaction, ".f", sep = ""),
    paste("win.", "deviation.", nth.previous.interaction, ".o", sep = "")
  )
  names(onegroup.glmmdata)[(current.ncol + 1):ncol(onegroup.glmmdata)] <- new.col.names
  
  
  return(onegroup.glmmdata)
}



glmm.l.data$focal <- as.factor(glmm.l.data$focal)
glmm.l.data$opponent <- as.factor(glmm.l.data$opponent)
glmm.l.data$type <- as.factor(glmm.l.data$type)

## all type = 0 so this doesn't do anything
## other parts don't run either, or don't do anything

glmm.l.data$type <- relevel(glmm.l.data$type, ref = "T")
glmm.l.data$previous.1.type.f <- as.factor(glmm.l.data$previous.1.type.f)
glmm.l.data$previous.1.type.f <- relevel(glmm.l.data$previous.1.type.f, ref = "T")
glmm.l.data$previous.1.type.o <- as.factor(glmm.l.data$previous.1.type.o)
glmm.l.data$previous.1.type.o <- relevel(glmm.l.data$previous.1.type.o, ref = "T")



glmm.l.data <- split(glmm.l.data, f = glmm.l.data$group)
glmm.l.data <- lapply(glmm.l.data, WUC_previous.outcome, nth.previous.interaction = 1)
glmm.l.data <- do.call(rbind, glmm.l.data)
glmm.l.data <- as.tibble(glmm.l.data)
glmm.l.data

# first, model fight outcomes as function of CURRENT situation
library(lme4)
model1 <- glmer(pwin ~ (1 | focal) + (1 | opponent),
                family = "binomial", data = glmm.l.data,
                control = glmerControl(optimizer = "bobyqa")
)

summary(model1)
plot(model1)

# now include effect of previous win/loss, separated from overall win/loss frequency
model2 <- glmer(
  pwin ~ fish.win.freq.1.f + fish.win.freq.1.o +
    win.deviation.1.f + win.deviation.1.o +
    (1 | focal) + (1 | opponent),
  family = "binomial", data = glmm.l.data,
  control = glmerControl(optimizer = "bobyqa")
)

summary(model2)

## not clear to me what this win deviation variable is really encoding
