ELOplotting <- function(filename, playername, title, newfilename) {
  p1 <- read.csv(paste0(filename, ".csv"))

  p1 <- p1 %>%
    filter(Event == "Rated Bullet game" | Event == "Rated bullet game")

  p1$newELO <- ifelse(p1$White == paste0(playername), p1$WhiteElo, p1$BlackElo)

  p1$datetime <- as.POSIXct(paste(p1$UTCDate, p1$UTCTime), format = "%Y.%m.%d %H:%M:%S")

  library(PlayerRatings)
  library(ggrepel)

  ltypes <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
  thetitle <- paste0(title)
  linewd <- 1
  ylim1 <- 1200
  ylim2 <- 3500
  ndays <- 1

  p1_sorted <- p1 %>% arrange(datetime)
  p1_sorted$event <- 1:nrow(p1_sorted)

  tidy_plot <- ggplot(p1_sorted, aes(x = event, y = newELO)) +
    scale_colour_manual(values = getPalette(colourCount)) +
    scale_linetype_manual(values = ltypes) +
    xlab("Games Played") +
    ylab("Lichess Rating") +
    ggtitle(thetitle) +
    ylim(ylim1, ylim2) +
    geom_line(lwd = linewd) +
    theme(
      plot.title = element_text(hjust = 0, vjust = 1, size = rel(1.7)),
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.grid.major.y = element_line(color = "gray75", linetype = "dotted"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1.1)),
      text = element_text(color = "gray20", size = 10),
      axis.text = element_text(size = rel(1.0)),
      axis.text.x = element_text(color = "gray20", size = rel(1.0)),
      axis.text.y = element_text(color = "gray20", size = rel(1.0)),
      axis.title.x = element_text(size = rel(1.0), vjust = 0),
      axis.title.y = element_text(size = rel(1.0), vjust = 1),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none"
    )

  theme_single <- function() {
    theme_bw() %+replace% # replace elements we want to change

      theme(

        # grid elements
        panel.spacing.x = unit(1, "lines"),

        # text elements
        plot.title = element_text(size = title_size),
        axis.text = element_text(size = axis_text_size - 2),
        axis.title = element_text(size = axis_title),
        legend.text = element_text(size = legend_text),
        strip.text = element_text(
          size = axis_title - 5,
          margin = margin(1.5, 1.5, 2, 1.5)
        ),
        legend.position = "none"
      )
  }

  tidy_plot <- tidy_plot + theme_single()

  ggsave(paste0(newfilename, ".png"), tidy_plot,
    dpi = 600, height = 5, width = 7
  )
}

# example call
ELOplotting("a20", "sandro2502", "Player From 1700-1900 Cohort", "plyr17001900elo2")



ELOplottingtwoplayer <- function(filename1, playername1, title1,
                                 filename2, playername2, title2,
                                 newfilename) {
  p1 <- read.csv(paste0(filename1, ".csv"))

  p1 <- p1 %>%
    filter(Event == "Rated Bullet game" | Event == "Rated bullet game")

  p1$newELO <- ifelse(p1$White == paste0(playername1), p1$WhiteElo, p1$BlackElo)

  p1$datetime <- as.POSIXct(paste(p1$UTCDate, p1$UTCTime), format = "%Y.%m.%d %H:%M:%S")

  library(PlayerRatings)
  library(ggrepel)

  ltypes <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
  thetitle <- paste0(title1)
  linewd <- 1
  ylim1 <- 1200
  ylim2 <- 3500
  ndays <- 1

  p1_sorted <- p1 %>% arrange(datetime)
  p1_sorted$event <- 1:nrow(p1_sorted)

  tidy_plot1 <- ggplot(p1_sorted, aes(x = event, y = newELO)) +
    scale_colour_manual(values = getPalette(colourCount)) +
    scale_linetype_manual(values = ltypes) +
    xlab("Games Played") +
    ylab("Lichess Rating") +
    ggtitle(thetitle) +
    ylim(ylim1, ylim2) +
    geom_line(lwd = linewd) +
    theme(
      plot.title = element_text(hjust = 0, vjust = 1, size = rel(1.7)),
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.grid.major.y = element_line(color = "gray75", linetype = "dotted"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1.1)),
      text = element_text(color = "gray20", size = 10),
      axis.text = element_text(size = rel(1.0)),
      axis.text.x = element_text(color = "gray20", size = rel(1.0)),
      axis.text.y = element_text(color = "gray20", size = rel(1.0)),
      axis.title.x = element_text(size = rel(1.0), vjust = 0),
      axis.title.y = element_text(size = rel(1.0), vjust = 1),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none"
    )

  p2 <- read.csv(paste0(filename2, ".csv"))

  p2 <- p2 %>%
    filter(Event == "Rated Bullet game" | Event == "Rated bullet game")

  p2$newELO <- ifelse(p2$White == paste0(playername2), p2$WhiteElo, p2$BlackElo)

  p2$datetime <- as.POSIXct(paste(p2$UTCDate, p2$UTCTime), format = "%Y.%m.%d %H:%M:%S")

  library(PlayerRatings)
  library(ggrepel)

  ltypes <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
  thetitle <- paste0(title2)
  linewd <- 1
  ylim1 <- 1200
  ylim2 <- 3500
  ndays <- 1

  p2_sorted <- p2 %>% arrange(datetime)
  p2_sorted$event <- 1:nrow(p2_sorted)

  tidy_plot2 <- ggplot(p2_sorted, aes(x = event, y = newELO)) +
    scale_colour_manual(values = getPalette(colourCount)) +
    scale_linetype_manual(values = ltypes) +
    xlab("Games Played") +
    ylab("Lichess Rating") +
    ggtitle(thetitle) +
    ylim(ylim1, ylim2) +
    geom_line(lwd = linewd) +
    theme(
      plot.title = element_text(hjust = 0, vjust = 1, size = rel(1.7)),
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.grid.major.y = element_line(color = "gray75", linetype = "dotted"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = rel(1.1)),
      text = element_text(color = "gray20", size = 10),
      axis.text = element_text(size = rel(1.0)),
      axis.text.x = element_text(color = "gray20", size = rel(1.0)),
      axis.text.y = element_text(color = "gray20", size = rel(1.0)),
      axis.title.x = element_text(size = rel(1.0), vjust = 0),
      axis.title.y = element_text(size = rel(1.0), vjust = 1),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none"
    )

  theme_single <- function() {
    theme_bw() %+replace% # replace elements we want to change

      theme(

        # grid elements
        panel.spacing.x = unit(1, "lines"),

        # text elements
        plot.title = element_text(size = title_size),
        axis.text = element_text(size = axis_text_size - 2),
        axis.title = element_text(size = axis_title),
        legend.text = element_text(size = legend_text),
        strip.text = element_text(
          size = axis_title - 5,
          margin = margin(1.5, 1.5, 2, 1.5)
        ),
        legend.position = "none"
      )
  }

  tdy1 <- tidy_plot1 + theme_single()
  tdy2 <- tidy_plot2 + theme_single()

  tidy_plot <- ggarrange(tdy1, tdy2)

  ggsave(paste0(newfilename, ".png"), tidy_plot,
    dpi = 600, height = 5, width = 12
  )
}

# example call
ELOplottingtwoplayer(
  "a20", "sandro2502", "Player From 1700-1900 Cohort",
  "gm14", "alireza2003", "Player From GM Cohort",
  "playercomp1719vsgm"
)
