
library(tidyverse)

kenpom <- read_csv("kenpom.csv") %>%
  select(Team, Conf, Record = `W-L`, AdjEM = NetRtg...5, Offense = ORtg...6, Defense = DRtg...8) %>%
  mutate(Team = str_remove_all(Team, " [0-9][0-9]"),
         Team = str_remove_all(Team, " [0-9]"),
         across(c(AdjEM, Offense, Defense), ~ as.numeric(.x)))

bt_prob <- function(team_i, team_j, kenpom, scale = 10) {
  
  theta_i <- kenpom %>% filter(Team == team_i) %>% pull(AdjEM)
  theta_j <- kenpom %>% filter(Team == team_j) %>% pull(AdjEM)
  
  p <- exp((theta_i - theta_j)/scale) /
    (1 + exp((theta_i - theta_j)/scale))
  
  return(p)
}

simulate_game <- function(team_i, team_j, kenpom, scale = 10) {
  
  p <- bt_prob(team_i, team_j, kenpom, scale)
  
  winner <- ifelse(runif(1) < p, team_i, team_j)
  
  return(winner)
}

bt_prob("Duke", "Arizona", kenpom)


