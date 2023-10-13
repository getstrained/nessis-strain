# packages ----------------------------------------------------------------
library(tidyverse)
library(here)
theme_set(theme_light())

# read in data ------------------------------------------------------------
plays <- read_csv(here("data", "plays.csv"))
players <- read_csv(here("data", "players.csv"))
games <- read_csv(here("data","games.csv"))
pffScoutingData <- read_csv(here("data", "pffScoutingData.csv"))

tracking <- map(1:8, \(i) read_csv(here("data", str_c("week", i, ".csv")))) |> 
  list_rbind()

# standardizing football field --------------------------------------------
tracking <- tracking |> 
  mutate(
    # make all plays go from left to right
    x = ifelse(playDirection == "left", 120 - x, x),
    y = ifelse(playDirection == "left", 160 / 3 - y, y),
    # flip player direction and orientation
    dir = ifelse(playDirection == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(playDirection == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  )

# get frames between snap and qb event ------------------------------------
tracking_start_end <- tracking |> 
  distinct(gameId, playId, frameId, event) |> 
  mutate(
    is_start = as.numeric(event %in% c("autoevent_ballsnap", "ball_snap")),
    is_end = as.numeric(event %in% c("fumble", "handoff", "lateral",
                                          "autoevent_passforward", "pass_forward",
                                          "qb_sack", "qb_strip_sack", "run"))) |> 
  group_by(gameId, playId) |> 
  mutate(any_start = any(is_start == 1), any_end = any(is_end == 1)) |> 
  filter(any_start, any_end) |> 
  summarize(start_frame = frameId[which(is_start == 1)[1]],
            end_frame = frameId[which(is_end == 1 & frameId > start_frame)[1]]) |> 
  ungroup()

tracking <- tracking |> 
  left_join(tracking_start_end, by = c("gameId", "playId")) |> 
  filter(!is.na(start_frame), !is.na(end_frame),
         frameId >= start_frame, frameId <= end_frame) |> 
  mutate(frameId_corrected = frameId - start_frame)

# calculate STRAIN for pass rushers ---------------------------------------
tracking_roles <- tracking |> 
  left_join(pffScoutingData, by = c("gameId", "playId", "nflId"))

tracking_qb <- tracking_roles |> 
  # remove 1 play where passer is SRWR
  filter(pff_role == "Pass", pff_positionLinedUp == "QB") |> 
  select(gameId, playId, frameId, x_qb = x, y_qb = y)

tracking_pass_rush <- tracking_roles |>
  filter(pff_role == "Pass Rush") |>
  left_join(tracking_qb) |>
  group_by(gameId, playId, nflId) |>
  mutate(d_qb = sqrt((x - x_qb) ^ 2 + (y - y_qb) ^ 2),
         v_qb = -(d_qb - lag(d_qb)) / 0.1,
         strain = v_qb / d_qb) |>
  ungroup()
