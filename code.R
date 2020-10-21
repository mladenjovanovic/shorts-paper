## ----setup, include=FALSE-------------------------------------------
require(knitr)
require(shorts)
require(tidyverse)
require(bookdown)

my_random_seed <- 1667
set.seed(my_random_seed)

options("width" = 70)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  cache = FALSE,
  fig.retina = 0.8, # figures are either vectors or 300 dpi diagrams
  dpi = 600,
  out.width = "100%",
  fig.align = "center",
  fig.width = 6,
  fig.height = 6 * 0.618, # 1 / phi
  fig.show = "hold",
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  width = 70
)

# automatically create a bib database for R packages
#knitr::write_bib(c(
#  .packages(), "bookdown", "knitr", "rmarkdown", "shorts", "nlme", "LambertW"
#), "packages.bib")


# Set rounding
op <- options()
options(digits = 3)


## ----four-athletes-table, echo=FALSE--------------------------------
athletes <- tribble(
  ~Athlete, ~MSS, ~MAC,
  "Athlete A", 12, 10,
  "Athlete B", 12, 6,
  "Athlete C", 8, 10,
  "Athlete D", 8, 6
)

athletes <- athletes %>%
  mutate(TAU = MSS / MAC)

knitr::kable(
  athletes,
  caption = "Four athletes with different MSS and MAC parameters.",
  digits = 2
)


## ----four-athletes-kinematics, echo=FALSE, fig.cap="Kinematic characteristic of four athletes with different MSS and MAC parameters over a period of 0 to 6seconds."----
kinematics <- expand_grid(
  athletes,
  time = seq(0, 6, length.out = 1000)
) %>%
  mutate(
    distance = predict_distance_at_time(time, MSS, TAU),
    velocity = predict_velocity_at_time(time, MSS, TAU),
    acceleration = predict_acceleration_at_time(time, MSS, TAU)
  )

kinematics_long <- kinematics %>%
  pivot_longer(
    cols = c("distance", "velocity", "acceleration"),
    names_to = "variable"
  ) %>%
  mutate(
    variable = factor(
      variable,
      levels = c("distance", "velocity", "acceleration"),
      labels = c(
        expression("Distance (m" * ")"),
        expression("Velocity (ms"^-1 * ")"),
        expression("Acceleration (ms"^-2 * ")")
      )
    )
  )

gg <- ggplot(
  kinematics_long,
  aes(x = time, y = value, color = Athlete)
) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  facet_wrap(
    ~variable,
    scales = "free_y",
    labeller = label_parsed
  ) +
  ylab(NULL) +
  xlab("Time (s)") +
  theme(
    legend.position = "top",
    legend.title = element_blank())

plot(gg)


## ----four-athletes-profile, echo=FALSE, fig.cap="Acceleration-Velocity profile of four athletes with different MSS and MAC parameters."----
gg <- ggplot(
  kinematics,
  aes(x = velocity, y = acceleration, color = Athlete)
) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  ylab(expression("Acceleration (ms"^-2 * ")")) +
  xlab(expression("Velocity (ms"^-1 * ")")) +
  theme(
    legend.position = "top",
    legend.title = element_blank())

plot(gg)


## -------------------------------------------------------------------
require(shorts)

split_distance <- c(10, 20, 30, 40)

split_time <- c(2.17, 3.43, 4.60, 5.73)

m1 <- model_using_splits(
  distance = split_distance,
  time = split_time
)

m1


## -------------------------------------------------------------------
summary(m1$model)


## -------------------------------------------------------------------
# Predict time at distance
predict_time_at_distance(
  distance = split_distance,
  MSS = m1$parameters$MSS,
  TAU = m1$parameters$TAU
)

# Predict acceleration at time
predict_acceleration_at_time(
  time = c(0, 1, 2, 3, 4, 5, 6),
  MSS = m1$parameters$MSS,
  TAU = m1$parameters$TAU
)


## -------------------------------------------------------------------
get_air_resistance(
  velocity = 5,
  bodymass = 80,
  bodyheight = 1.85,
  barometric_pressure = 780,
  air_temperature = 20,
  wind_velocity = 0.5
)


## -------------------------------------------------------------------
# To calculate horizontal force produced
predict_force_at_distance(
  distance = split_distance,
  MSS = m1$parameters$MSS,
  TAU = m1$parameters$TAU,
  # Additional parameters forwarded to get_air_resistance
  # Otherwise, defaults are used
  bodymass = 80,
  bodyheight = 1.85,
  barometric_pressure = 780,
  air_temperature = 20,
  wind_velocity = 0.5
)

# To calculate power produced
predict_power_at_distance(
  distance = split_distance,
  MSS = m1$parameters$MSS,
  TAU = m1$parameters$TAU,
  # Additional parameters forwarded to get_air_resistance
  # Otherwise, defaults are used
  bodymass = 80,
  bodyheight = 1.85,
  barometric_pressure = 780,
  air_temperature = 20,
  wind_velocity = 0.5
)


## -------------------------------------------------------------------
df <- predict_kinematics(
  m1,
  max_time = 6,
  frequency = 100,
  # Additional parameters forwarded to get_air_resistance
  # Otherwise, defaults are used
  bodymass = 80,
  bodyheight = 1.85,
  barometric_pressure = 780,
  air_temperature = 20,
  wind_velocity = 0.5
)

head(df)


## -------------------------------------------------------------------
require(tidyverse)

df <- pivot_longer(data = df, cols = -2)

ggplot(df, aes(x = distance, y = value)) +
  theme_bw(8) +
  facet_wrap(~name, scales = "free_y") +
  geom_line(alpha = 0.7) +
  ylab(NULL) +
  xlab("Distance (m)")


## -------------------------------------------------------------------
# Finds distance where 90% of maximum sprinting speed is reached
find_velocity_critical_distance(
  MSS = m1$parameters$MSS,
  TAU = m1$parameters$TAU,
  percent = 0.9
)

# Finds maximal power and distance (this time using air resistance)
find_max_power_distance(
  MSS = m1$parameters$MSS,
  TAU = m1$parameters$TAU,
  # Additional parameters forwarded to get_air_resistance
  # Otherwise, defaults are used
  bodymass = 80,
  bodyheight = 1.85,
  barometric_pressure = 780,
  air_temperature = 20,
  wind_velocity = 0.5
)

# Finds distance over 90% power range
find_power_critical_distance(
  MSS = m1$parameters$MSS,
  TAU = m1$parameters$TAU,
  # Additional parameters forwarded to get_air_resistance
  # Otherwise, defaults are used
  bodymass = 80,
  bodyheight = 1.85,
  barometric_pressure = 780,
  air_temperature = 20,
  wind_velocity = 0.5
)


## -------------------------------------------------------------------
data(split_times)

# Mixed model
m2 <- mixed_model_using_splits(
  data = split_times,
  distance = "distance",
  time = "time",
  athlete = "athlete",

  # Select random effects
  # Default is MSS and TAU
  random = MSS + TAU ~ 1
)

m2


## -------------------------------------------------------------------
summary(m2)


## -------------------------------------------------------------------
df <- predict_kinematics(m2, max_time = 10)

df <- pivot_longer(df, cols = c(-1, -3))

ggplot(
  filter(df, distance < 40),
  aes(x = distance, y = value, group = athlete, color = athlete)
) +
  theme_bw(8) +
  facet_wrap(~name, scales = "free_y") +
  geom_line(alpha = 0.7) +
  ylab(NULL) +
  xlab("Distance (m)") +
  theme(
    legend.position = "top",
    legend.title = element_blank())


## -------------------------------------------------------------------
sprint_time <- seq(0, 6, 1)

sprint_velocity <- c(0.00, 4.83, 7.07, 8.10, 8.59, 8.81, 8.91)

m3 <- model_using_radar(
  velocity = sprint_velocity,
  time = sprint_time
)

m3


## -------------------------------------------------------------------
m3_weighted <- model_using_radar(
  velocity = sprint_velocity,
  time = sprint_time,
  weights = 1 / (sprint_velocity + 1)
)

m3_weighted


## -------------------------------------------------------------------
data("radar_gun_data")

m4 <- mixed_model_using_radar(
  radar_gun_data,
  time = "time",
  velocity = "velocity",
  athlete = "athlete"
)

m4


## ----radar-gun-sync, include=FALSE, fig.cap="Radar gun synchronization issues"----
df <- tibble(
  `true time` = sprint_time,
  velocity = sprint_velocity,
  `0.5s added` = `true time` + 0.5,
  `0.5s deducted` = `true time` - 0.5
)

plot_df <- pivot_longer(df, cols = -2, names_to = "Sync issue")

ggplot(
  plot_df,
  aes(x = value, y = velocity, color = `Sync issue`)
) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  xlab("Time (s)") +
  ylab(expression("Velocity (" * ms^-1 * ")")) +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# Without synchronization issues
m5 <- model_using_radar(
  velocity = df$velocity,
  time = df$`true time`
)

# With time added
m6 <- model_using_radar(
  velocity = df$velocity,
  time = df$`0.5s added`
)

# With time deducted
m7 <- model_using_radar(
  velocity = df$velocity,
  time = df$`0.5s deducted`
)

rbind(
  data.frame(
    model = "True time",
    t(coef(m5))
  ),
  data.frame(
    model = "Added 0.5s time",
    t(coef(m6))
  ),
  data.frame(
    model = "Deducted 0.5s time",
    t(coef(m7))
  )
)


## -------------------------------------------------------------------
# With time added
m8 <- model_using_radar_with_time_correction(
  velocity = df$velocity,
  time = df$`0.5s added`
)
coef(m8)

# With time deducted
m9 <- model_using_radar_with_time_correction(
  velocity = df$velocity,
  time = df$`0.5s deducted`
)
coef(m9)


## -------------------------------------------------------------------
# Using the true time
predict_velocity_at_time(
  time = df$`true time`,
  MSS = m5$parameters$MSS,
  TAU = m5$parameters$TAU
)

# Using time with sync issues
predict_velocity_at_time(
  time = df$`0.5s added`,
  MSS = m8$parameters$MSS,
  TAU = m8$parameters$TAU,
  time_correction = m8$parameters$time_correction
)


## -------------------------------------------------------------------
# Adding 0.5s to radar_gun_data
radar_gun_data$time <- radar_gun_data$time + 0.5

# Mixed model with time correction being fixed effect
m10 <- mixed_model_using_radar_with_time_correction(
  radar_gun_data,
  time = "time",
  velocity = "velocity",
  athlete = "athlete",
  random = MSS + TAU ~ 1
)

m10

# Mixed model with time correction being random effect
m11 <- mixed_model_using_radar_with_time_correction(
  radar_gun_data,
  time = "time",
  velocity = "velocity",
  athlete = "athlete",
  random = MSS + TAU + time_correction ~ 1
)

m11


## -------------------------------------------------------------------
MSS <- 9
TAU <- 1.3
MAC <- MSS / TAU

split_times <- tibble(
  distance = c(5, 10, 20, 30, 40),
  john_time = predict_time_at_distance(distance, MSS, TAU),

  # Jack's performance
  jack_distance = distance + 0.5,
  jack_true_time = predict_time_at_distance(jack_distance, MSS, TAU),
  time_05m = predict_time_at_distance(0.5, MSS, TAU),
  jack_time = jack_true_time - time_05m
)

split_times


## -------------------------------------------------------------------
plot_df <- split_times %>%
  select(distance, john_time, jack_time) %>%
  rename(John = john_time, Jack = jack_time) %>%
  pivot_longer(cols = -1, names_to = "athlete", values_to = "time") %>%
  mutate(distance = factor(distance))

ggplot(
  plot_df,
  aes(x = distance, y = time, color = athlete, group = athlete)
) +
  theme_bw(8) +
  geom_point() +
  geom_line() +
  xlab("Distance (m)") +
  ylab("Time (s)") +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# Since this is a perfect simulation and stats::nls will complain
# we need to add very small noise, or measurement error to the times
set.seed(1667)
rand_noise <- rnorm(nrow(split_times), 0, 10^-5)
split_times$john_time <- split_times$john_time + rand_noise
split_times$jack_time <- split_times$jack_time + rand_noise

john_profile <- model_using_splits(
  distance = split_times$distance,
  time = split_times$john_time
)

jack_profile <- model_using_splits(
  distance = split_times$distance,
  time = split_times$jack_time
)

sprint_parameters <- rbind(
  unlist(john_profile$parameters),
  unlist(jack_profile$parameters)
)

rownames(sprint_parameters) <- c("John", "Jack")

sprint_parameters


## -------------------------------------------------------------------
sim_df <- expand.grid(
  MSS = c(6, 7, 8, 9),
  MAC = c(6, 7, 8, 9),
  flying_start_distance = c(
    seq(0, 0.001, length.out = 20),
    seq(0.001, 0.01, length.out = 20),
    seq(0.01, 0.1, length.out = 20),
    seq(0.1, 1, length.out = 20)
  ),
  distance = c(5, 10, 20, 30, 40, 50)
)

sim_df <- sim_df %>%
  mutate(
    TAU = MSS / MAC,
    PMAX = MSS * MAC / 4,
    true_distance = distance + flying_start_distance,
    true_time = predict_time_at_distance(true_distance, MSS, TAU),
    stolen_time = predict_time_at_distance(flying_start_distance, MSS, TAU),
    time = true_time - stolen_time
  )

# Add small noise to allow model fit
set.seed(1667)
rand_noise <- rnorm(nrow(sim_df), 0, 10^-4)
sim_df$time <- sim_df$time + rand_noise


## -------------------------------------------------------------------
# Prediction wrapper
pred_wrapper <- function(data) {
  model <- model_using_splits(
    distance = data$distance,
    time = data$time
  )

  params <- data.frame(t(unlist(model$parameters)))

  predicted_time <- predict_time_at_distance(
    distance = data$distance,
    MSS = model$parameters$MSS,
    TAU = model$parameters$TAU
  )

  colnames(params) <- c(
    "est_MSS", "est_TAU", "est_MAC", "est_PMAX",
    "est_time_correction", "est_distance_correction"
  )

  cbind(
    data,
    params,
    data.frame(predicted_time = as.numeric(predicted_time))
  )
}

# estimated parameters and predicted time
model_df <- sim_df %>%
  group_by(MSS, TAU, flying_start_distance) %>%
  do(pred_wrapper(.)) %>%
  ungroup()

# Prediction residuals
model_df$residuals <- model_df$predicted_time - model_df$time


## -------------------------------------------------------------------
# Estimates plot
df <- model_df %>%
  group_by(MSS, TAU, flying_start_distance) %>%
  slice(1) %>%
  mutate(
    MSS_string = paste("MSS =", MSS),
    TAU_string = paste("TAU =", TAU),
    MAC_string = paste("MAC = ", round(MAC, 2)),
    PMAX_string = paste("PMAX = ", round(PMAX, 2))
  )

# MSS
ggplot(
  df,
  aes(x = flying_start_distance, y = est_MSS, color = MAC_string)
) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  facet_wrap(~MSS_string, scales = "free_y") +
  xlab("Flying start distance (m)") +
  ylab("estimated MSS (m/s)") +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# MAC
ggplot(
  df,
  aes(x = flying_start_distance, y = est_MAC, color = MSS_string)
) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  facet_wrap(~MAC_string, scales = "free_y") +
  xlab("Flying start distance (m)") +
  ylab("estimated MAC (m/s/s)") +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# PMAX
ggplot(
  df,
  aes(x = flying_start_distance, y = est_PMAX, color = MSS_string)
) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  facet_wrap(~MAC_string, scales = "free_y") +
  xlab("Flying start distance (m)") +
  ylab("estimated PMAX (W/kg)") +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# Residuals
model_df <- model_df %>%
  mutate(
    MSS_string = paste("MSS =", MSS),
    TAU_string = paste("TAU =", TAU),
    MAC_string = paste("MAC = ", round(MAC, 2)),
    PMAX_string = paste("PMAX = ", round(PMAX, 2)),
    group = paste(MSS, MAC, flying_start_distance)
  )

ggplot(
  model_df,
  aes(y = residuals, x = distance, color = flying_start_distance, group = group)
) +
  theme_bw(8) +
  geom_line(alpha = 0.3) +
  facet_grid(MSS_string ~ MAC_string) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_gradientn(colours = terrain.colors(5, rev = FALSE)) +
  xlab("Distance (m)") +
  ylab("Predicted time - observed time (s)") +
  theme(legend.position = "top") +
  labs(color = "Flying start distance")


## -------------------------------------------------------------------
ggplot(
  model_df,
  aes(y = residuals, x = distance, color = flying_start_distance, group = group)
) +
  theme_bw(8) +
  geom_line(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_gradientn(colours = terrain.colors(5, rev = FALSE)) +
  xlab("Distance (m)") +
  ylab("Predicted time - observed time (s)") +
  theme(legend.position = "top") +
  labs(color = "Flying start distance")


## -------------------------------------------------------------------
jack_profile_fixed_time_short <- model_using_splits(
  distance = split_times$distance,
  time = split_times$jack_time,
  time_correction = 0.3
)

jack_profile_fixed_time_long <- model_using_splits(
  distance = split_times$distance,
  time = split_times$jack_time,
  time_correction = 0.5
)

jack_profile_time_estimated <- model_using_splits_with_time_correction(
  distance = split_times$distance,
  time = split_times$jack_time
)

jack_parameters <- rbind(
  unlist(john_profile$parameters),
  unlist(jack_profile$parameters),
  unlist(jack_profile_fixed_time_short$parameters),
  unlist(jack_profile_fixed_time_long$parameters),
  unlist(jack_profile_time_estimated$parameters)
)

rownames(jack_parameters) <- c(
  "John",
  "Jack - No corrections",
  "Jack - Fixed time correction (+0.3s)",
  "Jack - Fixed time correction (+0.5s)",
  "Jack - Estimated time correction"
)

jack_parameters


## -------------------------------------------------------------------
jack_profile_distance_correction <- model_using_splits_with_corrections(
  distance = split_times$distance,
  time = split_times$jack_time
)

jack_parameters <- rbind(
  unlist(john_profile$parameters),
  unlist(jack_profile$parameters),
  unlist(jack_profile_fixed_time_short$parameters),
  unlist(jack_profile_fixed_time_long$parameters),
  unlist(jack_profile_time_estimated$parameters),
  unlist(jack_profile_distance_correction$parameters)
)

rownames(jack_parameters) <- c(
  "John",
  "Jack - No corrections",
  "Jack - Fixed time correction (+0.3s)",
  "Jack - Fixed time correction (+0.5s)",
  "Jack - Estimated time correction",
  "Jack - Estimated distance correction"
)

jack_parameters


## -------------------------------------------------------------------
pred_wrapper <- function(data) {
  no_correction <- model_using_splits(
    distance = data$distance,
    time = data$time
  )

  fixed_correction_short <- model_using_splits(
    distance = data$distance,
    time = data$time,
    time_correction = 0.3
  )

  fixed_correction_long <- model_using_splits(
    distance = data$distance,
    time = data$time,
    time_correction = 0.5
  )

  time_correction <- model_using_splits_with_time_correction(
    distance = data$distance,
    time = data$time,
    control = nls.control(tol = 1)
  )

  time_dist_correction <- model_using_splits_with_corrections(
    distance = data$distance,
    time = data$time,
    control = nls.control(tol = 1)
  )


  params <- rbind(
    data.frame(
      model = "No correction",
      t(unlist(no_correction$parameters))
    ),
    data.frame(
      model = "Fixed correction +0.3s",
      t(unlist(fixed_correction_short$parameters))
    ),
    data.frame(
      model = "Fixed correction +0.5s",
      t(unlist(fixed_correction_long$parameters))
    ),
    data.frame(
      model = "Time correction",
      t(unlist(time_correction$parameters))
    ),
    data.frame(
      model = "Time and distance correction",
      t(unlist(time_dist_correction$parameters))
    )
  )

  colnames(params) <- c(
    "model", "est_MSS", "est_TAU", "est_MAC", "est_PMAX",
    "est_time_correction", "est_distance_correction"
  )

  df <- expand_grid(
    data,
    params
  )

  df$predicted_time <- predict_time_at_distance(
    distance = df$distance,
    MSS = df$est_MSS,
    TAU = df$est_TAU,
    time_correction = df$est_time_correction,
    distance_correction = df$est_distance_correction
  )

  df$residuals <- df$predicted_time - df$time
  return(df)
}

# estimated parameters and predicted time
model_df <- sim_df %>%
  group_by(MSS, TAU, flying_start_distance) %>%
  do(pred_wrapper(.)) %>%
  ungroup()


## -------------------------------------------------------------------
model_df$model <- factor(
  model_df$model,
  levels = c(
    "No correction",
    "Fixed correction +0.3s",
    "Fixed correction +0.5s",
    "Time correction",
    "Time and distance correction"
  )
)
# Estimates plot
df <- model_df %>%
  group_by(MSS, TAU, flying_start_distance, model) %>%
  slice(1) %>%
  mutate(
    MSS_string = paste("MSS =", MSS),
    TAU_string = paste("TAU =", TAU),
    MAC_string = paste("MAC = ", round(MAC, 2)),
    PMAX_string = paste("PMAX = ", round(PMAX, 2))
  )

# MSS
ggplot(
  df,
  aes(x = flying_start_distance, y = est_MSS, color = model)
) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  facet_grid(MSS_string ~ MAC_string, scales = "free_y") +
  xlab("Flying start distance (m)") +
  ylab("estimated MSS (m/s)") +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# MAC
ggplot(
  df,
  aes(x = flying_start_distance, y = est_MAC, color = model)
) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  facet_grid(MAC_string ~ MSS_string, scales = "free_y") +
  xlab("Flying start distance (m)") +
  ylab("estimated MAC (m/s/s)") +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# PMAX
ggplot(
  df,
  aes(x = flying_start_distance, y = est_PMAX, color = model)
) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  facet_grid(MAC_string ~ MSS_string, scales = "free_y") +
  xlab("Flying start distance (m)") +
  ylab("estimated PMAX (W/kg)") +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# time_correction
ggplot(
  df,
  aes(x = flying_start_distance, y = est_time_correction, color = model)
) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  geom_line(aes(y = stolen_time), color = "black", linetype = "dashed") +
  facet_grid(MAC_string ~ MSS_string, scales = "free_y") +
  xlab("Flying start distance (m)") +
  ylab("estimated time correction (s)")  +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# distance_correction
ggplot(
  df,
  aes(x = flying_start_distance, y = est_distance_correction, color = model)
) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  geom_abline(slope = 1, color = "black", linetype = "dashed") +
  facet_grid(MAC_string ~ MSS_string, scales = "free_y") +
  xlab("Flying start distance (m)") +
  ylab("estimated distance correction (m)") +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# Residuals
model_df <- model_df %>%
  mutate(
    MSS_string = paste("MSS =", MSS),
    TAU_string = paste("TAU =", TAU),
    MAC_string = paste("MAC = ", round(MAC, 2)),
    PMAX_string = paste("PMAX = ", round(PMAX, 2)),
    group = paste(MSS, MAC, flying_start_distance)
  )

ggplot(
  model_df,
  aes(y = residuals, x = distance, color = flying_start_distance, group = group)
) +
  theme_bw(8) +
  geom_line(alpha = 0.3) +
  facet_wrap(~model) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_gradientn(colours = terrain.colors(5, rev = FALSE)) +
  xlab("Distance (m)") +
  ylab("Predicted time - observed time (s)") +
  theme(legend.position = "top") +
  labs(color = "Flying start distance")


## -------------------------------------------------------------------
sim_df <- expand.grid(
  MSS = c(6, 7, 8, 9),
  MAC = c(6, 7, 8, 9),
  time_lag = seq(0, 0.5, length.out = 50),
  distance = c(5, 10, 20, 30, 40, 50)
)

sim_df <- sim_df %>%
  mutate(
    TAU = MSS / MAC,
    PMAX = MSS * MAC / 4,
    true_time = predict_time_at_distance(distance, MSS, TAU),
    time = true_time + time_lag
  )

# Add small noise to allow model fit
set.seed(1667)
rand_noise <- rnorm(nrow(sim_df), 0, 10^-4)
sim_df$time <- sim_df$time + rand_noise


## -------------------------------------------------------------------
# estimated parameters and predicted time
model_df <- sim_df %>%
  group_by(MSS, TAU, time_lag) %>%
  do(pred_wrapper(.)) %>%
  ungroup()


## -------------------------------------------------------------------
model_df$model <- factor(
  model_df$model,
  levels = c(
    "No correction",
    "Fixed correction +0.3s",
    "Fixed correction +0.5s",
    "Time correction",
    "Time and distance correction"
  )
)
# Estimates plot
df <- model_df %>%
  group_by(MSS, TAU, time_lag, model) %>%
  slice(1) %>%
  mutate(
    MSS_string = paste("MSS =", MSS),
    TAU_string = paste("TAU =", TAU),
    MAC_string = paste("MAC = ", round(MAC, 2)),
    PMAX_string = paste("PMAX = ", round(PMAX, 2))
  )

# MSS
ggplot(df, aes(x = time_lag, y = est_MSS, color = model)) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  facet_grid(MSS_string ~ MAC_string, scales = "free_y") +
  xlab("Time lag (s)") +
  ylab("estimated MSS (m/s)") +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# MAC
ggplot(df, aes(x = time_lag, y = est_MAC, color = model)) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  facet_grid(MAC_string ~ MSS_string, scales = "free_y") +
  xlab("Time lag (s)") +
  ylab("estimated MAC (m/s/s)") +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# time_correction
ggplot(
  df,
  aes(x = time_lag, y = est_time_correction, color = model)
) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  geom_abline(slope = -1, color = "black", linetype = "dashed") +
  facet_grid(MAC_string ~ MSS_string, scales = "free_y") +
  xlab("Time lag (s)") +
  ylab("estimated time correction (s)") +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# distance_correction
ggplot(
  df,
  aes(x = time_lag, y = est_distance_correction, color = model)
) +
  theme_bw(8) +
  geom_line(alpha = 0.7) +
  facet_grid(MAC_string ~ MSS_string, scales = "free_y") +
  xlab("Time lag (s)") +
  ylab("estimated distance correction (m)") +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
# Residuals
model_df <- model_df %>%
  mutate(
    MSS_string = paste("MSS =", MSS),
    TAU_string = paste("TAU =", TAU),
    MAC_string = paste("MAC = ", round(MAC, 2)),
    PMAX_string = paste("PMAX = ", round(PMAX, 2)),
    group = paste(MSS, MAC, time_lag)
  )

ggplot(
  model_df,
  aes(y = residuals, x = distance, color = time_lag, group = group)
) +
  theme_bw(8) +
  geom_line(alpha = 0.3) +
  facet_wrap(~model) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_gradientn(colours = terrain.colors(5, rev = FALSE)) +
  xlab("Distance (m)") +
  ylab("Predicted time - observed time (s)")  +
  theme(legend.position = "top") +
  labs(color = "Time lag")


## -------------------------------------------------------------------
jack_LOOCV <- model_using_splits_with_time_correction(
  distance = split_times$distance,
  time = split_times$jack_time,
  LOOCV = TRUE
)

jack_LOOCV


## -------------------------------------------------------------------
df <- jack_LOOCV$LOOCV$parameters

df <- pivot_longer(df, cols = 1:6, names_to = "parameter")

df$parameter <- factor(
  df$parameter,
  levels = c(
    "MSS",
    "TAU",
    "MAC",
    "PMAX",
    "time_correction",
    "distance_correction"
  )
)

ggplot(df, aes(x = value)) +
  theme_bw(8) +
  geom_boxplot() +
  facet_wrap(~parameter, scales = "free_x") +
  xlab(NULL) +
  ylab(NULL) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )


## -------------------------------------------------------------------
df <- data.frame(
  distance = jack_LOOCV$data$distance,
  time = jack_LOOCV$data$time,
  pred_time = jack_LOOCV$data$pred_time,
  LOOCV_time = jack_LOOCV$LOOCV$data$pred_time
)

df <- df %>%
  pivot_longer(cols = c("pred_time", "LOOCV_time"))

df$resid <- df$value - df$time

ggplot(df, aes(x = distance, y = resid, color = name)) +
  theme_bw(8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  theme(legend.title = element_blank()) +
  xlab("Distance (m)") +
  ylab("Predicted - observed time (s)") +
  theme(
    legend.title = element_blank(),
    legend.position = "top")


## -------------------------------------------------------------------
bolt_reaction_time <- 0.183

bolt_distance <- c(10, 20, 30, 40, 50, 60)
bolt_time <- c(1.963, 2.983, 3.883, 4.763, 5.643, 6.493)

# No corrections model
bolt_m1 <- model_using_splits(
  distance = bolt_distance,
  time = bolt_time
)

# Model with reaction time as fixed time correction
bolt_m2 <- model_using_splits(
  distance = bolt_distance,
  time = bolt_time,
  time_correction = -bolt_reaction_time
)

# Model with estimated time correction
bolt_m3 <- model_using_splits_with_time_correction(
  distance = bolt_distance,
  time = bolt_time
)

# Model with estimated time correction, but deducted reaction time
bolt_m4 <- model_using_splits_with_time_correction(
  distance = bolt_distance,
  time = bolt_time - bolt_reaction_time
)

# Model with estimated time and distance corrections
bolt_m5 <- model_using_splits_with_corrections(
  distance = bolt_distance,
  time = bolt_time
)

# Model with estimated time and distance corrections and deducted reaction time
bolt_m6 <- model_using_splits_with_corrections(
  distance = bolt_distance,
  time = bolt_time - bolt_reaction_time
)

bolt_model <- rbind(
  data.frame(
    model = "No correction",
    t(coef(bolt_m1))
  ),
  data.frame(
    model = "No correction - RT",
    t(coef(bolt_m2))
  ),
  data.frame(
    model = "Time correction",
    t(coef(bolt_m3))
  ),
  data.frame(
    model = "Time correction - RT",
    t(coef(bolt_m4))
  ),
  data.frame(
    model = "Distance correction",
    t(coef(bolt_m5))
  ),
  data.frame(
    model = "Distance correction - RT",
    t(coef(bolt_m6))
  )
)

bolt_model


## -------------------------------------------------------------------
data("vescovi")

# Convert data to long
df <- vescovi %>%
  select(1:13) %>%
  # slice(1:10) %>%
  pivot_longer(
    cols = 9:13,
    names_to = "distance",
    values_to = "time"
  ) %>%
  mutate(
    distance = as.numeric(str_extract(distance, "^[0-9]+"))
  )


## -------------------------------------------------------------------
no_corrections <- mixed_model_using_splits(
  df,
  distance = "distance",
  time = "time",
  athlete = "Athlete"
)

fixed_correction <- mixed_model_using_splits(
  df,
  distance = "distance",
  time = "time",
  athlete = "Athlete",
  time_correction = 0.3
)

time_correction_fixed <- mixed_model_using_splits_with_time_correction(
  df,
  distance = "distance",
  time = "time",
  athlete = "Athlete",
  random = MSS + TAU ~ 1
)

time_correction_random <- mixed_model_using_splits_with_time_correction(
  df,
  distance = "distance",
  time = "time",
  athlete = "Athlete",
  random = MSS + TAU + time_correction ~ 1
)

time_distance_correction_fixed <- mixed_model_using_splits_with_corrections(
  df,
  distance = "distance",
  time = "time",
  athlete = "Athlete",
  random = MSS + TAU + time_correction ~ 1
)

time_distance_correction_random <- mixed_model_using_splits_with_corrections(
  df,
  distance = "distance",
  time = "time",
  athlete = "Athlete",
  random = MSS + TAU + time_correction + distance_correction ~ 1
)


## -------------------------------------------------------------------
model_fit <- rbind(
  data.frame(
    model = "No corrections",
    t(unlist(no_corrections$model_fit))
  ),
  data.frame(
    model = "Fixed correction +0.3s",
    t(unlist(fixed_correction$model_fit))
  ),
  data.frame(
    model = "Time correction fixed",
    t(unlist(time_correction_fixed$model_fit))
  ),
  data.frame(
    model = "Time correction random",
    t(unlist(time_correction_random$model_fit))
  ),
  data.frame(
    model = "Time and distance correction fixed",
    t(unlist(time_distance_correction_fixed$model_fit))
  ),
  data.frame(
    model = "Time and distance correction random",
    t(unlist(time_distance_correction_random$model_fit))
  )
)

model_fit$model <- factor(
  model_fit$model,
  levels = rev(c(
    "No corrections",
    "Fixed correction +0.3s",
    "Time correction fixed",
    "Time correction random",
    "Time and distance correction fixed",
    "Time and distance correction random"
  ))
)

ggplot(model_fit, aes(x = RSE, y = model)) +
  theme_bw(8) +
  geom_point() +
  xlab("RSE (s)") +
  ylab(NULL)


## -------------------------------------------------------------------
est_params <- rbind(
  data.frame(
    model = "No corrections",
    no_corrections$parameters$random
  ),
  data.frame(
    model = "Fixed correction +0.3s",
    fixed_correction$parameters$random
  ),
  data.frame(
    model = "Time correction fixed",
    time_correction_fixed$parameters$random
  ),
  data.frame(
    model = "Time correction random",
    time_correction_random$parameters$random
  ),
  data.frame(
    model = "Time and distance correction fixed",
    time_distance_correction_fixed$parameters$random
  ),
  data.frame(
    model = "Time and distance correction random",
    time_distance_correction_random$parameters$random
  )
)

est_params$model <- factor(
  est_params$model,
  levels = rev(c(
    "No corrections",
    "Fixed correction +0.3s",
    "Time correction fixed",
    "Time correction random",
    "Time and distance correction fixed",
    "Time and distance correction random"
  ))
)

est_params <- est_params %>%
  pivot_longer(cols = -(1:2), names_to = "parameter")

est_params$parameter <- factor(
  est_params$parameter,
  levels = c(
    "MSS",
    "TAU",
    "MAC",
    "PMAX",
    "time_correction",
    "distance_correction"
  )
)

ggplot(est_params, aes(y = model, x = value)) +
  theme_bw(8) +
  geom_boxplot() +
  facet_wrap(~parameter, scales = "free_x") +
  xlab(NULL) +
  ylab(NULL)


## -------------------------------------------------------------------
model_resid <- rbind(
  data.frame(
    model = "No corrections",
    no_corrections$data
  ),
  data.frame(
    model = "Fixed correction +0.3s",
    fixed_correction$data
  ),
  data.frame(
    model = "Time correction fixed",
    time_correction_fixed$data
  ),
  data.frame(
    model = "Time correction random",
    time_correction_random$data
  ),
  data.frame(
    model = "Time and distance correction fixed",
    time_distance_correction_fixed$data
  ),
  data.frame(
    model = "Time and distance correction random",
    time_distance_correction_random$data
  )
)


model_resid$model <- factor(
  model_resid$model,
  levels = rev(c(
    "No corrections",
    "Fixed correction +0.3s",
    "Time correction fixed",
    "Time correction random",
    "Time and distance correction fixed",
    "Time and distance correction random"
  ))
)

model_resid$resid <- model_resid$pred_time - model_resid$time

# Create SWC / SESOI band
model_SESOI <- model_resid %>%
  group_by(model, distance) %>%
  summarise(
    bias = mean(resid),
    variance = sd(resid),
    upper = bias + variance,
    lower = bias - variance,
    MAD = mean(abs(resid)),
    SESOI_upper = sd(time) * 0.2,
    SESOI_lower = -sd(time) * 0.2
  )

# Plot
ggplot(model_resid, aes(y = model)) +
  theme_bw(8) +
  geom_vline(
    data = model_SESOI,
    aes(xintercept = SESOI_lower),
    color = "blue", alpha = 0.5, linetype = "dashed"
  ) +
  geom_vline(
    data = model_SESOI,
    aes(xintercept = SESOI_upper),
    color = "blue", alpha = 0.5, linetype = "dashed"
  ) +
  geom_vline(xintercept = 0, color = "blue", alpha = 0.5) +
  geom_jitter(aes(x = resid), alpha = 0.1, height = 0.25) +
  geom_errorbarh(
    data = model_SESOI,
    aes(xmin = lower, xmax = upper),
    height = 0.1, color = "black"
  ) +
  geom_point(data = model_SESOI, aes(x = bias), color = "black") +
  facet_wrap(~distance, scales = "free_x") +
  xlab("Predicted time - observed time (s)") +
  ylab(NULL)


## -------------------------------------------------------------------
df <- model_SESOI %>%
  pivot_longer(cols = -(1:2), names_to = "estimator") %>%
  filter(estimator %in% c("bias", "variance", "MAD"))

df$model <- factor(
  df$model,
  levels = rev(c(
    "No corrections",
    "Fixed correction +0.3s",
    "Time correction fixed",
    "Time correction random",
    "Time and distance correction fixed",
    "Time and distance correction random"
  ))
)

df$estimator <- factor(
  df$estimator,
  levels = c("bias", "variance", "MAD")
)

ggplot(df, aes(x = value, y = model)) +
  theme_bw(8) +
  geom_point() +
  facet_grid(distance ~ estimator, scales = "free_x") +
  xlab(NULL) +
  ylab(NULL)


## ----include=FALSE--------------------------------------------------
# Return original options
options(op)

# Session info
sessionInfo()

