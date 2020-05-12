### Libraries ##################################################################

library(tidyverse)
library(EpiEstim)
library(deSolve)

## Define custom functions #####################################################

rollmean <- function(x, k){
  
  if(k %% 2 != 1){stop("k must be uneven")}
  
  na_pad <- rep(NA, floor(k/2))
  
  out <- c(na_pad,
           zoo::rollmean(x = x, k = 5), 
           na_pad)

  out
}


rnorm.between <- function(n, minimum = 0, maximum = 1) {
  x <- rnorm(n)
  max_x <- max(x)
  min_x <- min(x)
  x <- x - min_x
  x <- x / (max_x - min_x)
  x <- x * (maximum - minimum)
  x <- x + minimum
  
  return(x)
}

## Fetch data from the covid19data API #########################################

# Get newly hospitalized
url_newly <- "https://api.covid19data.dk/ssi_newly_hospitalized"

newly_raw <- jsonlite::fromJSON(url_newly) %>%
  as_tibble() %>%
  mutate(date = date %>% lubridate::ymd_hms() %>% as.Date())

newly <- newly_raw %>% 
  slice(1:(n() - 4)) %>%
  mutate(rollingmean = rollmean(newly_hospitalized, 5)) %>%
  drop_na()

# Get current total hospitalized
url_hosp <- "https://api.covid19data.dk/ssi_hospitalized"

hosp_raw <- jsonlite::fromJSON(url_hosp)

hosp <- hosp_raw %>% 
  mutate(timestamp = timestamp %>% lubridate::ymd_hm(),
         date = timestamp %>% as.Date()) %>%
  group_by(date) %>%
  summarise_at(c("hospitalized", "critical", "respirator"), sum, na.rm  = TRUE) %>%
  mutate(hospitalized_rm = rollmean(hospitalized, 5),
         critical_rm = rollmean(critical, 5),
         respirator_rm = rollmean(respirator, 5)) %>%
  drop_na()

## Estimating R ################################################################

confirmed_cases <- newly %>% select(I = rollingmean)

R_estim <- estimate_R(confirmed_cases, 
                     method = "parametric_si", 
                     config = make_config(list(mean_si = 4.7, 
                                               std_si = 2.9)))

pd <- R_estim$R %>% 
  as_tibble() %>% 
  select(t = t_end, 
         R = `Mean(R)`, 
         lower = `Quantile.0.05(R)`, 
         upper = `Quantile.0.95(R)`) %>%
  mutate(t = (t-1) + min(newly$date))

anno <- pd %>% slice(n())

pd %>%
  filter(R <= 2) %>%
  ggplot() + 
  geom_ribbon(aes(x = t, ymin = lower, ymax = upper), fill = "lightgrey") + 
  geom_line(aes(x = t, y = R)) + 
  geom_hline(yintercept = 1, linetype = "dashed") + 
  geom_label(data = anno, aes(x = t, y = R, label = round(R, 2)), hjust = 0) +
  theme_minimal() + 
  labs(x = "", y = "Effektiv reproduktionstal") + 
  scale_x_date(breaks = "weeks", labels = scales::date_format("%d-%m")) 


### ODE model ##################################################################

sir_ode <- function(times, init, parms){
  
  with(as.list(c(parms, init)), {
    
    # ODEs
    dSdt = -S2I * S * I / N
    
    dIdt = S2I * S * I / N - (1 - Hpct) * I2R * I - I2H * I * Hpct
    
    dHdt = I2H * I * Hpct - H * H2R
    
    dRdt = (1 - Hpct) * I2R * I + H * H2R
    
    list(c(dSdt, dIdt, dHdt, dRdt))
  })
}
 
### Model inputs ###############################################################

newly_date <- newly %>% slice(n()) %>% pull(date)
newly_hosp <- newly %>% slice(n()) %>% pull(rollingmean)

# Defining R0
R_0 = anno$R

# Setting the parms
days2hosp = 8
I2H = 1 / days2hosp
Hpct = .02
I2R = 1 / 5
S2I = R_0 * I2R
H2R = 1 / 15


# Setup the model inputs
N = 5.8 * 10 ^ 6

S0 = N
I0 = (newly_hosp / Hpct) / S2I
H0 = hosp %>% filter(date == newly_date - days2hosp) %>% pull(hospitalized_rm)
R0 = 0

parms <- c(N = N, S2I = S2I, I2R = I2R, I2H = I2H, Hpct = Hpct, H2R = H2R)

init <- c(S = S0, I = I0, H = H0, R = R0) 

### Run model optimization #####################################################

hospitalized <- hosp %>% 
  filter(date >= (newly_date - days2hosp)) %>% 
  pull(hospitalized_rm)

times <- 1:length(hospitalized)

days_in_hosp_best_df <- map_dfr(8:22, function(days_in_hosp){
  
  H2R = 1 / days_in_hosp
  
  parms <- c(N = N, S2I = S2I, I2R = I2R, I2H = I2H, Hpct = Hpct, H2R = H2R)
  
  sir_out <- ode(init, times, sir_ode, parms)
  
  fit <- sir_out[,"H"]
  
  rss <- sum((hospitalized - fit)^2)
  
  tibble(days_in_hosp, rss)
})

days_in_hosp_best <- days_in_hosp_best_df %>%
  filter(rss == min(rss)) %>% 
  pull(days_in_hosp)

### Run optimized model ########################################################

parms["H2R"] <- 1/days_in_hosp_best

sir_out <- ode(init, times, sir_ode, parms)

sir_out_df <- sir_out %>% as.data.frame() %>% 
  as_tibble()

sir_out_df$date <- (sir_out_df$time - 1) + (newly_date - days2hosp)
sir_out_df$date <- (sir_out_df$time - 1) + (newly_date - days2hosp)

pd <- sir_out_df %>% 
  select(date, time, S, I, H, R) %>%
  select(date, H) %>%
  left_join(hosp, by = "date") %>%
  select(date, H, hospitalized_rm) 

### Run future model with R_0 bootstrap ########################################

lower <- anno %>% pull(lower)
upper <- anno %>% pull(upper)
r0_potential <- rnorm.between(n = 1000, minimum = lower, maximum = upper)

times <- 1:60

runs <- 10 ^ 4
pb <- progress::progress_bar$new(total = runs)

out_df <- map_dfr(1:runs, function(i){
  
  pb$tick()
  
  # Defining R0
  R_0 = sample(r0_potential, 1)
  
  # Setting the parms
  days2hosp = 8
  I2H = 1 / days2hosp
  Hpct = .02
  I2R = 1 / 5
  S2I = R_0 * I2R
  H2R = 1 / days_in_hosp_best
  
  
  # Setup the model inputs
  parms <- c(N = N, S2I = S2I, I2R = I2R, I2H = I2H, Hpct = Hpct, H2R = H2R)
  
  init <- sir_out_df %>% slice(n()) %>% select(S, I, H, R) %>% as_vector()
  
  sir_out <- ode(init, times, sir_ode, parms)
  
  sir_out_df <- sir_out %>% as.data.frame() %>% 
    as_tibble() %>% mutate(r0 = r0, id = i)
  
  sir_out_df
  
})

################################################################################

# Plot model

mydate <- sir_out_df %>% slice(n()) %>% pull(date)
out_df$date <- (out_df$time - 1) + mydate

myspread <- out_df %>%
  select(date, id, H) %>%
  spread(date, H)

qs <- myspread[,-1] %>% map(quantile, probs = c(0.05, .5, .95))

new_pd <- qs %>% as_tibble() %>%
  mutate(qs = c(0.05, .5, .95)) %>%
  gather(date, H, -qs) %>%
  spread(qs, H)

new_pd$date  <- new_pd$date %>% as.Date()




################################################################################


ggplot() + 
  
  # Hospital
  geom_line(data = pd, aes(date, H), linetype = "dashed", color = "blue") + 
  geom_point(data = pd, aes(date, hospitalized_rm), color = "blue") +
  
  # The Future
  geom_ribbon(data = new_pd, aes(x = date, ymin = `0.05`, ymax = `0.95`), fill = "lightgrey") +
  geom_line(data = new_pd, aes(x = date, y = `0.5`), linetype = "dashed", color = "blue") + 
  
  # Aesthetics
  theme_minimal() + 
  scale_x_date(breaks = "weeks", labels = scales::date_format("%d-%m")) +
  theme(panel.grid = element_blank()) +
  
  # Labels
  labs(y = "Indlagte på hospitalet",
       x = "")


################################################################################
