library(tidyverse)
pop_params <- tibble(age = paste0(seq(0,80,10)," - ", seq(9,89,10)),
       symp_ssick = c(0.001,0.003,0.012,0.032,0.049,0.102,0.166,0.243,0.273),
       hosp_crit = c(0.05,0.05,0.05,0.05,0.63,0.122,0.274,0.432,0.709),
       infectious_symp = c(0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4),
       susc_exp = c(3,3,3,3,3,3,3,3,3),
       crit_dead = c(0,0.1,0.1,0.2,0.2,0.3,0.4,0.5,0.5),
       init_susc = c(1000,1000,1000,1000,1000,1000,1000,1000,1000),
       init_infected = c(0,0,0,0,1,0,0,0,0))
pop_params$age[9] <- "80+"
meet <- 5
transition_params <- tibble(transition = c("exp->inf","inf->symp","inf->rec","symp->ssick","symp->rec","ssick->crit","ssick->rec","crit->ssick","cri+ssi>rec"),
       param = c("gamma","delta","rho_i","epsilon","rho_y","mu","rho_k","theta","rho_m"),
       med = c(3.5,2,4,4,8,1,14,21,7),
       SD = c(3.5,2,4,4,8,1,14,21,7))
transition_params <- transition_params %>% 
  mutate(mu = log(med),
         sigma_sqre = log((1 + sqrt(1+4*SD^2/med^2))/2),
         mean_est = med*exp(sigma_sqre/2),
         prob = c(1/mean_est))
max_infections <- 200
t0 <- tibble(age = pop_params$age,
             susceptible = pop_params$init_susc,
             exposed = 0,
             infectious = pop_params$init_infected,
             recovering_after_infect=0,
             symp=0,
             recovering_after_symp=0,
             ssick=0,
             recovering_after_ssick=0,
             critical=0, 
             ssick_after_crit=0,
             recovered_completely=0,
             died=0)
get_transition_param <- function(name){
  return(pull(transition_params[transition_params$transition == name, "prob"]))
}



transition <- function(phase){
  # t
  died <- phase$died
  # fix
  alive <- sum(pop_params$init_susc) - sum(died)
  tot_infectious <- sum(phase$infectious) + sum(phase$symp) # + sum(phase$ssick) + sum(phase$ssick_after_crit)
  exposed <- phase$exposed
  infectious <- phase$infectious
  symp <- phase$symp
  ssick <- phase$ssick
  critical <- phase$critical
  recovering_after_infect <- phase$recovering_after_infect
  recovering_after_symp <- phase$recovering_after_symp
  recovering_after_ssick <- phase$recovering_after_ssick
  ssick_after_crit <- phase$ssick_after_crit
  # t+1
  # browser()
  t_1 <- data.frame(age = pop_params$age)
  t_1$susceptible <- phase$susceptible - pop_params$susc_exp*tot_infectious*phase$susceptible/alive
  t_1$exposed <- (1-get_transition_param("exp->inf")) * exposed +
    pop_params$susc_exp*tot_infectious*phase$susceptible/alive
  t_1$infectious <- (1-get_transition_param("inf->symp"))*infectious + 
    get_transition_param("exp->inf")*(1-pop_params$infectious_symp) * exposed
  t_1$recovering_after_infect <- (1-get_transition_param("inf->rec"))*recovering_after_infect +
    pop_params$susc_exp*pop_params$infectious_symp*exposed
  t_1$symp <- (1-get_transition_param("symp->ssick")) * symp + 
    get_transition_param("inf->symp")*pop_params$infectious_symp*infectious
  t_1$recovering_after_symp <- (1- get_transition_param("symp->rec")) * recovering_after_symp + 
    get_transition_param("inf->symp") * (1-pop_params$infectious_symp) *infectious
  t_1$ssick <- (1-get_transition_param("ssick->crit")) * ssick + 
    get_transition_param("symp->ssick") * pop_params$hosp_crit * symp
  t_1$recovering_after_ssick <- (1-get_transition_param("ssick->rec"))*recovering_after_ssick + 
    get_transition_param("symp->ssick") * (1-pop_params$hosp_crit) * symp
  t_1$critical <- (1 - pop_params$crit_dead - get_transition_param("crit->ssick"))* critical + 
    get_transition_param("ssick->crit") * ssick
  t_1$ssick_after_crit <- (1-get_transition_param("cri+ssi>rec")) * ssick_after_crit + 
    get_transition_param("crit->ssick")*critical
  t_1$died <- died + pop_params$crit_dead*critical
  t_1$recovered_completely <- phase$recovered_completely +
    get_transition_param("inf->rec") * recovering_after_infect+
    get_transition_param("symp->rec") * recovering_after_symp+
    get_transition_param("ssick->rec") * recovering_after_ssick+
    get_transition_param("cri+ssi>rec") * ssick_after_crit
  return(t_1)
}
t_next <- t0
for(i in 1:100){
  t_next <- transition(t_next)
}

transition(t0)
