library(cmdstanr)
set_cmdstan_path("~/.cmdstan/cmdstan-2.31.0/")
library(tidyverse)
library(brms)

save_dir <- "saves/study1"
load(file.path(save_dir, "df_workspace.RData"))

run_name <- "s1-age-x-domain_ID_age-domain_Sce_age-domain"
tryCatch(
  expr = {
    model <-brm(r1 ~ age_factor * domain + (age_factor + domain | id) + (age_factor + domain | scene),
                data   = df,
                family = Beta,
                warmup = 2000,
                iter   = 8000,
                chains = 4,
                cores  = 4,
                threads = threading(8),
                backend = "cmdstanr",
                control = list(adapt_delta = 0.99)) %>%
      add_criterion(., c("waic","loo"))
    
    saveRDS(model, file.path(save_dir,paste0(run_name, ".rds")))
    
    sink(file.path(save_dir,paste0(run_name, ".summary.txt")))
    print(summary(model))
    sink()
  }
)

run_name <- "s1-age-domain_ID_age-domain_Sce_age-domain"
tryCatch(
  expr = {
    model <-brm(r1 ~ age_factor + domain + (age_factor + domain | id) + (age_factor + domain | scene), 
                data   = df,
                family = Beta,
                warmup = 2000,
                iter   = 8000,
                chains = 4,
                cores  = 4,
                threads = threading(8),
                backend = "cmdstanr",
                control = list(adapt_delta = 0.99)) %>%
      add_criterion(., c("waic","loo"))
    
    saveRDS(model, file.path(save_dir,paste0(run_name, ".rds")))
    
    sink(file.path(save_dir,paste0(run_name, ".summary.txt")))
    print(summary(model))
    sink()
  }
)

run_name <- "s1-age_ID_age_Domain_age"
tryCatch(
  expr = {
    model <-brm(r1 ~  age_factor + (age_factor | id) + (age_factor | scene), 
                data   = df,
                family = Beta,
                warmup = 2000,
                iter   = 8000,
                chains = 4,
                cores  = 4,
                threads = threading(8),
                backend = "cmdstanr",
                control = list(adapt_delta = 0.99)) %>%
      add_criterion(., c("waic","loo"))
    
    saveRDS(model, file.path(save_dir,paste0(run_name, ".rds")))
    
    sink(file.path(save_dir,paste0(run_name, ".summary.txt")))
    print(summary(model))
    sink()
  }
)

