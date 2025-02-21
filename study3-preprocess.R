library(tidyverse)

save_dir <- "saves/study3"

current_script_path <- rstudioapi::getSourceEditorContext()$path 
script_text <- readLines(current_script_path)

data_raw <- read.csv("data/study3-n240.csv") 

scene_labels <- c("1" = "Glass",
                  "2" = "Chick",
                  "3" = "Flowerpot",
                  "4" = "Candle",
                  "5" = "Rock"
)
domain_order <- c("motor", "inhibit", "plan", "tom", "moral")
sceneID_order <- c("1", "2", "3", "4", "5")
exprCondition_order <- c("R1high", "R1low", "R2high", "R2low")

response_cols <- names(data_raw)[grepl("\\.SQ", names(data_raw)) & !grepl("revise|attentionQ|f1|f2", names(data_raw))]
bg_cols <- names(data_raw)[grepl("f1*|f2", names(data_raw)) & !grepl("Time", names(data_raw))]
scene_cols <- names(data_raw)[grepl("storyAssign.", names(data_raw))]
age_options <- c(2,5,8,11,14,17,40)

df <- data_raw %>%
  filter(returncode == "C107BK71" & refurl == "https://app.prolific.com/" & f2 <=7) %>% 
  dplyr::select(id, response_cols, scene_cols, bg_cols) %>%
  gather(key = "response_per_age", value = "response", response_cols) %>%
  separate(response_per_age, into = c("domain", "age_index", "extra"), sep = "\\.") %>%
  separate(domain, into = c("domain", "response_which"), sep = "R", remove = FALSE) %>%
  pivot_wider(names_from = response_which, names_prefix="r", values_from = response) %>%
  rowwise() %>%
  mutate(scene = as.factor(get(paste0("storyAssign.", domain, "StoryID."))),
         age_index = as.numeric(sub("SQ", "", age_index)), 
         age = as.numeric(age_options[age_index]), 
         r1 = as.numeric(r1),
         r2_0_100 = as.numeric(r2),
         r2 = as.numeric(r2) / 100) %>%
  ungroup() %>%
  mutate(domain = factor(domain, levels=domain_order),
         scene = factor(scene, levels=sceneID_order),
         id = factor(id),
         age_factor = factor(age, levels = age_options),
         is_parent = factor(ifelse(grepl("Yes", f1.SQ001.), 1, 0), levels=c("0","1")),
         is_sibling = factor(ifelse(grepl("Yes", f1.SQ002.), 1, 0), levels=c("0","1")),
         is_pro = factor(ifelse(grepl("Yes", f1.SQ003.), 1, 0), levels=c("0","1")),
         r1_scale = scale(r1),
         r1_01 = ifelse(r1/100 < 0.001, 0.001, ifelse(r1/100 > 0.999, 0.999, r1/100)),
         r2 = ifelse(r2 < 0.001, 0.001, ifelse(r2 > 0.999, 0.999, r2)),
         is_parent = factor(ifelse(grepl("Yes", f1.SQ001.), 1, 0), levels=c("0","1")),
         is_sibling = factor(ifelse(grepl("Yes", f1.SQ002.), 1, 0), levels=c("0","1")),
         is_pro = factor(ifelse(grepl("Yes", f1.SQ003.), 1, 0), levels=c("0","1"))
  ) %>%
  dplyr::select(id, r1, r2, r1_01, r2_0_100, domain, scene, age_factor, is_parent, is_sibling, is_pro)

save.image(file.path(save_dir, "df_workspace.RData"))
