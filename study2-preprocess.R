library(tidyverse)

save_dir <- "saves/study2"

current_script_path <- rstudioapi::getSourceEditorContext()$path 
script_text <- readLines(current_script_path)

data_raw <- read.csv("data/study2-n120.csv") 

scene_labels <- c("1" = "Glass",
                  "2" = "Chick",
                  "3" = "Flowerpot",
                  "4" = "Candle",
                  "5" = "Rock"
)
domain_order <- c("motor", "inhibit", "plan", "tom", "moral")
sceneID_order <- c("1", "2", "3", "4", "5")

# into long format
response_cols <- names(data_raw)[grepl("\\.SQ", names(data_raw)) & !grepl("revise|attentionQ|f1|f2", names(data_raw))]
bg_cols <- names(data_raw)[grepl("f1*|f2", names(data_raw)) & !grepl("Time", names(data_raw))]
scene_cols <- names(data_raw)[grepl("storyAssign.", names(data_raw))]
age_options <- c(2,4,6,8,10,12,14,16,20,25,30)

df <- data_raw %>%
  filter(returncode == "C107BK71" & refurl == "https://app.prolific.com/") %>% 
  dplyr::select(id, response_cols, scene_cols, bg_cols) %>%
  gather(key = "response_per_age", value = "response", response_cols) %>%
  separate(response_per_age, into = c("domain", "age_index", "extra"), sep = "\\.") %>%
  separate(domain, into = "domain", remove = FALSE) %>%
  rowwise() %>%
  mutate(response = as.numeric(response) / 100,
         age_index = as.numeric(sub("SQ", "", age_index)), 
         age = as.numeric(age_options[age_index]), 
         scene = as.factor(get(paste0("storyAssign.", domain, "StoryID."))))%>%
  ungroup() %>%
  mutate(domain = factor(domain, levels=domain_order),
         scene = factor(scene, levels=sceneID_order),
         id = factor(id),
         age_factor = factor(age, levels = age_options),
         r2 = ifelse(response < 0.001, 0.001, ifelse(response > 0.999, 0.999, response)),
         is_parent = factor(ifelse(grepl("Yes", f1.SQ001.), 1, 0), levels=c("0","1")),
         is_sibling = factor(ifelse(grepl("Yes", f1.SQ002.), 1, 0), levels=c("0","1")),
         is_pro = factor(ifelse(grepl("Yes", f1.SQ003.), 1, 0), levels=c("0","1"))
  )

save.image(file.path(save_dir, "df_workspace.RData"))
