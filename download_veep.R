library(tidyverse)
library(rvest)


# Grab all episode information
all_episodes_html <- "https://subslikescript.com/series/Veep-1759761"
seasons <- read_html(all_episodes_html)  %>% 
  html_nodes(".season")

grab_episode_info <- function(.season){
  ep_link <- .season %>% 
    html_nodes("li a") %>% 
    html_attr("href") %>% 
    paste0("https://subslikescript.com", .)
  
  ep_title <- .season %>% 
    html_nodes("li") %>% 
    html_text() 
    
  
  data.frame(ep_title, ep_link, stringsAsFactors = FALSE) %>% 
    as_tibble()
}

complete_episode_list <- seasons %>% 
  html_node("ul") %>% 
  map_dfr(grab_episode_info, .id = "season")


# Grab scripts per episode
grab_script <- function(.ep_link){
  message(.ep_link)
  read_html(.ep_link) %>% 
    html_node(".full-script") %>% 
    html_text(trim = FALSE)
}

grab_script(complete_episode_list$ep_link[5]) %>% class

df <- complete_episode_list %>% 
  mutate(script = map_chr(ep_link, grab_script))


df <- df %>% 
  mutate(ep_number = str_extract(ep_title, "^[0-9]+(?=\\.)"), 
         ep_title_clean = str_extract(ep_title, "(?<=[0-9]{1,2}\\. ).+")) %>% 
  select(season, ep_number, ep_title = ep_title_clean, ep_link, script)

saveRDS(df, file = "data/veep_episode_scripts.rda")

