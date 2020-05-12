library(rvest)

dates <- read_html("https://github.com/nychealth/coronavirus-data/commits/master/tests-by-zcta.csv") %>%
  html_nodes(".commit-group-title") %>% 
  html_text() %>%
  str_match("Commits on ([A-Za-z]+)\\s([0-9]+),\\s2020") %>%
  data.frame() %>%
  set_colnames(c("all", "month", "day")) %>%
  select(-all) %>% 
  mutate(date = mdy(paste(month, day, "2020")))


to_parse <- read_html("https://github.com/nychealth/coronavirus-data/commits/master/tests-by-zcta.csv") %>%
  html_nodes(".commit-title>a") %>%
  html_attr("href") %>%
  str_match("commit/([a-f0-9]+)#diff") %>%
  data.frame() %>%
  set_colnames(c("toss", "commit")) %>%
  select(-toss) %>% 
  bind_cols(dates)

get_data <- function(commit, date) read_csv(
  paste0(
    "https://raw.githubusercontent.com/nychealth/coronavirus-data/",
    commit, "/tests-by-zcta.csv"
    )
  ) %>%
  mutate(date = date)

all_ny <- to_parse %>%
  mutate(
    data = map2(commit, date, ~get_data(.x, .y))
  ) %>%
  use_series(data) %>%
  bind_rows() %>% 
  filter(! is.na(MODZCTA)) %>%
  rename(zipcode=MODZCTA) 

save(all_ny, file="nyzipcodes.Rda")
