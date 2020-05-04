url2 <- "https://www.smchealth.org/sites/main/files/file-attachments/zip_code_population_data_4.29.2020final.pdf"

install.packages("tabulizer")
out <- tabulizer::extract_tables(url2)

san_mateo <- data.frame(out) %>% 
  set_colnames(c("zipcode", "city", "drop", "count")) %>%
  select(-drop, -city) %>% 
  mutate(zipcode = as.character(as.numeric(as.character(zipcode)))) %>%
  filter(! is.na(zipcode)) %>% 
  mutate(
    count = case_when(
      count == "<10" ~ 10, 
      TRUE ~ as.numeric(count)
    )
  )

save(san_mateo, file="san_mateo.Rda")
