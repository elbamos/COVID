library(rnaturalearth)
library(ozmaps)
library(geojsonsf)

# SETUP LOCATIONS ####

confirmed <- read_csv("./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

locations <- confirmed[, 1:4] %>%
  set_colnames(c("Sublocation", "Location", "lat", "lng")) %>%
  mutate(us_level="Country", Subsublocation=NA) %>% 
  bind_rows(
    geometries %>% 
      rename(Sublocation=state, Subsublocation=county) %>%
      mutate(
        Location="US",
        us_level = if_else(is.na(Subsublocation), "State", "County")
      ) %>% select(Location, Sublocation, Subsublocation, us_level, lat, lng)
  ) %>%
  mutate(loc_id = 1:n()) %>%
  select(Location, Sublocation, Subsublocation, lat, lng, us_level, loc_id) 

save(locations, file="./data/locations.Rda")

# MAKE CENSUS DATA 
library(tidycensus)

census_to_pull <- data.frame(
  variable = c("B00001_001", # Pop
               "B00002_001", # Housing units, 
               "B01001_002", # Male
               "B01001_026"  # Female
  ),
  names = c("pop", "housing_units", "male", "female")
)

census_data <- get_acs(geography = "county", year = 2015, 
                       variables=as.character(census_to_pull$variable)
) %>% 
  bind_rows(
    get_acs(geography = "state", year = 2015, 
            variables=as.character(census_to_pull$variable)
    )
  ) %>%
  inner_join(census_to_pull)

save(census_data, file="./data/census_data.Rda")

# MAKE GEOMETRIES ####

counties <- USAboundaries::us_counties()
states <- USAboundaries::us_states()

load("./data/badzips.Rda")
zips <- USAboundaries::us_zipcodes() %>%
  filter(str_starts(zipcode, c("10", "11", "94")),
         ! zipcode %in% badzips)

save(zips, file="./data/zips.Rda")

load("./data/census_data.Rda")

census_data %<>%
  select(geoid=GEOID, estimate, names) %>% 
  spread(key="names", value="estimate") %>%
  mutate(pop = male + female)

geometries <- counties %>%
  select(county=name, geoid, state=state_name, geometry, aland, awater) %>%
  rbind(
    states %>% select(geoid, state=state_name, geometry, aland, awater) %>%
      mutate(county=NA)
  ) %>%
  left_join(census_data) %>% 
  rbind(
    counties %>% 
      filter(
        state_name == "New York", 
        name %in% c("Kings", "Queens", "New York", "Bronx", "Staten Island", "Richmond")
      ) %>% 
      left_join(census_data) %>% 
      summarize(
        geometry = st_combine(geometry), 
        aland = sum(aland), 
        awater = sum(awater), 
        male = sum(male), 
        female = sum(female), 
        housing_units = sum(housing_units), 
        pop = sum(pop)
      ) %>%
      mutate(
        state = "New York", 
        county = "New York City",
        geoid = NA
      )
  ) %>% 
  mutate(
    centroid = sf::st_centroid(geometry), 
    lat = map(centroid, ~.x[[2]][1]), 
    lng = map(centroid, ~.x[[1]][1]), 
    lat = unlist(lat), 
    lng = unlist(lng),
    point = map2(lng, lat, ~st_point(c(.x, .y), dim="XY")),
    bbox = map(geometry, ~st_bbox(.))
  ) %>% select(-centroid) 

rm(counties)
rm(states)

save(geometries, file="./data/geometries.Rda")

# COUNTRY GEOMS ####

chn <- st_read("./Shapes/chn_admbnda_adm1_ocha.shp") %>% 
  group_by(ADM1_EN) %>%
  summarize() %>%
  mutate(
    ADM1_EN = str_replace(ADM1_EN, " Province", ""), 
    ADM1_EN = str_replace(ADM1_EN, " Municipality", ""),
    Location = "China"
  ) %>%
  select(Location, Sublocation = ADM1_EN, geometry) %>%
  mutate(
    Sublocation = str_replace(Sublocation, " Autonomous Region", ""), 
    Sublocation = str_replace(Sublocation, " Special Administrative Region", ""),
    Sublocation = case_when(
      Sublocation == "Xinjiang Uygur" ~ "Xinjiang",
      Sublocation == "Guangxi Zhuang" ~ "Guangxi",
      Sublocation == "Ningxia Hui" ~ "Ningxia",
      Sublocation == "Gansu province" ~ "Gansu",
      TRUE ~ Sublocation
    )
  )

can <- st_read("./Shapes/Canada_AL263.shp") %>%
  mutate(
    Location="Canada",
    Sublocation=as.character(locname),
    Sublocation = if_else(Sublocation == "Québec", "Quebec", Sublocation)
  ) %>% 
  select(Location, Sublocation, geometry) %>%
  st_transform(crs=st_crs(chn))

data("ozmap_states")

aus <- ozmap_states %>%
  rename(Sublocation=NAME) %>%
  mutate(Location="Australia") %>%
  st_transform(crs=st_crs(chn))

sub_countries <- rbind(aus, can, chn) %>%
  mutate(pop_est = NA)

countries_to_load <- locations %>% filter(us_level == "Country") %>% 
  group_by(Location) %>% summarize() %>% use_series(Location) %>%
  c("Greenland", "Ivory Coast", "Czech Republic", "United States of America", "Yemen", 
    "Democratic Republic of the Congo", "Myanmar", "South Korea", "Taiwan", 
    "Republic of Serbia", "Macedonia", "United Republic of Tanzania")

country_geoms <- ne_countries(scale="small", country=countries_to_load, returnclass="sf") %>%
  select(Location=admin, pop_est, geometry) %>%
  mutate(
    Sublocation = case_when(
      Location == "Greenland" ~ "Greenland",
      TRUE ~ NA_character_
    ), 
    Location = case_when(
      Location == "Greenland" ~ "Denmark", 
      Location == "Ivory Coast" ~ "Cote d'Ivoire",
      Location == "Czech Republic" ~ "Czechia", 
      Location == "United States of America" ~ "US", 
      Location == "Myanmar" ~ "Burma",
      Location == "South Korea" ~ "Korea, South",
      Location == "Taiwan" ~ "Taiwan*",
      Location == "Republic of Serbia" ~ "Serbia", 
      Location == "Macedonia" ~ "North Macedonia",
      Location == "United Republic of Tanzania" ~ "Tanzania",
      TRUE ~ Location
    )
  )

region_geometries <- rbind(sub_countries, country_geoms)
save(region_geometries, file="./data/region_geometries.Rda")

# ZIPCODE GEOMS ####

zip_geoms <- geojson_sf("https://data.sfgov.org/resource/favi-qct6.geojson") %>%
  select(zipcode = zip_code, pop=estimated_2017_acs_population, geometry) %>% 
  group_by(zipcode, pop) %>% 
  summarize() %>%
  mutate(pop = as.numeric(pop)) %>% 
  ungroup() %>%
  st_transform(crs=st_crs(chn))

alameda <- geojson_sf("https://opendata.arcgis.com/datasets/55a1734dba614a60b2b81f2fde6c4c0b_0.geojson") %>%
  select(
    pop = `POPULATION`, 
    zipcode = ZIP_CODE
  ) %>%
  st_transform(crs=st_crs(chn))

sanmateo <- geojson_sf("./Shapes/stanford-gm175wm1954-geojson.json") %>% 
  select(zipcode, geometry) %>% 
  mutate(pop = NA) %>% 
  st_transform(crs=st_crs(chn))


ny_zip_geoms <- st_read("./Shapes/nyzipcodes/ZIP_CODE_040114.shp") %>%
  select(zipcode = ZIPCODE, geometry, pop=POPULATION) %>%
  st_transform(crs=st_crs(chn))

zip_geoms %<>% rbind(ny_zip_geoms) %>% rbind(alameda) %>% rbind(sanmateo)

save(zip_geoms, file="./data/zip_geoms.Rda")

# LOS ANGELES REGION GEOMS ####

la_polys <- geojson_sf("https://opendata.arcgis.com/datasets/7b0998f4e2ea42bda0068afc8eeaf904_19.geojson") %>% 
  filter(
    FEAT_TYPE == "Land", 
    CITY_NAME != "Unincorporated" | CITY_TYPE != "Unincorporated"
  ) %>% 
  select(CITY, CITY_TYPE, area_sm = OF_AREA_SM, geometry) 

save(la_polys, file="./data/la_polys.Rda")
