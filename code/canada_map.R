# Canadian Map
#---------------------------------------------------------------------

# load data
covid_survey <- fread(here::here("data", "CAPS-ACSP survey on returning to workplace.csv"))
glimpse(covid_survey)

#---------------------------------------------------------------------
# Clean out provinces

covid_survey <- covid_survey %>%
  rename(province = `Where is your organization based?`) 

covid_survey_map <- covid_survey %>%
  group_by(province) %>%
  mutate(respondents = n()) %>%
  distinct(province, respondents) 
covid_survey_map <- tibble::rowid_to_column(covid_survey_map, "ID")

#---------------------------------------------------------------------

# canadian province/territory names
mapcan(boundaries = province,
       type = standard) %>%
  distinct(pr_english)

# visualize provinces/territories
mapcan(boundaries = province,
                 type = standard) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_fixed() +
  theme_mapcan() +
  ggtitle("Map of Canada")

# integrate data and map
pr_geographic <- mapcan(boundaries = province,
                        type = standard)


pr_geographic <- inner_join(pr_geographic, 
                            covid_survey_map, 
                            by = c("pr_english" = "province"))

#---------------------------------------------------------------------

# visualize 
library(sf)
centroids.df <- as.data.frame(coordinates(pr_geographic[,c(1,2,11)]))

pr_geographic %>%
  ggplot(aes(x = long, y = lat, group = group, fill = respondents)) +
  geom_polygon() +
  coord_fixed() +
  theme_mapcan() +
  scale_fill_viridis_c(name = "Respondents") +
  ggtitle("") 
  #+geom_text(data=centroids.df, aes(label = ID, x = long, y = lat)) 
  
