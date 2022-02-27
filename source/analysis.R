 library("dplyr")
 library("ggplot2")
 library("reshape2")
 library("leaflet")
 library("maps")
 library("mapproj")
 county_level <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
 white_in_jail <- county_level$white_jail_pop
 aapi_in_jail <- county_level$aapi_jail_pop
#Introduction + summary 5 values

#Summary information
# What is the average value of white in jail across all the counties in 2018?
ave_white_in_jail <- county_level %>% 
  group_by(year) %>% 
  summarize(average = mean(na.omit(white_jail_pop))) %>% 
  filter(year == "2018") %>% 
  pull(average)
# Where is white_in_jail highest/lowest in 2018
 highest_white_in_jail <- county_level %>%
   filter(year == "2018") %>% 
   filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>% 
   pull(white_jail_pop)
# What is the average value of aapi in jail across all the counties in 2018
ave_aapi_jail <- county_level %>% 
  group_by(year) %>% 
  summarize(average = mean(na.omit(aapi_jail_pop))) %>% 
  filter(year == "2018") %>% 
  pull(average)
# Where is aapi_in_jail highest/lowest
 highest_aapi_in_jail <- county_level %>% 
   filter(year == "2018") %>% 
   filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE)) %>% 
   pull(aapi_jail_pop)
# The Number of white in jail in 2018 in Washington, WA
 white_in_jail_2018_WA <- county_level %>% 
   group_by(state, year) %>% 
   filter(year == "2018", state == "WA") %>% 
   summarize(WA_num = sum(white_jail_pop)) %>% 
   pull(WA_num)
#trend over time chart
 white_trends <- county_level %>% 
   filter(state == "WA") %>% 
   ggplot(aes(x=year, y=white_jail_pop)) + geom_line() + 
   ggtitle("The trend of total white jail population across all counties")
 white_trends
#variable comparison chart
 white_aapi_data <- county_level %>% 
   group_by(year) %>% 
   select(year, white_jail_pop, aapi_jail_pop)
 white_aapi_data1 <- melt(white_aapi_data, id.vars = "year", na.rm = TRUE)
 white_aapi_com <- ggplot(white_aapi_data1, aes(x=year, y=value, fill=variable)) + 
   geom_bar(stat="identity", position="dodge") + 
   ggtitle("comparison between white jail pop and aapi jail pop") +
   labs(y = "population of white and aapi")
 white_aapi_com
#map
#U.S county map
 county_map_info <- map_data("county") %>% 
   rename(County = subregion) %>% 
   rename(STATE = region)
 #
 white_jail_pop_2018 <- county_level %>% 
   group_by(year) %>% 
   filter(year == "2018") %>% 
#  mutate(new_names = adjust_name) %>% 
#  group_by(new_names) %>% 
   select(year, county_name, white_jail_pop)
 adjust_name <- sub(" .*", "", tolower(white_jail_pop_2018$county_name))
 white_jail_pop_2018 <- white_jail_pop_2018 %>% 
   mutate(new_names = adjust_name)
#
 map_info <- left_join(county_map_info, white_jail_pop_2018, by = c("County" = "new_names"))
# Draw U.S. map
 plot <- ggplot(map_info) +
   geom_polygon(
     mapping = aes(x = long, y = lat, group = group, fill = white_jail_pop),
     color = "white",
     size = .1
     ) +
   coord_map() + 
   scale_fill_continuous(low = "grey", high = "Red") +
   labs(fill = "white_in_jail_pop")
plot
