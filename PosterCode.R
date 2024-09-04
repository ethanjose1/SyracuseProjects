# Ethan Jose
# IST421 Poster Project- W7 HW


### Housekeeping/Packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggalluvial)
library(viridisLite)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(mapdata)
library(sf)
library(grDevices)

# get file name
fname <- file.choose()



### read in files

# data set 1: General and historic earnings data
general_data <- read_csv("C:\\Users\\ethan\\Documents\\Data Viz Poster Project\\G_GeneralEsportData.csv")
historical_data <- read_csv("C:\\Users\\ethan\\Documents\\Data Viz Poster Project\\G_HistoricalEsportData.csv")


# data set 2: player, teams and locations esports data
player_data <- read_csv("C:\\Users\\ethan\\Documents\\Data Viz Poster Project\\S_highest_earning_players.csv")
team_data <- read_csv("C:\\Users\\ethan\\Documents\\Data Viz Poster Project\\S_highest_earning_teams.csv")
location_codes_data <- read_csv("C:\\Users\\ethan\\Documents\\Data Viz Poster Project\\S_country-and-continent-codes-list.csv")



### pre aggregation


# add year column 
historical_data <- historical_data %>% 
  mutate(Year = year(Date))


# lowercase the 2 letter countrycodes 
location_codes_data$Two_Letter_Country_Code <- tolower(location_codes_data$Two_Letter_Country_Code)

###############################################################################


### line plot for total earnings by year


# group earnings by year
year_earnings <- historical_data %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year) %>% 
  summarize(earnings = sum(Earnings))



# fix scientific notation
options(scipen = 999)

# plot line chart of annual earnings
lineplot <- ggplot(year_earnings, aes(x = Year, y = earnings))+
  geom_line() +
  ggtitle("Video Game Earnings between 1998 and 2023")

lineplot
ggsave("lineplot.pdf")





##### multi lineplot per month showing fortnite and dota earnings over time




year_earnings_games <- historical_data %>% 
  filter(Year >= 2018,
         Year <= 2020) %>% 
  filter(Game %in% c('Dota 2', 'Fortnite', 'League of Legends'))


line_plot <- ggplot(year_earnings_games, aes(x = Date, y = Earnings, color = Game)) +
  geom_line(size = 1.2) +
  labs(title = "Earnings Over Time",
       x = "Date",
       y = "Earnings",
       color = "Game") +
  theme_minimal()


line_plot
ggsave('lineplotzoomed.pdf')



year_earnings_games.1 <- historical_data %>% 
  filter(Year >= 2014,
         Year <= 2020) %>% 
  filter(Game %in% c('Dota 2', 'Fortnite', 'League of Legends'))


line_plot.1 <- ggplot(year_earnings_games.1, aes(x = Date, y = Earnings, color = Game)) +
  geom_line(size = 1.2) +
  labs(title = "Earnings Over Time",
       x = "Date",
       y = "Earnings",
       color = "Game") +
  theme_minimal()

line_plot.1

ggsave('lineplotwide.pdf')

#### several boxplots for each genre and its earnings


year_earnings_5 <- year_earnings %>% 
  slice(13:26)







### bar graph for earnings per genre

# group earnings by genre
genre_earnings <- general_data %>% 
  group_by(Genre) %>% 
  summarize(earnings = sum(TotalEarnings)) %>% 
  arrange(desc(earnings)) %>% 
  mutate(Genre = factor(Genre, levels = Genre)) # make genre a factor for least to greatest

barplot <- ggplot(genre_earnings, aes(x = Genre, y = earnings,  fill = Genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Esports Earnings Per Genre (1981-2023)",
       x = "Genre",
       y = "Earnings in Dollars") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot barchart
barplot
ggsave('barplot1.pdf')

#### barplot for game eanrings


game_earnings <- general_data %>% 
  group_by(Game) %>% 
  summarize(TotalEarnings = sum(TotalEarnings)) %>% 
  arrange(desc(TotalEarnings)) %>% 
  slice(1:15) %>% 
  mutate(Game = factor(Game, levels = Game))



barplot.1 <- ggplot(game_earnings, aes(x = Game, y = TotalEarnings, fill = Game))+
  geom_bar(stat = 'identity') +
  labs(title = "Top 15 Esports Earnings Per Game (1981-2023)",
       x = "Game",
       y = "Earnings (USD)")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

barplot.1
ggsave("barplot2.pdf")





### alluvial


# add genre column from general data
combined <- left_join(historical_data, general_data) %>% 
  filter(Genre %in% c('Multiplayer Online Battle Arena',
                      'First-Person Shooter',
                      'Battle Royale',
                      'Strategy',
                      'Sports')) %>% 
  select(TotalEarnings, Game, Genre)



top_ten <- combined %>% 
  group_by(Game) %>% 
  summarise(TotalEarnings = sum(TotalEarnings)) %>% 
  arrange(desc(TotalEarnings)) %>% 
  slice(1:10) %>% 
  ungroup()

combined.1 <- combined %>% 
  filter(Game %in% top_ten$Game)



alluvial <- ggplot(combined.1)+
  aes(y = TotalEarnings,
      axis1 = Genre,
      axis2 = Game)+
  geom_stratum()+
  geom_text(stat = 'stratum', 
            size = 3, 
            aes(label = after_stat(stratum)  
            ))+
  geom_alluvium(aes(fill = Genre))+
  scale_fill_viridis_d()+
  labs(title = "Earnings Flow from Genre to Games")+
  theme_void()

alluvial
ggsave('alluvial1.pdf')

#### alluvial teamname to games earnings


top_ten_teams <- team_data %>% 
  group_by(TeamName) %>% 
  summarize(TotalEarnings = sum(TotalUSDPrize)) %>% 
  arrange(desc(TotalEarnings)) %>% 
  slice(1:10) %>% 
  ungroup()

team_data.1 <- team_data %>% 
  filter(TeamName %in% top_ten_teams$TeamName)

alluvial.1 <- ggplot(team_data.1)+
  aes(y = TotalUSDPrize,
      axis1 = TeamName,
      axis2 = Genre,
      axis3 = Game)+
  geom_stratum()+
  geom_text(stat = 'stratum',
            size = 3,
            aes(label = after_stat(stratum)))+
  geom_alluvium(aes(fill = TeamName))+
  labs(title = "Earnings Flow")+
  theme_void()


alluvial.1

ggsave("alluvial2.pdf")






#################################################### 
#   MAP
#################################################



# group player earnings by region


player_data_by_region <- player_data %>% 
  group_by(CountryCode) %>% 
  summarise(totalEarnings = sum(TotalUSDPrize))




# merge data
player_data_by_region_joined <- merge(player_data_by_region, location_codes_data, by.x = 'CountryCode', by.y = "Two_Letter_Country_Code")


simplified_player_earnings_data <- player_data_by_region_joined %>% 
  select(totalEarnings, Continent_Name, Three_Letter_Country_Code)



world_map <- ne_countries(scale = "medium", returnclass = "sf")

merged_map <- merge(simplified_player_earnings_data, world_map, by.x = 'Three_Letter_Country_Code', by.y = 'adm0_a3', all.x = T)



ggplot(merged_map, aes(geometry = geometry, fill = totalEarnings))+
  geom_sf(color = 'white')+
  scale_fill_gradient(low = '#edf8fb', high = '#6e016b', name = "Earnings")+
  theme_void()

ggsave('mapfinal.pdf')




# bar graph of top ten country earnings


earings_bar_simplified = merged_map %>% 
  select(totalEarnings, admin) %>% 
  arrange(desc(totalEarnings)) %>% 
  slice(1:7,)

bar_custom_cols <- rev(brewer.pal(7, 'BuPu'))

ggplot(earings_bar_simplified, aes(x = reorder(admin, totalEarnings), y = totalEarnings)) +
  geom_bar(stat = 'identity', fill = bar_custom_cols)+
  coord_flip()

ggsave('sidewaysbarfinal.pdf')



### alluvial from continent to genre to game by earnings

# group earnings by region
player_data_by_region_game_genre <- player_data %>% 
  group_by(Game, Genre, CountryCode) %>% 
  summarise(totalEarnings = sum(TotalUSDPrize))


# merge country code data
player_data_by_region_game_genre.1 <-  merge(player_data_by_region_game_genre,
                                             location_codes_data,
                                             by.x = 'CountryCode', 
                                             by.y = "Two_Letter_Country_Code")

custom_colors <- c('Asia' = '#3E1340',
                   'Europe' = '#EE018F',
                   'North America' = '#FF00FE',
                   'Oceania'= '#41FEFF',
                   'South America' = '#BFFFFC')

custom_colors.1 <- c('Asia' = '#27005D',
                   'Europe' = '#9400FF',
                   'North America' = '#AED2FF',
                   'Oceania'= '#E4F1FF',
                   'South America' = 'white')

custom_colors.2 <- rev(brewer.pal(5, 'BuPu'))


# simplify data
player_data_by_region_game_genre.2 <- player_data_by_region_game_genre.1 %>% 
  select(Game, Genre, totalEarnings, Continent_Name)

alpha_value <- 1

alluvial.2 <- ggplot(player_data_by_region_game_genre.2)+
  aes(y = totalEarnings,
      axis1 = Continent_Name,
      axis2 = Genre,
      axis3 = Game)+
  geom_stratum()+
  geom_text(stat = 'stratum',
            size = 3,
            aes(label = after_stat(stratum)))+
  geom_alluvium(aes(fill = Continent_Name))+
  scale_fill_manual(values = custom_colors.2)+
  labs(title = "Earnings Flow")+
  theme_void()+guides(scale='none')+
  theme(
    panel.spacing = unit(0, 'lines'))

alluvial.2
ggsave('alluvialfinal.pdf')
