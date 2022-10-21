library(tidyverse)

# load in data
gapminder_data <- read_csv("data/gapminder_data.csv")

# summarizing our data

summarize(gapminder_data,averageLifeExp=mean(lifeExp))

gapminder_data %>% summarize(averageLifeExp=mean(lifeExp))

gapminder_data_summarized <- gapminder_data %>% 
  summarize(averageLifeExp=mean(lifeExp))





# filtering data using filter()
gapminder_data %>%
  filter(year == 2007) %>%
  summarize(average=mean(lifeExp))

# finding min year
gapminder_data %>% summarize(earliest_year = min(year))

# based on min year, what is GDP
gapminder_data %>%
  filter(year == min(year)) %>%
  summarize(Avg_GDP = mean(gdpPercap))

# grouping data 
gapminder_data %>%
  group_by(year) %>%
  summarize(average = mean(lifeExp))

gapminder_data %>%
  group_by(continent) %>%
  summarize(average = mean(lifeExp), min = min(lifeExp))

# adding new columns with mutate()
gapminder_data %>%
  mutate(gdp = pop * gdpPercap,
         popInMillions = pop/1000000)

# select columns with select() and change their order
gapminder_data %>%
  select(pop,year)

gapminder_data %>%
  select(-pop, -gdpPercap,-year, continent, country)

gapminder_data %>%
  select(gdpPercap, everything())

# convert between long/wide data with pivot_wider()/pivot_longer()
gapminder_data %>%
  select(country,continent,year,lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp) #example of wide data

# original csv is long data

# dataset for analysis

gapminder_data_2007 <- read_csv("Data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year,-continent)

# data cleaning

read_csv("data/co2-un-data.csv",skip = 1)

read_csv("data/co2-un-data.csv",skip = 2,
         col_names = c("region","country","year",
                       "series","value","footnotes","source")) %>%
  select(-footnotes)

read_csv("data/co2-un-data.csv",skip = 1) %>%
  rename(country = ...2)

read_csv("data/co2-un-data.csv",skip = 1) %>%
  rename_all(tolower)

co2_emissions_dirt <- read_csv("data/co2-un-data.csv",skip = 2,
                                col_names = c("region","country","year",
                                              "series","value","footnotes","source"))


co2_emissions_dirt %>%
  select(country,year,series,value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)"="Total Emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "Per Capita Emissions"))%>%
  pivot_wider(names_from = series, values_from = value)%>%
  count(year) #number of oveservations per year

co2_emissions <- co2_emissions_dirt %>%
  select(country,year,series,value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)"="Total_Emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "Per_Capita_Emissions"))%>%
  pivot_wider(names_from = series, values_from = value)%>%
  filter(year == 2005) %>%
  select(-year)

# joining data frames

df <- inner_join(gapminder_data_2007, co2_emissions)

anti_join(gapminder_data_2007, co2_emissions,
          by = "country")

co2_emissions <- read_csv("data/co2-un-data.csv",
                          skip = 2,
                          col_names = c("region","country","year",
                                        "series","value","footnotes","source")) %>%
  select(country,year,series,value)%>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)"="Total_Emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "Per_Capita_Emissions"))%>%
  pivot_wider(names_from = series, values_from = value)%>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country, "Bolivia (Plurin. State of)"= "Bolivia",
                          "United States of America"= "United States",
                          "Venezuela (Boliv. Rep. of)"= "Venezuela"))

# check which country doesnt join nicely 
anti_join(gapminder_data_2007,co2_emissions, by = "country")

gapminder_data_2007 <- read_csv("data/gapminder_data.csv")%>%
  filter(year == 2007 & continent == "Americas")%>%
  select(-year,-continent)%>%
  mutate(country = recode(country, "Puerto Rico" = "United States"))

anti_join(gapminder_data_2007,co2_emissions,by = "country")

# reads in gapminder data, filters by 2007, changed p.r. to u.s.
gapminder_data_2007 <- read_csv("data/gapminder_data.csv")%>%
  filter(year == 2007 & continent == "Americas")%>%
  select(-year,-continent)%>%
  mutate(country = recode(country, "Puerto Rico" = "United States"))%>%
  group_by(country) %>%
  summarize(lifeExp = sum(lifeExp * pop)/sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop))

gapminder_co2 <- inner_join(gapminder_data_2007,co2_emissions, by = "country")

gapminder_co2 %>%
  mutate(region = if_else(country == "Canada"|
                            country == "United States"|
                            country == "Mexico", "North", "South"))
write_csv(gapminder_co2,"data/gapminder_co2.csv")

# plotting

plot <- ggplot(gapminder_co2) +
  aes(x=gdpPercap, y=Per_Capita_Emissions)+
  labs(x = "GDP (per capita)",
       y = "Emissions per capita",
       title = "There is a strong association between a nation's GDP \n 
and the amount of CO2 it produces")+
  geom_point()+ 
  geom_smooth(method = "lm")
 

ggsave(plot, filename = "figures/Emissions_per_cap_GDP.png",width = 6, height = 4)
