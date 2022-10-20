gapminder_1997 <- read_csv("gapminder_1997.csv")


ggplot(data = gapminder_1997)+
  aes(x = gdpPercap, y = lifeExp, size = pop/1000000, color = continent, pallete = "Set1")+
  labs(x = "GPD per capital", y = "Life Expectancy", size = "Population in millions", title = "Do people in wealthy countries live longer?")+
  geom_point()
  
# Plotting for data exploration

gapminder_data <- read_csv("gapminder_data.csv")

ggplot(data = gapminder_data)+
  aes(x=year,y=lifeExp,color = continent)+
  geom_point()

ggplot(data = gapminder_data)+
  aes(x=year,y=lifeExp,color = continent, group = country)+
  geom_line()

ggplot(data = gapminder_data)+
  aes(x=continent,y=lifeExp, color = continent)+
  geom_boxplot()

ggplot(data = gapminder_1997)+
  aes(x=continent,y=lifeExp)+
  geom_violin(aes(fill = "lavenderblush"))+
  geom_jitter(alpha = 0.5)


ggplot(gapminder_1997)+
  aes(x = lifeExp)+
  geom_histogram(bins = 100)

ggplot(gapminder_1997)+
  aes(x = lifeExp)+
  geom_density()

# ggplot2 Themes

ggplot(gapminder_1997)
  aes(x = lifeExp)+
  geom_histogram()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
  
# Facet
  
ggplot(gapminder_1997)+
  aes(x=gdpPercap,y=lifeExp)+
  geom_point()+
  facet_wrap(vars(continent))

ggplot(gapminder_1997)+
  aes(x=gdpPercap,y=lifeExp)+
  geom_point()+
  facet_grid(rows=vars(continent))


ggsave("awesome_plot.jpg", width = 6, height = 4)

violin_plot <- ggplot(data = gapminder_1997) +
  aes(x=continent,y=lifeExp)+
  geom_violin(aes(fill = continent))+
  theme_bw()
violin_plot 

ggsave(violin_plot,filename = "awesome_violin_plot.jpg",width = 6, height = 4)

install.packages(c("gganimate","gifski"))

library(gifski)
library(gganimate)

ggplot(data = gapminder_data)+
  aes(x = log(gdpPercap), y = lifeExp, size = pop, color = continent)+
  geom_point()

staticHansPlot <- ggplot(data = gapminder_data)+
  aes(x = log(gdpPercap), y = lifeExp, size = pop/1000000, color = continent)+
  geom_point(alpha = 0.5)+
  scale_color_brewer(palette = "Set1")+
  labs(x = "GDP per Capita", y = "Life Expectantcy", color = "Continent", size = "Population in Millions")+
  theme_classic()
staticHansPlot

animatedHansPlot <- staticHansPlot +
  transition_states(year, transition_length = 1, state_length = 1) +
  ggtitle("{closest_state}")

anim_save("hansAnimatedPlot.gif", animatedHansPlot,renderer = gifski_renderer())
