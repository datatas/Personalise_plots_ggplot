#############################################################################
#Workshop: Personalising your plots using ggplot2 
#Author: Denisse Fierro Arcos
#Date of creation: 2021-09-06
#Details: 
#In this script we will personalise a plot using ggplot2 


# Loading libraries -------------------------------------------------------
library(tidyverse)
library(magrittr)
library(ggrepel)


# Loading data ------------------------------------------------------------
#We will use Our World in Data information about life satisfaction
#Information about the dataset can be found here: 
#https://ourworldindata.org/happiness-and-life-satisfaction

#We will try to replicate this graph: 
#https://ourworldindata.org/grapher/gdp-vs-happiness

happiness <- read_csv("data/gdp-vs-happiness.csv")

#We can now check the structure of our data
glimpse(happiness)

#Let's create a dataset with information about countries and continents
continent <- happiness %>% 
  #We select the columns of interest only
  select(c(Entity, Continent)) %>% 
  #Remove any rows with NA values
  drop_na()

# Data manipulation -------------------------------------------------------
happy <- happiness %>% 
  #Let's select the columns that we need
  rename("life_satisfaction" = `Life satisfaction in Cantril Ladder (World Happiness Report 2021)`,
         "gdp_per_capita" = `GDP per capita, PPP (constant 2017 international $)`,
         "total_population" = `Total population (Gapminder, HYDE & UN)`) %>% 
  #Let's make those names better
  janitor::clean_names() %>%
  #Select all columns in happy except continent
  select(-continent) %>% 
  #A left join will only keep the information on the left dataframe
  #which in this case is happy (without the continent column)
  left_join(continent, by = c('entity'= 'Entity')) %>% 
  #Let's remove any rows for which there is no information about life satisfaction
  drop_na(life_satisfaction:total_population) %>% 
  #Let's make those names better
  janitor::clean_names() %>%
  #Now let's select the latest year for which there was data for each country
  top_n(n = 1, wt = year) %>%
  #We will transform all columns to factors except for life satisfaction and GDP
  mutate_at(vars(-(life_satisfaction:total_population)), as.factor) %>% 
  mutate(gdp_log = log10(gdp_per_capita))

#We can check the result
glimpse(happy)


# Plotting data -----------------------------------------------------------
#Basic plot
happy %>% 
  ggplot(aes(x = gdp_log, y = life_satisfaction))+
  geom_point()

#Changing colors and point sizes
happy %>% 
  ggplot(aes(x = gdp_log, y = life_satisfaction, 
             col = continent, size = total_population))+
  #We can also change the transparency
  geom_point(alpha = 0.5)

#Let's add labels and change the theme
happy %>% 
  ggplot(aes(x = gdp_log, y = life_satisfaction, 
             col = continent, size = total_population))+
  geom_point(alpha = 0.75)+
  #Add a title, caption and labels for axes
  labs(x = "GDP per capita (USD$)", 
       y = "Average life satisfaction (0-10)",
       title = "Self-reported life satisfaction vs. GDP per capita",
       caption = "Source: World Happiness Report (2021)",
       col = " ")+
  #Remove the legend related to point size
  guides(size = "none")+
  #We can change the theme
  theme_bw()+
  #Change the grid lines
  theme(panel.grid = element_line(colour = "#dde0e3", linetype = "dashed"), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "#dde0e3", linetype = "dashed"))

#We are almost there, let's change the point size so they look a little bigger
happy %>% 
  ggplot(aes(x = gdp_log, y = life_satisfaction, 
             col = continent, size = total_population/1e6))+
  geom_point(alpha = 0.75)+
  #Labels
  labs(x = "GDP per capita (USD$)", 
       y = "Average life satisfaction (0-10)",
       title = "Self-reported life satisfaction vs. GDP per capita",
       caption = "Source: World Happiness Report (2021)",
       col = " ")+
  #Let's make the points a little bigger
  scale_size(range = c(1, 10))+
  #We can choose a different color palette
  scale_color_brewer(palette = "Dark2")+
  #We can change the labels of the x axis
  #First we define the breaks and then the labels
  scale_x_continuous(breaks = c(log10(500), log10(1e3), log10(2e3), log10(5e3),
                                log10(1e4), log10(2e4), log10(1e5)),
                     labels = function(x) format(x = c(500, 1000, 2000, 5000, 
                                                       10000, 20000, 100000), 
                                                 scientific = F))+
  #Remove the legend related to point size
  guides(size = "none")+
  #We can change the theme
  theme_bw()+
  #Change the grid lines
  theme(panel.grid = element_line(colour = "#dde0e3", linetype = "dashed"), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "#dde0e3", linetype = "dashed"))
  
#We can now add annotations - Let's try it on the basic plot
happy %>% 
  ggplot(aes(x = gdp_log, y = life_satisfaction, 
             col = continent, size = total_population))+
  #We can also change the transparency
  geom_point(alpha = 0.5)+
  geom_text(aes(label = entity))

#Does not look great - let's make some changes
happy %>% 
  #We will also move size away from the global aesthetics
  ggplot(aes(x = gdp_log, y = life_satisfaction, 
             col = continent))+
  #We move it under point so it does not affect any other geometry
  geom_point(aes(size = total_population), 
             alpha = 0.5)+
  geom_label(aes(label = entity), label.padding = unit(0.2, "lines"),
             alpha = 0.7)

#There are too many labels, so we can choose a few countries
#We will choose them base on their total population
top_labels <- happy %>% 
  group_by(continent) %>% 
  top_n(n = 1, wt = total_population)

bottom_labels <- happy %>% 
  group_by(continent) %>% 
  top_n(n = -1, wt = total_population)

#Let's try again
happy %>% 
  #We will also move size away from the global aesthetics
  ggplot(aes(x = gdp_log, y = life_satisfaction, 
             col = continent))+
  #We move it under point so it does not affect any other geometry
  geom_point(aes(size = total_population), 
             alpha = 0.5)+
  #We will also use a new function to improve the position of labels
  geom_label_repel(data = top_labels, aes(label = entity), 
             label.padding = unit(0.2, "lines"),
             alpha = 0.7)+
  geom_label_repel(data = bottom_labels, aes(label = entity),
             label.padding = unit(0.2, "lines"),
             alpha = 0.7)+
  #We will also add a ring around the points that have labels,
  #This way they can be identified easier
  geom_point(data = top_labels, shape = 1, color = "black",
             aes(size = total_population))+
  geom_point(data = bottom_labels, shape = 1, color = "black",
             aes(size = total_population))

#That is looking better, now we add this to our previous code
happy %>% 
  #We will also move size away from the global aesthetics
  ggplot(aes(x = gdp_log, y = life_satisfaction, 
             col = continent))+
  #We move it under point so it does not affect any other geometry
  geom_point(aes(size = total_population), 
             alpha = 0.5)+
  #We will also use a new function to improve the position of labels and remove it from
  #the legend
  geom_label_repel(data = top_labels, aes(label = entity), 
                   label.padding = unit(0.2, "lines"), show.legend = F,
                   alpha = 0.7, min.segment.length = 0, nudge_x = 0.1, nudge_y = -0.1)+
  geom_label_repel(data = bottom_labels, aes(label = entity),
                   label.padding = unit(0.2, "lines"), show.legend = F,
                   alpha = 0.7, min.segment.length = 0, nudge_x = -0.1, nudge_y = 0.1)+
  #We will also add a ring around the points that have labels,
  #This way they can be identified easier
  geom_point(data = top_labels, shape = 1, color = "black",
             aes(size = total_population))+
  geom_point(data = bottom_labels, shape = 1, color = "black",
             aes(size = total_population))+
  #Labels
  labs(x = "GDP per capita (USD$)", 
       y = "Average life satisfaction (0-10)",
       title = "Self-reported life satisfaction vs. GDP per capita",
       caption = "Source: World Happiness Report (2021)",
       col = " ")+
  #Let's make the points a little bigger
  scale_size(range = c(1, 10))+
  #We can choose a different color palette
  scale_color_brewer(palette = "Dark2")+
  #We can change the labels of the x axis
  #First we define the breaks and then the labels
  scale_x_continuous(breaks = c(log10(500), log10(1e3), log10(2e3), log10(5e3),
                                log10(1e4), log10(2e4), log10(1e5)),
                     labels = function(x) format(x = c(500, 1000, 2000, 5000, 
                                                       10000, 20000, 100000), 
                                                 scientific = F))+
  #Remove the legend related to point size
  guides(size = "none")+
  #We can change the theme
  theme_bw()+
  #Change the grid lines
  theme(panel.grid = element_line(colour = "#dde0e3", linetype = "dashed"), 
        panel.border = element_blank(),
        axis.line = element_line(colour = "#dde0e3", linetype = "dashed"),
        #We will increase the size of the text
        legend.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, hjust = 0.5))

#If we are happy with the result, we can save
ggsave("figures/Satisfaction_GDP.png", device = "png", dpi = 300, width = 10, height = 8)
