#############################################################################
#Workshop: Personalising your plots using ggplot2 
#Author: Denisse Fierro Arcos
#Date of creation: 2021-09-05
#Details: 
#In this script we will personalise a plot using ggplot2. We will combine
#two plots and insert an image to the plots


# Loading libraries -------------------------------------------------------
library(tidyverse)
library(magrittr)
library(patchwork)
library(cowplot)


# Loading data ------------------------------------------------------------
#We will use TidyTuesday data about bird bath sightings in Australia.
#Information about the dataset can be found here: 
#https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-08-31/readme.md

#We will try to replicate this graph: 
#https://twitter.com/tacheboutit/status/1432809364549771265

bird_baths <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

#We can now check the structure of our data
glimpse(bird_baths)

#How many bioregions are there
bird_baths %>% 
  distinct(bioregions)

#How many species
bird_baths %>% 
  distinct(bird_type) %>% 
  View()

# Data manipulation -------------------------------------------------------
counts_birds <- bird_baths %>% 
  drop_na() %>% 
  mutate_at(vars(-bird_count), as.factor) %>% 
  group_by(bird_type, urban_rural) %>% 
  summarise(N = sum(bird_count)) %>% 
  arrange(N)

#We can check the result
glimpse(counts_birds)

#We need to do some additional data manipulation
counts_birds %<>% 
  group_by(urban_rural) %>% 
  arrange(N) %>% 
  top_n(n = 10) %>% 
  mutate(order_label = row_number()) %>% 
  top_n(n = 10, wt = order_label) %>% 
  mutate(bird_type = fct_reorder(droplevels(bird_type), order_label, .desc = F))

#We will now split the dataset in two based on whether they are urban or rural
urb_rur <- counts_birds %>% 
  group_split()

#Let's check the names of the list items
names(urb_rur)

#Let's change the names of the items in the list for easier identification
names(urb_rur) <- counts_birds %>% 
  group_keys() %>% 
  pull()

#Now we check the names again
names(urb_rur)


# Plotting data -----------------------------------------------------------
urb_rur$Rural %>% 
  ggplot(aes(y = N, x = bird_type, fill = N))+
  geom_col(position = "dodge2")+
  coord_polar("x")

#We can fix the order of the factors
urb_rur$Rural %>%
  ggplot(aes(y = N, x = reorder(bird_type, order_label), fill = N))+
  geom_col(position = "dodge2")+
  coord_polar("x")+
  #Let's also add a line through the middle of each block
  geom_segment(aes(y = 0, 
                   xend = reorder(bird_type, order_label), yend = max(N)),
               linetype = "dashed", size = 1)+
  #Let's add the dot at the top of each bar
  geom_point(size = 3)+
  #Let's ensure the background lines appear every 20 individuals
  scale_y_continuous(name = "", limits = c(0, max(urb_rur$Rural$N)))+
  #And let's remove the grey background
  theme_bw()

#We are getting closer to our final result, let's buil on it a little more
f_rural <- urb_rur$Rural %>%
  #Let's add line breaks to the species names
  ggplot(aes(y = N, x = reorder(str_wrap(bird_type, 4), order_label), fill = N))+
  geom_col(position = "dodge2")+
  #We will turn off the clip option so the names of the species are not cut out
  coord_polar("x", clip = "off")+
  #Let's also add a line through the middle of each block
  geom_segment(aes(y = 0, 
                   #Note that we have change xend to match the global x
                   xend = reorder(str_wrap(bird_type, 4), order_label), 
                   yend = max(N)),
               linetype = "dashed", size = 1)+
  #Let's add the dot at the top of each bar
  geom_point(size = 3)+
  #Let's ensure the background lines appear every 20 individuals
  scale_y_continuous(name = "", limits = c(0, max(urb_rur$Rural$N)))+
  #And let's remove the grey background
  theme_bw()+
  #We will change the colour palette
  scale_fill_distiller(palette = "YlOrRd", direction = -1)+
  #Let's change the labels
  labs(title = "Most common birds in Australian \n bird baths (Rural)",
       caption = "Data source: Cleary et al, 2016
       Original figure: Julia Tache",
       x = "",
       fill = "Number of birds")+
  #We will change the grid color
  theme(panel.grid = element_line(colour = "#abb2ba"),
        #Remove the border
        panel.border = element_blank(),
        #Remove axis y
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10,
                                   colour = "black"),
        #Move legend to the bottom
        legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-15, 0, 0, 0),
        #We can center the plot title and the font
        plot.title = element_text(hjust = 0.5, face = "bold", 
                                  size = 14),
        plot.caption = element_text(hjust = 1,
                                    size = 10))+
  #We will do the final touch for the legend
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 15, 
                               ticks = F,
                               frame.colour = "black"))
f_rural

#Insert image of Superb fairy wren
fairy_wren <- png::readPNG("data/superb_fairy_wren.png", native = T)
f_rural <- ggdraw()+
  draw_plot(f_rural)+
  draw_image(fairy_wren, x = 0.01, y = 0.07, scale = .5)
#Let's check the result
f_rural


#Now let's do the same with the other dataset
f_urban <- urb_rur$Urban %>%
  #Let's add line breaks to the species names
  ggplot(aes(y = N, x = reorder(str_wrap(bird_type, 4), order_label), fill = N))+
  geom_col(position = "dodge2")+
  #We will turn off the clip option so the names of the species are not cut out
  coord_polar("x", clip = "off")+
  #Let's also add a line through the middle of each block
  geom_segment(aes(y = 0, 
                   #Note that we have change xend to match the global x
                   xend = reorder(str_wrap(bird_type, 4), order_label), 
                   yend = max(N)),
               linetype = "dashed", size = 1)+
  #Let's add the dot at the top of each bar
  geom_point(size = 3)+
  #Let's ensure the background lines appear every 20 individuals
  scale_y_continuous(name = "", limits = c(0, max(urb_rur$Urban$N)))+
  #And let's remove the grey background
  theme_bw()+
  #We will change the colour palette
  scale_fill_distiller(palette = "YlOrRd", direction = -1)+
  #Let's change the labels
  labs(title = "Most common birds in Australian \n bird baths (Urban)",
       caption = "Data source: Cleary et al, 2016
       Original figure: Julia Tache",
       x = "",
       fill = "Number of birds")+
  #We will change the grid color
  theme(panel.grid = element_line(colour = "#abb2ba"),
        #Remove the border
        panel.border = element_blank(),
        #Remove axis y
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10,
                                   colour = "black"),
        #Move legend to the bottom
        legend.position = "bottom",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-15, 0, 0, 0),
        #We can center the plot title and the font
        plot.title = element_text(hjust = 0.5, face = "bold", 
                                  size = 14),
        plot.caption = element_text(hjust = 1,
                                    size = 10))+
  #We will do the final touch for the legend
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 15, 
                               ticks = F,
                               frame.colour = "black"))
#We can check the result
f_urban

#Add the bird image
noisy_miner <- png::readPNG("data/noisy_miner.png", native = T)
f_urban <- ggdraw()+ draw_plot(f_urban)+
  draw_image(noisy_miner, x = 0.01, y = 0.045, scale = .5)
#Let's check the result
f_urban

#Now let's put the two plots together
comb_fig <- f_rural+f_urban
comb_fig

#Finally we can save the results
ggsave("figures/ruralAustralianBirds.png", f_rural, device = "png", 
      dpi = 300)

ggsave("figures/urbanAustralianBirds.png", f_urban, device = "png", 
       dpi = 300)

ggsave("figures/AustralianBirds.png", comb_fig, device = "png", 
       dpi = 300)
