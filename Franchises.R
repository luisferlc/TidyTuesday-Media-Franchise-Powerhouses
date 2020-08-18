library(tidyverse)
library(ggthemes)
library(RColorBrewer)

#source: https://en.wikipedia.org/wiki/List_of_highest-grossing_media_franchises

data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")
write.csv(data, file="C:/Users/luisf/Documents/R/TidyTuesdays/Media Franchise Powerhouses/highest grossing media franchises.csv")

data %>%
  glimpse()

view(data)

#1.- Top 15 most revenue franchises with revenue categories
#2.- Top 10 Owners most revenue and their franchises
#3.- Top 10 most profitable franchises


#NA's in revenues?:
data %>%
  summarise(NAs = sum(is.na(revenue)))

#######################################################################################################

                                ################################
                                ############## #1 ##############
                                ################################

                            #Learning to use fct_reorder and fct_lump

#######################################################################################################

##Top 15 most revenue franchises with categories

top15franchises_categories <- data %>%
  group_by(franchise, revenue_category, year_created) %>%
  summarise(sum_revenue = sum(revenue)) %>%
  ungroup() %>%
  mutate(revenue_category = fct_reorder(revenue_category, sum_revenue, sum, .desc = T),
         franchise = fct_lump(franchise, n = 15, w = sum_revenue)) %>%
  filter(franchise != "Other") %>%
  mutate(franchise = str_c(franchise, "(",year_created,")"),
         franchise = fct_reorder(franchise, sum_revenue, sum))

display.brewer.all()

ggplot(top15franchises_categories, aes(franchise, sum_revenue, fill = revenue_category)) +
  geom_col(width = .8) + coord_flip() +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_clean() +
  theme(
    plot.margin = margin(15,15,2,2),
    panel.background = element_rect(fill = "black", color = "white"),
    plot.background = element_rect(fill = "black"), 
    plot.caption = element_text(color = "white"),
    plot.title = element_text(size = 15, face = "bold", color = "white", hjust = .5),
    axis.text = element_text(color = "white", size = 10),
    axis.title = element_text(face="bold", color = "white", size = 12),
    legend.text = element_text(size = 10, color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black", color = "white"),
    legend.position = c(0.77, 0.3)
  ) +
  labs(title="Highest-grossing media franchises", x=NULL, y="Revenue (in billions)", caption= "Source: Wikipedia \n Made by @ShiXiong3 for #TidyTuesday",
       fill = "Revenue Categories")

#######################################################################################################

                                ################################
                                ############## #2 ##############
                                ################################

                            #Learning to user geom_sina() and geom_repel()

#######################################################################################################

## Top 10 owners per revenue

#Rename some owner names
data2 <- data %>%
  mutate(owners = recode(owners, "Nintendo (trademark) The Pokémon Company (Nintendo, Game Freak, Creatures) (copyright)" = "Nintendo",
                         "Shueisha (Hitotsubashi Group)" = "Shueisha"))

topowners <- data2 %>%
  group_by(owners) %>%
  summarise(sum_revenue = sum(revenue)) %>%
  top_n(5, sum_revenue) %>%
  pull(owners)


###Plotting
install.packages("ggforce")
install.packages("ggrepel")
library(ggforce)
library(ggrepel)

data2 %>%
  unique() %>%
  filter(owners %in% topowners) %>%
ggplot(aes(owners, revenue)) +
  geom_violin(aes(fill = owners),alpha = .6) +
  geom_sina(aes(fill = owners), color = "black", size = 3, shape=21) +
  geom_label_repel(
    data = data2 %>%
    unique() %>%
    mutate(revenue_category = recode(revenue_category, "Merchandise, Licensing & Retail" = "Merchandise")) %>%
    filter(owners %in% topowners) %>%
    filter(revenue >= 15) %>%
    mutate(categ = str_c(franchise, "(",revenue_category, ")")),
    aes(owners, revenue, label = categ), color="black", fill = "white", size = 3.5) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_clean() +
  theme(
    panel.background = element_rect(fill = "black", color = "white"),
    plot.background = element_rect(fill = "black"), 
    plot.caption = element_text(color = "white"),
    plot.title = element_text(size = 15, face = "bold", color = "white", hjust = .5),
    axis.text = element_text(color = "white", size = 10),
    axis.title = element_text(face="bold", color = "white", size = 11),
  ) +
  guides(fill = F) +
  labs(title="Top Franchise Owners", x=NULL, y="Revenue (in billions)", caption= "Source: Wikipedia \n Made by @ShiXiong3 for #TidyTuesday")


data2 %>%
  filter(owners == "Froebel-kan" ) %>%
  view() #This is why the unique()

#######################################################################################################

                                    ################################
                                    ############## #3 ##############
                                    ################################

#######################################################################################################

##Top 10 most profitable franchises

total_revenue <- sum(data$revenue)

top10most_profitable <- data %>%
  group_by(franchise) %>%
  transmute(
    revenue_sum = sum(revenue),
    year_created = year_created,
    life_time = 2019 - year_created,
    revenue_rate = revenue_sum/life_time
  ) %>%
  unique() %>%
  ungroup() %>%
  mutate(
    franchise = fct_lump(franchise, n = 10, w = revenue_rate)) %>%
  filter(franchise != "Other") %>%
  mutate(
    franchise = str_c(franchise, " (",year_created,"/",life_time,")"),
    franchise = fct_reorder(franchise, revenue_rate),
    revenue_sum = str_c(revenue_sum, "$")
  ) 

###Plotting

ggplot(top10most_profitable, aes(franchise, revenue_rate, fill = franchise)) +
  geom_col(width = .8) + coord_flip() +
  geom_label(aes(franchise, revenue_rate, label = revenue_sum),color="black", fill = "white",
             label.padding = unit(0.20, "lines"),
             ) +
  scale_fill_brewer(palette = "PRGn") +
  theme_clean() +
  theme(
    plot.margin = margin(10,10,2,2),
    panel.background = element_rect(fill = "black", color = "white"),
    plot.background = element_rect(fill = "black"), 
    plot.caption = element_text(color = "white"),
    plot.title = element_text(size = 15, face = "bold", color = "white", hjust = .5),
    axis.text = element_text(color = "white", size = 10),
    axis.title = element_text(face="bold", color = "white", size = 12),
    legend.text = element_text(size = 10, color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_rect(fill = "black", color = "white"),
    legend.position = c(0.77, 0.3)
  ) +
  guides(fill = F) +
  labs(title="Most Profitable Franchises", x="(Year Created / Life Time)", y="Billion Rate Per Year", caption= "Source: Wikipedia \n Made by @ShiXiong3 for #TidyTuesday")
