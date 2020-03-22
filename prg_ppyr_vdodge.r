##############################################################################################################################
##POPULATION PYRAMID WITH A DECOMPOSITION FOR THREE VARIABLES
##
##CÉSAR CRISTANCHO. MARCH, 2020 (LAST UPDATED: MARCH, 2020)
##
##cacristanchof@gmail.com
##
##
##
##THERE IS NO WARRANTY FOR THIS CODE
##############################################################################################################################

library(tidyverse)
library(data.table)
library(ggthemes)
library(forcats)

bd <- fread('Casos1_2103B.csv', encoding = 'UTF-8')

bdb <- bd %>% 
  count(Sexo, Edad, `Tipo*`) %>% 
  mutate(Sexo = factor(Sexo, levels = c('M', 'F'), labels = c('Hombres', 'Mujeres' ) ))  %>% 
  mutate(`Tipo*`= factor(`Tipo*`) ) %>%
  ungroup() %>%
  complete(Sexo, Edad, `Tipo*`,
           fill = list(n = 0))

p1 <- 
  ggplot() + 
  geom_bar(data = filter(bdb, Sexo=='Hombres'), aes(Edad, n* -1), 
           stat = "identity", fill = "dodgerblue1") +
  geom_bar(data = filter(bdb, Sexo=='Hombres'), aes(Edad, n* -1, fill = `Tipo*`), 
           stat = "identity", position = "dodge", colour='black') + 
  geom_bar(data = filter(bdb, Sexo=='Mujeres'), aes(Edad, n ),
           stat = "identity", fill = "dodgerblue1") +
  geom_bar(data = filter(bdb, Sexo=='Mujeres'), aes(Edad, n , fill = `Tipo*`),
           stat = "identity", position = "dodge", colour='black') +
  scale_fill_brewer(palette = "PuBu", drop = FALSE)   +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    breaks = seq(-20, 20, 10), 
    labels =  abs( seq(-20, 20, 10) )
    ) +
  coord_flip() +
  theme_gray(base_size = 20  ) +
  theme(panel.background = element_rect(fill = "#34495E", colour = "#435E7A"),
        panel.grid = element_line(colour = "#435E7A") ,
        strip.background = element_rect(colour = "#C4DDEC", fill = "#C4DDEC"),
        plot.background = element_rect(fill = "#C1DBEC") ,
        legend.background = element_rect(fill =   "#C4DDEC"), 
        legend.position = 'bottom'
        ) +
  ylab('Número de casos') +
  annotate('text', x = 9, y = -20, label = 'Hombres', colour='white', size = 8) +
  annotate('text', x = 9, y =  20, label = 'Mujeres', colour='white', size = 8) +
  labs(title = 'Casos de COVID-19 por edad y sexo según tipo en Colombia', 
       subtitle = 'Fuente: Instituto Nacional de Salud. 21 de Marzo de 2020', 
       caption = 'cacristanchof@gmail.com') 

p1
