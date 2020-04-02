#Clear all workspace

rm(list = ls())

#Add third-party libraries

library(rgdal)
library(ggplot2)
library(dplyr)
library(grid)
library(sp)
library(maptools)
library(mapproj)
library(sf)
library(gpclib)
library(classInt)
library(tmaptools)


#Loading shape-file of Russian administrative division in R

##Loadnig shape-file of Russian adm.division without Crimea

russia.adm1.shp = readOGR(dsn = "data", 
                          layer = "gadm36_RUS_1", 
                          stringsAsFactors = FALSE)

##Loadnig shape-file of Ukraine adm.division

ukraine.adm1.shp = readOGR(dsn = "data", 
                           layer = "gadm36_UKR_1", 
                           stringsAsFactors = FALSE)

##Fix bug related to the division of Chukotka into 2 parts

for (i in 1:length(russia.adm1.shp@polygons)) {
  for (j in 1:length(russia.adm1.shp@polygons[[i]]@Polygons)) {
    russia.adm1.shp@polygons[[i]]@Polygons[[j]]@coords[,1] = sapply(russia.adm1.shp@polygons[[i]]@Polygons[[j]]@coords[,1], function(x){
      if (x < 0) {
        x = 359.999 + x
      }
      else{x}
    })
  }
}

##Conversion shape-files for ggplot reading 

russia.adm1.shp.df = fortify(russia.adm1.shp)
russia.adm1.shp.df$id = as.integer(russia.adm1.shp.df$id)

ukraine.adm1.shp.df = fortify(ukraine.adm1.shp)
crimea.shp.df = dplyr::filter(ukraine.adm1.shp.df, (id == '3' | id == '19'))
crimea.shp.df$group = as.numeric(crimea.shp.df$group)

for (k in 1:length(crimea.shp.df$id)) {
  if (crimea.shp.df$id[k] == 3) {
    crimea.shp.df$id[k] = 83
    
  }else {
    crimea.shp.df$id[k] = 84
    crimea.shp.df$group[k] = "84.1"
  }
}

for (i in 1:31711) {
  crimea.shp.df$group[i] = "83.1"
}

for (i in 31712:32336) {
  crimea.shp.df$group[i] = "83.2"
}

for (i in 32337:32447) {
  crimea.shp.df$group[i] = "83.3"
}

for (i in 32448:32451) {
  crimea.shp.df$group[i] = "83.4"
}

for (i in 32452:32455) {
  crimea.shp.df$group[i] = "83.5"
}

for (i in 32456:32540) {
  crimea.shp.df$group[i] = "83.6"
}

for (i in 32541:32597) {
  crimea.shp.df$group[i] = "83.7"
}

for (i in 32598:32656) {
  crimea.shp.df$group[i] = "83.8"
}

for (i in 32657:32694) {
  crimea.shp.df$group[i] = "83.9"
}

for (i in 32695:32707) {
  crimea.shp.df$group[i] = "83.10"
}

for (i in 32708:32728) {
  crimea.shp.df$group[i] = "83.11"
}


crimea.shp.df$group = as.factor(crimea.shp.df$group)

rusmap.without.df = rbind(crimea.shp.df, russia.adm1.shp.df)
rusmap.without.df$id = as.integer(rusmap.without.df$id)


#Loading dataset 
osn = 1.01
map.data = read.csv('~/data/data.csv', header = TRUE, encoding = 'UTF-8', sep = ";")
map.data = mutate(map.data, DATA = as.numeric(log(DATA, base=osn)))

#Merging dataset and shape-file

map.df = merge(rusmap.without.df, map.data, by.x = 'id', by.y = 'ID', all.x = TRUE)

brks = classIntervals(map.df$DATA, n = 11, style = "kmeans")$brks
nbrks = length(brks)

theme_bare = theme(axis.line = element_blank(),
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title.x = element_blank(), 
                   axis.title.y = element_blank(), 
                   panel.spacing = unit(c(0,0,0,0), 'cm'), 
                   axis.ticks.length = unit(0.001, 'cm'), 
                   plot.margin = unit(c(0,0,0,0), 'cm'),
                   legend.position = c(.92,.5),
                   panel.grid = element_blank(), 
                   panel.background = element_blank(), 
                   plot.title = element_text(hjust = .5, 
                                             vjust = -9 , 
                                             size = 12, 
                                             face = 'bold'))


ggplot() + 
  geom_polygon(data = map.df, 
               mapping = aes(x = long, 
                             y = lat, 
                             group = group, fill = DATA), color = "#383838") + 
  coord_map(projection = 'azequidist', 
            orientation = c(90, 5, 95)) + 
  theme_bare + 
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'RdYlGn'), 
                       breaks = brks,
                       labels = paste(c(0, round(osn^(brks[-nbrks]))), '-', round(osn^(brks))),
                       guide = guide_legend(keyheight = unit(0.5, "cm"), reverse = TRUE)
  ) +
  labs(title = 'Региональная структура инвестиций в основной капитал за исключением бюджетных инвестиций \nза 2012-2018 г.г. накопленным итогом', fill = '\nмлрд. руб' ) + 
  geom_point(aes(x = c(37.615, 30.181, 33.791), 
                 y = c(55.752, 59.890, 44.390)), 
             color = c('#006837',
                      '#006837', 
                      '#A50026'), 
             size = 3) + 
  geom_label(x = c(40.115,26.481, 32.091), 
             y = c(55.752, 60.090, 41.590), 
             aes(label = c("Москва", "Санкт-Петербург", "Севастополь")), 
             color = "black", 
             size = 3, 
             fontface = c('bold','bold' ,'bold' ), 
             alpha = c(0.5,0.5,0.5))
