rm(list = ls())
library(viridis)
colors = c('blue4', 'dodgerblue1', 'cyan', 'green', 'gold', 'darkorange2', 'red')
map.colors = function(temp){
  col = temp
  col[temp == 13] = colors[1]
  col[temp == 16] = colors[2]
  col[temp == 19] = colors[3]
  col[temp == 22] = colors[4]
  col[temp == 26] = colors[5]
  col[temp == 27] = colors[6]
  col[temp == 28] = colors[7]
  return(col)
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

library(factoextra)
library(FactoMineR)
library(ade4)
library(measurements)
detach(tab)
tab = read.csv('/home/bg33novu/projects/WarmingWebs/results/network_topologies.csv', header = T)
names(tab) = c("name", "temp_sea", "temp", "area", "detph" ,"elevation", "Nb species", "Nb links", "Connectance", "Mean omnivory", 
              "PredPreyRatio", "Mean generalism", "% basal", "%intermediate", "%top", "Mean TL", "Mean TL top species", "Avg path length" )
library(ggplot2)
head(tab)
# size = tab$area*tab$detph
# considering pools as spherical caps:
radius2 = tab$area / pi
size = (1/6) * pi * tab$detph *(3*radius2 + tab$detph*tab$detph)

tab$min.sea.temp = NA
tab$max.sea.temp = NA
tab$mean.sea.temp = NA
tab$mean.summer = NA
tab$min.summer = NA
tab$max.summer = NA


seetemps = read.table('~/projects/WarmingWebs/R_code2/see_temps', header = T, sep = ',')
for (name in seetemps$name){
  cat(name, '\n')
  xx = grep(pattern = name, x = tab$name, fixed = TRUE)
  tab$min.sea.temp[xx] = seetemps$Min[seetemps$name == name]
  tab$max.sea.temp[grep(pattern = name, x = tab$name, fixed = TRUE)] = seetemps$Max[seetemps$name == name]
  tab$mean.sea.temp[grep(pattern = name, x = tab$name, fixed = TRUE)] = seetemps$Mean[seetemps$name == name]
  tab$mean.summer[grep(pattern = name, x = tab$name, fixed = TRUE)] = seetemps$MeanSummer[seetemps$name == name]
  tab$min.summer[grep(pattern = name, x = tab$name, fixed = TRUE)] = seetemps$MaxSummer[seetemps$name == name]
  tab$max.summer[grep(pattern = name, x = tab$name, fixed = TRUE)] = seetemps$MinSummer[seetemps$name == name]
}

# latitude = tab$temp_see
# latitude[latitude == 13] = 48
# latitude[latitude == 16] = 50
# latitude[latitude == 19] = 38
# latitude[latitude == 22] = 32
# latitude[latitude == 26] = -23
# latitude[latitude == 27] = -25
# latitude[latitude == 28] = -3


latitude = tab$tempsee
latitude[grepl('Portugal_txt', tab$name)] = '38 42 38'
latitude[grepl('Canada_txt/PP', tab$name)] = '48 29 33'
latitude[grepl('Canada_txt/SF', tab$name)] = '48 36 43'
latitude[grepl('England_txt/MB', tab$name)] = '50 21 24'
latitude[grepl('England_txt/W', tab$name)] = '50 19 00'
latitude[grepl('Portugal_txt/CR', tab$name)] = '38 42 38'
latitude[grepl('Portugal_txt/RV', tab$name)] = '39 17 11'
latitude[grepl('Mad_txt/PC', tab$name)] = '32 46 32'
latitude[grepl('Mad_txt/RM', tab$name)] = '32 38 44'
latitude[grepl('Mad_txt/RM', tab$name)] = '32 38 44'
latitude[grepl('Brasil\\(SP\\)_txt/', tab$name)] = '-23 35 00'
latitude[grepl('Brasil\\(CE\\)_txt/FX', tab$name)] = '3 13 04'
latitude[grepl('Brasil\\(CE\\)_txt/GJ', tab$name)] = '3 14 14'
latitude[grepl('Moz_txt', tab$name)] = '-25 58 36'

# unique(cbind.data.frame(tab$name, latitude))

latitude = as.numeric(conv_unit(latitude, "deg_min_sec", "dec_deg"))

latitude[grepl('Portugal_txt/L1', tab$name)] = 39.1508
latitude[grepl('Portugal_txt/L2', tab$name)] = 39.1508
latitude[grepl('Portugal_txt/L3', tab$name)] = 39.245223
latitude[grepl('Portugal_txt/L4', tab$name)] = 39.245223

tab$dif.with.sea = tab$mean.summer - tab$temp

data = tab[,7:dim(tab)[2]]

data2 = cbind(tab$elevation, tab$temp, size, abs(latitude), data, data$max.sea.temp - data$min.sea.temp)
names(data2) = c(c('Elevation', 'Temperature', 'Size', 'latitude'), names(data), 'Amplitude')

cbPalette <- c('blue4', 'dodgerblue3', 'lightskyblue', 'lightseagreen', 'gold', 'darkorange2', 'red')

z2 = PCA(data2, quanti.sup = c(1:4, 17:24))
z2$eig
z2$var
dimdesc(z2)
z2$quanti.sup


rowSums(z2$quanti.sup$coord)

g = fviz_pca_ind(z2,
                 geom.ind = c("point"),
                 col.ind = tab$temp,
                 # col.ind = size,
                 repel = TRUE,
                 legend.tittle = "water temp",
                 gradient.cols = c("blue", "#E7B800", "red"),
                 pointsize = 1.8,
                 addEllipses = F,
                 alpha = 0.8
)
g = g + labs(colour= 'Temperature', alpha.var="contrib")
par(mfrow = c(2,1))
g

### plot with elipses based on see temperature
g1 = fviz_pca_var(z2, 
                  repel = TRUE, 
                  )
g1 = g1 + guides(fill = 'none') + labs(title = 'a)') + theme(legend.position="none")
g1

col_pal = viridis(7, option = 'plasma')

g.mean.see = fviz_pca_ind(z2,
                 geom.ind = c("point"),
                 col.ind = as.factor(round(data2$mean.summer,1)),
                 addEllipses = TRUE,
                 repel = TRUE,
                 # legend.tittle = "water temp",
                 pointsize = 1.8,
                 alpha = 0.8,
                 palette = col_pal,
                 pointshape = 19,
                 # legend.tittle = "water temp",
                 # colours = 'adfasf',
                 show.legend.text = FALSE
                 
)
g.mean.see = g.mean.see + guides(fill = 'none') + labs(title = 'b)')
g.mean.see = g.mean.see + theme(legend.position="none")
g.mean.see

g.legend = fviz_pca_ind(z2,
                          geom.ind = c("point"),
                          col.ind = as.factor(round(data2$mean.summer,1)),
                          addEllipses = TRUE,
                          repel = TRUE,
                          # legend.tittle = "water temp",
                          pointsize = 1.8,
                          alpha = 0.8,
                          palette = col_pal,
                          pointshape = 19,
                          # legend.tittle = "water temp",
                          # colours = 'adfasf',
                          show.legend.text = FALSE
                          
)
g.legend = g.legend + guides(fill = 'none') + labs(title = 'b)', colour= 'Average \nsummer \nsea temp.')+
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14)
  )
legend = g_legend(g.legend)
save(g.legend, file = '/home/bg33novu/projects/WarmingWebs/R_code2/legend.Rdata')

save(g1, file = '~/projects/WarmingWebs/R_code2/acp_arrows.Rdata')
save(g.mean.see, file = '~/projects/WarmingWebs/R_code2/acp_elipsesRdata')

# g.mean.see$colour = 'aaa'
# g.mean.see$group = 'aaa'
# g.mean.see$col = 'aaa'
library(gridExtra)
# g.mean.see = g.mean.see + guides(fill = 'none') + labs(title = 'b)', colour= 'Summer See \nTemperature')
pdf('/homes/bg33novu/projects/WarmingWebs/paper/figures/Pcas.pdf', width = 28)
grid.arrange(arrangeGrob(g1, g.mean.see, ggplot(), legend, ncol = 4))
dev.off()
##############################


g.amplitude = fviz_pca_ind(z2,
                          geom.ind = c("point"),
                          col.ind = as.factor(data2$Amplitude),
                          addEllipses = TRUE,
                          repel = TRUE,
                          # legend.tittle = "water temp",
                          pointsize = 1.8,
                          alpha = 0.8,
                          # palette = cbPalette,
                          pointshape = 19,
                          # legend.tittle = "water temp",
                          # colours = 'adfasf',
                          show.legend.text = FALSE
                          
)


g.bi = fviz_pca_biplot(z2,
                geom.ind = c("point"),
                col.ind = tab$temp,
                # fill.ind = tab$temp,
                # habillage = as.factor(tab$temp),
                repel = TRUE,
                legend.tittle = "water\ntemp",
                col.var = 'black',
                col.quanti.sup = "blue",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                # scale_fill_gradient = 
                # pointshape = 21,
                pointsize = 1.8,
                addEllipses = FALSE,
                alpha = 0.8,
                title = '',
                # label = "none"
)

g.bi = g.bi + labs(colour= 'Pool \nTemperature')
g.bi
 fviz_pca_ind(z2)
dimdesc(z2)


boxplot(tab$temp ~ tab$temp_see, xlab = "see temperature", ylab = 'pool temperature')
averages = tapply(tab$temp, tab$temp_see, mean)
mean.temp = ave(tab$temp, as.factor(tab$temp_see), FUN = mean)

boxplot(tab$temp ~ mean.temp)



legendd2 = gsub(" ", "\n", legendd)
g + labs(colour= 'Temperature')
g + labs(colour= legendd)
g + labs(colour= legendd2) 



fviz_pca_var(z,
             col.var = "black", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)


g




scaterplot(z)
