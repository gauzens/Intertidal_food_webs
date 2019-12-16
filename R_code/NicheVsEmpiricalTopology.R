library(ggplot2)
library(gridExtra)
rm(list = ls())
detach(tab)
tab = read.csv('/home/bg33novu/projects/WarmingWebs/results/NicheVSEmpirical_topology.csv')


do.plot = function(df, title){
  p = ggplot(df, aes(x = empirical, y = niche, color = as.factor(temperature)))+
    geom_point(cex = 4)+
    scale_color_viridis_d(option = "plasma", name = paste('Average \nsummer \nsea temp.'))+
    theme_classic()+
    ggtitle(title)+
    ylab( 'Mean value for synthetic webs')+
    xlab('Mean value for natural webs')+
    geom_abline(slope=1, intercept=0, cex = 1.8, alpha = 0.8)+
    theme(
      plot.title = element_text(size = 20, hjust = 0),
      axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      legend.position = 'none',
    )
  
}


# jpeg('/homes/bg33novu/projects/WarmingWebs/plots/NicheVSEmpirical_topology/nicheVSnaturalTopologies.jpeg', width = 1200, height = 400)
# png('/homes/bg33novu/projects/WarmingWebs/plots/NicheVSEmpirical_topology/nicheVSnaturalTopologies.png', width = 12, height = 4)
# pdf('/homes/bg33novu/projects/WarmingWebs/plots/NicheVSEmpirical_topology/nicheVSnaturalTopologies.pdf', width = 12, height = 4)

par(mfrow = c(1,3), mar = c(5, 6, 6, 2) + 0.1)
indices = c('meanTL', 'topTL', 'omni', 'basal', 'int', 'top', 'outbasal')
indices =c('meanTL', 'topTL', 'outbasal')

######################## mean TL ###############################

data = tab[tab$index == 'meanTL', ]
df = data.frame(niche = data$niche, empirical = data$emp, temperature = data$temp)
mTL = do.plot(df, 'a) \t\t Mean trophic level')

########################topTL ###############################

data = tab[tab$index == 'topTL', ]
df = data.frame(niche = data$niche, empirical = data$emp, temperature = data$temp)
TopTL = do.plot(df, 'b) \t Mean trophic level of top species')

########################outbasal ###############################

data = tab[tab$index == 'outbasal', ]
df = data.frame(niche = data$niche, empirical = data$emp, temperature = data$temp)
outbasal = do.plot(df, 'c) \t Mean number of herbivory \n \tlinks per basal species')


pdf('/home/bg33novu/projects/WarmingWebs/paper/figures/nicheVSnaturalTopologies_colorBlind.pdf', width = 16, height = 6)
grid.arrange(mTL, TopTL, outbasal, nrow = 1)

dev.off()


############### comment from R3: relationship between herbivory link per basal species and temperature
data = tab[tab$index == 'outbasal', ]
df = data.frame(niche = data$niche, herb = data$emp, temperature = data$temp)

ggplot(df, aes(x = as.factor(temperature), y = herb))+
  stat_boxplot(cex = 0.3, fill = 'grey')+
  geom_point()+
  ylab('Mean number of herbivory\nlinks per basal species')+
  xlab('local temperature')+
  theme_minimal()




# colors = c('blue4', 'dodgerblue1', 'cyan', 'green', 'gold', 'darkorange2', 'red')
# colors = c('blue4', 'dodgerblue3', 'lightskyblue', 'lightseagreen', 'gold', 'darkorange2', 'red')
# map.colors = function(temp){
#   col = temp
#   col[temp == 13] = colors[1]
#   col[temp == 16] = colors[2]
#   col[temp == 19] = colors[3]
#   col[temp == 22] = colors[4]
#   col[temp == 26] = colors[5]
#   col[temp == 27] = colors[6]
#   col[temp == 28] = colors[7]
#   return(col)
#   }
# 
# map.indices =function(index){
#   if (index == 'meanTL'){ return('Mean trophic level')}
#   if (index == 'topTL'){ return('Mean trophic level \nof top species')}
#   if (index == 'outbasal'){ return('Mean number of herbivory \n links per basal species')}
# }
# 
# 
# for (ind in indices){
#   data = tab[tab$index == ind, ]
#   plot(data$niche ~ data$emp, col = adjustcolor(map.colors(data$temp), alpha.f = 0.6), main = map.indices(ind), 
#        ylim = c(min(data$niche)*0.8, max(data$niche*1.2)), xlim = c(min(data$emp)*0.8, max(data$emp*1.2)), 
#        xlab = 'Mean value for empirical webs', ylab = 'mean value for synthetic webs', 
#        cex.lab = 2, cex.main = 2, cex.axis = 1.4,
#        cex = 1.8, pch = 16)
#   abline(0,1)
# }
# dev.off()
# 
# for (ind in indices){
#   data = tab[tab$index == ind, ]
#   plot(data$niche - data$emp ~ data$temp, col = map.colors(data$temp), main = ind, 
#          xlab = 'temperature', ylab = 'residuals')
#   abline(0,0)
#   
# }?
# 
# 
# boxplot(data$niche - data$emp ~ data$temp)
# abline(0,0)
