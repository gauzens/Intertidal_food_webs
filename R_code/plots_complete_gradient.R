rm(list = ls())
library(ggplot2)
library(gridExtra)
library(viridis)
library("grid")

# tab.exp = read.csv('/homes/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentalfinals.csv', header = F)
tab.exp = read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K4.csv', header = F)
# tab.exp = read.csv('/homes/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K20.csv', header = F)

names(tab.exp) = c('name', 'region', 'init_temp', 'richness', 'temperature', 'nb_ext_nn_basal', 'nb_ext', 'resilience', 'oi', 'tl', 'connectance')
tab.exp$region = sub("u'", "", tab.exp$region)
tab.exp$region = sub("'", "", tab.exp$region)
tab.exp$region = sub(" ", "", tab.exp$region)
tab.exp$prop_ext = 1 - tab.exp$nb_ext/tab.exp$richness
tab.exp$mean.temp = round(ave(tab.exp$init_temp, tab.exp$region, FUN = mean, na.rm = TRUE), 1)
seetemps = read.table('~/projects/WarmingWebs/R_code2/see_temps', header = T, sep = ',')

tab.exp$mean.summer = NA
for (name in seetemps$name){
  cat(name, '\n')
  xx = grep(pattern = name, x = tab.exp$name, fixed = TRUE)
  tab.exp$mean.summer[grep(pattern = name, x = tab.exp$name, fixed = TRUE)] = round(seetemps$MeanSummer[seetemps$name == name],1)
}

# tapply(tab.exp$mean.summer, tab.exp$region, mean)


tab.exp = subset(tab.exp, tab.exp$temperature == as.integer(tab.exp$temperature))

tab.niche = read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/niches_k4.csv', header = F)
names(tab.niche) = c('name', 'region', 'init_temp', 'richness', 'temperature', 'nb_ext_nn_basal', 'nb_ext', 'resilience', 'oi', 'tl', 'connectance')
tab.niche$region = sub("u'", "", tab.niche$region)
tab.niche$region = sub("'", "", tab.niche$region)
tab.niche$region = sub(" ", "", tab.niche$region)

tab.niche$prop_ext = 1 - tab.niche$nb_ext/tab.niche$richness
tab.niche$mean.temp = round(ave(tab.niche$init_temp, tab.niche$region, FUN = mean, na.rm = TRUE), 1)

tab.niche$mean.summer = NA
for (name in seetemps$name){
  cat(name, '\n')
  xx = grep(pattern = name, x = tab.niche$name, fixed = TRUE)
  tab.niche$mean.summer[grep(pattern = name, x = tab.niche$name, fixed = TRUE)] = round(seetemps$MeanSummer[seetemps$name == name],1)
}

tab.expniches = read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/expNW_nicheBM_k4.csv', header = F)
# tab.expniches = read.csv('/homes/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/expNW_nicheBM_NCC_check.csv', header = F)

names(tab.expniches) = c('name', 'region', 'init_temp', 'richness', 'temperature', 'nb_ext_nn_basal', 'nb_ext', 'resilience', 'oi', 'tl', 'connectance')
tab.expniches$region = sub("u'", "", tab.expniches$region)
tab.expniches$region = sub("'", "", tab.expniches$region)
tab.expniches$region = sub(" ", "", tab.expniches$region)

tab.expniches$prop_ext = 1 - tab.expniches$nb_ext/tab.expniches$richness
# tab.expniches$mean.temp = round(ave(tab.expniches$init_temp, tab.expniches$region, FUN = mean, na.rm = TRUE), 1)
tab.expniches$mean.temp = NA
tab.expniches$mean.temp[tab.expniches$region == 'Brasil(CE)_txt'] = unique(tab.exp$mean.temp[tab.exp$region == 'Brasil(CE)_txt'])
tab.expniches$mean.temp[tab.expniches$region == 'Brasil(SP)_txt'] = unique(tab.exp$mean.temp[tab.exp$region == 'Brasil(SP)_txt'])
tab.expniches$mean.temp[tab.expniches$region == 'Canada_txt'] = unique(tab.exp$mean.temp[tab.exp$region == 'Canada_txt'])
tab.expniches$mean.temp[tab.expniches$region == 'Portugal_txt'] = unique(tab.exp$mean.temp[tab.exp$region == 'Portugal_txt'])
tab.expniches$mean.temp[tab.expniches$region == 'England_txt'] = unique(tab.exp$mean.temp[tab.exp$region == 'England_txt'])
tab.expniches$mean.temp[tab.expniches$region == 'Mad_txt'] = unique(tab.exp$mean.temp[tab.exp$region == 'Mad_txt'])
tab.expniches$mean.temp[tab.expniches$region == 'Moz_txt'] = unique(tab.exp$mean.temp[tab.exp$region == 'Moz_txt'])

sum(is.na(tab.expniches$mean.temp))

tab.expniches$prop_ext = 1 - tab.expniches$nb_ext/tab.expniches$richness

tab.expniches$mean.summer = NA
for (name in seetemps$name){
  cat(name, '\n')
  xx = grep(pattern = name, x = tab.expniches$name, fixed = TRUE)
  tab.expniches$mean.summer[grep(pattern = name, x = tab.expniches$name, fixed = TRUE)] = round(seetemps$MeanSummer[seetemps$name == name],1)
}
# tapply(tab.expniches$mean.summer, tab.expniches$region, mean)



###############################
## first empirical networks ###
###############################

attach(tab.exp)
temp_incr = temperature - init_temp
temps = unique(init_temp)
warming = unique(temp_incr)
netws = unique(name)


max.min = c(NA, NA, NA)
for  (nom in unique(name)){
  aaa = tab.exp[tab.exp$name == nom, ]
  aaa = aaa[aaa$temperature <=50, ]
  res = c(unique(aaa$init_temp), unique(aaa$mean.temp), max(aaa$prop_ext) - min(aaa$prop_ext))
  max.min = rbind(max.min, res)
}


# cbPalette <- c('blue4', 'dodgerblue3', 'lightskyblue', 'lightseagreen', 'gold', 'darkorange2', 'red')
df = data.frame(persistence = tab.exp$prop_ext, temperature = tab.exp$temperature, init_temp = tab.exp$mean.summer, mean.temp = tab.exp$mean.summer)
df = df[df$temperature <=50, ]

experimentals = ggplot(df, 
       aes(x=temperature, y=persistence, group = as.factor(mean.temp), fill = as.factor(mean.temp), colour = as.factor(mean.temp)), 
)+
  stat_summary(geom="point", fun.y=mean, cex = 0.8)+
  # geom_smooth(method = 'lm', alpha = 0.2, formula = y ~ x + I(x^2), cex = 0.3)+
  # scale_color_manual(values=cbPalette, name = 'Initial temperature')+
  # scale_fill_manual(values=cbPalette, name = 'Initial temperature')+
  geom_smooth( alpha=0.2, cex = 0.3) +
  scale_fill_viridis_d(option = "plasma", name = paste('Average \nsummer temp.'))+
  scale_color_viridis_d(option = "plasma", name = paste('Average \nsummer temp.'))+

  theme_classic()+
  # coord_cartesian(ylim = c(0.87, 1.02))+
  # coord_cartesian(ylim = c(0.5, 1))+
  # textGrob("A)", x = 0, y = 0.9, just = c("left", "top"), gp = gpar(fontsize = 18, col =  "black"))+
  # arrangeGrob(top = textGrob('A)', x = 0, y = 0.9))+
  # geom_text(aes(label = 'sentimentview.com', x = -1, y = 1), hjust = -2, vjust = 6)+
  ggtitle('a)')+
  theme(
    plot.title = element_text(size = 14, hjust = 0),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.position = 'none',
    plot.margin = unit(c(1.5,0.1,0.1,1), "cm"),
  )


experimentals
detach(tab.exp)


####################################
###### then niche models ###########
####################################

attach(tab.niche)
temp_incr = temperature - init_temp
temps = unique(init_temp)
warming = unique(temp_incr)
netws = unique(name)

cbPalette <- c('blue4', 'dodgerblue3', 'lightskyblue', 'lightseagreen', 'gold', 'darkorange2', 'red')
df = data.frame(persistence = tab.niche$prop_ext, temperature = tab.niche$temperature, init_temp = tab.niche$mean.summer, mean.temp = tab.niche$mean.summer)
df = df[df$temperature <=50, ]

niches = ggplot(df, 
       aes(x=temperature, y=persistence, group = as.factor(mean.temp), fill = as.factor(mean.temp), colour = as.factor(mean.temp)), 
)+
  stat_summary(geom="point", fun.y=mean, cex = 0.8)+
  # geom_smooth(method = 'lm', alpha = 0.2, formula = y ~ x + I(x^2), cex = 0.3)+
    geom_smooth(alpha=0.2, cex = 0.3) +
  # scale_color_manual(values=cbPalette, name = 'Initial temperature')+
  # scale_fill_manual(values=cbPalette, name = 'Initial temperature')+
  scale_fill_viridis_d(option = "plasma", name = paste('Average \nsummer temp.'))+
  scale_color_viridis_d(option = "plasma", name = paste('Average \nsummer temp.'))+
  ggtitle('b)')+
  theme_classic()+
  # coord_cartesian(ylim = c(0.5, 0.8))+
  theme(
    plot.title = element_text(size = 14, hjust = 0),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.position = 'none',
    plot.margin = unit(c(0.5,0.1,0.1,1), "cm")
  )
niches
detach(tab.niche)

################################################
## experimental but with theoretical body masses
################################################


attach(tab.expniches)
temps = unique(init_temp)
warming = unique(temperature)
netws = unique(name)

cbPalette <- c('blue4', 'dodgerblue3', 'lightskyblue', 'lightseagreen', 'gold', 'darkorange2', 'red')
df = data.frame(persistence = tab.expniches$prop_ext, temperature = tab.expniches$temperature, init_temp = tab.expniches$mean.summer, mean.temp = tab.expniches$mean.summer)
df = df[df$temperature <=50, ]

expNWnicheBM = ggplot(df, 
       aes(x=temperature, y=persistence, group = as.factor(mean.temp), fill = as.factor(mean.temp), colour = as.factor(mean.temp)), 
)+
  stat_summary(geom="point", fun.y=mean, cex = 0.8)+
  # geom_smooth(method = 'lm', alpha = 0.2, formula = y ~ x + I(x^2), cex = 0.3)+
  geom_smooth(alpha=0.2, cex = 0.3) +
  # scale_color_manual(values=cbPalette, name = 'Average \nsummer \nsea temp.:')+
  # scale_fill_manual(values=cbPalette, name = 'Average \nsummer \nsea temp.:')+
  scale_fill_viridis_d(option = "plasma", name = paste('Average \nsummer \nsea temp.:'))+
  scale_color_viridis_d(option = "plasma", name = paste('Average \nsummer \nsea temp.:'))+
  ggtitle('c)')+
  theme_classic()+
  # coord_cartesian(ylim = c(0.5, 1))+
  theme(
    plot.title = element_text(size = 14, hjust = 0),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.position = 'bottom',
    # legend.position = c(0,-1),
    # legend.direction = 'horizontal',
    # legend.title.align = 0, 
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    plot.margin = unit(c(0.5,0.5,0.1,1), "cm"),
    legend.box.margin=margin(c(5,32,5,5))
  )
expNWnicheBM

detach(tab.expniches)

# grid.arrange(experimentals, niches, expNWnicheBM, ncol = 1)

library(grid)

mmToInches = function(x){
  return(x/25.4)
}
# final_plot = grid.draw(rbind(ggplotGrob(experimentals), ggplotGrob(niches), ggplotGrob(expNWnicheBM), size = "last"))
pdf('/home/bg33novu/projects/WarmingWebs/paper/figures/Fig.4_colorBlind.pdf', 
    width = mmToInches(84), height = mmToInches(78*3))
# par(mar = c(1.5, 4.5, 4, 5))
grid.draw(rbind(ggplotGrob(experimentals), ggplotGrob(niches), ggplotGrob(expNWnicheBM), size = "last"))
# final_plot
dev.off()

plot_grid(ggplotGrob(experimentals), ggplotGrob(niches), ggplotGrob(expNWnicheBM), ncol = 1)




#########################################################
#### exp NW, niche BM, but small variance (fig SI) ######
###########################################################

tab = read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/expNW_nicheBM_NCC_check_var_0.1.csv', header = F)
tab = read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/expNW_nicheBM_k5_smallvar.csv', header = F)
names(tab) = c('name', 'region', 'init_temp', 'richness', 'temperature', 'nb_ext_nn_basal', 'nb_ext', 'resilience', 'oi', 'tl', 'connectance')
tab$region = sub("u'", "", tab$region)
tab$region = sub("'", "", tab$region)
tab$region = sub(" ", "", tab$region)

tab$prop_ext = 1 - tab$nb_ext/tab$richness
tab$mean.temp = round(ave(tab$init_temp, tab$region, FUN = mean, na.rm = TRUE), 1)
tab$prop_ext = 1 - tab$nb_ext/tab$richness
# tab$network[tab$resilience == 'integration_err']

tab$prop_ext = 1 - tab$nb_ext/tab$richness

attach(tab)

temps = unique(init_temp)
warming = unique(temperature)
netws = unique(name)

cbPalette <- c('blue4', 'dodgerblue3', 'lightskyblue', 'lightseagreen', 'gold', 'darkorange2', 'red')
df = data.frame(persistence = tab$prop_ext, temperature = tab$temperature, init_temp = tab$init_temp, mean.temp = tab$mean.temp)
# df = df[df$temperature <=40, ]

expNWnicheBMVar0.1 = ggplot(df, 
                      aes(x=temperature, y=persistence, group = as.factor(mean.temp), fill = as.factor(mean.temp), colour = as.factor(mean.temp)), 
)+
  stat_summary(geom="point", fun.y=mean, cex = 0.8)+
  geom_smooth(method = 'lm', alpha = 0.2, formula = y ~ x + I(x^2), cex = 0.3)+
  # scale_color_manual(values=cbPalette, name = 'Local \ntemp.:')+
  # scale_fill_manual(values=cbPalette, name = 'Local \ntemp.:')+
  scale_fill_viridis_d(option = "plasma", name = paste('Average \nsummer \nsea temp.:'))+
  scale_color_viridis_d(option = "plasma", name = paste('Average \nsummer \nsea temp.:'))+
  # ggtitle('C)')+
  theme_classic()+
  # coord_cartesian(ylim = c(0.5, 1))+
  theme(
    plot.title = element_text(size = 14, hjust = 0),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.position = 'bottom',
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    plot.margin = unit(c(0.5,0.1,0.1,1), "cm")
  )
# expNWnicheBM
expNWnicheBMVar0.1


detach(tab)


############## test for different curvatures, depending on local temperatures
# curvature of the humped shaped in function of pools temp (individuals)
plot(log(max.min[,3]+1) ~ log(max.min[,1]+1))
summary(lm(log(max.min[,3]+1) ~ log(max.min[,1]+1)))
# curvature of the humped shaped in function of pools temp (individuals, grouped by region)
plot(max.min[,3] ~ max.min[,2])
abline(lm(max.min[,3] ~ max.min[,2]))
summary(lm(max.min[,3] ~ max.min[,2]))
# curvature of the humped shaped in function of pools temp (averaged by region)
maxs = tapply(max.min[,3], max.min[,2], max)
mins = tapply(max.min[,3], max.min[,2], min)
maxs - mins
plot(maxs - mins ~ sort(unique(max.min[,2])))
summary(lm(maxs - mins ~ sort(unique(max.min[,2]))))
### => the responseof food webs from cold region is not flatter than the one of warmer regions





# model.niche = lme(prop_ext ~ temperature * init_temp, random = ~ 1 |name, data = tab)
# anova(model.niche)
# summary(model.niche)
# 
# t2 = temperature * temperature
# model.niche.quadr = lme(prop_ext ~ init_temp*temperature * t2, random = ~ 1 |name, data = tab)
# anova(model.niche.quadr)
# summary(model.niche.quadr)
# 
# model.niche.quadr.simple = lme(prop_ext ~ temperature * t2, random = ~ 1 |name, data = tab)
# anova(model.niche.quadr.simple)
# summary(model.niche.quadr.simple)
# 
# model.niche.quadr.supersimple = lme(prop_ext ~ t2, random = ~ 1 |name, data = tab)
# anova(model.niche.quadr.supersimple)
# summary(model.niche.quadr.supersimple)
# 
# model.niche.simple = lme(prop_ext ~ temperature, random = ~ 1 |name, data = tab)
# anova(model.niche.simple)
# summary(model.niche.simple)
# 
# AIC(model.niche.simple)
#################



# tapply(tab.niche$mean.summer, tab.niche$region, mean)

# 
# To restart simulations of niche model that failed...
# 
# interact = interaction(tab.niche$name, tab.niche$temperature)
# to_redo = c()
# comp = 0
# for (i in unique(interact)){
#   comp = comp + 1
#   sous_tab = tab.niche[interact == i, ]
#   reps = dim(sous_tab)[1]
# 
#   if (reps <= 100){
#     cat('facteur: ', i, ' nb reps: ', reps, ' comp', '\n')
#     # temp_to_add = unique(sous_tab$temperature)
#     # a = comp*200
#     # b = comp*200 + 100 - reps
#     # seeds = a:b
#     # if (unique(sous_tab$name) == "[u'/homes/bg33novu/projects/WarmingWebs/data/Portugal_txt/L4P1'"){
#     #   file = '112'
#     # }
#     # if (unique(sous_tab$name) == "[u'/homes/bg33novu/projects/WarmingWebs/data/Portugal_txt/CR1P1'"){
#     #   file = '99'
#     # }
#     # if (unique(sous_tab$name) == "[u'/homes/bg33novu/projects/WarmingWebs/data/Moz_txt/F2P3'"){
#     #   file = '46'
#     # }
#     # if (unique(sous_tab$name) == "[u'/homes/bg33novu/projects/WarmingWebs/data/Portugal_txt/L4P4'"){
#     #   file = '97'
#     # }
#     # to_add = paste('/home/gauzens/warmingHPC/web_list/graph', file, sep = '')
#     # to_add = paste(to_add, temp_to_add)
#     # to_add = cbind(to_add, as.character(seeds))
#     # to_redo = rbind.data.frame(to_redo, to_add)
#     # # to_redo = as.data.frame(to_redo)
#   }
# }


# to_redo2 = paste(to_redo[,1], to_redo[,2], sep = ' ')
# write(to_redo2, file = '/homes/bg33novu/projects/WarmingWebs/WarmingHPC/launchers/parameter_file2')


