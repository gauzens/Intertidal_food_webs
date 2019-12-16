library(ggplot2)
library(viridis)
library(gridExtra)

prepare_tab = function(tab.exp){
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
  
  return(tab.exp)
}

mmToInches = function(x){
  return(x/25.4)
}

do.plot = function(df, k){
  experimentals = ggplot(df, 
                         aes(x=temperature, y=persistence, group = as.factor(mean.temp), fill = as.factor(mean.temp), colour = as.factor(mean.temp)), 
  )+
    stat_summary(geom="point", fun.y=mean, cex = 1)+
    # geom_smooth(method = 'lm', alpha = 0.2, formula = y ~ x + I(x^2), cex = 0.3)+
    scale_fill_viridis_d(option = "plasma", name = paste('Average \nsummer \nsea temp.:'))+
    scale_color_viridis_d(option = "plasma", name = paste('Average \nsummer \nsea temp.:'))+
    geom_smooth(alpha=0.2, cex = 0.3) +
    theme_classic()+
    # coord_cartesian(ylim = c(0.87, 1.02))+
    # coord_cartesian(ylim = c(0.5, 1))+
    # textGrob("A)", x = 0, y = 0.9, just = c("left", "top"), gp = gpar(fontsize = 18, col =  "black"))+
    # arrangeGrob(top = textGrob('A)', x = 0, y = 0.9))+
    # geom_text(aes(label = 'sentimentview.com', x = -1, y = 1), hjust = -2, vjust = 6)+
    ggtitle(k)+
    theme(
      plot.title = element_text(size = 14, hjust = 0),
      # axis.title.x=element_blank(),
      # axis.text.x=element_blank(),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 10),
      legend.position = 'none',
      plot.margin = unit(c(1.5,0.1,0.1,1), "cm"),
    )
  return(experimentals)
}

# cbPalette <- c('blue4', 'dodgerblue3', 'lightskyblue', 'lightseagreen', 'gold', 'darkorange2', 'red')
tab.exp2 = prepare_tab(read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K2.csv', header = F))
tab.exp3 = prepare_tab(read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K3.csv', header = F))
tab.exp4 = prepare_tab(read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K4.csv', header = F))
tab.exp5 = prepare_tab(read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K5.csv', header = F))
tab.exp7 = prepare_tab(read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K7.csv', header = F))
tab.exp10 = prepare_tab(read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_k10.csv', header = F))
tab.exp15 = prepare_tab(read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K15.csv', header = F))
tab.exp20 = prepare_tab(read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K20.csv', header = F))

df_K2 = data.frame(persistence = tab.exp2$prop_ext, temperature = tab.exp2$temperature, init_temp = tab.exp2$mean.summer, mean.temp = tab.exp2$mean.summer)
df_K3 = data.frame(persistence = tab.exp3$prop_ext, temperature = tab.exp3$temperature, init_temp = tab.exp3$mean.summer, mean.temp = tab.exp3$mean.summer)
df_K7 = data.frame(persistence = tab.exp7$prop_ext, temperature = tab.exp7$temperature, init_temp = tab.exp7$mean.summer, mean.temp = tab.exp7$mean.summer)
df_K15 = data.frame(persistence = tab.exp15$prop_ext, temperature = tab.exp15$temperature, init_temp = tab.exp15$mean.summer, mean.temp = tab.exp15$mean.summer)
df_K4 = data.frame(persistence = tab.exp4$prop_ext, temperature = tab.exp4$temperature, init_temp = tab.exp4$mean.summer, mean.temp = tab.exp4$mean.summer)
df_K5 = data.frame(persistence = tab.exp5$prop_ext, temperature = tab.exp5$temperature, init_temp = tab.exp5$mean.summer, mean.temp = tab.exp5$mean.summer)
df_K10 = data.frame(persistence = tab.exp10$prop_ext, temperature = tab.exp10$temperature, init_temp = tab.exp10$mean.summer, mean.temp = tab.exp10$mean.summer)
df_K20 = data.frame(persistence = tab.exp20$prop_ext, temperature = tab.exp20$temperature, init_temp = tab.exp20$mean.summer, mean.temp = tab.exp20$mean.summer)

pdf('/home/bg33novu/projects/WarmingWebs/paper/figures/effects_of_K.pdf', width = mmToInches(200), height = mmToInches(290))
k2 = do.plot(df_K2, 'K = 2')
k3 = do.plot(df_K3, 'K = 3')
k4 = do.plot(df_K4, 'K = 4')
k5 = do.plot(df_K5, 'K = 5')
k7 = do.plot(df_K7, 'K = 7')
k10 = do.plot(df_K10, 'K = 10')
k15 = do.plot(df_K15, 'K = 15')
k20 = do.plot(df_K20, 'K = 20')
grid.arrange(k2, k3, k4, k5, k7, k10, k15, k20, ncol = 2)
dev.off()

experimentals


