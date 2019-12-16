
library(ggplot2)
library(gridExtra)
rm(list = ls())

##################################################################################
#################### based on individual pool temperatures #######################
##################################################################################



tab.exp = read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K5.csv', head = FALSE)
names(tab.exp) = c('name', 'region', 'init_temp', 'richness', 'temperature', 'nb_ext_nn_basal', 'nb_ext', 'resilience', 'oi', 'tl', 'connectance')
tab.exp$region = sub("u'", "", tab.exp$region)
tab.exp$region = sub("'", "", tab.exp$region)
tab.exp$region = sub(" ", "", tab.exp$region)
tab.exp$prop_ext = 1 - tab.exp$nb_ext/tab.exp$richness
tab.exp$mean.temp = round(ave(tab.exp$init_temp, tab.exp$region, FUN = mean, na.rm = TRUE), 1)
head(tab.exp)





unique(tab.exp$mean.temp)
# cbPalette <- c('blue4', 'dodgerblue3', 'lightskyblue', 'lightseagreen', 'gold', 'darkorange2', 'red')
# cbPalette = adjustcolor(cbPalette, alpha.f = 0.5)
# attach(tab.exp)
# warming = temperature - init_temp
# pdf('/homes/bg33novu/projects/WarmingWebs/plots/compare_with non_warmed.pdf')
tab.exp$init_temp = round(tab.exp$init_temp, digits = 1)
dif.persistence.tot = c()
temp.plot.tot = c()
cols = c()
proportion.of.maximised = c()
i = 0
for (temp in sort(unique(tab.exp$mean.temp))){
  i = i + 1
  print(c('temperature is: ', temp))
  sub.res = tab.exp[tab.exp$mean.temp == temp,]
  attach(sub.res)
  # sub.res$init_temp = round(sub.res$init_temp, digits = 1)
  warming = temperature - init_temp
  webs = unique(sub.res$name)
  # web = webs[1]
  temp.plot = c()
  dif.persistence = c()
  to.compare = c()
  for (web in webs){
    ref = sub.res[name == web & temperature == init_temp, ]
    # get indices when temperature + warming of i match the initial temp of another FW
    to.compare = which(warming == 0)
    cols = c(cols, rep(cbPalette[i], length(to.compare)))
    # temperature of network that I want to compare
    temps = temperature[to.compare]
    # get the persistence of the reference food webs at the different temperature we use to compare it
    pers.reference = sub.res[name == web, ]$prop_ext[match(temps, sub.res[name == web, ]$temperature)]
    # pres.other: persistence of food web at their local condition (without warming)
    pers.other = prop_ext[to.compare]
    temp.plot = c(temp.plot, temperature[to.compare] - ref$init_temp)
    # a dif.pers larger than 0 would mean better local adaptation
    dif.pers = pers.other - pers.reference
    dif.persistence = c(dif.persistence, dif.pers)
    proportion.of.maximised = c(proportion.of.maximised, length(dif.pers[dif.pers > 0]) / length(dif.pers))
    
    temp.plot = c(temp.plot, temperature[to.compare] - ref$init_temp)
    dif.persistence = c(dif.persistence, pers.other - pers.reference)
    temp.plot.tot = c(temp.plot.tot, temperature[to.compare] - ref$init_temp)
    dif.persistence.tot = c(dif.persistence.tot, pers.other - pers.reference)
  }
  # print(length(unique(webs)))
  # print(length(temp.plot))
  # print(length(to.compare))
  # proportion of values where the without warming is more stable than the reference + warming 
  # i.e. 
  prop.evolution = sum(dif.persistence > 0) / length(dif.persistence)
  # plot(dif.persistence ~ temp.plot,
  # main = paste('average temp is', temp, '\nnweb = ', length(webs), '\nprop of stability maximised at own temp: ', prop.evolution,  sep = ' '))
  detach(sub.res)
}

pdf('/home/bg33novu/projects/WarmingWebs/paper/figures/the_fig_x_color_blind.pdf', width = 14)
par(mfrow = c(1,2))
df = data.frame(dif.persistence.tot =  ,temp.plot.tot = , color = )
plot(dif.persistence.tot ~ temp.plot.tot, col = cols, pch = 16, 
     xlab = 'difference in temperature', ylab = 'difference in persistence')
    legend('topright', legend = paste(sort(unique(tab.exp$mean.temp)), '°', sep = ''), col = cbPalette, pch = 16, bty = 'n')
# dev.off()
hist(proportion.of.maximised, nclass = 20, xlab = 'proportion of food webs arguing \nfor an effect of local adaptation', main = '')
dev.off()

square.factor = temp.plot.tot * temp.plot.tot
model = lm(dif.persistence.tot ~ temp.plot.tot * square.factor)
summ = summary(model)
# plot(model)

x.values = seq(-15, 15, 0.1)
y.values = summ$coefficients[1] + summ$coefficients[2] * x.values + x.values*x.values*summ$coefficients[3]
lines(y.values ~ x.values)

t.test(proportion.of.maximised, mu = 0.5, alternative = 'greater')









##################################################################################
#################### based on average sea temperatures #######################
##################################################################################


tab.exp = read.csv('/home/bg33novu/projects/WarmingWebs/WarmingHPC/outputs/experimentals_K5.csv', header = F)

names(tab.exp) = c('name', 'region', 'init_temp', 'richness', 'temperature', 'nb_ext_nn_basal', 'nb_ext', 'resilience', 'oi', 'tl', 'connectance')
tab.exp$region = sub("u'", "", tab.exp$region)
tab.exp$region = sub("'", "", tab.exp$region)
tab.exp$region = sub(" ", "", tab.exp$region)
tab.exp$region = sub("_txt", "", tab.exp$region)
tab.exp$prop_ext = 1 - tab.exp$nb_ext/tab.exp$richness
tab.exp$mean.temp = round(ave(tab.exp$init_temp, tab.exp$region, FUN = mean, na.rm = TRUE), 1)
# tab.exp = subset(tab.exp, tab.exp$temperature == as.integer(tab.exp$temperature))

# tab.exp$temp_incr = temperature - init_temp

# temps = sort(unique(mean.temp))

# pairs = combn(temps, 2)


# get mean summer sea temperature 
tab.exp$mean.summer = NA
seetemps = read.table('~/projects/WarmingWebs/R_code2/see_temps', header = T, sep = ',')
for (name in seetemps$name){
  cat(name, '\n')
  xx = grep(pattern = name, x = tab.exp$region, fixed = TRUE)
  tab.exp$mean.summer[xx] = seetemps$MeanSummer[seetemps$name == name]
}
tab.exp$mean.summer = round(tab.exp$mean.summer, 1)

sum(is.na(tab.exp$mean.summer))
unique((tab.exp$mean.summer))

attach(tab.exp)

temps = sort(unique(mean.summer))

i = 0
cbPalette <- c('blue4', 'dodgerblue3', 'lightskyblue', 'lightseagreen', 'gold', 'darkorange2', 'red')
colors = c()
dif.temp = c()
result = c()
prop.maximising = c()
col.maximising = c()
difs.maximising = c()
ref.temp = c()
ref.temp.maximising = c()
# temperature = c()
for (local in temps){
  i = i+1
  for (other in temps){
    # cat(local, '\t', other, '\n')
    persistence.local = prop_ext[mean.summer == local & temperature == local]
    persistence.other = prop_ext[mean.summer == other & temperature == local]
    difs = sapply(persistence.local, '-', persistence.other)
    res = as.vector(difs)
    result = c(result, res)
    dif.temp = c(dif.temp, rep(local - other, length(res)))
    color.to.use = cbPalette[i]
    colors = c(colors, rep(color.to.use, length(res)))
    ref.temp = c(ref.temp, rep(local, length(res)))
    
    if (local != other){
      prop.maximising = c(prop.maximising, length(res[res>0]) / length(res))
      col.maximising = c(col.maximising, cbPalette[i])
      difs.maximising = c(difs.maximising, local - other)
      ref.temp.maximising = c(ref.temp.maximising, local)
    }
  }
}



quantile(result,probs=c(.025,.975))
quantile(result,probs=c(.05,.95))
percentile <- ecdf(result)
percentile(0)
unique(dif.temp[colors == 'blue4'])

qnorm(c(0.025, 0.975), mean = mean(result), sd = sd(result)/sqrt(length(result)))
plot(dnorm(seq(-0.4, 0.4, 0.001), mean = mean(result), sd = sd(result)/sqrt(length(result))))

pdf('/home/bg33novu/projects/WarmingWebs/paper/figures/adaptations_color_blind.pdf', width = 13)
df = data.frame(result = result, dif.temp = dif.temp, color = ref.temp)
pts = ggplot(df, aes(x = dif.temp, y = result, colour = as.factor(color)))+
  geom_point()+
  scale_color_viridis_d(option = "plasma", name = paste('Average \nsummer \nsea temp.:'))+
  theme_classic()+
  ggtitle('a)')+
  xlab( 'Difference between temperature used for comparing \nlocal and non-local persistence')+
  ylab('Persistence differences')+
  theme(
    plot.title = element_text(size = 18, hjust = 0),
    # axis.title.x=element_blank(),
    # axis.text.x=element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.position = 'none',
    plot.margin = unit(c(1.5,0.1,0.1,1), "cm"),
  )

histo = ggplot(df, aes(x = result))+
  geom_histogram(aes(y = ..density..), fill = 'lightgrey', colour = 'black')+  
  theme_classic()+
  ggtitle('b)')+
  xlab('distribution of the differences\nbetween local and non-local persistence')+  theme(
    plot.title = element_text(size = 18, hjust = 0),
    # axis.title.x=element_blank(),
    # axis.text.x=element_blank(),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.position = 'none',
    plot.margin = unit(c(1.5,0.1,0.1,1), "cm"),
  )

grid.arrange(pts, histo, ncol = 2)
dev.off()
detach(tab.exp)


# 
# points(prop.maximising - 0.5 ~ difs.maximising, col = col.maximising, pch = 19)
# abline(0,0, lty = 2)
# # legend('topleft', 'A)')
# # text(-0.42,3400,"B")
# 
# # par(yaxs="i", mar=c(6, 4, 4, 2) + 0.1 ) 
# hist(result, 
#      main = 'difference between the persistence of food webs at local \nconditions and 
#      persistence of food webs from other location ',
#      xlab = '')
# 
# plot(tapply(prop.maximising, ref.temp.maximising, mean) ~ temps)
# plot(prop.maximising ~ difs.maximising, col = col.maximising)
# summary(lm(prop.maximising ~ difs.maximising, col = col.maximising))
# # plot proportion of 'best adapted depending on local temperature'
# maximised.at.local = c()
# for (i in temps){
#   res = result[ref.temp == i]
#   maximised.at.local = c(maximised.at.local, sum(res>0)/length(res))
# }
# plot(maximised.at.local ~ temps)
# abline(lm(maximised.at.local ~ temps))
# summary(lm(maximised.at.local ~ temps))
# detach(tab.exp)
