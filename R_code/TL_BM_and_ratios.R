library(nlme)
rm(list = ls())
detach(data)
data = read.csv('/home/bg33novu/projects/WarmingWebs/species_info.csv', header = T)
data = data[!is.na(data$TL), ]

colors = c('blue4', 'dodgerblue3', 'lightskyblue', 'lightseagreen', 'gold', 'darkorange2', 'red')
# change see temperature by avg region pool temperature
data$temp[data$temp == 13] = 11.5
data$temp[data$temp == 16] = 16.4
data$temp[data$temp == 19] = 19.5
data$temp[data$temp == 22] = 22.9
data$temp[data$temp == 26] = 26.3
data$temp[data$temp == 27] = 27.4
data$temp[data$temp == 28] = 28.4
attach(data)

map.colors = function(temp){
  col = temp
  col[temp == 11.5] = colors[1]
  col[temp == 16.4] = colors[2]
  col[temp == 19.5] = colors[3]
  col[temp == 22.9] = colors[4]
  col[temp == 26.3] = colors[5]
  col[temp == 27.4] = colors[6]
  col[temp == 38.4] = colors[7]
  return(col)
}


# jpeg('/homes/bg33novu/projects/WarmingWebs/plots/BM_relationships.jpeg', width = 800, height = 250)
# pdf('/homes/bg33novu/projects/WarmingWebs/plots/BM_relationships.pdf', width = 15, height = 9)
layout(matrix(c(1,2,3,4,5,6,7,8), 2,4), widths = c(3,3,3,1.3))
par(mar = c(5, 5, 4, 2) + 0.1)
data2 = data[TL > 1, ]


######### plot pred_prey BM ratio ~ TL
plot(log10(data2$BM / data2$mean_prey_BM) ~ data2$TL, col = 'white', 
     ylab = 'log10 of pred prey mass ratio', xlab = 'Trophic levels', 
     cex.lab = 2, cex.axis = 1.4)
legend('topright', 'A', bty = 'n', cex = 2)
coefsBM.TL = c()
for (temperature in unique(temp)){
  cat(temperature, '\n')
  tab = data2[temp == temperature, ]
  points(log10(tab$BM / tab$mean_prey_BM) ~ tab$TL, col = adjustcolor(map.colors(temperature), alpha.f = 0.6), cex = 1, pch = 16)
  y.loess = loess(log10(tab$BM / tab$mean_prey_BM) ~ tab$TL)
  y.predict = predict(y.loess, data.frame(x =  tab$TL))
  # lines( sort(tab$TL), y.predict, col = map.colors(temperature))
  abline(lm(log10(tab$BM / tab$mean_prey_BM) ~ tab$TL), col = map.colors(temperature))
  coefsBM.TL = c(coefsBM.TL, summary(lm(log10(tab$BM / tab$mean_prey_BM) ~ tab$TL))$coefficients[2])
}

data2$ratio = log10(data2$BM / data2$mean_prey_BM)
# data2$ratio = log10(data2$BM) / log10(data2$mean_prey_BM)
# data2$ratio = data2$BM / data2$mean_prey_BM
model = lme(ratio ~ TL * temp, random = ~1|name, data = data2)
# anova(model)
# summary(model)
# plot(model)
BIC(model)
model2 = lme(ratio ~ TL + temp, random = ~1|name, data = data2)
# anova(model2)
# summary(model2)
# plot(model)
BIC(model2)

plot(coefsBM.TL ~ unique(temp), ylab = 'slopes of individual regressions', xlab = 'temperature')
legend('topright', 'D', bty = 'n', cex = 2)
summary(lm(coefsBM.TL~ unique(temp)))
# summary(lm(coefsBM.TL[unique(temp) !=23.8] ~ unique(temp[temp !=23.8])))
# 
# plot(coefsBM.TL ~ unique(temp))
# 
# 
# data2$temp2 = data2$temp - 20
# data3 = data2[data2$temp != 23.8,]
# 
# model2 = lme(ratio ~ TL * temp2, random = ~1|name, data = data3)
# anova(model2)
# summary(model2)
# plot(model2)
# AIC(model2)
# 
# model3 = lme(ratio ~ TL+temp, random = ~1|name, data = data2)
# anova(model3)
# summary(model3)
# plot(model2)
# AIC(model3)
# 
# model4 = lm(ratio ~ TL * temp, data = data2)
# anova(model4)
# summary(model4)
# # plot(model)
# plot(coefsBM.TL ~ unique(temp))
# summary(lm(coefsBM.TL[unique(temp) !=23.8] ~ unique(temp[temp !=23.8])))
# 
# 
# 
# unique(data2$temp)


##### prey mass ~ pred mass
data2 = data[TL > 1, ]
res = c()
for (i in 1:dim(data2)[1]){
  predM = data2$BM[i]
  predID = as.character(i)
  preyM = strsplit(as.character(data2$prey_BM_list[i]), '_')[[1]][-1]
  res0 = cbind.data.frame(predM, as.numeric(preyM), data2$temp[i],data2$name[i], 'predID')
  res = rbind.data.frame(res, res0)
}
res = as.data.frame(res)
names(res) = c('predBM', 'preyBM', 'temp', 'name', 'predID')
head(res)

plot(log10(res[,2]) ~ log10(res[,1]), col = adjustcolor(map.colors(data2$temp), alpha.f = 0.6), 
     cex = 1, pch = 16, ylab = 'log10 of Prey BM', xlab = 'log10 of Pred BM', cex.lab = 2, cex.axis = 1.4)
# abline(0,1, lty = 2)
legend('topright', 'B', bty = 'n', cex = 2)

coefsBMratio = c()
for (t in unique(res[,3])){
  sub.model = lm(log10(res[,2][res[,3] == t]) ~ log10(res[,1][res[,3] == t]))
  coefsBMratio = c(coefsBMratio, summary(sub.model)$coefficients[2])
  abline(sub.model, col = map.colors(t))
}

res$temp = res$temp - 20

# abline(lm(coefsBMratio ~ unique(res[,3])))
summary(lm(coefsBMratio ~ unique(res[,3])))
plot(coefsBMratio ~ unique(res[,3]), ylab = 'slopes of individual regressions', xlab = 'temperature')
legend('topright', 'E', bty = 'n', cex = 2)
# 
# 
model = lme(log10(preyBM) ~ log10(predBM) * temp, random =~1|temp/name, data = res)
model = lme(log10(preyBM) ~ log10(predBM) * temp, random =~1|name, data = res)
model = lme(log10(preyBM) ~ log10(predBM) * temp, random = list(name =~1, predID =~1), data = res)
# plot(model)
# anova(model)
# summary(model)
# 
model2 = lme(log10(preyBM) ~ log10(predBM) + temp, random = ~1|name, data = res)
# plot(model2)
# anova(model2)
# summary(model2)
# AIC(model)
# AIC(model2)
# 
BIC(model)
BIC(model2)




############33 mass ~ TL
cbPalette <- c('blue4', 'dodgerblue3', 'lightskyblue', 'lightseagreen', 'gold', 'darkorange2', 'red')
# par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

p = plot(log10(data$BM) ~ data$TL, col = adjustcolor(cbPalette, alpha.f = 0.8), 
     ylab = 'log10 of species BM', xlab = 'species TL', cex = 1, pch = 16, cex.lab = 1.8, cex.axis = 1.4, ann = FALSE)
# legend('topright', 'C', bty = 'n', cex = 2)





coefsB.TL = c()
comp = 0
for (i in unique(data$temp)){
  comp = comp + 1
  cat(i, '\n')
  abline(lm(log10(data$BM[data$temp == i]) ~ data$TL[data$temp == i]), col = cbPalette[comp])
  coefsB.TL =c(coefsB.TL, summary(lm(log10(data$BM[data$temp == i]) ~ data$TL[data$temp == i]))$coefficients[2])
}
abline(lm(log10(data$BM) ~ data$TL), lwd = 4)
mtext(side = 1, text = "species TL", line = 3, cex = 1.8)
mtext(side = 2, text = "log10 of species BM", line = 2.5, cex = 1.8)


log10BM = log10(data$BM)

cbPalette <- c('blue4', 'dodgerblue3', 'lightskyblue', 'lightseagreen', 'gold', 'darkorange2', 'red')
col_pal = viridis(7, option = 'plasma')
plot(c(1:7) ~ 1, col = col_pal, pch = 16, cex = 2)
df = data.frame(speciesBM = log10BM, TL = data$TL, init_temp = data$temp)
TL_BM = ggplot(df, 
       aes(x=TL, y=speciesBM, colour = as.factor(init_temp)), 
)+
  geom_point(alpha = 0.8)+
  # stat_summary(geom="point", fun.y=mean, cex = 1)+
  geom_smooth(method = 'lm', cex = 0.7, se = FALSE)+
  stat_smooth(aes(x=TL, y=speciesBM, group = NULL), method = 'lm', formula = y ~x, cex = 1.6, se = FALSE, colour = "black")+
  scale_color_manual(values=col_pal, name = paste('Average \nsummer \nsea temp.'))+
  theme_classic()+
  ggtitle('c)')+
  theme(
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10),
    legend.position = 'none',
  )
  
save(TL_BM, file = '~/projects/WarmingWebs/R_code2/TL_BM.Rdata')







summary(lm(coefsB.TL ~ unique(data$temp)))

plot(coefsB.TL ~ unique(data$temp), ylab = 'slopes of individual regressions', xlab = 'temperature')
legend('topleft', 'F', bty = 'n', cex = 2)
par(mar=c(0.1, 0.1, 0.1, 0.1)) 
plot.new()
legend('left', legend = sort(unique(temp)), col = colors, lty = rep(1,7), title = 'Initial temperature',  box.lty=0, cex = 2)

regions.l = strsplit(as.character(data$name), split = '/')
data$region = data.table::transpose(regions.l)[[7]]
data$beach = data.table::transpose(regions.l)[[8]]

# variable for beach:
data$beach = gsub("u'", "", data$beach)
data$beach = gsub("'", "", data$beach)
unique(data$beach)
unique(cbind.data.frame(data$region, data$beach))
data$beach = gsub("\\d+$", "", data$beach)
unique(data$beach)
data$beach[grep("PP+", data$beach)] = "PP"
data$beach[grep("SF+", data$beach)] = "SF"
random_model = ~ warming |region/beach/name   # need to change the control here
random_model = ~ 1 | region/beach/name
random_model = ~ 1 | name


model = lme(log10(BM) ~ TL * temp, random = random_model, data = data)
anova(model)
summary(model)
# plot(model)


model2 =lme(log10(BM) ~ TL + temp, random = random_model, data = data)
model3 =lme(log10(BM) ~ TL , random = random_model, data = data)

anova(model3)
plot(model3)
BIC(model)
BIC(model2)
BIC(model3)

AIC(model)
AIC(model2)
AIC(model3)

dev.off()
detach(data)


