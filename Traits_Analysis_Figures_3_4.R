
source('~/R/startup.R') #
#~/VMFR/Dynamics Paper 2019/Data/

source('~/R/Loess_Density_Correction.R')
source('~/R/Range01.R')


# ~~~~ Figure 2 -----------------------------------------------------------


# Start up ----------------------------------------------------------------


bad.plots.25 <- c(33, 34, 37, 39, 49, 53, 59, 69, 71, 58, 79)

# SP data

df <- read.csv('SP_dynamics_nobin.csv') %>%
        group_by(PSP, SP) %>%
        summarise(RGR = mean(rgr.sp, na.rm = T), M = mean(M.r, na.rm = T)*100, pf = mean(pf.BA.sp, na.rm = T)) %>%
        mutate(plot = paste0(PSP,'.',SP)) %>% 
        na.exclude()

WD <- read.csv('../../Data/VMFR_corrected_AGB.csv') %>%
        filter(foresttype == 'diverse') %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Su.Pl.No) %>%
        summarise(WD = mean(Density, na.rm = T)) %>%
        mutate(plot = paste0(PSP,'.',Su.Pl.No))

wd <- rep(0, times = nrow(df))

wd[df$plot %in% WD$plot] <- WD$WD[WD$plot %in% df$plot]
wd[wd==0] <- NA
df$WD <- wd


# PSP data 


df.psp <- read.csv('PSP_dynamics_no_bin.csv') %>%
        group_by(PSP) %>%
        summarise(RGR = mean(rgr, na.rm = T), M = mean(M.r, na.rm = T)*100, pf = mean(pf.BA.psp, na.rm = T)) %>%
        na.exclude()



WD.psp <- read.csv('../../Data/VMFR_corrected_AGB.csv') %>%
        filter(foresttype == 'diverse') %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP) %>%
        summarise(WD = mean(Density, na.rm = T))

wd.psp <- rep(0, times = nrow(df.psp))

wd.psp[df.psp$PSP %in% WD.psp$PSP] <- WD.psp$WD[WD.psp$PSP %in% df.psp$PSP]
wd.psp[wd.psp==0] <- NA
df.psp$WD <- wd.psp



## Fixer NonFixer ##


df2 <- read.csv('Fixer_Nonfixer_dynamics_SP.csv') %>%
        group_by(PSP, SP, Fixer) %>%
        summarise(RGR = mean(rgr, na.rm = T),
                  M = mean(M.r, na.rm = T)*100,
                  pf = mean(pf.sp.BA.total, na.rm = T),
                  SR = mean(SR.sp, na.rm = T),
                  Nf = mean(Nf.sp.total, na.rm = T),
                  N = mean(N.sp.total, na.rm = T)) %>%
        mutate(plot = paste0(PSP,'.',SP)) %>%
        mutate(Nnf = N-Nf) %>%
        ungroup()

df2.psp <- read.csv('Fixer_Nonfixer_dynamics_nobin.csv') %>%
        group_by(PSP, Fixer) %>%
        summarise(RGR = mean(rgr, na.rm = T),
                  M = mean(M.r, na.rm = T)*100,
                  pf = mean(pf.BA.total, na.rm = T),
                  SR = mean(SR, na.rm = T),
                  Nf = mean(Nf.total, na.rm = T),
                  N = mean(N.total, na.rm = T)) %>%
        mutate(Nnf = N-Nf) %>%
        ungroup()


WD2 <- read.csv('../../Data/VMFR_corrected_AGB.csv') %>%
        filter(foresttype == 'diverse') %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Su.Pl.No, Fixer) %>%
        summarise(WD = mean(Density, na.rm = T)) %>%
        mutate(plot = paste0(PSP,'.',Su.Pl.No))
WD2 <- WD2 %>% filter(plot %in% df2$plot)

WD2.psp <- read.csv('../../Data/VMFR_corrected_AGB.csv') %>%
        filter(foresttype == 'diverse') %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Fixer) %>%
        summarise(WD = mean(Density, na.rm = T))
WD2.psp <- WD2.psp %>% filter(PSP %in% df2.psp$PSP)


df2.f <- df2 %>% filter(Fixer %in% 'fixer')
df2.nf <- df2 %>% filter(Fixer %in% 'nonfixer')
WD2.f <- WD2 %>% filter(Fixer %in% 1)
WD2.nf <- WD2 %>% filter(Fixer %in% 0)



df2.f.psp <- df2.psp %>% filter(Fixer %in% 'fixer')
df2.nf.psp <- df2.psp %>% filter(Fixer %in% 'nonfixer')
WD2.f.psp <- WD2.psp %>% filter(Fixer %in% 1)
WD2.nf.psp <- WD2.psp %>% filter(Fixer %in% 0)




wd.f <- wd.nf <- rep(0, times = nrow(df2.f))
wd.f.psp <- wd.nf.psp <- rep(0, times = nrow(df2.f.psp))



wd.f[df2.f$plot %in% WD2.f$plot] <- WD2.f$WD[WD2.f$plot %in% df2.f$plot]
wd.f[wd.f==0] <- NA

wd.f.psp[df2.f.psp$PSP %in% WD2.f.psp$PSP] <- WD2.f.psp$WD[WD2.f.psp$PSP %in% df2.f.psp$PSP]
wd.f.psp[wd.f.psp==0] <- NA



wd.nf[df2.nf$plot %in% WD2.nf$plot] <- WD2.nf$WD[WD2.nf$plot %in% df2.nf$plot]
wd.nf[wd.nf==0] <- NA

wd.nf.psp[df2.nf.psp$PSP %in% WD2.nf.psp$PSP] <- WD2.nf.psp$WD[WD2.nf.psp$PSP %in% df2.nf.psp$PSP]
wd.nf.psp[wd.nf.psp==0] <- NA



df2.f$WD <- wd.f
df2.nf$WD <- wd.nf

df2.f.psp$WD <- wd.f.psp
df2.nf.psp$WD <- wd.nf.psp



df2 <- rbind(df2.f, df2.nf)  %>%
        na.exclude

df2.psp <- rbind(df2.f.psp, df2.nf.psp)  %>%
        na.exclude



div <- read.csv('../../Data/VMFR_corrected_AGB.csv') %>%
        filter(foresttype == 'diverse') %>%
        filter(Fixer == 0) %>%
        mutate(id = paste0(PSP,'.',Su.Pl.No)) %>%
        select(id, Species) %>%
        cbind(abund = rep(1)) %>% 
        matrify %>%
        diversity()

div.psp <- read.csv('../../Data/VMFR_corrected_AGB.csv') %>%
        filter(foresttype == 'diverse') %>%
        filter(Fixer == 0) %>%
        select(PSP, Species) %>%
        cbind(abund = rep(1)) %>% 
        matrify %>%
        diversity()



nf <- df2 %>% filter(Fixer == 'nonfixer')
nf$H <- div[names(div) %in% nf$plot]
nf <- nf %>% mutate(E = H/log(13)) %>% # max species is 13
        mutate(pf.o = pf) %>%
        mutate(pf = range01(pf.o)) %>%
        mutate(SR = range01(SR))

nf.psp <- df2.psp %>% filter(Fixer == 'nonfixer')
nf.psp$H <- div.psp[names(div.psp) %in% nf.psp$PSP]
nf.psp <- nf.psp %>% mutate(E = H/log(44)) %>% # max species is 44
        mutate(pf.o = pf) %>%
        mutate(pf = range01(pf.o)) %>%
        mutate(SR = range01(SR))


fixer <- read.csv('../../Data/VMFR_corrected_BA.csv') %>%
        filter(foresttype == 'diverse') %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Year, Su.Pl.No) %>%
        mutate(N = length(BA)) %>%
        mutate(nf = sum(Fixer)) %>%
        mutate(pf = nf/N) %>%
        group_by(PSP, Su.Pl.No, Fixer) %>%
        summarise(BA.fix = mean(BA), pf = mean(pf)) %>%
        filter(Fixer == 1) %>%
        mutate(ID = paste0(PSP, '.',Su.Pl.No))

nonfixer <- read.csv('../../Data/VMFR_corrected_BA.csv') %>%
        filter(foresttype == 'diverse') %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Year, Su.Pl.No, Fixer) %>%
        mutate(N = length(BA)) %>%
        group_by(PSP, Su.Pl.No, Fixer) %>%
        summarise(BA.nf = mean(BA), N.nf = mean(N)) %>%
        filter(Fixer == 0) %>%
        mutate(ID = paste0(PSP, '.',Su.Pl.No))

nonfixer <- nonfixer %>% filter(ID %in% fixer$ID)




fixer.psp <- read.csv('../../Data/VMFR_corrected_BA.csv') %>%
        filter(foresttype == 'diverse') %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Year) %>%
        mutate(N = length(BA)) %>%
        mutate(nf = sum(Fixer)) %>%
        mutate(pf = nf/N) %>%
        group_by(PSP, Fixer) %>%
        summarise(BA.fix = mean(BA), pf = mean(pf)) %>%
        filter(Fixer == 1)

nonfixer.psp <- read.csv('../../Data/VMFR_corrected_BA.csv') %>%
        filter(foresttype == 'diverse') %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Year, Fixer) %>%
        mutate(N = length(BA)) %>%
        group_by(PSP, Fixer) %>%
        summarise(BA.nf = mean(BA), N.nf = mean(N)) %>%
        filter(Fixer == 0)

nonfixer.psp <- nonfixer.psp %>% filter(PSP %in% fixer.psp$PSP)







# Height


H.data <- read.csv('../../Data/Plot5_18_30_41_Height_data.csv')
names(H.data) <- c('PSP', 'SP', 'Genus', 'Species', 'Height', 'Rho', 'Bin', 'DBH')
fixers.genus <- c("Pentaclethra", 'Clathrotropis',"Swartzia", "Inga", "Lonchocarpus")
H.data$Fixer <- H.data$Genus %in% fixers.genus %>% as.numeric
H.data <- H.data %>% filter(Height<45)



H.df <- H.data %>% 
        group_by(PSP, SP) %>%
        mutate(N = length(SP)) %>%
        mutate(pf = sum(Fixer, na.rm = T)/N) %>%
        filter(Fixer == 0) %>% 
        summarise(pf = mean(pf, na.rm = T),
                  H = gm.mean(Height, na.rm = T),
                  H.max = max(Height, na.rm = T),
                  H.95 = quantile(Height, 0.95))%>%
        mutate(pf = range01(pf))

H.df.psp <- H.data %>% 
        group_by(PSP) %>%
        mutate(N = length(PSP)) %>%
        mutate(pf = sum(Fixer, na.rm = T)/N) %>%
        filter(Fixer == 0) %>% 
        summarise(pf = mean(pf, na.rm = T),
                  H = gm.mean(Height, na.rm = T),
                  H.max = max(Height, na.rm = T),
                  H.95 = quantile(Height, 0.95)) %>%
        mutate(pf = range01(pf))




slopes <- data.frame(var = c('c)', 'd)', 'e)', 'f )', 'g)', 'h)'),
                     slope = rep(0),
                     upr = rep(0),
                     lwr = rep(0),
                     sig = rep(0))

slopes.psp <- data.frame(var = c('c)', 'd)', 'e)', 'f )', 'g)', 'h)'),
                         slope = rep(0),
                         upr = rep(0),
                         lwr = rep(0),
                         sig = rep(0))




# Graphics parameters -----------------------------------------------------

plot.size <- 2
plot.stroke <- 1.2
plot.color <- 'dodgerblue'
plot.shape <- 0
plot.alpha <- 0.6

subplot.size <- 2
subplot.stroke <- 1
subplot.color <- 'blue'
subplot.shape <- 16
subplot.alpha <- 0.1

text.size <- 18





# Hist 1 & 2 ------------------------------------------------------------------

hist.plot.sp <- ggplot(df) +
        geom_histogram(aes(x = pf),
                       binwidth = 0.05, color = subplot.color, fill = subplot.color, alpha = 0.6, size = 1) +
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text = element_text(size = text.size),
              axis.title.y = element_text(size = text.size),
              axis.title.x = element_text(size = text.size-1, color = 'grey30')) +
        scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75), labels = c(0, 25, 50, 75)) +
        ylim(NA, 450) +
        labs(x = 'Relative Abundance (%)', y = bquote('Subplot Density'))



hist.plot.psp <- ggplot(df.psp) +
        geom_histogram(aes(x = pf),
                       binwidth = 0.05, color = plot.color, fill = plot.color, alpha = 0.6, size = 1) +
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text = element_text(size = text.size),
              axis.title.y = element_text(size = text.size),
              axis.title.x = element_text(size = text.size-1, color = 'grey30')) +
        scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4), labels = c(0, 10, 20, 30, 40)) +
        labs(x = 'Relative Abundance (%)', y = bquote('Plot Density'))




# SR 3 ----------------------------------------------------------------------

### ~~~~ Slope effect size plot ~~~~ ###

# SUBPLOT - significant slope

mod.s <- summary(lmer(scale(SR) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
slopes$slope[1] <- mod.s$coefficients[2,1]

CI <- confint(lmer(scale(SR) ~ pf + (1|PSP), data = nf))
slopes$upr[1] <- CI[4,2]
slopes$lwr[1] <- CI[4,1]

slopes$sig[1] <- 1

# PLOT - n.s. slope

mod.s <- summary(lm(scale(SR) ~ pf, data = nf.psp)) # standardized slope: -0.56
slopes.psp$slope[1] <- mod.s$coefficients[2,1]

CI <- confint(lm(scale(SR) ~ pf, data = nf.psp))
slopes.psp$upr[1] <- CI[2,2]
slopes.psp$lwr[1] <- CI[2,1]

slopes.psp$sig[1] <- 0




### ~~~~ Mixed effects model & summary ~~~~ ###

# SUBPLOT

mod <- lmer(SR ~ pf +  (1|PSP), data = nf)
mod %>% summary

ci.list3 <- ci.bands(nf$pf, nf$SR, mod)

# PLOT

mod <- lm(SR ~ pf, data = nf.psp)
mod %>% summary

ci.list3.psp <- ci.bands(nf.psp$pf, nf.psp$SR, mod)



### ~~~~ Plotting ~~~~ ###


SR.nf.plot <- ggplot() +
        geom_point(data = nf, aes(x = pf, y = SR),
                   size = subplot.size, shape = subplot.shape, alpha = subplot.alpha, color = subplot.color) +
        geom_point(data = nf.psp , aes(x = pf, y = SR),
                   size = plot.size, shape = plot.shape, alpha = plot.alpha, color = plot.color, stroke = plot.stroke) +
        
        geom_ribbon(data = nf.psp, aes(x = ci.list3.psp$ci.df$x, ymin = ci.list3.psp$ci.df$lwr, ymax = ci.list3.psp$ci.df$upr),
                    alpha = 0.4, fill = 'white') +
        annotate('segment',
                 x = ci.list3.psp$x[1], xend = ci.list3.psp$x[2],
                 y = ci.list3.psp$y[1], yend = ci.list3.psp$y[2],
                 size = 1.5, color = plot.color, alpha = 1, linetype = 'longdash') +
        geom_ribbon(data = nf.psp, aes(x = ci.list3.psp$ci.df$x, ymin = ci.list3.psp$ci.df$lwr, ymax = ci.list3.psp$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = plot.color) +
        
        annotate('segment',
                 x = ci.list3$x[1], xend = ci.list3$x[2],
                 y = ci.list3$y[1], yend = ci.list3$y[2],
                 size = 1.5, color = 'blue', alpha = 1) +
        geom_ribbon(data = nf, aes(x = ci.list3$ci.df$x, ymin = ci.list3$ci.df$lwr, ymax = ci.list3$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = subplot.color) +
        
        
        scale_x_continuous(breaks = c(0.05, 0.5, 0.95), labels = c('Low', 'Medium', 'High'), limits = c(0,1)) +
        scale_y_continuous(limits = c(0, 1.1), breaks = c(0, 0.5, 1), labels = c('low', 'med', 'high')) +
        
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text.x = element_text(size = text.size),
              axis.text.y = element_text(size = 14),
              axis.title = element_text(size = text.size)) +
        labs(x = '', y = bquote('Species Richness (#)'))




# Evenness 4 ----------------------------------------------------------------

### ~~~~ Slope effect size plot ~~~~ ###

# SUBPLOT 

mod.s <- summary(lmer(scale(E) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
slopes$slope[2] <- mod.s$coefficients[2,1]

CI <- confint(lmer(scale(E) ~ pf + (1|PSP), data = nf))
slopes$upr[2] <- CI[4,2]
slopes$lwr[2] <- CI[4,1]

slopes$sig[2] <- 0


# PLOT

mod.s <- summary(lm(scale(E) ~ pf, data = nf.psp)) # standardized slope: -0.56
slopes.psp$slope[2] <- mod.s$coefficients[2,1]

CI <- confint(lm(scale(E) ~ pf, data = nf.psp))
slopes.psp$upr[2] <- CI[2,2]
slopes.psp$lwr[2] <- CI[2,1]

slopes.psp$sig[2] <- 0




### ~~~~ Mixed effects model & summary ~~~~ ###

# SUBPLOT

mod <- lmer(E ~ pf +  (1|PSP), data = nf)
mod %>% summary

ci.list4 <- ci.bands(nf$pf, nf$E, mod)

# PLOT

mod <- lm(E ~ pf, data = nf.psp)
mod %>% summary

ci.list4.psp <- ci.bands(nf.psp$pf, nf.psp$E, mod)



### ~~~~ Plotting ~~~~ ###


E.nf.plot <- ggplot() +
        geom_jitter(data = nf, aes(x = pf, y = E),
                    size = subplot.size, shape = subplot.shape, alpha = subplot.alpha, color = subplot.color, height = 0.1) +
        geom_jitter(data = nf.psp, aes(x = pf, y = E),
                    size = plot.size, shape = plot.shape, alpha = plot.alpha, color = plot.color, height = 0.1, stroke = plot.stroke) +
        
        geom_ribbon(data = nf.psp, aes(x = ci.list4.psp$ci.df$x, ymin = ci.list4.psp$ci.df$lwr, ymax = ci.list4.psp$ci.df$upr),
                    alpha = 0.4, fill = 'white') +
        annotate('segment',
                 x = ci.list4.psp$x[1], xend = ci.list4.psp$x[2],
                 y = ci.list4.psp$y[1], yend = ci.list4.psp$y[2],
                 size = 1.5, color = plot.color, alpha = 1, linetype = 'longdash') +
        geom_ribbon(data = nf.psp, aes(x = ci.list4.psp$ci.df$x, ymin = ci.list4.psp$ci.df$lwr, ymax = ci.list4.psp$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = plot.color) +
        
        annotate('segment',
                 x = ci.list4$x[1], xend = ci.list4$x[2],
                 y = ci.list4$y[1], yend = ci.list4$y[2],
                 size = 1.5, color = subplot.color, alpha = 1, linetype = 'longdash') +
        geom_ribbon(data = nf, aes(x = ci.list4$ci.df$x, ymin = ci.list4$ci.df$lwr, ymax = ci.list4$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = subplot.color) +

        scale_x_continuous(breaks = c(0.05, 0.5, 0.95), labels = c('Low', 'Medium', 'High'), limits = c(0,1)) +
        scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('low', 'med', 'high')) +
        
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text.x = element_text(size = text.size),
              axis.text.y = element_text(size = 14),
              axis.title = element_text(size = text.size)) +
        labs(x = '', y = bquote('Species Evenness'))





# WD 5 ----------------------------------------------------------------

### ~~~~ Slope effect size plot ~~~~ ###

# SUBPLOT

mod.s <- summary(lmer(scale(WD) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
slopes$slope[3] <- mod.s$coefficients[2,1]

CI <- confint(lmer(scale(WD) ~ pf + (1|PSP), data = nf))
slopes$upr[3] <- CI[4,2]
slopes$lwr[3] <- CI[4,1]

slopes$sig[3] <- 1

# PLOT

mod.s <- summary(lm(scale(WD) ~ pf, data = nf.psp)) # standardized slope: -0.56
slopes.psp$slope[3] <- mod.s$coefficients[2,1]

CI <- confint(lm(scale(WD) ~ pf, data = nf.psp))
slopes.psp$upr[3] <- CI[2,2]
slopes.psp$lwr[3] <- CI[2,1]

slopes.psp$sig[3] <- 1


### ~~~~ Mixed effects model & summary ~~~~ ###

# SUBPLOT

mod <- lmer(WD ~ pf +  (1|PSP), data = nf)
mod %>% summary # -0.05

ci.list5 <- ci.bands(nf$pf, nf$WD, mod)

# PLOT

mod <- lm(WD ~ pf, data = nf.psp)
mod %>% summary # -0.05

ci.list5.psp <- ci.bands(nf.psp$pf, nf.psp$WD, mod)



### ~~~~ Plotting ~~~~ ###


WD.nf.plot <- ggplot(nf) +
        geom_point(data = nf, aes(x = pf, y = WD),
                   size = subplot.size, shape = subplot.shape, alpha = subplot.alpha, color = subplot.color) +
        geom_point(data = nf.psp , aes(x = pf, y = WD),
                   size = plot.size, shape = plot.shape, alpha = plot.alpha, color = plot.color, stroke = plot.stroke) +
        
        geom_ribbon(data = nf.psp, aes(x = ci.list5.psp$ci.df$x, ymin = ci.list5.psp$ci.df$lwr, ymax = ci.list5.psp$ci.df$upr),
                    alpha = 0.4, fill = 'white') +
        annotate('segment',
                 x = ci.list5.psp$x[1], xend = ci.list5.psp$x[2],
                 y = ci.list5.psp$y[1], yend = ci.list5.psp$y[2],
                 size = 1.5, color = plot.color, alpha = 1) +
        geom_ribbon(data = nf.psp, aes(x = ci.list5.psp$ci.df$x, ymin = ci.list5.psp$ci.df$lwr, ymax = ci.list5.psp$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = plot.color) +
        
        annotate('segment',
                 x = ci.list5$x[1], xend = ci.list5$x[2],
                 y = ci.list5$y[1], yend = ci.list5$y[2],
                 size = 1.5, color = subplot.color, alpha = 1) +
        geom_ribbon(data = nf, aes(x = ci.list5$ci.df$x, ymin = ci.list5$ci.df$lwr, ymax = ci.list5$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = subplot.color) +
        
        
        scale_x_continuous(breaks = c(0.05, 0.5, 0.95), labels = c('Low', 'Medium', 'High'), limits = c(0,1)) +
        scale_y_continuous(limits = c(0.4, 0.95), breaks = c(0.45, 0.65, 0.85)) +
        
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text = element_text(size = text.size),
              axis.title = element_text(size = text.size)) +
        labs(x = '', y = bquote('Wood Density (g'~cm^-3*')'))




# RGR 6 ---------------------------------------------------------------------

### ~~~~ Slope effect size plot ~~~~ ###

# SUBPLOT

mod.s <- summary(lmer(scale(RGR) ~ pf + (1|PSP), data = nf)) # standardized slope: 0.56
slopes$slope[4] <- mod.s$coefficients[2,1]

CI <- confint(lmer(scale(RGR) ~ pf + (1|PSP), data = nf))
slopes$upr[4] <- CI[4,2]
slopes$lwr[4] <- CI[4,1]

slopes$sig[4] <- 1

# PLOT

mod.s <- summary(lm(scale(RGR) ~ pf, data = nf.psp)) # standardized slope: 0.56
slopes.psp$slope[4] <- mod.s$coefficients[2,1]

CI <- confint(lm(scale(RGR) ~ pf, data = nf.psp))
slopes.psp$upr[4] <- CI[2,2]
slopes.psp$lwr[4] <- CI[2,1]

slopes.psp$sig[4] <- 0


### ~~~~ Mixed effects model & summary ~~~~ ###

# SUBPLOT

mod <- lmer(RGR ~ pf + (1|PSP), data = nf)
mod %>% summary # 0.51

ci.list6 <- ci.bands(nf$pf, nf$RGR, mod)


# PLOT

mod <- lm(RGR ~ pf, data = nf.psp)
mod %>% summary # 0.51


ci.list6.psp <- ci.bands(nf.psp$pf, nf.psp$RGR, mod)



### ~~~~ Plotting ~~~~ ###


RGR.nf.plot <- ggplot() +
        geom_point(data = nf, aes(x = pf, y = RGR),
                   size = subplot.size, shape = subplot.shape, alpha = subplot.alpha, color = subplot.color) +
        geom_point(data = nf.psp , aes(x = pf, y = RGR),
                   size = plot.size, shape = plot.shape, alpha = plot.alpha, color = plot.color, stroke = plot.stroke) +
        
        geom_ribbon(data = nf.psp, aes(x = ci.list6.psp$ci.df$x, ymin = ci.list6.psp$ci.df$lwr, ymax = ci.list6.psp$ci.df$upr),
                    alpha = 0.4, fill = 'white') +
        annotate('segment',
                 x = ci.list6.psp$x[1], xend = ci.list6.psp$x[2],
                 y = ci.list6.psp$y[1], yend = ci.list6.psp$y[2],
                 size = 1.5, color = plot.color, alpha = 1, linetype = 'longdash') +
        geom_ribbon(data = nf.psp, aes(x = ci.list6.psp$ci.df$x, ymin = ci.list6.psp$ci.df$lwr, ymax = ci.list6.psp$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = plot.color) +
        
        annotate('segment',
                 x = ci.list6$x[1], xend = ci.list6$x[2],
                 y = ci.list6$y[1], yend = ci.list6$y[2],
                 size = 1.5, color = subplot.color, alpha = 0.7) +
        geom_ribbon(data = nf, aes(x = ci.list6$ci.df$x, ymin = ci.list6$ci.df$lwr, ymax = ci.list6$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = subplot.color) +
        
        scale_x_continuous(breaks = c(0.05, 0.5, 0.95), labels = c('Low', 'Medium', 'High'), limits = c(0,1)) +
        scale_y_continuous(limits = c(0, 3.5), breaks = c(0, 1, 2, 3)) +
        
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text = element_text(size = text.size),
              axis.title = element_text(size = text.size)) +
        labs(x = '', y = bquote('RGR (% '~yr^-1*')'))



# Height 7 ------------------------------------------------------------------

### ~~~~ Slope effect size plot ~~~~ ###

# SUBPLOT

mod.s <- summary(lmer(scale(H) ~ pf + (1|PSP), data = H.df)) # standardized slope: -0.56
slopes$slope[5] <- mod.s$coefficients[2,1]

CI <- confint(lmer(scale(H) ~ pf + (1|PSP), data = H.df))
slopes$upr[5] <- CI[4,2]
slopes$lwr[5] <- CI[4,1]

slopes$sig[5] <- 1

# PLOT

mod.s <- summary(lm(scale(H) ~ pf, data = H.df.psp)) # standardized slope: -0.56
slopes.psp$slope[5] <- mod.s$coefficients[2,1]

CI <- confint(lm(scale(H) ~ pf, data = H.df.psp))
slopes.psp$upr[5] <- NA
slopes.psp$lwr[5] <- NA

slopes.psp$sig[5] <- 0


### ~~~~ Mixed effects model & summary ~~~~ ###

# SUBPLOT

mod <- lmer(H ~ pf + (1|PSP), data = H.df)
summary(mod) #  sig

ci.list7 <- ci.bands(H.df$pf, H.df$H, mod)


# PLOT

mod <- lm(H ~ pf, data = H.df.psp)
summary(mod) #  sig

ci.list7.psp <- ci.bands(H.df.psp$pf, H.df.psp$H, mod)

h.nf.plot <- ggplot() +
        geom_point(data = H.df, aes(x = pf, y = H),
                   size = subplot.size, shape = subplot.shape, alpha = 0.3, color = subplot.color) +
        geom_point(data = H.df.psp , aes(x = pf, y = H),
                   size = plot.size, shape = plot.shape, alpha = 0.8, color = plot.color, stroke = plot.stroke) +
        
        geom_ribbon(data = H.df.psp, aes(x = ci.list7.psp$ci.df$x, ymin = ci.list7.psp$ci.df$lwr, ymax = ci.list7.psp$ci.df$upr),
                    alpha = 0.4, fill = 'white') +
        annotate('segment',
                 x = ci.list7.psp$x[1], xend = ci.list7.psp$x[2],
                 y = ci.list7.psp$y[1], yend = ci.list7.psp$y[2],
                 size = 1.5, color = plot.color, alpha = 0.7, linetype = 'longdash') +
        geom_ribbon(data = H.df.psp, aes(x = ci.list7.psp$ci.df$x, ymin = ci.list7.psp$ci.df$lwr, ymax = ci.list7.psp$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = plot.color) +
        
        annotate('segment',
                 x = ci.list7$x[1], xend = ci.list7$x[2],
                 y = ci.list7$y[1], yend = ci.list7$y[2],
                 size = 1.5, color = subplot.color, alpha = 0.7) +
        geom_ribbon(data = H.df, aes(x = ci.list7$ci.df$x, ymin = ci.list7$ci.df$lwr, ymax = ci.list7$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = subplot.color) +
        
        
        scale_x_continuous(breaks = c(0.05, 0.5, 0.95), labels = c('Low', 'Medium', 'High'), limits = c(0,1)) +
        scale_y_continuous(breaks = c(10, 20, 30)) +
        
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text = element_text(size = text.size),
              axis.title = element_text(size = text.size)) +
        labs(x = '', y = bquote('Mean Height (m)'))



# Mort 8 --------------------------------------------------------------------

### ~~~~ Slope effect size plot ~~~~ ###

# SUBPLOT

mod.s <- summary(lmer(scale(M) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
slopes$slope[6] <- mod.s$coefficients[2,1]

CI <- confint(lmer(scale(M) ~ pf + (1|PSP), data = nf))
slopes$upr[6] <- CI[4,2]
slopes$lwr[6] <- CI[4,1]

slopes$sig[6] <- 1

# PLOT

mod.s <- summary(lm(scale(M) ~ pf, data = nf.psp)) # standardized slope: -0.56

slopes.psp$slope[6] <- mod.s$coefficients[2,1]

CI <- confint(lm(scale(M) ~ pf, data = nf.psp))
slopes.psp$upr[6] <- CI[2,2]
slopes.psp$lwr[6] <- CI[2,1]

slopes.psp$sig[6] <- 0




### ~~~~ Mixed effects model & summary ~~~~ ###

# SUBPLOT

mod <- lmer(M ~ pf + (1|PSP), data = nf)
summary(mod)

ci.list8 <- ci.bands(nf$pf, nf$M, mod)

# PLOT

mod <- lm(M ~ pf, data = nf.psp)
summary(mod)

ci.list8.psp <- ci.bands(nf.psp$pf, nf.psp$M, mod)



### ~~~~ Plotting ~~~~ ###


M.nf.plot <- ggplot() +
        geom_point(data = nf, aes(x = pf, y = M),
                   size = subplot.size, shape = subplot.shape, alpha = subplot.alpha, color = subplot.color) +
        geom_point(data = nf.psp , aes(x = pf, y = M),
                   size = plot.size, shape = plot.shape, alpha = plot.alpha, color = plot.color, stroke = plot.stroke) +
        
        geom_ribbon(data = nf.psp, aes(x = ci.list8.psp$ci.df$x, ymin = ci.list8.psp$ci.df$lwr, ymax = ci.list8.psp$ci.df$upr),
                    alpha = 0.4, fill = 'white') +
        annotate('segment',
                 x = ci.list8.psp$x[1], xend = ci.list8.psp$x[2],
                 y = ci.list8.psp$y[1], yend = ci.list8.psp$y[2],
                 size = 1.5, color = plot.color, alpha = 1, linetype = 'longdash') +
        geom_ribbon(data = nf.psp, aes(x = ci.list8.psp$ci.df$x, ymin = ci.list8.psp$ci.df$lwr, ymax = ci.list8.psp$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = plot.color) +
        
        annotate('segment',
                 x = ci.list8$x[1], xend = ci.list8$x[2],
                 y = ci.list8$y[1], yend = ci.list8$y[2],
                 size = 1.5, color = subplot.color, alpha = 1) +
        geom_ribbon(data = nf, aes(x = ci.list8$ci.df$x, ymin = ci.list8$ci.df$lwr, ymax = ci.list8$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = subplot.color) +
        
        scale_x_continuous(breaks = c(0.05, 0.5, 0.95), labels = c('Low', 'Medium', 'High'), limits = c(0,1)) +
        scale_y_continuous(limits = c(NA, 7.5)) +
        
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text = element_text(size = text.size),
              axis.title = element_text(size = text.size)) +
        labs(x = '', y = bquote('Mortality (% '~yr^-1*')'))



# Plotting ----------------------------------------------------------------


# individual level


figure(
        ggarrange(
                hist.plot.sp,
                hist.plot.psp,
                SR.nf.plot,
                E.nf.plot,
                WD.nf.plot,
                RGR.nf.plot,
                h.nf.plot,
                M.nf.plot,
                nrow = 4, ncol = 2, 
                align = 'hv'),
        path.name = 'c:/users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Figures/Nfix_traits_nonfix',
        height = 12.1,
        width = 8,
        save = T
)




slopes <- slopes %>%
        arrange(var) %>%
        mutate(var = factor(var, levels = var))

slopes.psp <- slopes.psp %>%
        arrange(var) %>%
        mutate(var = factor(var, levels = var))



s1 <- ggplot(slopes) +
        geom_errorbar(aes(x = var, ymax = upr, ymin = lwr, width = 0), size = 1.5, color = subplot.color) +
        geom_point(aes(y = slope, x = var, group = factor(sig), shape = factor(sig)), size = 5, color = subplot.color) +
        scale_shape_manual(values = c(1,16)) +
        geom_hline(yintercept = 0, color = 'grey20', linetype = 'longdash') +
        theme(panel.background = element_blank(),
              axis.line.y = element_line(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              axis.title = element_text(size = text.size),
              axis.ticks = element_blank(),
              legend.position = 'none') +
        labs(x = '', y = '')

s2 <- ggplot(slopes.psp) +
        geom_errorbar(aes(x = var, ymax = upr, ymin = lwr, width = 0), size = 1.5, color = plot.color) +
        geom_point(aes(y = slope, x = var, group = factor(sig), shape = factor(sig)), size = 5, color = plot.color) +
        scale_shape_manual(values = c(1,16)) +
        geom_hline(yintercept = 0, color = 'grey20', linetype = 'longdash') +
        theme(panel.background = element_blank(),
              axis.line.y = element_line(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = text.size),
              axis.title = element_text(size = text.size),
              axis.ticks = element_blank(),
              legend.position = 'none') +
        labs(x = '', y = '')


figure(
        ggarrange(s1, s2,
                  nrow = 2,
                  align = 'hv'),
        path = 'c:/users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Figures/',
        filename = 'Nfix_traits_nonfix_sidepanel',
        height = 3,
        width = 7.3,
        save = F
)





# ~~~~ Supplemental Figure ------------------------------------------------


# Start up ----------------------------------------------------------------


bad.plots.25 <- c(33, 34, 37, 39, 49, 53, 59, 69, 71, 58, 79)

# SP data

df <- read.csv('SP_dynamics_nobin.csv') %>%
        filter(PSP %!in% bad.plots.25) %>% 
        group_by(PSP, SP) %>%
        summarise(RGR = mean(rgr.sp, na.rm = T), M = mean(M.r, na.rm = T)*100, pf = mean(pf.BA.sp, na.rm = T)) %>%
        mutate(plot = paste0(PSP,'.',SP)) %>% 
        na.exclude()

WD <- read.csv('VMFR_corrected_AGB_diverse.csv') %>%
        filter(PSP %!in% bad.plots.25) %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Su.Pl.No) %>%
        summarise(WD = mean(Density, na.rm = T)) %>%
        mutate(plot = paste0(PSP,'.',Su.Pl.No))

wd <- rep(0, times = nrow(df))

wd[df$plot %in% WD$plot] <- WD$WD[WD$plot %in% df$plot]
wd[wd==0] <- NA
df$WD <- wd


# PSP data 


df.psp <- read.csv('PSP_dynamics_no_bin.csv') %>%
        filter(PSP %!in% bad.plots.25) %>% 
        group_by(PSP) %>%
        summarise(RGR = mean(rgr, na.rm = T), M = mean(M.r, na.rm = T)*100, pf = mean(pf.BA.psp, na.rm = T)) %>%
        na.exclude()



WD.psp <- read.csv('VMFR_corrected_AGB_diverse.csv') %>%
        filter(PSP %!in% bad.plots.25) %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP) %>%
        summarise(WD = mean(Density, na.rm = T))

wd.psp <- rep(0, times = nrow(df.psp))

wd.psp[df.psp$PSP %in% WD.psp$PSP] <- WD.psp$WD[WD.psp$PSP %in% df.psp$PSP]
wd.psp[wd.psp==0] <- NA
df.psp$WD <- wd.psp



## Fixer NonFixer ##


df2 <- read.csv('Fixer_Nonfixer_dynamics_SP.csv') %>%
        filter(PSP %!in% bad.plots.25) %>% 
        group_by(PSP, SP, Fixer) %>%
        summarise(RGR = mean(rgr, na.rm = T),
                  M = mean(M.r, na.rm = T)*100,
                  pf = mean(pf.sp.BA.total, na.rm = T),
                  SR = mean(SR.sp, na.rm = T),
                  Nf = mean(Nf.sp.total, na.rm = T),
                  N = mean(N.sp.total, na.rm = T)) %>%
        mutate(plot = paste0(PSP,'.',SP)) %>%
        mutate(Nnf = N-Nf) %>%
        ungroup()

df2.psp <- read.csv('Fixer_Nonfixer_dynamics_nobin.csv') %>%
        filter(PSP %!in% bad.plots.25) %>% 
        group_by(PSP, Fixer) %>%
        summarise(RGR = mean(rgr, na.rm = T),
                  M = mean(M.r, na.rm = T)*100,
                  pf = mean(pf.BA.total, na.rm = T),
                  SR = mean(SR, na.rm = T),
                  Nf = mean(Nf.total, na.rm = T),
                  N = mean(N.total, na.rm = T)) %>%
        mutate(Nnf = N-Nf) %>%
        ungroup()


WD2 <- read.csv('VMFR_corrected_AGB_diverse.csv') %>%
        filter(PSP %!in% bad.plots.25) %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Su.Pl.No, Fixer) %>%
        summarise(WD = mean(Density, na.rm = T)) %>%
        mutate(plot = paste0(PSP,'.',Su.Pl.No))
WD2 <- WD2 %>% filter(plot %in% df2$plot)

WD2.psp <- read.csv('VMFR_corrected_AGB_diverse.csv') %>%
        filter(PSP %!in% bad.plots.25) %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Fixer) %>%
        summarise(WD = mean(Density, na.rm = T))

WD2.psp <- WD2.psp %>% filter(PSP %in% df2.psp$PSP)


df2.f <- df2 %>% filter(Fixer %in% 'fixer')
df2.nf <- df2 %>% filter(Fixer %in% 'nonfixer')
WD2.f <- WD2 %>% filter(Fixer %in% 1)
WD2.nf <- WD2 %>% filter(Fixer %in% 0)



df2.f.psp <- df2.psp %>% filter(Fixer %in% 'fixer')
df2.nf.psp <- df2.psp %>% filter(Fixer %in% 'nonfixer')
WD2.f.psp <- WD2.psp %>% filter(Fixer %in% 1)
WD2.nf.psp <- WD2.psp %>% filter(Fixer %in% 0)




wd.f <- wd.nf <- rep(0, times = nrow(df2.f))
wd.f.psp <- wd.nf.psp <- rep(0, times = nrow(df2.f.psp))



wd.f[df2.f$plot %in% WD2.f$plot] <- WD2.f$WD[WD2.f$plot %in% df2.f$plot]
wd.f[wd.f==0] <- NA

wd.f.psp[df2.f.psp$PSP %in% WD2.f.psp$PSP] <- WD2.f.psp$WD[WD2.f.psp$PSP %in% df2.f.psp$PSP]
wd.f.psp[wd.f.psp==0] <- NA



wd.nf[df2.nf$plot %in% WD2.nf$plot] <- WD2.nf$WD[WD2.nf$plot %in% df2.nf$plot]
wd.nf[wd.nf==0] <- NA

wd.nf.psp[df2.nf.psp$PSP %in% WD2.nf.psp$PSP] <- WD2.nf.psp$WD[WD2.nf.psp$PSP %in% df2.nf.psp$PSP]
wd.nf.psp[wd.nf.psp==0] <- NA



df2.f$WD <- wd.f
df2.nf$WD <- wd.nf

df2.f.psp$WD <- wd.f.psp
df2.nf.psp$WD <- wd.nf.psp



df2 <- rbind(df2.f, df2.nf)  %>%
        na.exclude

df2.psp <- rbind(df2.f.psp, df2.nf.psp)  %>%
        na.exclude



div <- read.csv('VMFR_corrected_AGB_diverse.csv') %>%
        filter(PSP %!in% bad.plots.25) %>%
        filter(Fixer == 1) %>%
        mutate(id = paste0(PSP,'.',Su.Pl.No)) %>%
        dplyr::select(id, Species) %>%
        cbind(abund = rep(1)) %>% 
        matrify %>%
        diversity()

div.psp <- read.csv('VMFR_corrected_AGB_diverse.csv') %>%
        filter(PSP %!in% bad.plots.25) %>%
        filter(Fixer == 1) %>%
       dplyr::select(PSP, Species) %>%
        cbind(abund = rep(1)) %>% 
        matrify %>%
        diversity()



nf <- df2 %>% filter(Fixer == 'fixer')
nf$H <- div[names(div) %in% nf$plot]
nf <- nf %>% mutate(E = H/log(3)) %>% # max species is 4
        mutate(pf.o = pf) %>%
        mutate(pf = range01(pf.o)) %>%
        mutate(SR = range01(SR))

nf.psp <- df2.psp %>% filter(Fixer == 'fixer')
nf.psp$H <- div.psp[names(div.psp) %in% nf.psp$PSP]
nf.psp <- nf.psp %>% mutate(E = H/log(5)) %>% # max species is 48
        mutate(pf.o = pf) %>%
        mutate(pf = range01(pf.o)) %>%
        mutate(SR = range01(SR))


fixer <- read.csv('VMFR_corrected_BA.csv') %>%
        filter(PSP %!in% bad.plots.25) %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Year, Su.Pl.No) %>%
        mutate(N = length(BA)) %>%
        mutate(nf = sum(Fixer)) %>%
        mutate(pf = nf/N) %>%
        group_by(PSP, Su.Pl.No, Fixer) %>%
        summarise(BA.fix = mean(BA), pf = mean(pf)) %>%
        filter(Fixer == 1) %>%
        mutate(ID = paste0(PSP, '.',Su.Pl.No))

nonfixer <- read.csv('VMFR_corrected_BA.csv') %>%
        filter(PSP %!in% bad.plots.25) %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Year, Su.Pl.No, Fixer) %>%
        mutate(N = length(BA)) %>%
        group_by(PSP, Su.Pl.No, Fixer) %>%
        summarise(BA.nf = mean(BA), N.nf = mean(N)) %>%
        filter(Fixer == 0) %>%
        mutate(ID = paste0(PSP, '.',Su.Pl.No))

nonfixer <- nonfixer %>% filter(ID %in% fixer$ID)




fixer.psp <- read.csv('VMFR_corrected_BA.csv') %>%
        filter(PSP %!in% bad.plots.25) %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Year) %>%
        mutate(N = length(BA)) %>%
        mutate(nf = sum(Fixer)) %>%
        mutate(pf = nf/N) %>%
        group_by(PSP, Fixer) %>%
        summarise(BA.fix = mean(BA), pf = mean(pf)) %>%
        filter(Fixer == 1)

nonfixer.psp <- read.csv('VMFR_corrected_BA.csv') %>%
        filter(PSP %!in% bad.plots.25) %>%
        gather(key = 'Year', value = 'BA', X1983:X2013, na.rm = T) %>%
        group_by(PSP, Year, Fixer) %>%
        mutate(N = length(BA)) %>%
        group_by(PSP, Fixer) %>%
        summarise(BA.nf = mean(BA), N.nf = mean(N)) %>%
        filter(Fixer == 0)

nonfixer.psp <- nonfixer.psp %>% filter(PSP %in% fixer.psp$PSP)







# Height


H.data <- read.csv('Plot5_18_30_41_Height_data.csv')
names(H.data) <- c('PSP', 'SP', 'Genus', 'Species', 'Height', 'Rho', 'Bin', 'DBH')
fixers.genus <- c("Pentaclethra", 'Clathrotropis',"Swartzia", "Inga", "Lonchocarpus")
H.data$Fixer <- H.data$Genus %in% fixers.genus %>% as.numeric
H.data <- H.data %>% filter(Height<45)



H.df <- H.data %>% 
        group_by(PSP, SP) %>%
        mutate(N = length(SP)) %>%
        mutate(pf = sum(Fixer, na.rm = T)/N) %>%
        filter(Fixer == 1) %>% 
        summarise(pf = mean(pf, na.rm = T),
                  H = gm.mean(Height, na.rm = T),
                  H.max = max(Height, na.rm = T),
                  H.95 = quantile(Height, 0.95))%>%
        mutate(pf = range01(pf))

H.df.psp <- H.data %>% 
        group_by(PSP) %>%
        mutate(N = length(PSP)) %>%
        mutate(pf = sum(Fixer, na.rm = T)/N) %>%
        filter(Fixer == 1) %>% 
        summarise(pf = mean(pf, na.rm = T),
                  H = gm.mean(Height, na.rm = T),
                  H.max = max(Height, na.rm = T),
                  H.95 = quantile(Height, 0.95)) %>%
        mutate(pf = range01(pf))




slopes <- data.frame(var = c('a)', 'b)', 'c)', 'd)', 'e)', 'f)'),
                     slope = rep(0),
                     upr = rep(0),
                     lwr = rep(0),
                     sig = rep(0))

slopes.psp <- data.frame(var = c('a)', 'b)', 'c)', 'd )', 'e)', 'f)'),
                         slope = rep(0),
                         upr = rep(0),
                         lwr = rep(0),
                         sig = rep(0))




# Graphics parameters -----------------------------------------------------

plot.size <- 2
plot.stroke <- 1.2
plot.color <- 'red'
plot.shape <- 0
plot.alpha <- 0.6

subplot.size <- 2
subplot.stroke <- 1
subplot.color <- 'red4'
subplot.shape <- 16
subplot.alpha <- 0.1

text.size <- 18






# SR 3 ----------------------------------------------------------------------

### ~~~~ Slope effect size plot ~~~~ ###

# SUBPLOT - significant slope

mod.s <- summary(lmer(scale(SR) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
slopes$slope[1] <- mod.s$coefficients[2,1]

CI <- confint(lmer(scale(SR) ~ pf + (1|PSP), data = nf))
slopes$upr[1] <- CI[4,2]
slopes$lwr[1] <- CI[4,1]

slopes$sig[1] <- 1

# PLOT - n.s. slope

mod.s <- summary(lm(scale(SR) ~ pf, data = nf.psp)) # standardized slope: -0.56
slopes.psp$slope[1] <- mod.s$coefficients[2,1]

CI <- confint(lm(scale(SR) ~ pf, data = nf.psp))
slopes.psp$upr[1] <- CI[2,2]
slopes.psp$lwr[1] <- CI[2,1]

slopes.psp$sig[1] <- 0




### ~~~~ Mixed effects model & summary ~~~~ ###

# SUBPLOT

mod <- lmer(SR ~ pf +  (1|PSP), data = nf)
mod %>% summary

ci.list3 <- ci.bands(nf$pf, nf$SR, mod)

# PLOT

mod <- lm(SR ~ pf, data = nf.psp)
mod %>% summary

ci.list3.psp <- ci.bands(nf.psp$pf, nf.psp$SR, mod)



### ~~~~ Plotting ~~~~ ###


SR.nf.plot <- ggplot() +
        geom_point(data = nf, aes(x = pf, y = SR),
                   size = subplot.size, shape = subplot.shape, alpha = subplot.alpha, color = subplot.color) +
        geom_point(data = nf.psp , aes(x = pf, y = SR),
                   size = plot.size, shape = plot.shape, alpha = plot.alpha, color = plot.color, stroke = plot.stroke) +
        
        geom_ribbon(data = nf.psp, aes(x = ci.list3.psp$ci.df$x, ymin = ci.list3.psp$ci.df$lwr, ymax = ci.list3.psp$ci.df$upr),
                    alpha = 0.4, fill = 'white') +
        annotate('segment',
                 x = ci.list3.psp$x[1], xend = ci.list3.psp$x[2],
                 y = ci.list3.psp$y[1], yend = ci.list3.psp$y[2],
                 size = 1.5, color = plot.color, alpha = 1, linetype = 'longdash') +
        geom_ribbon(data = nf.psp, aes(x = ci.list3.psp$ci.df$x, ymin = ci.list3.psp$ci.df$lwr, ymax = ci.list3.psp$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = plot.color) +
        
        annotate('segment',
                 x = ci.list3$x[1], xend = ci.list3$x[2],
                 y = ci.list3$y[1], yend = ci.list3$y[2],
                 size = 1.5, color = subplot.color, alpha = 1) +
        geom_ribbon(data = nf, aes(x = ci.list3$ci.df$x, ymin = ci.list3$ci.df$lwr, ymax = ci.list3$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = subplot.color) +
        
        
        scale_x_continuous(breaks = c(0.05, 0.5, 0.95), labels = c('Low', 'Medium', 'High'), limits = c(0,1)) +
        scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('low', 'med', 'high'), limits = c(0, 1.2)) +
        
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text.x = element_text(size = text.size),
              axis.text.y = element_text(size = 14),
              axis.title = element_text(size = text.size)) +
        labs(x = '', y = bquote('Species Richness (#)'))




# Evenness 4 ----------------------------------------------------------------

### ~~~~ Slope effect size plot ~~~~ ###

# SUBPLOT 

mod.s <- summary(lmer(scale(E) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
slopes$slope[2] <- mod.s$coefficients[2,1]

CI <- confint(lmer(scale(E) ~ pf + (1|PSP), data = nf))
slopes$upr[2] <- CI[4,2]
slopes$lwr[2] <- CI[4,1]

slopes$sig[2] <- 0


# PLOT

mod.s <- summary(lm(scale(E) ~ pf, data = nf.psp)) # standardized slope: -0.56
slopes.psp$slope[2] <- mod.s$coefficients[2,1]

CI <- confint(lm(scale(E) ~ pf, data = nf.psp))
slopes.psp$upr[2] <- CI[2,2]
slopes.psp$lwr[2] <- CI[2,1]

slopes.psp$sig[2] <- 0




### ~~~~ Mixed effects model & summary ~~~~ ###

# SUBPLOT

mod <- lmer(E ~ pf +  (1|PSP), data = nf)
mod %>% summary

ci.list4 <- ci.bands(nf$pf, nf$E, mod)

# PLOT

mod <- lm(E ~ pf, data = nf.psp)
mod %>% summary

ci.list4.psp <- ci.bands(nf.psp$pf, nf.psp$E, mod)



### ~~~~ Plotting ~~~~ ###


E.nf.plot <- ggplot() +
        geom_jitter(data = nf, aes(x = pf, y = E),
                    size = subplot.size, shape = subplot.shape, alpha = subplot.alpha, color = subplot.color, height = 0.1) +
        geom_jitter(data = nf.psp, aes(x = pf, y = E),
                    size = plot.size, shape = plot.shape, alpha = plot.alpha, color = plot.color, height = 0.1, stroke = plot.stroke) +
        
        geom_ribbon(data = nf.psp, aes(x = ci.list4.psp$ci.df$x, ymin = ci.list4.psp$ci.df$lwr, ymax = ci.list4.psp$ci.df$upr),
                    alpha = 0.4, fill = 'white') +
        annotate('segment',
                 x = ci.list4.psp$x[1], xend = ci.list4.psp$x[2],
                 y = ci.list4.psp$y[1], yend = ci.list4.psp$y[2],
                 size = 1.5, color = plot.color, alpha = 1, linetype = 'longdash') +
        geom_ribbon(data = nf.psp, aes(x = ci.list4.psp$ci.df$x, ymin = ci.list4.psp$ci.df$lwr, ymax = ci.list4.psp$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = plot.color) +
        
        annotate('segment',
                 x = ci.list4$x[1], xend = ci.list4$x[2],
                 y = ci.list4$y[1], yend = ci.list4$y[2],
                 size = 1.5, color = subplot.color, alpha = 1, linetype = 'longdash') +
        geom_ribbon(data = nf, aes(x = ci.list4$ci.df$x, ymin = ci.list4$ci.df$lwr, ymax = ci.list4$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = subplot.color) +
        
        scale_x_continuous(breaks = c(0.05, 0.5, 0.95), labels = c('Low', 'Medium', 'High'), limits = c(0,1)) +
        scale_y_continuous(breaks = c(0, 0.5, 1), labels = c('low', 'med', 'high')) +
        
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text.x = element_text(size = text.size),
              axis.text.y = element_text(size = 14),
              axis.title = element_text(size = text.size)) +
        labs(x = '', y = bquote('Species Evenness'))





# WD 5 ----------------------------------------------------------------

### ~~~~ Slope effect size plot ~~~~ ###

# SUBPLOT

mod.s <- summary(lmer(scale(WD) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
slopes$slope[3] <- mod.s$coefficients[2,1]

CI <- confint(lmer(scale(WD) ~ pf + (1|PSP), data = nf))
slopes$upr[3] <- CI[4,2]
slopes$lwr[3] <- CI[4,1]

slopes$sig[3] <- 0

# PLOT

mod.s <- summary(lm(scale(WD) ~ pf, data = nf.psp)) # standardized slope: -0.56
slopes.psp$slope[3] <- mod.s$coefficients[2,1]

CI <- confint(lm(scale(WD) ~ pf, data = nf.psp))
slopes.psp$upr[3] <- CI[2,2]
slopes.psp$lwr[3] <- CI[2,1]

slopes.psp$sig[3] <- 0


### ~~~~ Mixed effects model & summary ~~~~ ###

# SUBPLOT

mod <- lmer(WD ~ pf +  (1|PSP), data = nf)
mod %>% summary # -0.05

ci.list5 <- ci.bands(nf$pf, nf$WD, mod)

# PLOT

mod <- lm(WD ~ pf, data = nf.psp)
mod %>% summary # -0.05

ci.list5.psp <- ci.bands(nf.psp$pf, nf.psp$WD, mod)



### ~~~~ Plotting ~~~~ ###


WD.nf.plot <- ggplot(nf) +
        geom_point(data = nf, aes(x = pf, y = WD),
                   size = subplot.size, shape = subplot.shape, alpha = subplot.alpha, color = subplot.color) +
        geom_point(data = nf.psp , aes(x = pf, y = WD),
                   size = plot.size, shape = plot.shape, alpha = plot.alpha, color = plot.color, stroke = plot.stroke) +
        
        geom_ribbon(data = nf.psp, aes(x = ci.list5.psp$ci.df$x, ymin = ci.list5.psp$ci.df$lwr, ymax = ci.list5.psp$ci.df$upr),
                    alpha = 0.4, fill = 'white') +
        annotate('segment',
                 x = ci.list5.psp$x[1], xend = ci.list5.psp$x[2],
                 y = ci.list5.psp$y[1], yend = ci.list5.psp$y[2],
                 size = 1.5, color = plot.color, alpha = 1, linetype = 'longdash') +
        geom_ribbon(data = nf.psp, aes(x = ci.list5.psp$ci.df$x, ymin = ci.list5.psp$ci.df$lwr, ymax = ci.list5.psp$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = plot.color) +
        
        annotate('segment',
                 x = ci.list5$x[1], xend = ci.list5$x[2],
                 y = ci.list5$y[1], yend = ci.list5$y[2],
                 size = 1.5, color = subplot.color, alpha = 1, linetype = 'longdash') +
        geom_ribbon(data = nf, aes(x = ci.list5$ci.df$x, ymin = ci.list5$ci.df$lwr, ymax = ci.list5$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = subplot.color) +
        
        
        scale_x_continuous(breaks = c(0.05, 0.5, 0.95), labels = c('Low', 'Medium', 'High'), limits = c(0,1)) +
        scale_y_continuous(limits = c(0.4, 0.95), breaks = c(0.45, 0.65, 0.85)) +
        
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text = element_text(size = text.size),
              axis.title = element_text(size = text.size)) +
        labs(x = '', y = bquote('Wood Density (g'~cm^-3*')'))




# RGR 6 ---------------------------------------------------------------------

### ~~~~ Slope effect size plot ~~~~ ###

# SUBPLOT

mod.s <- summary(lmer(scale(RGR) ~ pf + (1|PSP), data = nf)) # standardized slope: 0.56
slopes$slope[4] <- mod.s$coefficients[2,1]

CI <- confint(lmer(scale(RGR) ~ pf + (1|PSP), data = nf))
slopes$upr[4] <- CI[4,2]
slopes$lwr[4] <- CI[4,1]

slopes$sig[4] <- 1

# PLOT

mod.s <- summary(lm(scale(RGR) ~ pf, data = nf.psp)) # standardized slope: 0.56
slopes.psp$slope[4] <- mod.s$coefficients[2,1]

CI <- confint(lm(scale(RGR) ~ pf, data = nf.psp))
slopes.psp$upr[4] <- CI[2,2]
slopes.psp$lwr[4] <- CI[2,1]

slopes.psp$sig[4] <- 0


### ~~~~ Mixed effects model & summary ~~~~ ###

# SUBPLOT

mod <- lmer(RGR ~ pf + (1|PSP), data = nf)
mod %>% summary # 0.51

ci.list6 <- ci.bands(nf$pf, nf$RGR, mod)


# PLOT

mod <- lm(RGR ~ pf, data = nf.psp)
mod %>% summary # 0.51


ci.list6.psp <- ci.bands(nf.psp$pf, nf.psp$RGR, mod)



### ~~~~ Plotting ~~~~ ###


RGR.nf.plot <- ggplot() +
        geom_point(data = nf, aes(x = pf, y = RGR),
                   size = subplot.size, shape = subplot.shape, alpha = subplot.alpha, color = subplot.color) +
        geom_point(data = nf.psp , aes(x = pf, y = RGR),
                   size = plot.size, shape = plot.shape, alpha = plot.alpha, color = plot.color, stroke = plot.stroke) +
        
        geom_ribbon(data = nf.psp, aes(x = ci.list6.psp$ci.df$x, ymin = ci.list6.psp$ci.df$lwr, ymax = ci.list6.psp$ci.df$upr),
                    alpha = 0.4, fill = 'white') +
        annotate('segment',
                 x = ci.list6.psp$x[1], xend = ci.list6.psp$x[2],
                 y = ci.list6.psp$y[1], yend = ci.list6.psp$y[2],
                 size = 1.5, color = plot.color, alpha = 1, linetype = 'longdash') +
        geom_ribbon(data = nf.psp, aes(x = ci.list6.psp$ci.df$x, ymin = ci.list6.psp$ci.df$lwr, ymax = ci.list6.psp$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = plot.color) +
        
        annotate('segment',
                 x = ci.list6$x[1], xend = ci.list6$x[2],
                 y = ci.list6$y[1], yend = ci.list6$y[2],
                 size = 1.5, color = subplot.color, alpha = 0.7) +
        geom_ribbon(data = nf, aes(x = ci.list6$ci.df$x, ymin = ci.list6$ci.df$lwr, ymax = ci.list6$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = subplot.color) +
        
        scale_x_continuous(breaks = c(0.05, 0.5, 0.95), labels = c('Low', 'Medium', 'High'), limits = c(0,1)) +
        scale_y_continuous(limits = c(0, 3.5), breaks = c(0, 1, 2, 3)) +
        
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text = element_text(size = text.size),
              axis.title = element_text(size = text.size)) +
        labs(x = '', y = bquote('RGR (% '~yr^-1*')'))



# Height 7 ------------------------------------------------------------------

### ~~~~ Slope effect size plot ~~~~ ###

# SUBPLOT

mod.s <- summary(lmer(scale(H) ~ pf + (1|PSP), data = H.df)) # standardized slope: -0.56
slopes$slope[5] <- mod.s$coefficients[2,1]

CI <- confint(lmer(scale(H) ~ pf + (1|PSP), data = H.df))
slopes$upr[5] <- CI[4,2]
slopes$lwr[5] <- CI[4,1]

slopes$sig[5] <- 0

# PLOT

mod.s <- summary(lm(scale(H) ~ pf, data = H.df.psp)) # standardized slope: -0.56
slopes.psp$slope[5] <- mod.s$coefficients[2,1]

CI <- confint(lm(scale(H) ~ pf, data = H.df.psp))
slopes.psp$upr[5] <- NA
slopes.psp$lwr[5] <- NA

slopes.psp$sig[5] <- 0


### ~~~~ Mixed effects model & summary ~~~~ ###

# SUBPLOT

mod <- lmer(H ~ pf + (1|PSP), data = H.df)
summary(mod) #  sig

ci.list7 <- ci.bands(H.df$pf, H.df$H, mod)


# PLOT

mod <- lm(H ~ pf, data = H.df.psp)
summary(mod) #  sig

ci.list7.psp <- ci.bands(H.df.psp$pf, H.df.psp$H, mod)

h.nf.plot <- ggplot() +
        geom_point(data = H.df, aes(x = pf, y = H),
                   size = subplot.size, shape = subplot.shape, alpha = 0.3, color = subplot.color) +
        geom_point(data = H.df.psp , aes(x = pf, y = H),
                   size = plot.size, shape = plot.shape, alpha = 0.8, color = plot.color, stroke = plot.stroke) +
        
        geom_ribbon(data = H.df.psp, aes(x = ci.list7.psp$ci.df$x, ymin = ci.list7.psp$ci.df$lwr, ymax = ci.list7.psp$ci.df$upr),
                    alpha = 0.4, fill = 'white') +
        annotate('segment',
                 x = ci.list7.psp$x[1], xend = ci.list7.psp$x[2],
                 y = ci.list7.psp$y[1], yend = ci.list7.psp$y[2],
                 size = 1.5, color = plot.color, alpha = 0.7, linetype = 'longdash') +
        geom_ribbon(data = H.df.psp, aes(x = ci.list7.psp$ci.df$x, ymin = ci.list7.psp$ci.df$lwr, ymax = ci.list7.psp$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = plot.color) +
        
        annotate('segment',
                 x = ci.list7$x[1], xend = ci.list7$x[2],
                 y = ci.list7$y[1], yend = ci.list7$y[2],
                 size = 1.5, color = subplot.color, alpha = 0.7, linetype = 'longdash') +
        geom_ribbon(data = H.df, aes(x = ci.list7$ci.df$x, ymin = ci.list7$ci.df$lwr, ymax = ci.list7$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = subplot.color) +
        
        
        scale_x_continuous(breaks = c(0.05, 0.5, 0.95), labels = c('Low', 'Medium', 'High'), limits = c(0,1)) +
        scale_y_continuous(breaks = c(10, 20)) +
        
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text = element_text(size = text.size),
              axis.title = element_text(size = text.size)) +
        labs(x = '', y = bquote('Mean Height (m)'))



# Mort 8 --------------------------------------------------------------------

### ~~~~ Slope effect size plot ~~~~ ###

# SUBPLOT

mod.s <- summary(lmer(scale(M) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
slopes$slope[6] <- mod.s$coefficients[2,1]

CI <- confint(lmer(scale(M) ~ pf + (1|PSP), data = nf))
slopes$upr[6] <- CI[4,2]
slopes$lwr[6] <- CI[4,1]

slopes$sig[6] <- 1

# PLOT

mod.s <- summary(lm(scale(M) ~ pf, data = nf.psp)) # standardized slope: -0.56

slopes.psp$slope[6] <- mod.s$coefficients[2,1]

CI <- confint(lm(scale(M) ~ pf, data = nf.psp))
slopes.psp$upr[6] <- CI[2,2]
slopes.psp$lwr[6] <- CI[2,1]

slopes.psp$sig[6] <- 0




### ~~~~ Mixed effects model & summary ~~~~ ###

# SUBPLOT

mod <- lmer(M ~ pf + (1|PSP), data = nf)
summary(mod)

ci.list8 <- ci.bands(nf$pf, nf$M, mod)

# PLOT

mod <- lm(M ~ pf, data = nf.psp)
summary(mod)

ci.list8.psp <- ci.bands(nf.psp$pf, nf.psp$M, mod)



### ~~~~ Plotting ~~~~ ###


M.nf.plot <- ggplot() +
        geom_point(data = nf, aes(x = pf, y = M),
                   size = subplot.size, shape = subplot.shape, alpha = subplot.alpha, color = subplot.color) +
        geom_point(data = nf.psp , aes(x = pf, y = M),
                   size = plot.size, shape = plot.shape, alpha = plot.alpha, color = plot.color, stroke = plot.stroke) +
        
        geom_ribbon(data = nf.psp, aes(x = ci.list8.psp$ci.df$x, ymin = ci.list8.psp$ci.df$lwr, ymax = ci.list8.psp$ci.df$upr),
                    alpha = 0.4, fill = 'white') +
        annotate('segment',
                 x = ci.list8.psp$x[1], xend = ci.list8.psp$x[2],
                 y = ci.list8.psp$y[1], yend = ci.list8.psp$y[2],
                 size = 1.5, color = plot.color, alpha = 1, linetype = 'longdash') +
        geom_ribbon(data = nf.psp, aes(x = ci.list8.psp$ci.df$x, ymin = ci.list8.psp$ci.df$lwr, ymax = ci.list8.psp$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = plot.color) +
        
        annotate('segment',
                 x = ci.list8$x[1], xend = ci.list8$x[2],
                 y = ci.list8$y[1], yend = ci.list8$y[2],
                 size = 1.5, color = subplot.color, alpha = 1) +
        geom_ribbon(data = nf, aes(x = ci.list8$ci.df$x, ymin = ci.list8$ci.df$lwr, ymax = ci.list8$ci.df$upr),
                    alpha = 0.3, fill = 'grey30', color = subplot.color) +
        
        scale_x_continuous(breaks = c(0.05, 0.5, 0.95), labels = c('Low', 'Medium', 'High'), limits = c(0,1)) +
        scale_y_continuous(limits = c(NA, 7.5)) +
        
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line = element_line(),
              axis.text = element_text(size = text.size),
              axis.title = element_text(size = text.size)) +
        labs(x = '', y = bquote('Mortality (% '~yr^-1*')'))



# Plotting ----------------------------------------------------------------


# individual level

figure(
        ggarrange(SR.nf.plot,
                  E.nf.plot,
                  WD.nf.plot,
                  RGR.nf.plot,
                  h.nf.plot,
                  M.nf.plot,
                  nrow = 3, ncol = 2, 
                  align = 'hv'),
        path = 'c:/users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Figures/',
        filename = 'Nfix_traits_fix',
        height = 9,
        width = 8,
        save = T
)




slopes <- slopes %>%
        arrange(var) %>%
        mutate(var = factor(var, levels = var))

slopes.psp <- slopes.psp %>%
        arrange(var) %>%
        mutate(var = factor(var, levels = var))



s1 <- ggplot(slopes) +
        geom_errorbar(aes(x = var, ymax = upr, ymin = lwr, width = 0), size = 1.5, color = subplot.color) +
        geom_point(aes(y = slope, x = var, group = factor(sig), shape = factor(sig)), size = 5, color = subplot.color) +
        scale_shape_manual(values = c(1,16)) +
        geom_hline(yintercept = 0, color = 'grey20', linetype = 'longdash') +
        theme(panel.background = element_blank(),
              axis.line.y = element_line(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              axis.title = element_text(size = text.size),
              axis.ticks = element_blank(),
              legend.position = 'none') +
        labs(x = '', y = '')

s2 <- ggplot(slopes.psp) +
        geom_errorbar(aes(x = var, ymax = upr, ymin = lwr, width = 0), size = 1.5, color = plot.color) +
        geom_point(aes(y = slope, x = var, group = factor(sig), shape = factor(sig)), size = 5, color = plot.color) +
        scale_shape_manual(values = c(1,16)) +
        geom_hline(yintercept = 0, color = 'grey20', linetype = 'longdash') +
        theme(panel.background = element_blank(),
              axis.line.y = element_line(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = text.size),
              axis.title = element_text(size = text.size),
              axis.ticks = element_blank(),
              legend.position = 'none') +
        labs(x = '', y = '')


figure(
        ggarrange(s1, s2,
                  nrow = 2,
                  align = 'hv'),
        path = 'c:/users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Figures/',
        filename = 'Nfix_traits_fix_sidepanel',
        height = 3,
        width = 7.3,
        save = T
)


