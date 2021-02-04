source('~/R/startup.R')
#C:/Users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Data/
source('~/R/Loess_Density_Correction.R')
source('~/R/Range01.R')

# ~~~~ Figure 2 ~~~~ ------------------------------------------------------


# Data --------------------------------------------------------------------


P.data <- read.csv('PSP_dynamics_no_bin.csv') %>% 
     mutate(P = (AGB.delta + R)/1000) %>%
     filter(!is.na(P)) %>%
     mutate(time = Year - 1982) %>%
     filter(P < mean(P) + 2*sd(P) & P > mean(P) - 2*sd(P)) %>%
     mutate(harvest = as.factor(harvest))

P.sp <- read.csv('SP_dynamics_nobin.csv') %>% 
     mutate(P = (AGB.delta + R)/1000) %>%
     filter(!is.na(P)) %>%
     mutate(time = Year - 1982) %>%
     filter(P < mean(P) + 2*sd(P) & P > mean(P) - 2*sd(P)) %>%
     mutate(unique.id = paste0(PSP,'.',SP)) %>%
     mutate(harvest = as.factor(harvest))


M.data <- read.csv('PSP_dynamics_no_bin.csv') %>% 
     mutate(M = M/1000 * -1) %>%
     filter(!is.na(M)) %>%
     mutate(time = Year - 1982) %>%
     filter(M < mean(M) + 2*sd(M) & M > mean(M) - 2*sd(M)) %>%
     mutate(harvest = as.factor(harvest))

M.sp <- read.csv('SP_dynamics_nobin.csv') %>% 
     mutate(M = M/1000 * -1) %>%
     filter(!is.na(M)) %>%
     mutate(time = Year - 1982) %>%
     filter(M < mean(M) + 2*sd(M) & M > mean(M) - 2*sd(M)) %>%
     mutate(unique.id = paste0(PSP,'.',SP))%>%
     mutate(harvest = as.factor(harvest))


I.data <- read.csv('PSP_dynamics_no_bin.csv') %>% 
     mutate(I = (AGB.delta + R + M)/1000) %>%
     filter(!is.na(I)) %>%
     filter(I < mean(I) + 2*sd(I) & I > mean(I) - 2*sd(I)) %>%
     mutate(Is = scale(I))

I.sp <- read.csv('SP_dynamics_nobin.csv') %>% 
     mutate(I = (AGB.delta + R + M)/1000) %>%
     filter(!is.na(I)) %>%
     filter(I < mean(I) + 2*sd(I) & I > mean(I) - 2*sd(I)) %>%
     mutate(Is = scale(I)*25) %>%
     mutate(unique.id = paste0(PSP,'.',SP))




# Dynamics Models ------------------------------------------------------------------

# Takeaway:
# Results don't change between harvested/non-harvested plots. 
# Harvested plots do have higher productivity and mortaltiy, but no difference in incremental.


# Productivity

mod <- lmer(P ~ Year + (1|PSP), data = P.data)
mod %>% summary
mod <- lmer(P ~ Year + (1|harvest/PSP), data = P.data)
mod %>% summary
mod <- lmer(P ~ Year + harvest + (1|PSP), data = P.data)
mod %>% summary
# Year: slight but non-sig decrease ovetime
# Harvest: harvested plots slightly higher

mod <- lmer(P ~ Year + (1|PSP/SP), data = P.sp)
mod %>% summary
mod <- lmer(P ~ Year + (1|harvest/PSP/SP), data = P.sp)
mod %>% summary
mod <- lmer(P ~ Year + harvest + (1|PSP/SP), data = P.sp)
mod %>% summary
# Year: slight sig decrease ovetime
# Harvest: harvested plots slightly higher 






# Mortality

mod <- lmer(M ~ Year + (1|PSP), data = M.data)
mod %>% summary
mod <- lmer(M ~ Year + (1|harvest/PSP), data = M.data)
mod %>% summary
mod <- lmer(M ~ Year + harvest + (1|PSP), data = M.data)
mod %>% summary
# Year: slight sig increase ovetime
# Harvest: harvested plots slightly higher


mod <- lmer(M ~ Year + (1|PSP/SP), data = M.sp)
mod %>% summary
mod <- lmer(M ~ Year + (1|harvest/PSP/SP), data = M.sp)
mod %>% summary
mod <- lmer(M ~ Year + harvest + (1|PSP/SP), data = M.sp)
mod %>% summary

# Year: slight sig increase ovetime
# Harvest: harvested plots slightly higher





# Incremental

mod <- lmer(I ~ Year + (1|PSP), data = I.data)
mod %>% summary
mod <- lmer(I ~ Year + (1|harvest/PSP), data = I.data)
mod %>% summary
mod <- lmer(I ~ Year + harvest + (1|PSP), data = I.data)
mod %>% summary
# Year: sig decrease ovetime
# Harvest: no difference between plots


mod <- lmer(I ~ Year + (1|PSP/SP), data = I.sp)
mod %>% summary
mod <- lmer(I ~ Year + (1|harvest/PSP/SP), data = I.sp)
mod %>% summary
mod <- lmer(I ~ Year + harvest + (1|PSP), data = I.data)
mod %>% summary
# Year: sig decrease ovetime
# Harvest: no difference between plots



# ~~~~ Figure 2 Supp ~~~~ -----------------------------------------------------------

# Data --------------------------------------------------------------------


AGB.data <- read.csv('../../Data/VMFR_corrected_AGB.csv') %>%
     filter(foresttype == 'diverse') %>%
     dplyr::select(PSP, Harvest, 16:37) %>% 
     dplyr::group_by(PSP) %>% 
     gather(key = Year, value = AGB, -c(1:2)) %>%
     dplyr::mutate(Year = substr(Year,2,5) %>% as.numeric) %>%
     dplyr::filter(!is.na(AGB)) %>%
     dplyr::group_by(PSP, Year) %>% 
     dplyr::summarise(AGB = sum(AGB/1000), harvest = mean(Harvest)) %>%
     filter(AGB > 50) %>%
     mutate(time = Year - 1982)

AGB.sp <- read.csv('../../Data/VMFR_corrected_AGB.csv') %>%
     filter(foresttype == 'diverse') %>%
     dplyr::select(PSP, Su.Pl.No, Harvest, 16:37) %>% 
     dplyr::group_by(PSP, Su.Pl.No) %>% 
     gather(key = Year, value = AGB, -c(1:3)) %>%
     dplyr::mutate(Year = substr(Year,2,5) %>% as.numeric) %>%
     dplyr::filter(!is.na(AGB)) %>%
     dplyr::group_by(PSP, Su.Pl.No, Year) %>% 
     dplyr::summarise(AGB = sum(AGB/1000), harvest = mean(Harvest)) %>%
     filter(AGB*25 > 50) %>%
     mutate(time = Year - 1982) %>%
     mutate(unique.id = paste0(PSP,'.',Su.Pl.No))

ba.data <- read.csv('PSP_dynamics_no_bin.csv')
ba.sp <- read.csv('SP_dynamics_nobin.csv') %>%
     mutate(unique.id = paste0(PSP,'.',SP))

d.data <- read.csv('PSP_dynamics_no_bin.csv')
d.sp <- read.csv('SP_dynamics_nobin.csv') %>%
     mutate(unique.id = paste0(PSP,'.',SP))


# Models ------------------------------------------------------------------

# Change in biomass over time (all bins) 

mod <- lmer(AGB ~ Year + (1|harvest/PSP), data = AGB.data)
mod %>% summary


mod <- lmer(AGB ~ Year + (1|harvest/PSP/Su.Pl.No), data = AGB.sp)
mod %>% summary


# Change in Basal Area over time 

mod <- lmer(BA ~ Year + (1|harvest/PSP), data = ba.data)
mod %>% summary

mod <- lmer(BA ~ Year + (1|harvest/PSP/SP), data = ba.sp)
mod %>% summary




# Change in density over time (all bins) 

mod <- lmer(N.psp ~ Year + (1|harvest/PSP), data = d.data)
mod %>% summary


mod <- lmer(N.sp ~ Year + (1|harvest/PSP/SP), data = d.sp)
mod %>% summary



# ~~~~ Figure 3 ~~~~ ------------------------------------------------------



# Data --------------------------------------------------------------------


df2 <- read.csv('Fixer_Nonfixer_dynamics_SP.csv') %>%
     group_by(PSP, SP, Fixer) %>%
     summarise(harvest = mean(harvest),
               RGR = mean(rgr, na.rm = T),
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
     summarise(harvest = mean(harvest),
               RGR = mean(rgr, na.rm = T),
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
     mutate(SR = range01(SR)) %>%
     mutate(harvest = factor(harvest))

nf.psp <- df2.psp %>% filter(Fixer == 'nonfixer')
nf.psp$H <- div.psp[names(div.psp) %in% nf.psp$PSP]
nf.psp <- nf.psp %>% mutate(E = H/log(44)) %>% # max species is 44
     mutate(pf.o = pf) %>%
     mutate(pf = range01(pf.o)) %>%
     mutate(SR = range01(SR)) %>%
     mutate(harvest = factor(harvest))



# Height


H.data <- read.csv('../../Data/Plot5_18_30_41_Height_data.csv')
names(H.data) <- c('PSP', 'SP', 'Genus', 'Species', 'Height', 'Rho', 'Bin', 'DBH')
fixers.genus <- c("Pentaclethra", 'Clathrotropis',"Swartzia", "Inga", "Lonchocarpus")
H.data$Fixer <- H.data$Genus %in% fixers.genus %>% as.numeric
H.data <- H.data %>% filter(Height<45)
H.data$harvest <- rep(0)
cut <- c(5, 18, 30)
H.data$harvest[H.data$PSP %in% cut] <- 1



H.df <- H.data %>% 
     group_by(PSP, SP) %>%
     mutate(N = length(SP)) %>%
     mutate(pf = sum(Fixer, na.rm = T)/N) %>%
     filter(Fixer == 0) %>% 
     summarise(harvest = mean(harvest),
               pf = mean(pf, na.rm = T),
               H = gm.mean(Height, na.rm = T),
               H.max = max(Height, na.rm = T),
               H.95 = quantile(Height, 0.95))%>%
     mutate(pf = range01(pf))

H.df.psp <- H.data %>% 
     group_by(PSP) %>%
     mutate(N = length(PSP)) %>%
     mutate(pf = sum(Fixer, na.rm = T)/N) %>%
     filter(Fixer == 0) %>% 
     summarise(harvest = mean(harvest),
               pf = mean(pf, na.rm = T),
               H = gm.mean(Height, na.rm = T),
               H.max = max(Height, na.rm = T),
               H.95 = quantile(Height, 0.95)) %>%
     mutate(pf = range01(pf))

# Nonfixer Trait Models ---------------------------------------------------


# Takeaway:
# Results don't change between harvested/non-harvested plots, except slightly for height at plot level. 


# Species Richness
summary(lm(scale(SR) ~ pf, data = nf.psp)) # standardized slope: -0.56
summary(lm(scale(SR) ~ pf + harvest, data = nf.psp)) # standardized slope: -0.56
summary(lmer(scale(SR) ~ pf + (1|harvest), data = nf.psp)) # standardized slope: -0.56


summary(lmer(scale(SR) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(SR) ~ pf + harvest + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(SR) ~ pf + (1|harvest/PSP), data = nf)) # standardized slope: -0.56


# Evenness
summary(lm(scale(E) ~ pf, data = nf.psp)) # standardized slope: -0.56
summary(lm(scale(E) ~ pf + harvest, data = nf.psp)) # standardized slope: -0.56
summary(lmer(scale(E) ~ pf + (1|harvest), data = nf.psp)) # standardized slope: -0.56

summary(lmer(scale(E) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(E) ~ pf + harvest + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(E) ~ pf + (1|harvest/PSP), data = nf)) # standardized slope: -0.56



# WD
summary(lm(scale(WD) ~ pf, data = nf.psp)) # standardized slope: -0.56
summary(lm(scale(WD) ~ pf + harvest, data = nf.psp)) # standardized slope: -0.56
summary(lmer(scale(WD) ~ pf + (1|harvest), data = nf.psp)) # standardized slope: -0.56


summary(lmer(scale(WD) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(WD) ~ pf + harvest + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(WD) ~ pf + (1|harvest/PSP), data = nf)) # standardized slope: -0.56


# RGR
summary(lm(scale(RGR) ~ pf, data = nf.psp)) # standardized slope: 0.56
summary(lm(scale(RGR) ~ pf + harvest, data = nf.psp)) # standardized slope: 0.56
summary(lmer(scale(RGR) ~ pf + (1|harvest), data = nf.psp)) # standardized slope: 0.56

summary(lmer(scale(RGR) ~ pf + (1|PSP), data = nf)) # standardized slope: 0.56
summary(lmer(scale(RGR) ~ pf + harvest + (1|PSP), data = nf)) # standardized slope: 0.56
summary(lmer(scale(RGR) ~ pf + (1|harvest/PSP), data = nf)) # standardized slope: 0.56



# Height
summary(lm(scale(H) ~ pf, data = H.df.psp)) # standardized slope: -0.56
summary(lm(scale(H) ~ pf + harvest, data = H.df.psp)) # standardized slope: -0.56
summary(lmer(scale(H) ~ pf + (1|harvest), data = H.df.psp)) # standardized slope: -0.56

summary(lmer(scale(H) ~ pf + (1|PSP), data = H.df)) # standardized slope: -0.56
summary(lmer(scale(H) ~ pf + harvest + (1|PSP), data = H.df)) # standardized slope: -0.56
summary(lmer(scale(H) ~ pf + (1|harvest/PSP), data = H.df)) # standardized slope: -0.56



# Mortality
summary(lm(scale(M) ~ pf, data = nf.psp)) # standardized slope: -0.56
summary(lm(scale(M) ~ pf + harvest, data = nf.psp)) # standardized slope: -0.56
summary(lmer(scale(M) ~ pf + (1|harvest), data = nf.psp)) # standardized slope: -0.56

summary(lmer(scale(M) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(M) ~ pf + harvest + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(M) ~ pf + (1|harvest/PSP), data = nf)) # standardized slope: -0.56

# ~~~~ Figure 4 ~~~~ ------------------------------------------------------



# Data --------------------------------------------------------------------


df2 <- read.csv('Fixer_Nonfixer_dynamics_SP.csv') %>%
     group_by(PSP, SP, Fixer) %>%
     summarise(harvest = mean(harvest),
               RGR = mean(rgr, na.rm = T),
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
     summarise(harvest = mean(harvest),
               RGR = mean(rgr, na.rm = T),
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
     filter(Fixer == 1) %>%
     mutate(id = paste0(PSP,'.',Su.Pl.No)) %>%
     select(id, Species) %>%
     cbind(abund = rep(1)) %>% 
     matrify %>%
     diversity()

div.psp <- read.csv('../../Data/VMFR_corrected_AGB.csv') %>%
     filter(foresttype == 'diverse') %>%
     filter(Fixer == 1) %>%
     select(PSP, Species) %>%
     cbind(abund = rep(1)) %>% 
     matrify %>%
     diversity()



nf <- df2 %>% filter(Fixer == 'fixer')
nf$H <- div[names(div) %in% nf$plot]
nf <- nf %>% mutate(E = H/log(13)) %>% # max species is 13
     mutate(pf.o = pf) %>%
     mutate(pf = range01(pf.o)) %>%
     mutate(SR = range01(SR))%>%
     mutate(harvest = factor(harvest))

nf.psp <- df2.psp %>% filter(Fixer == 'fixer')
nf.psp$H <- div.psp[names(div.psp) %in% nf.psp$PSP]
nf.psp <- nf.psp %>% mutate(E = H/log(44)) %>% # max species is 44
     mutate(pf.o = pf) %>%
     mutate(pf = range01(pf.o)) %>%
     mutate(SR = range01(SR))%>%
     mutate(harvest = factor(harvest))



# Height


H.data <- read.csv('../../Data/Plot5_18_30_41_Height_data.csv')
names(H.data) <- c('PSP', 'SP', 'Genus', 'Species', 'Height', 'Rho', 'Bin', 'DBH')
fixers.genus <- c("Pentaclethra", 'Clathrotropis',"Swartzia", "Inga", "Lonchocarpus")
H.data$Fixer <- H.data$Genus %in% fixers.genus %>% as.numeric
H.data <- H.data %>% filter(Height<45)
H.data$harvest <- rep(0)
cut <- c(5, 18, 30)
H.data$harvest[H.data$PSP %in% cut] <- 1


H.df <- H.data %>% 
     group_by(PSP, SP) %>%
     mutate(N = length(SP)) %>%
     mutate(pf = sum(Fixer, na.rm = T)/N) %>%
     filter(Fixer == 1) %>% 
     summarise(harvest = mean(harvest),
               pf = mean(pf, na.rm = T),
               H = gm.mean(Height, na.rm = T),
               H.max = max(Height, na.rm = T),
               H.95 = quantile(Height, 0.95))%>%
     mutate(pf = range01(pf))

H.df.psp <- H.data %>% 
     group_by(PSP) %>%
     mutate(N = length(PSP)) %>%
     mutate(pf = sum(Fixer, na.rm = T)/N) %>%
     filter(Fixer == 1) %>% 
     summarise(harvest = mean(harvest),
               pf = mean(pf, na.rm = T),
               H = gm.mean(Height, na.rm = T),
               H.max = max(Height, na.rm = T),
               H.95 = quantile(Height, 0.95)) %>%
     mutate(pf = range01(pf))


# Fixer Trait Models ---------------------------------------------------

# Species Richness
summary(lm(scale(SR) ~ pf, data = nf.psp)) # standardized slope: -0.56
summary(lm(scale(SR) ~ pf + harvest, data = nf.psp)) # standardized slope: -0.56
summary(lmer(scale(SR) ~ pf + (1|harvest), data = nf.psp)) # standardized slope: -0.56


summary(lmer(scale(SR) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(SR) ~ pf + harvest + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(SR) ~ pf + (1|harvest/PSP), data = nf)) # standardized slope: -0.56


# Evenness
summary(lm(scale(E) ~ pf, data = nf.psp)) # standardized slope: -0.56
summary(lm(scale(E) ~ pf + harvest, data = nf.psp)) # standardized slope: -0.56
summary(lmer(scale(E) ~ pf + (1|harvest), data = nf.psp)) # standardized slope: -0.56

summary(lmer(scale(E) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(E) ~ pf + harvest + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(E) ~ pf + (1|harvest/PSP), data = nf)) # standardized slope: -0.56



# WD
summary(lm(scale(WD) ~ pf, data = nf.psp)) # standardized slope: -0.56
summary(lm(scale(WD) ~ pf + harvest, data = nf.psp)) # standardized slope: -0.56
summary(lmer(scale(WD) ~ pf + (1|harvest), data = nf.psp)) # standardized slope: -0.56


summary(lmer(scale(WD) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(WD) ~ pf + harvest + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(WD) ~ pf + (1|harvest/PSP), data = nf)) # standardized slope: -0.56



# RGR
summary(lm(scale(RGR) ~ pf, data = nf.psp)) # standardized slope: 0.56
summary(lm(scale(RGR) ~ pf + harvest, data = nf.psp)) # standardized slope: 0.56
summary(lmer(scale(RGR) ~ pf + (1|harvest), data = nf.psp)) # standardized slope: 0.56

summary(lmer(scale(RGR) ~ pf + (1|PSP), data = nf)) # standardized slope: 0.56
summary(lmer(scale(RGR) ~ pf + harvest + (1|PSP), data = nf)) # standardized slope: 0.56
summary(lmer(scale(RGR) ~ pf + (1|harvest/PSP), data = nf)) # standardized slope: 0.56



# Height
summary(lm(scale(H) ~ pf, data = H.df.psp)) # standardized slope: -0.56
summary(lm(scale(H) ~ pf + harvest, data = H.df.psp)) # standardized slope: -0.56
summary(lmer(scale(H) ~ pf + (1|harvest), data = H.df.psp)) # standardized slope: -0.56

summary(lmer(scale(H) ~ pf + (1|PSP), data = H.df)) # standardized slope: -0.56
summary(lmer(scale(H) ~ pf + harvest + (1|PSP), data = H.df)) # standardized slope: -0.56
summary(lmer(scale(H) ~ pf + (1|harvest/PSP), data = H.df)) # standardized slope: -0.56




# Mortality
summary(lm(scale(M) ~ pf, data = nf.psp)) # standardized slope: -0.56
summary(lm(scale(M) ~ pf + harvest, data = nf.psp)) # standardized slope: -0.56
summary(lmer(scale(M) ~ pf + (1|harvest), data = nf.psp)) # standardized slope: -0.56

summary(lmer(scale(M) ~ pf + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(M) ~ pf + harvest + (1|PSP), data = nf)) # standardized slope: -0.56
summary(lmer(scale(M) ~ pf + (1|harvest/PSP), data = nf)) # standardized slope: -0.56



# ~~~~ Figure 5 ~~~~ ------------------------------------------------------

vmfr <- read.csv('Fixer_Nonfixer_dynamics_SP.csv') %>%
     filter(Fixer == 'fixer') %>%
     mutate(P = (AGB.delta + R)/1000) %>%
     filter(!is.na(P)) %>% 
     group_by(bin) %>%
     filter(P > mean(P) - 2*sd(P) & P < mean(P) + 2*sd(P)) %>%
     mutate(lnP = log(P+0.01)) %>%
     filter(!is.na(lnP)) %>%
     mutate(slnP = scale(lnP)) %>%
     mutate(slnPdc = dc(slnP,N.bin.sp)) %>%
     ungroup(bin) %>%
     filter(N.bin.sp > 0) %>%
     mutate(bin = factor(bin,  labels = c('<30 cm',
                                          "30-60 cm",
                                          ">60 cm")))


summary(lmer(slnP ~ pf.sp.BA.total * bin + (1|PSP/SP) + (1|Year), data = vmfr))
summary(lmer(slnP ~ pf.sp.BA.total * bin + harvest + (1|PSP/SP) + (1|Year), data = vmfr))
summary(lmer(slnP ~ pf.sp.BA.total * bin + (1|harvest/PSP/SP) + (1|Year), data = vmfr))


vmfr <- read.csv('Fixer_Nonfixer_dynamics.csv') %>%
     filter(Fixer == 'fixer') %>%
     mutate(P = (AGB.delta + R)/1000) %>%
     filter(!is.na(P)) %>% 
     group_by(bin) %>%
     filter(P > mean(P) - 2*sd(P) & P < mean(P) + 2*sd(P)) %>%
     mutate(lnP = log(P+0.01)) %>%
     filter(!is.na(lnP)) %>%
     mutate(slnP = scale(lnP)) %>%
     mutate(slnPdc = dc(slnP,N.bin)) %>%
     ungroup(bin) %>%
     filter(N.bin > 0) %>%
     mutate(bin = factor(bin,  labels = c('<30 cm',
                                          "30-60 cm",
                                          ">60 cm")))


summary(lmer(slnP ~ pf.BA.total * bin + (1|PSP) + (1|Year), data = vmfr))
summary(lmer(slnP ~ pf.BA.total * bin + harvest + (1|PSP) + (1|Year), data = vmfr))
summary(lmer(slnP ~ pf.BA.total * bin + (1|harvest/PSP) + (1|Year), data = vmfr))
