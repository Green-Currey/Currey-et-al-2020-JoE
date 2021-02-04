# Looking at mortality biomass relative to cutting times

source('~/R/startup.R')
#C:/Users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Data/





# PSP data (wide) ---------------------------------------------------------
PSP.wide <- read.csv('VMFR_corrected_AGB.csv')


# Dynamics data (long) ----------------------------------------------------
PSP.long <- read.csv('PSP_dynamics_no_bin.csv')
# PSP.long <- read.csv('SP_dynamics_nobin.csv')
PSP.Fixer.long <- read.csv('Fixer_Nonfixer_dynamics_nobin.csv')

# Harvest Data ------------------------------------------------------------

# Harvest data provide by Mike Oatham on August 8, 2020
cut <- read.csv('c:/users/bryce/onedrive/documents/vmfr/data/VMFR_PSP_havest_dates_modified.csv') %>%
     mutate(PSP.Number = as.numeric(substr(PSP.Number,4,6))) %>%
     select(-2)

names(cut) <- c('PSP', 'Block', 'Harvest1', 'Harvest2', 'Time_since_last', 'Harvest_num')
cut$Time_since_last[cut$Time_since_last=='Not Harvested'] <- NA

# Original .xlsx file was modified to remove question marks
# question marked sections are examined below for data cleaning.
# Harvest Number == "Vol Control" was changed to actual number of harvests



# Combining dataframes ----------------------------------------------------

bad.plots.25 <- c(33, 34, 37, 39, 49, 53, 59, 69, 71, 58, 79)

# ---- setting up the dataframe for entire PSP

PSP.data <- PSP.wide %>% 
     dplyr::select(1, c(15:39)) %>% 
     gather(key = 'Year', value = 'AGB', -c(1)) %>%
     mutate(Year = as.numeric(substr(Year,2,5))) %>%
     group_by(PSP, Year) %>%
     summarise(AGB = sum(AGB, na.rm = T)/1000) %>%
     filter(AGB > 0) %>%
     mutate(AGB_delta = c(NA,diff(AGB))) %>%
     ungroup() %>%
     merge(PSP.long %>% select(c(1:6, 8:10, 12, 14, 15, 18, 19)),
           by = c('PSP', 'Year')) %>%
     arrange(PSP, Year) 

PSP.data <- PSP.data %>% 
     mutate(Harvested = cut$Harvest_num[match(PSP.data$PSP, cut$PSP)]>0,
            Harvest_num = cut$Harvest_num[match(PSP.data$PSP, cut$PSP)],
            time_since_last = cut$Time_since_last[match(PSP.data$PSP, cut$PSP)],
            Harvest1 = !is.na(cut$Harvest1[match(PSP.data$PSP, cut$PSP)]),
            Harvest2 = cut$Harvest2[match(PSP.data$PSP, cut$PSP)]
     )

PSP.data <- PSP.data %>% filter(PSP %!in% bad.plots.25)

str(PSP.data)


# ---- setting up the dataframe for fixer nonfixer

fixer.data <- PSP.Fixer.long %>% select(c(1,2,4,7,9,11:16,18,19,22,23)) %>%
     mutate(AGB = AGB/1000, AGB.delta = AGB.delta/1000, M = M*-1/1000) %>%
     arrange(PSP, Year)

fixer.data <- fixer.data %>% 
     mutate(Harvested = cut$Harvest_num[match(fixer.data$PSP, cut$PSP)]>0,
            Harvest_num = cut$Harvest_num[match(fixer.data$PSP, cut$PSP)],
            time_since_last = cut$Time_since_last[match(fixer.data$PSP, cut$PSP)],
            Harvest1 = !is.na(cut$Harvest1[match(fixer.data$PSP, cut$PSP)]),
            Harvest2 = cut$Harvest2[match(fixer.data$PSP, cut$PSP)]
     )

fixer.data <- fixer.data %>% filter(PSP %!in% bad.plots.25)

str(fixer.data)



# Harvest counts ----------------------------------------------------------

harvested <- PSP.data %>% group_by(PSP) %>%
     summarise(H = mean(Harvested), H1 = mean(Harvest1), H2 = mean(Harvest2>1983), NfixerRA = mean(pf.BA.psp))
table(harvested$H)
table(harvested$H1)
table(harvested$H2)
# 37 diverse plots have been up for harvest at least once
# 34 diverse plots were cut pre VMFR
# 37 diverse plots were cut post VMFR


# ------ ------------------------------------------------------------------



# Individual year analyses ------------------------------------------------
highMort <- PSP.data %>%
        filter(Year < 1990) %>%
        group_by(PSP) %>%
        summarise(AGB.delta = min(AGB_delta),
                  P = max(R + AGB_delta), 
                  M = min(M),
                  Harvested = mean(Harvested))

summary(lm(AGB.delta ~ Harvested, data = highMort))

summary(lm(P ~ Harvested, data = highMort))

summary(lm(M ~ Harvested, data = highMort))

# Entire PSP anaylses -------------------------------------



# -------------------------------------------- #
# Examining cut vs uncut over entire VMFR period
# -------------------------------------------- #

harvested1 <- PSP.data %>%
     group_by(PSP) %>%
     summarise(AGB = mean(AGB),
               WD = mean(WD.psp),
               N = mean(N.psp),
               Nf = mean(Nf.psp),
               SR = mean(SR.psp),
               pf = mean(pf.BA.psp),
               rgr = mean(rgr),
               BA = mean(BA),
               M = mean(M),
               Mr = mean(M.r),
               R = mean(R),
               Rr = mean(R.r),
               H = mean(Harvested)) %>%
     mutate(H = factor(H, labels = c('Not Harvested', 'Harvested')))


# ---- Density
summary(lm(N ~ H, data = harvested1)) # Sig

# ---- pf
summary(lm(pf ~ H, data = harvested1)) # Sig

# ---- Fixers
summary(lm(Nf ~ H, data = harvested1)) # Sig

# ---- WD
summary(lm(WD ~ H, data = harvested1)) # Sig

# ---- AGB
summary(lm(AGB ~ H, data = harvested1)) # NS

# ---- SR
summary(lm(SR ~ H, data = harvested1)) # NS

# ---- rgr
summary(lm(rgr ~ H, data = harvested1)) # NS

# ---- BA
summary(lm(BA ~ H, data = harvested1)) # NS

# ---- M
summary(lm(M ~ H, data = harvested1)) # NS

# ---- Mr
summary(lm(Mr ~ H, data = harvested1)) # NS

# ---- R
summary(lm(R ~ H, data = harvested1)) # NS

# ---- Rr
summary(lm(Rr ~ H, data = harvested1)) # NS



# -------------------------------------------- #
# Examining cut vs uncut up until second harvest
# -------------------------------------------- #

plots <- as.numeric(as.character(unique(PSP.data$PSP)))
harvested2 <- data.frame()
for (i in plots) {
     data <- PSP.data %>%
          filter(PSP == i) %>%
          select(Year, AGB, N.psp, WD.psp, Nf.psp, SR.psp, pf.BA.psp, rgr, BA, M, M.r, R, R.r, Harvest2)
     cut <- data$Harvest2[1]
     
     if (is.na(cut)) {
          harvested2 <- harvested2 %>% rbind(data %>% summarise(PSP = i,
                                                                AGB = mean(AGB),
                                                                N = mean(N.psp),
                                                                Nf = mean(Nf.psp),
                                                                WD = mean(WD.psp),
                                                                SR = mean(SR.psp),
                                                                pf = mean(pf.BA.psp),
                                                                rgr = mean(rgr),
                                                                BA = mean(BA),
                                                                M = mean(M),
                                                                Mr = mean(M.r),
                                                                R = mean(R),
                                                                Rr = mean(R.r),
                                                                H = 0) 
          )
          
     } else if (cut < data$Year[1]) { 
          
          next
          
     } else {
          harvested2 <- harvested2 %>% rbind(data %>%
                                                  filter(Year <= cut) %>%
                                                  summarise(PSP = i,
                                                            AGB = mean(AGB),
                                                            N = mean(N.psp),
                                                            Nf = mean(Nf.psp),
                                                            WD = mean(WD.psp),
                                                            SR = mean(SR.psp),
                                                            pf = mean(pf.BA.psp),
                                                            rgr = mean(rgr),
                                                            BA = mean(BA),
                                                            M = mean(M),
                                                            Mr = mean(M.r),
                                                            R = mean(R),
                                                            Rr = mean(R.r),
                                                            H = 1)
          )
          
     }
}

harvested2 <- harvested2 %>%
     mutate(H = factor(H, labels = c('Not Harvested', 'Harvested')))




# ---- Density
summary(lm(N ~ H, data = harvested2)) # Sig

# ---- pf
summary(lm(pf ~ H, data = harvested2)) # Sig

# ---- Fixers
summary(lm(Nf ~ H, data = harvested2)) # Sig

# ---- rgr
summary(lm(rgr ~ H, data = harvested2)) # Sig

# ---- BA
summary(lm(BA ~ H, data = harvested2)) # NS

# ---- WD
summary(lm(WD ~ H, data = harvested2)) # NS

# ---- AGB
summary(lm(AGB ~ H, data = harvested2)) # NS

# ---- SR
summary(lm(SR ~ H, data = harvested2)) # NS

# ---- M
summary(lm(M ~ H, data = harvested2)) # NS

# ---- Mr
summary(lm(Mr ~ H, data = harvested2)) # NS

# ---- R
summary(lm(R ~ H, data = harvested2)) # NS

# ---- Rr
summary(lm(Rr ~ H, data = harvested2)) # NS




# -------------------------------------------- #
# Examining cut vs uncut up until second harvest
# -------------------------------------------- #

plots <- as.numeric(as.character(unique(PSP.data$PSP)))
harvested3 <- data.frame()
for (i in plots) {
     data <- PSP.data %>%
          filter(PSP == i) %>%
          select(Year, AGB, N.psp, WD.psp, Nf.psp, SR.psp, pf.BA.psp, rgr, BA, M, M.r, R, R.r, Harvest2)
     cut <- data$Harvest2[1]
     
     if (is.na(cut)) {
          harvested3 <- harvested3 %>% rbind(data %>% summarise(PSP = i,
                                                                AGB = mean(AGB),
                                                                N = mean(N.psp),
                                                                Nf = mean(Nf.psp),
                                                                WD = mean(WD.psp),
                                                                SR = mean(SR.psp),
                                                                pf = mean(pf.BA.psp),
                                                                rgr = mean(rgr),
                                                                BA = mean(BA),
                                                                M = mean(M),
                                                                Mr = mean(M.r),
                                                                R = mean(R),
                                                                Rr = mean(R.r),
                                                                H = 0)
          )
          
     } else if (cut < data$Year[1]) { 
          
          next
          
     } else {
          harvested3 <- harvested3 %>% rbind(data %>%
                                                  filter(Year > cut) %>%
                                                  summarise(PSP = i,
                                                            AGB = mean(AGB),
                                                            N = mean(N.psp),
                                                            Nf = mean(Nf.psp),
                                                            WD = mean(WD.psp),
                                                            SR = mean(SR.psp),
                                                            pf = mean(pf.BA.psp),
                                                            rgr = mean(rgr),
                                                            BA = mean(BA),
                                                            M = mean(M),
                                                            Mr = mean(M.r),
                                                            R = mean(R),
                                                            Rr = mean(R.r),
                                                            H = 1)
          )
          
     }
}

harvested3 <- harvested3 %>%
     mutate(H = factor(H, labels = c('Not Harvested', 'Harvested')))



# ---- pf
summary(lm(pf ~ H, data = harvested3)) # Sig

# ---- rgr
summary(lm(rgr ~ H, data = harvested3)) # Sig


# ---- Density
summary(lm(Nf ~ H, data = harvested3)) # NS


# ---- Mr
summary(lm(Mr ~ H, data = harvested3)) # BS

# ---- WD
summary(lm(WD ~ H, data = harvested3))

# ---- Density
summary(lm(N ~ H, data = harvested3)) # NS

#---- AGB
summary(lm(AGB ~ H, data = harvested3)) # NS

# ---- SR
summary(lm(SR ~ H, data = harvested3)) # NS

# ---- BA
summary(lm(BA ~ H, data = harvested3)) # NS

# ---- M
summary(lm(M ~ H, data = harvested3)) # NS

# ---- R
summary(lm(R ~ H, data = harvested3)) # NS

# ---- Rr
summary(lm(Rr ~ H, data = harvested3)) # NS



# Fixer-Nonfixer anaylses -------------------------------------



# -------------------------------------------- #
# Examining cut vs uncut over entire VMFR period
# -------------------------------------------- #

harvested4 <- fixer.data %>%
     group_by(PSP, Fixer) %>%
     summarise(AGB = mean(AGB),
               N = mean(N),
               WD = mean(WD),
               SR = mean(SR),
               rgr = mean(rgr),
               BA = mean(BA),
               M = mean(M),
               Mr = mean(M.r),
               R = mean(R),
               Rr = mean(R.r),
               H = mean(Harvested)) %>%
     ungroup() %>%
     mutate(H = factor(H, labels = c('Not Harvested', 'Harvested'))) %>%
     mutate(H.f = interaction(H, Fixer))


# ---- AGB
# summary(lmer(AGB ~ H.f - 1 + (1|PSP), data = harvested4)) # Sig
TukeyHSD(aov(AGB ~ H * Fixer, data = harvested4), ordered = T) # NS


# ---- BA
# summary(lmer(BA ~ H*Fixer + (1|PSP), data = harvested4)) # Sig
TukeyHSD(aov(BA ~ H * Fixer, data = harvested4), ordered = T) # NS


# ---- Density
# summary(lmer(N ~ H*Fixer + (1|PSP), data = harvested4)) # NS
TukeyHSD(aov(N ~ H * Fixer, data = harvested4), ordered = T) # NS

# ---- Density
# summary(lmer(N ~ H*Fixer + (1|PSP), data = harvested4)) # NS
TukeyHSD(aov(WD ~ H * Fixer, data = harvested4), ordered = T) # NS


# ---- SR
# summary(lmer(SR ~ H*Fixer + (1|PSP), data = harvested4)) # NS
TukeyHSD(aov(SR ~ H * Fixer, data = harvested4), ordered = T) # NS

# ---- rgr
# summary(lmer(rgr ~ H*Fixer + (1|PSP), data = harvested4)) # NS
TukeyHSD(aov(rgr ~ H * Fixer, data = harvested4), ordered = T) # NS


# ---- M
# summary(lmer(M ~ H*Fixer + (1|PSP), data = harvested4)) # NS
TukeyHSD(aov(M ~ H * Fixer, data = harvested4), ordered = T) # NS


# ---- Mr
TukeyHSD(aov(Mr ~ H * Fixer, data = harvested4), ordered = T) # NS


# ---- R
TukeyHSD(aov(R ~ H * Fixer, data = harvested4), ordered = T) # NS


# ---- Rr
TukeyHSD(aov(Rr ~ H * Fixer, data = harvested4), ordered = T) # NS


# -------------------------------------------- #
# Examining cut vs uncut up until second harvest
# -------------------------------------------- #

plots <- as.numeric(as.character(unique(fixer.data$PSP)))
harvested5 <- data.frame()
for (i in plots) {
     data <- fixer.data %>%
          filter(PSP == i) %>%
          select(Year, Fixer, WD, AGB, N, SR, rgr, BA, R, R.r, M, M.r, Harvest2)
     cut <- data$Harvest2[1]
     
     if (is.na(cut)) {
          harvested5 <- harvested5 %>% rbind(data %>%
                                                  group_by(Fixer) %>%
                                                  summarise(PSP = i,
                                                            AGB = mean(AGB),
                                                            N = mean(N),
                                                            WD = mean(WD),
                                                            SR = mean(SR),
                                                            rgr = mean(rgr),
                                                            BA = mean(BA),
                                                            M = mean(M),
                                                            Mr = mean(M.r),
                                                            R = mean(R),
                                                            Rr = mean(R.r),
                                                            H = 0)
          )
          
     } else if (cut < data$Year[1]) { 
          
          next
          
     } else {
          harvested5 <- harvested5 %>% rbind(data %>%
                                                  filter(Year <= cut) %>%
                                                  group_by(Fixer) %>%
                                                  summarise(PSP = i,
                                                            AGB = mean(AGB),
                                                            N = mean(N),
                                                            WD = mean(WD),
                                                            SR = mean(SR),
                                                            rgr = mean(rgr),
                                                            BA = mean(BA),
                                                            M = mean(M),
                                                            Mr = mean(M.r),
                                                            R = mean(R),
                                                            Rr = mean(R.r),
                                                            H = 1)
          )
          
     }
}

harvested5 <- harvested5 %>%
     mutate(H = factor(H, labels = c('Not Harvested', 'Harvested')))

# ---- AGB
# summary(lm(AGB ~ H*Fixer, data = harvested5)) # Sig
TukeyHSD(aov(AGB ~ H * Fixer, data = harvested5), ordered = T) # NS

# ---- BA
# summary(lm(BA ~ H*Fixer, data = harvested5)) # Sig
TukeyHSD(aov(BA ~ H * Fixer, data = harvested5), ordered = T) # NS

# ---- M
# summary(lm(M ~ H*Fixer, data = harvested5)) # Sig
TukeyHSD(aov(M ~ H * Fixer, data = harvested5), ordered = T) # NS

# ---- WD
# summary(lm(M ~ H*Fixer, data = harvested5)) # Sig
TukeyHSD(aov(WD ~ H * Fixer, data = harvested5), ordered = T) # NS


# ---- Density
# summary(lm(N ~ H*Fixer, data = harvested5)) # Sig
TukeyHSD(aov(N ~ H * Fixer, data = harvested5), ordered = T) # NS

# ---- SR
# summary(lm(SR ~ H*Fixer, data = harvested5)) # NS
TukeyHSD(aov(SR ~ H * Fixer, data = harvested5), ordered = T) # NS

# ---- rgr
# summary(lm(rgr ~ H*Fixer, data = harvested5)) # NS
TukeyHSD(aov(rgr ~ H * Fixer, data = harvested5), ordered = T) # NS

# ---- Mr
# summary(lm(Mr ~ H*Fixer, data = harvested5)) # NS
TukeyHSD(aov(Mr ~ H * Fixer, data = harvested5), ordered = T) # NS


# ---- R
TukeyHSD(aov(R ~ H * Fixer, data = harvested5), ordered = T) # NS


# ---- Rr
TukeyHSD(aov(Rr ~ H * Fixer, data = harvested5), ordered = T) # NS




# ----------------------------------------- #
# Examining cut vs uncut after second harvest
# ----------------------------------------- #

plots <- as.numeric(as.character(unique(PSP.data$PSP)))
harvested6 <- data.frame()
for (i in plots) {
     data <- fixer.data %>%
          filter(PSP == i) %>%
          select(Year, Fixer, AGB, WD, N, SR, rgr, BA, R, R.r, M, M.r, Harvest2)
     cut <- data$Harvest2[1]
     
     if (is.na(cut)) {
          harvested6 <- harvested6 %>% rbind(data %>%
                                                  group_by(Fixer) %>%
                                                  summarise(PSP = i,
                                                            AGB = mean(AGB),
                                                            N = mean(N),
                                                            WD = mean(WD),
                                                            SR = mean(SR),
                                                            rgr = mean(rgr),
                                                            BA = mean(BA),
                                                            M = mean(M),
                                                            Mr = mean(M.r),
                                                            R = mean(R),
                                                            Rr = mean(R.r),
                                                            H = 0)
          )
          
     } else if (cut < data$Year[1]) { 
          
          next
          
     } else {
          harvested6 <- harvested6 %>% rbind(data %>%
                                                  filter(Year > cut) %>%
                                                  group_by(Fixer) %>%
                                                  summarise(PSP = i,
                                                            AGB = mean(AGB),
                                                            N = mean(N),
                                                            WD = mean(WD),
                                                            SR = mean(SR),
                                                            rgr = mean(rgr),
                                                            BA = mean(BA),
                                                            M = mean(M),
                                                            Mr = mean(M.r),
                                                            R = mean(R),
                                                            Rr = mean(R.r),
                                                            H = 1)
          )
          
     }
}

harvested6 <- harvested6 %>%
     mutate(H = factor(H, labels = c('Not Harvested', 'Harvested')))

# ---- M
# summary(lm(M ~ H*Fixer, data = harvested6)) # Sig
TukeyHSD(aov(M ~ H * Fixer, data = harvested6), ordered = T) # NS

#---- AGB
# summary(lm(AGB ~ H*Fixer, data = harvested6)) # Sig
TukeyHSD(aov(AGB ~ H * Fixer, data = harvested6), ordered = T) # NS

# ---- BA
# summary(lm(BA ~ H*Fixer, data = harvested6)) # sig
TukeyHSD(aov(BA ~ H * Fixer, data = harvested6), ordered = T) # NS

# ---- BA
# summary(lm(BA ~ H*Fixer, data = harvested6)) # sig
TukeyHSD(aov(WD ~ H * Fixer, data = harvested6), ordered = T) # NS


# ---- rgr
# summary(lm(rgr ~ H*Fixer, data = harvested6)) # NS
TukeyHSD(aov(rgr ~ H * Fixer, data = harvested6), ordered = T) # NS


# ---- Mr
# summary(lm(Mr ~ H*Fixer, data = harvested6)) # NS
TukeyHSD(aov(Mr ~ H * Fixer, data = harvested6), ordered = T) # NS

# ---- Density
# summary(lm(N ~ H*Fixer, data = harvested6)) # NS
TukeyHSD(aov(N ~ H * Fixer, data = harvested6), ordered = T) # NS

# ---- SR
# summary(lm(SR ~ H*Fixer, data = harvested6)) # NS
TukeyHSD(aov(SR ~ H * Fixer, data = harvested6), ordered = T) # NS


# ---- R
TukeyHSD(aov(R ~ H * Fixer, data = harvested6), ordered = T) # NS


# ---- Rr
TukeyHSD(aov(Rr ~ H * Fixer, data = harvested6), ordered = T) # NS





# Plotting significant relationships general --------------------------------------


# Entire PSPs: All time
# --- density and pf
figure(
     ggplot(harvested1) +
          geom_boxplot(aes(x = H, y = N)) + 
          theme_classic(base_size = 17) +
          theme(axis.title.x = element_blank()) +
          labs(y = 'Stem Density'),
     path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/EntirePSP_alltime_N',
     height = 4, width = 3.5,
     save = T)


figure(
     ggplot(harvested1) +
          geom_boxplot(aes(x = H, y = pf)) + 
          theme_classic(base_size = 17) +
          theme(axis.title.x = element_blank()) +
          labs(y = 'N-fixer RA'),
     path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/EntirePSP_alltime_pf',
     height = 4, width = 3.5,
     save = T)


# Entire PSPs: Pre 2nd Cut
# --- density and pf
figure(
     ggplot(harvested2) +
          geom_boxplot(aes(x = H, y = N)) + 
          theme_classic(base_size = 17) +
          theme(axis.title.x = element_blank()) +
          labs(y = 'Stem Density'),
     path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/EntirePSP_preCut_N',
     height = 4, width = 3.5,
     save = T)

figure(
     ggplot(harvested2) +
          geom_boxplot(aes(x = H, y = pf)) + 
          theme_classic(base_size = 17) +
          theme(axis.title.x = element_blank()) +
          labs(y = 'N-fixer RA'),
     path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/EntirePSP_preCut_pf',
     height = 4, width = 3.5,
     save = T)


# Entire PSPs: Post 2nd Cut
# --- density, pf, rgr
figure(
     ggplot(harvested3) +
          geom_boxplot(aes(x = H, y = N)) + 
          theme_classic(base_size = 17) +
          theme(axis.title.x = element_blank()) +
          labs(y = 'Stem Density'),
     path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/EntirePSP_postCut_N',
     height = 4, width = 3.5,
     save = T)

figure(
     ggplot(harvested3) +
          geom_boxplot(aes(x = H, y = pf)) + 
          theme_classic(base_size = 17) +
          theme(axis.title.x = element_blank()) +
          labs(y = 'N-fixer RA'),
     path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/EntirePSP_postCut_pf',
     height = 4, width = 3.5,
     save = T)

figure(
     ggplot(harvested3) +
          geom_boxplot(aes(x = H, y = rgr)) + 
          theme_classic(base_size = 17) +
          theme(axis.title.x = element_blank()) +
          labs(y = 'RGR'),
     path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/EntirePSP_postCut_rgr',
     height = 4, width = 3.5,
     save = T)

# Plotting significant relationships Nfixer --------------------------------------

# -------------------------------------------- #
# Examining cut vs uncut over entire VMFR period
# -------------------------------------------- #
# ---- Density
figure(
        ggplot(harvested4) +
                geom_boxplot(aes(x = H, y = N, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'Stem Density'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_alltime_N',
        height = 4, width = 3.5,
        save = T)

# ---- RGR
figure(
        ggplot(harvested4) +
                geom_boxplot(aes(x = H, y = rgr, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'rgr'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_alltime_rgr',
        height = 4, width = 3.5,
        save = T)

# ---- Mr
figure(
        ggplot(harvested4) +
                geom_boxplot(aes(x = H, y = Mr, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'Mortality rate'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_alltime_Mr',
        height = 4, width = 3.5,
        save = T)

# ---- AGB
figure(
        ggplot(harvested4) +
                geom_boxplot(aes(x = H, y = AGB, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'AGB'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_alltime_AGB',
        height = 4, width = 3.5,
        save = T)

# ---- BA
figure(
        ggplot(harvested4) +
                geom_boxplot(aes(x = H, y = BA, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'BA'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_alltime_BA',
        height = 4, width = 3.5,
        save = T)

# ---- M
figure(
        ggplot(harvested4) +
                geom_boxplot(aes(x = H, y = M, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'Mortality'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_alltime_M',
        height = 4, width = 3.5,
        save = T)



# -------------------------------------------- #
# Examining cut vs uncut up until second harvest
# -------------------------------------------- #
# ---- AGB
figure(
        ggplot(harvested5) +
                geom_boxplot(aes(x = H, y = AGB, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'AGB'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_preCut_AGB',
        height = 4, width = 3.5,
        save = T)

# ---- BA
figure(
        ggplot(harvested5) +
                geom_boxplot(aes(x = H, y = BA, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'BA'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_preCut_BA',
        height = 4, width = 3.5,
        save = T)

# ---- M
figure(
        ggplot(harvested5) +
                geom_boxplot(aes(x = H, y = M, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'Mortality'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_preCut_M',
        height = 4, width = 3.5,
        save = T)

# ---- Density
figure(
        ggplot(harvested5) +
                geom_boxplot(aes(x = H, y = N, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'Stem Density'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_preCut_N',
        height = 4, width = 3.5,
        save = T)

# ---- rgr
figure(
        ggplot(harvested5) +
                geom_boxplot(aes(x = H, y = rgr, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'RGR'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_preCut_rgr',
        height = 4, width = 3.5,
        save = T)

# ---- Mr
figure(
        ggplot(harvested5) +
                geom_boxplot(aes(x = H, y = Mr, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'Mortality rate'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_preCut_Mr',
        height = 4, width = 3.5,
        save = T)



# ----------------------------------------- #
# Examining cut vs uncut after second harvest
# ----------------------------------------- #

# ---- AGB
figure(
        ggplot(harvested6) +
                geom_boxplot(aes(x = H, y = AGB, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'AGB'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_postCut_AGB',
        height = 4, width = 3.5,
        save = T)

# ---- BA
figure(
        ggplot(harvested6) +
                geom_boxplot(aes(x = H, y = BA, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'BA'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_postCut_BA',
        height = 4, width = 3.5,
        save = T)


# ---- Density
figure(
        ggplot(harvested6) +
                geom_boxplot(aes(x = H, y = N, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'Stem Density'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_postCut_N',
        height = 4, width = 3.5,
        save = T)

# ---- M
figure(
        ggplot(harvested6) +
                geom_boxplot(aes(x = H, y = M, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'Mortality'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_postCut_M',
        height = 4, width = 3.5,
        save = T)

# ---- rgr

figure(
        ggplot(harvested6) +
                geom_boxplot(aes(x = H, y = rgr, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'RGR'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_postCut_rgr',
        height = 4, width = 3.5,
        save = T)

# ---- Mr

figure(
        ggplot(harvested6) +
                geom_boxplot(aes(x = H, y = Mr, fill = Fixer)) + 
                scale_fill_manual(values = c('red3', 'blue')) +
                theme_classic(base_size = 17) +
                theme(axis.title.x = element_blank(),
                      legend.position = 'none') +
                labs(y = 'Mortality rate'),
        path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/Fixer_postCut_Mr',
        height = 4, width = 3.5,
        save = T)
