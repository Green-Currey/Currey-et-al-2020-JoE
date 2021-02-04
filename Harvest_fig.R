source('~/R/startup.R')
#C:/Users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Data/




# PSP data (wide) ---------------------------------------------------------
PSP.wide <- read.csv('../../Data/VMFR_corrected_AGB.csv')


# Dynamics data (long) ----------------------------------------------------
PSP.long <- read.csv('PSP_dynamics_no_bin.csv')
PSP.Fixer.long <- read.csv('Fixer_Nonfixer_dynamics_nobin.csv')

# Harvest Data ------------------------------------------------------------

# Harvest data provide by Mike Oatham on August 8, 2020
cut <- read.csv('../../data/VMFR_PSP_havest_dates_modified.csv') %>%
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
        gather(key = 'Year', value = 'AGB', -c(1:2)) %>%
        mutate(Year = as.numeric(substr(Year,2,5))) %>%
        group_by(PSP, Year) %>%
        summarise(AGB = sum(AGB, na.rm = T)/1000) %>%
        filter(AGB > 0) %>%
        mutate(AGB_delta = c(NA,diff(AGB))) %>%
        ungroup() %>%
        merge(PSP.long %>% select(c(1:6, 8:10, 12, 15, 18, 19)),
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

fixer.data <- PSP.Fixer.long %>% select(c(1,2,5,8,10,12:16,20,23,24)) %>%
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




# ------- -----------------------------------------------------------------


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
                  H = mean(Harvested)) %>%
        mutate(H = factor(H, labels = c('Not Harvested', 'Harvested')))

# ---- AGB
summary(lm(AGB ~ H, data = harvested1)) # NS

# ---- BA
summary(lm(BA ~ H, data = harvested1)) # NS

# ---- Density
summary(lm(N ~ H, data = harvested1)) # Sig

# ---- WD
summary(lm(WD ~ H, data = harvested1)) # Sig

# ---- rgr
summary(lm(rgr ~ H, data = harvested1)) # NS

# ---- Mr
summary(lm(Mr ~ H, data = harvested1)) # NS


# ---- pf
summary(lm(pf ~ H, data = harvested1)) # Sig



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
                  H = mean(Harvested)) %>%
        ungroup() %>%
        mutate(H = factor(H, labels = c('Not Harvested', 'Harvested'))) %>%
        mutate(H.f = interaction(H, Fixer))


# ---- AGB
TukeyHSD(aov(AGB ~ H * Fixer, data = harvested4), ordered = T) # NS

# ---- BA
TukeyHSD(aov(BA ~ H * Fixer, data = harvested4), ordered = T) # NS

# ---- Density
TukeyHSD(aov(N ~ H * Fixer, data = harvested4), ordered = T) # NS

# ---- Wood Density
TukeyHSD(aov(WD ~ H * Fixer, data = harvested4), ordered = T) # NS

# ---- rgr
TukeyHSD(aov(rgr ~ H * Fixer, data = harvested4), ordered = T) # NS

# ---- Mr
TukeyHSD(aov(Mr ~ H * Fixer, data = harvested4), ordered = T) # NS




# Plotting significant relationships general --------------------------------------


# Entire PSPs: All time
# ---- AGB
p1 <- ggplot() +
        geom_boxplot(data = harvested1, aes(x = H, y = AGB), color = 'black', fill = 'grey90') + 
        geom_boxplot(data = harvested4, aes(x = H, y = AGB, fill = Fixer), color = 'black') +
        scale_fill_manual(values = c('red3', 'blue')) +
        scale_y_continuous(limits = c(NA, 630)) +
        theme_classic(base_size = 17) +
        theme(axis.title.x = element_blank(),
              legend.position = 'none') +
        labs(y = bquote('Biomass (Mg '~ha^-1*')'))

# ---- BA
p2 <- ggplot() +
        geom_boxplot(data = harvested1, aes(x = H, y = BA), color = 'black', fill = 'grey90') + 
        geom_boxplot(data = harvested4, aes(x = H, y = BA, fill = Fixer), color = 'black') +
        scale_fill_manual(values = c('red3', 'blue')) +
        scale_y_continuous(limits = c(NA, 62)) +
        theme_classic(base_size = 17) +
        theme(axis.title.x = element_blank(),
              legend.position = 'none') +
        labs(y = bquote('Basal Area ('*m^2~ha^-1*')'))

p2b <- ggplot() +
        geom_boxplot(data = harvested1, aes(x = H, y = pf*100), color = 'black', fill = 'grey90') + 
        scale_fill_manual(values = c('red3', 'blue')) +
        scale_y_continuous(limits = c(NA, 50)) +
        theme_classic(base_size = 17) +
        theme(axis.title.x = element_blank(),
              legend.position = 'none') +
        labs(y = bquote('N-fixer RA (% '~ha^-1*')'))

# ---- Density
p3 <- ggplot() +
        geom_boxplot(data = harvested1, aes(x = H, y = N), color = 'black', fill = 'grey90') + 
        geom_boxplot(data = harvested4, aes(x = H, y = N, fill = Fixer), color = 'black') +
        scale_fill_manual(values = c('red3', 'blue')) +
        scale_y_continuous(limits = c(NA, 500)) +
        theme_classic(base_size = 17) +
        theme(axis.title.x = element_blank(),
              legend.position = 'none') +
        labs(y = bquote('Stem Density (Stems '~ha^-1*')'))

# ---- Wood Density
p4 <- ggplot() +
        geom_boxplot(data = harvested1, aes(x = H, y = WD), color = 'black', fill = 'grey90') + 
        geom_boxplot(data = harvested4, aes(x = H, y = WD, fill = Fixer), color = 'black') +
        scale_fill_manual(values = c('red3', 'blue')) +
        scale_y_continuous(limits = c(NA, 0.9)) +
        theme_classic(base_size = 17) +
        theme(axis.title.x = element_blank(),
              legend.position = 'none') +
        labs(y = bquote('Wood Density (g '~cm^-3*')'))

# ---- RGR
p5 <- ggplot() +
        geom_boxplot(data = harvested1, aes(x = H, y = rgr), color = 'black', fill = 'grey90') + 
        geom_boxplot(data = harvested4, aes(x = H, y = rgr, fill = Fixer), color = 'black') +
        scale_fill_manual(values = c('red3', 'blue')) +
        scale_y_continuous(limits = c(NA, 3.5)) +
        theme_classic(base_size = 17) +
        theme(axis.title.x = element_blank(),
              legend.position = 'none') +
        labs(y = bquote('RGR (% '~yr^-1*')'))

# ---- Mort rate
p6 <- ggplot() +
        geom_boxplot(data = harvested1, aes(x = H, y = Mr*100), color = 'black', fill = 'grey90') + 
        geom_boxplot(data = harvested4, aes(x = H, y = Mr*100, fill = Fixer), color = 'black') +
        scale_fill_manual(values = c('red3', 'blue')) +
        scale_y_continuous(limits = c(NA, 8)) +
        theme_classic(base_size = 17) +
        theme(axis.title.x = element_blank(),
              legend.position = 'none') +
        labs(y = bquote('Mortality (% '~yr^-1*')'))


# Figure
figure(
        ggarrange(p1,p2,
                  p3,p4,
                  p5,p6,
                  nrow = 3, ncol = 2,
                  align = 'hv'),
        height = 10.5, width = 8,
        path.name = 'c:/Users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Figures/Harvest/Harvest_figure',
        save = T
)

figure(p2b,
        height = 3.5, width = 3.95,
        path.name = 'c:/Users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Figures/Harvest/Harvest_figure_2',
        save = T
)
