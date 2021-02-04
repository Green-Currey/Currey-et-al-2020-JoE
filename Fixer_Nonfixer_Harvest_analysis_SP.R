
# Looking at mortality biomass relative to cutting times

source('~/R/startup.R')
#C:/Users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Data/





# PSP data (wide) ---------------------------------------------------------
PSP.wide <- read.csv('VMFR_corrected_AGB.csv')


# Dynamics data (long) ----------------------------------------------------
PSP.long <- read.csv('SP_dynamics_nobin.csv')

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
     dplyr::select(1,3, c(15:38)) %>% 
     gather(key = 'Year', value = 'AGB', -c(1,2)) %>%
     mutate(Year = as.numeric(substr(Year,2,5))) %>%
     dplyr::rename(SP = Su.Pl.No) %>%
     group_by(PSP, SP, Year) %>%
     summarise(AGB = sum(AGB, na.rm = T)/1000) %>%
     filter(AGB > 0) %>%
     mutate(AGB_delta = c(NA,diff(AGB))) %>%
     ungroup() %>%
     merge(PSP.long %>% select(-c(10,12,15,17,18)),
           by = c('PSP', 'SP', 'Year')) %>%
     arrange(PSP, SP, Year) 

PSP.data <- PSP.data %>% 
     mutate(Harvested = cut$Harvest_num[match(PSP.data$PSP, cut$PSP)]>0,
            Harvest_num = cut$Harvest_num[match(PSP.data$PSP, cut$PSP)],
            time_since_last = cut$Time_since_last[match(PSP.data$PSP, cut$PSP)],
            Harvest1 = !is.na(cut$Harvest1[match(PSP.data$PSP, cut$PSP)]),
            Harvest2 = cut$Harvest2[match(PSP.data$PSP, cut$PSP)]
     )

PSP.data <- PSP.data %>% filter(PSP %!in% bad.plots.25)

str(PSP.data)


# ------ ------------------------------------------------------------------



# Entire PSP anaylses -------------------------------------



# -------------------------------------------- #
# Examining cut vs uncut over entire VMFR period
# -------------------------------------------- #

harvested1 <- PSP.data %>%
     group_by(PSP, SP) %>%
     summarise(AGB = mean(AGB),
               # WD = mean(WD.sp),
               N = mean(N.sp),
               Nf = mean(Nf.sp),
               SR = mean(SR.sp),
               pf = mean(pf.BA.sp),
               rgr = mean(rgr.sp),
               BA = mean(BA),
               M = mean(M),
               Mr = mean(M.r),
               R = mean(R),
               Rr = mean(R.r),
               H = mean(Harvested)) %>%
     mutate(H = factor(H))#, labels = c('Not Harvested', 'Harvested')))


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
     save = F)


figure(
     ggplot(harvested1) +
          geom_boxplot(aes(x = H, y = pf)) + 
          theme_classic(base_size = 17) +
          theme(axis.title.x = element_blank()) +
          labs(y = 'N-fixer RA'),
     path.name = 'c:/users/bryce/onedrive/documents/vmfr/Dynamics Paper 2019/Figures/Harvest/EntirePSP_alltime_pf',
     height = 4, width = 3.5,
     save = F)

