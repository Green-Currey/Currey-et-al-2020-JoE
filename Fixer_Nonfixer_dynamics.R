## Dynamics demographics dataframe creator at the Plot level ##

source('~/R/startup.R')
# C:/Users/bryce/OneDrive/Documents/VMFR/Data/

bad.plots.25 <- c(31, 33, 34, 37, 39, 49, 53, 59, 60, 69, 71, 58, 79) # plots with one census or unfeasable dynamics



# data
VMFR <- read.csv(file = 'VMFR_corrected_AGB.csv') %>%
  filter(Species != unique(Species)[88]) %>%
  filter(foresttype == 'diverse') %>% # only diverse forests
  select(-c(38:39)) %>% # removes 2015 & 2017
  filter(PSP %!in% bad.plots.25)

BA.VMFR <- read.csv(file = 'VMFR_corrected_BA.csv') %>%
  filter(Species != unique(Species)[88]) %>%
  filter(foresttype == 'diverse') %>% # only diverse forests
  select(-c(38:39)) %>% # removes 2015 & 2017
  filter(PSP %!in% bad.plots.25)

DBH.VMFR <- read.csv(file = 'VMFR_corrected_DBH.csv') %>%
  filter(Species != unique(Species)[88]) %>%
  filter(foresttype == 'diverse') %>% # only diverse forests
  select(-c(38:39)) %>% # removes 2015 & 2017
  filter(PSP %!in% bad.plots.25)


min.biomass <- 214.1756 # kg
min.biomass2 <- 37.95386 # kg

VMFR.fix <- VMFR %>% filter(Fixer == 1); BA.fix <- BA.VMFR %>% filter(Fixer == 1); DBH.fix <- DBH.VMFR %>% filter(Fixer == 1)
VMFR.nonfix <- VMFR %>% filter(Fixer != 1); BA.nonfix <- BA.VMFR %>% filter(Fixer != 1); DBH.nonfix <- DBH.VMFR %>% filter(Fixer != 1)


# Dataframe creation with no Bin Category ------------------------------------


for (fix in c(1,2)) {
  
  if (fix == 1) {
    VMFR2 <- VMFR.nonfix
    BA2 <- BA.nonfix
    DBH2 <- DBH.nonfix
  } else {
    VMFR2 <- VMFR.fix
    BA2 <- BA.fix
    DBH2 <- DBH.fix
  }
  
  dyndemo.psp.nobin <- data.frame()
  cn <- which(names(VMFR2) %in% 'X1983'):dim(VMFR2)[2]
  
  plots <- unique(VMFR2$PSP)
  pb <- txtProgressBar(min = 1, max = length(plots), style = 3)
  for (p in seq(length(plots))) {
    PSP <- VMFR2 %>% filter(PSP == plots[p])
    BA <- BA2 %>% filter(PSP == plots[p])
    DBH <- DBH2 %>% filter(PSP == plots[p])
    harvest <- PSP$Harvest[1]
    
    PSP.total <- VMFR %>% filter(PSP == plots[p])
    BA.total <- BA.VMFR %>% filter(PSP == plots[p])
    
    
    N <-  nrow(PSP.total) # density
    pCensus <- names(PSP.total[,cn])[apply(PSP.total[,cn],2,sum,na.rm = T)>0] # possible census years
    
    ## This section checks for anomolously high recruitment, likely caused by census error
    cc <- apply(PSP.total[pCensus]>0, 2, sum, na.rm = T) %>% diff /N
    while(sum(cc*100 < -50)>0) {
      c.rm <- names(cc)[cc < -0.5]
      pCensus <- pCensus[-match(c.rm, pCensus)]
      if(length(pCensus)>1) {
        cc <- apply(PSP.total[pCensus]>0, 2, sum, na.rm = T) %>% diff /N} else {cc <- 1}
    }
    
    year.vec <- pCensus %>% substr(2,5) %>% as.numeric
    per.year <- year.vec %>% diff
    
    
    stem.total <- apply(PSP.total[,pCensus] %>% data.frame, 2, function(x) !is.na(x))
    stem.BA <- BA.total[,pCensus] %>% data.frame
    stem <- apply(PSP[,pCensus] %>% data.frame, 2, function(x) !is.na(x))
    
    N.total <-  apply(stem.total, 2, sum) # density
    BA.psp <- apply(stem.BA, 2, sum, na.rm = T)
    N <-  apply(stem, 2, sum)
    
    Nf.total <-  apply(stem.total*PSP.total$Fixer, 2, sum, na.rm = T) # of fixers
    Nf.BA <- apply(stem.BA*BA.total$Fixer, 2, sum, na.rm = T)
    
    pf.total <-  Nf.total/N.total # prop fixers
    pf.BA <- Nf.BA/BA.psp
    
    SR.total <- apply(stem.total, 2, function(x) length(unique(PSP.total$Spec.Code[x])))
    SR <- apply(stem, 2, function(x) length(unique(PSP$Spec.Code[x])))
    
    WD.total <- apply(PSP.total$Density*stem.total, 2, gm.mean, na.rm = T)
    WD <- apply(PSP$Density*stem, 2, gm.mean, na.rm = T)
    
    BA.data <- BA[,pCensus] %>% data.frame
    AGB.data <- PSP[,pCensus] %>% data.frame
    DBH.data <- DBH[,pCensus] %>% data.frame
    yr.delta <- pCensus[-1]
    
    if (length(pCensus)==1) {
      
      R.r <- NA
      M.r <- NA
      born <- NA
      alive <- NA
      dead <- NA
      
      rgr <- NA
      AGB <- NA
      AGB.delta <- NA
      R <- NA
      M <- NA
      
    } else if (length(pCensus)==2 & nrow(PSP[pCensus]) > 1) {
      
      RM <- apply(apply(PSP[,pCensus], 2, function(x) !is.na(x)), 1, diff) %>% data.frame
      RM.r <- apply(apply(RM, 2, function(x) x==1), 2, function(x) ifelse(x==1,1,NA)) %>% data.frame
      RM.m <- apply(apply(RM, 2, function(x) x==-1), 2, function(x) ifelse(x==1,1,NA)) %>% data.frame
      
      R.r <- (apply(RM.r,2,sum,na.rm = T)/sum(!is.na(PSP[,pCensus])[,-1]))/per.year
      born <- apply(RM.r,2,sum,na.rm = T)/per.year
      
      M.r <- (apply(RM.m,2,sum,na.rm = T)/sum(!is.na(PSP[,pCensus])[,-1]))/per.year
      dead <- apply(RM.m,2,sum,na.rm = T)/per.year
      alive <- sum(!is.na(PSP[,pCensus])[,-1])/per.year
      
      AGB <- sum(AGB.data[,-1], na.rm = T)
      rgr <- NA
      BA.sum <- sum(BA.data[,-1], na.rm = T)
      
      RM.r[RM.r!=1] <- NA
      RM.m <- RM.m * -1
      AGB.delta <- apply(t(apply(AGB.data, 1, diff)), 2, sum, na.rm = T) %>% data.frame %>% t /per.year
      R <- apply(X = AGB.data[-1]*RM.r-min.biomass2, 2, sum, na.rm = T) %>% data.frame %>% t /per.year
      M <- apply(X = AGB.data[-ncol(AGB.data)]*RM.m, 2, sum, na.rm = T) %>% data.frame %>% t /per.year; colnames(M) <- yr.delta
      
    } else if (nrow(PSP[pCensus]) == 1) {
      
      RM <-  as.numeric(!is.na(PSP[pCensus])) %>% diff
      
      RM.r <- RM; RM.r[RM!=1] <- 0
      RM.m <- RM; RM.m[RM!=-1] <- 0; RM.m <- RM.m*-1
      
      R.r <- (RM.r/!is.na(PSP[,pCensus][,-1]))/per.year
      born <- RM.r/per.year
      
      M.r <- (RM.m/!is.na(PSP[,pCensus][,-1]))/per.year
      dead <- RM.m/per.year
      alive <- !is.na(PSP[,pCensus][,-1])/per.year
      
      AGB <- apply(AGB.data[,-1], 2, sum, na.rm = T)
      rgr <- apply(t(apply(log(DBH.data), 1, diff, na.rm = T))/per.year,
                   MARGIN = 2, 
                   FUN = mean, na.rm  = T) * 100
      BA.sum <- apply(BA.data[,-1], 2, sum, na.rm = T)
      
      
      RM.r[RM.r!=1] <- NA
      RM.m <- RM.m * -1
      AGB.delta <- apply(t(apply(AGB.data, 1, diff)), 2, sum, na.rm = T) %>% data.frame %>% t /per.year
      R <- apply(X = AGB.data[-1]*RM.r-min.biomass2, 2, sum, na.rm = T) %>% data.frame %>% t /per.year
      M <- apply(X = AGB.data[-ncol(AGB.data)]*RM.m, 2, sum, na.rm = T) %>% data.frame %>% t /per.year; colnames(M) <- yr.delta
      
      
    } else {
      
      RM <- apply(apply(PSP[,pCensus], 2, function(x) !is.na(x)), 1, diff) %>% t
      RM.r <- apply(apply(RM, 2, function(x) x==1), 2, function(x) ifelse(x==1,1,NA))
      RM.m <- apply(apply(RM, 2, function(x) x==-1), 2, function(x) ifelse(x==1,1,NA))
      
      R.r <- apply(RM.r,2,sum,na.rm = T)/apply(!is.na(PSP[,pCensus])[,-1], 2, sum)/per.year
      born <- apply(RM.r,2,sum,na.rm = T)/apply(!is.na(PSP[,pCensus])[,-1], 2, sum)/per.year
      
      M.r <- apply(RM.m,2,sum,na.rm = T)/apply(!is.na(PSP[,pCensus])[,-1], 2, sum)/per.year
      dead <- apply(RM.m,2,sum,na.rm = T)/per.year
      alive <-apply(!is.na(PSP[,pCensus])[,-1], 2, sum)/per.year
      
      AGB <- apply(AGB.data[,-1], 2, sum, na.rm = T)
      rgr <- apply(t(apply(log(DBH.data), 1, diff, na.rm = T))/per.year,
                   MARGIN = 2, 
                   FUN = mean, na.rm  = T) * 100
      BA.sum <- apply(BA.data[,-1], 2, sum, na.rm = T)
      
      RM.r[RM.r!=1] <- NA
      RM.m <- RM.m * -1
      AGB.delta <- apply(t(apply(AGB.data, 1, diff)), 2, sum, na.rm = T) %>% data.frame %>% t /per.year
      R <- apply(X = AGB.data[-1]*RM.r-min.biomass2, 2, sum, na.rm = T) %>% data.frame %>% t /per.year
      M <- apply(X = AGB.data[-ncol(AGB.data)]*RM.m, 2, sum, na.rm = T) %>% data.frame %>% t /per.year; colnames(M) <- yr.delta
      
    }
    
    
    ## Assembling the data frame
    
    if (length(yr.delta)>0) {
      RM.df <- cbind(Year = yr.delta %>% substr(2,5) %>% as.numeric,
                     PSP = plots[p],
                     harvest = harvest,
                     N.total = N.total[-1],
                     N = N[-1],
                     Nf.total = Nf.total[-1],
                     SR.total = SR.total[-1],
                     SR = SR[-1],
                     WD.total = WD.total[-1],
                     WD = WD[-1],
                     pf.total = pf.total[-1],
                     pf.BA.total = pf.BA[-1],
                     rgr = rgr,
                     AGB = AGB,
                     BA = BA.sum,
                     AGB.delta = AGB.delta %>% as.numeric,
                     R.r = R.r %>% as.numeric,
                     born = born %>% as.numeric,
                     R = R %>% as.numeric,
                     M.r = M.r %>% as.numeric,
                     dead = dead %>% as.numeric,
                     alive = alive %>% as.numeric,
                     M = M %>% as.numeric)
      
      dyndemo.psp.nobin <- rbind(dyndemo.psp.nobin, RM.df)}
    
    setTxtProgressBar(pb, p)
  } # PSP loop
  if (fix == 1) {dyndemo.psp.nobin.nfixer <- dyndemo.psp.nobin} else {dyndemo.psp.nobin.fixer <- dyndemo.psp.nobin}
close(pb)
} # fixer switch loop


is.na(dyndemo.psp.nobin.fixer) <- sapply(dyndemo.psp.nobin.fixer, is.infinite)
is.na(dyndemo.psp.nobin.fixer) <- sapply(dyndemo.psp.nobin.fixer, is.nan)
is.na(dyndemo.psp.nobin.nfixer) <- sapply(dyndemo.psp.nobin.nfixer, is.infinite)
is.na(dyndemo.psp.nobin.nfixer) <- sapply(dyndemo.psp.nobin.nfixer, is.nan)

dyndemo.nobin <- rbind(cbind(dyndemo.psp.nobin.fixer, Fixer = rep('fixer')),
                 cbind(dyndemo.psp.nobin.nfixer, Fixer = rep('nonfixer')))

write.csv(dyndemo.nobin, "../Dynamics Paper 2019/Data/Fixer_Nonfixer_dynamics_nobin.csv", row.names = F)


# Dataframe creation with Bin category ---------------------------------


for (fix in c(1,2)) {
  
  if (fix == 1) {
    VMFR2 <- VMFR.nonfix
    BA2 <- BA.nonfix
    DBH2 <- DBH.nonfix
  } else {
    VMFR2 <- VMFR.fix
    BA2 <- BA.fix
    DBH2 <- DBH.fix
  }
  
  dyndemo.psp <- data.frame()
  cn <- which(names(VMFR2) %in% 'X1983'):dim(VMFR2)[2]
  
  plots <- unique(VMFR2$PSP)
  pb <- txtProgressBar(min = 1, max = length(plots), style = 3)
  for (p in seq(length(plots))) {
    PSP <- VMFR2 %>% filter(PSP == plots[p])
    BA <- BA2 %>% filter(PSP == plots[p])
    DBH <- DBH2 %>% filter(PSP == plots[p])
    harvest <- PSP$Harvest[1]
    
    PSP.total <- VMFR %>% filter(PSP == plots[p])
    BA.total <- BA.VMFR %>% filter(PSP == plots[p])
    
    
    N <-  nrow(PSP.total) # density
    pCensus <- names(PSP.total[,cn])[apply(PSP.total[,cn],2,sum,na.rm = T)>0] # possible census years
    
    ## This section checks for anomolously high recruitment, likely caused by census error
    cc <- apply(PSP.total[pCensus]>0, 2, sum, na.rm = T) %>% diff /N
    while(sum(cc*100 < -50)>0) {
      c.rm <- names(cc)[cc < -0.5]
      pCensus <- pCensus[-match(c.rm, pCensus)]
      if(length(pCensus)>1) {
        cc <- apply(PSP.total[pCensus]>0, 2, sum, na.rm = T) %>% diff /N} else {cc <- 1}
    }
    
    year.vec <- pCensus %>% substr(2,5) %>% as.numeric
    per.year <- year.vec %>% diff
    
    
    stem.total <- apply(PSP.total[,pCensus] %>% data.frame, 2, function(x) !is.na(x))
    stem.BA <- BA.total[,pCensus] %>% data.frame
    stem <- apply(PSP[,pCensus] %>% data.frame, 2, function(x) !is.na(x))
    
    N.total <-  apply(stem.total, 2, sum) # density
    BA.psp <- apply(stem.BA, 2, sum, na.rm = T)
    N <-  apply(stem, 2, sum)
    
    Nf.total <-  apply(stem.total*PSP.total$Fixer, 2, sum, na.rm = T) # of fixers
    Nf.BA <- apply(stem.BA*BA.total$Fixer, 2, sum, na.rm = T)
    
    pf.total <-  Nf.total/N.total # prop fixers
    pf.BA.total <- Nf.BA/BA.psp
    
    SR.total <- apply(stem.total, 2, function(x) length(unique(PSP.total$Spec.Code[x])))
    SR <- apply(stem, 2, function(x) length(unique(PSP$Spec.Code[x])))
    
    bins <- unique(PSP$DBH_bin)
    
    for (b in seq(length(bins))) {
      psp.bin <- PSP %>% filter(DBH_bin == bins[b]) # filters by bin
      BA.bin <- BA %>% filter(DBH_bin == bins[b])
      DBH.bin <- DBH %>% filter(DBH_bin == bins[b])
      
      AGB.data <- psp.bin[,pCensus] %>% data.frame
      BA.data <- BA.bin[,pCensus] %>% data.frame
      DBH.data <- DBH.bin[,pCensus] %>% data.frame
      
      yr.delta <- pCensus[-1]
      
      
      if(nrow(AGB.data) == 1) {N.bin <- rep(1, times = ncol(AGB.data))} else {N.bin <-  apply(apply(AGB.data, 2, function(x) !is.na(x)), 2, sum)}
      

      if (length(pCensus)==1) {
        
        R.r <- NA
        M.r <- NA
        born <- NA
        alive <- NA
        dead <- NA
        
        rgr <- NA
        AGB <- NA
        AGB.delta <- NA
        R <- NA
        M <- NA
        
      } else if (length(pCensus)==2 & nrow(psp.bin[pCensus]) > 1) {
        
        RM <- apply(apply(psp.bin[,pCensus], 2, function(x) !is.na(x)), 1, diff) %>% data.frame
        RM.r <- apply(apply(RM, 2, function(x) x==1), 2, function(x) ifelse(x==1,1,NA)) %>% data.frame
        RM.m <- apply(apply(RM, 2, function(x) x==-1), 2, function(x) ifelse(x==1,1,NA)) %>% data.frame
        
        R.r <- (apply(RM.r,2,sum,na.rm = T)/sum(!is.na(psp.bin[,pCensus])[,-1]))/per.year
        born <- apply(RM.r,2,sum,na.rm = T)/per.year
        
        M.r <- (apply(RM.m,2,sum,na.rm = T)/sum(!is.na(psp.bin[,pCensus])[,-1]))/per.year
        dead <- apply(RM.m,2,sum,na.rm = T)/per.year
        alive <- sum(!is.na(psp.bin[,pCensus])[,-1])/per.year
        
        AGB <- sum(AGB.data[,-1], na.rm = T)
        rgr <- NA
        BA.sum <- sum(BA.data[,-1], na.rm = T)
        
        RM.r[RM.r!=1] <- NA
        RM.m <- RM.m * -1
        AGB.delta <- apply(t(apply(AGB.data, 1, diff)), 2, sum, na.rm = T) %>% data.frame %>% t /per.year
        R <- apply(X = AGB.data[-1]*RM.r-min.biomass2, 2, sum, na.rm = T) %>% data.frame %>% t /per.year
        M <- apply(X = AGB.data[-ncol(AGB.data)]*RM.m, 2, sum, na.rm = T) %>% data.frame %>% t /per.year; colnames(M) <- yr.delta
        
      } else if (nrow(psp.bin[pCensus]) == 1) {
        
        RM <-  as.numeric(!is.na(psp.bin[pCensus])) %>% diff
        
        RM.r <- RM; RM.r[RM!=1] <- 0
        RM.m <- RM; RM.m[RM!=-1] <- 0; RM.m <- RM.m*-1
        
        R.r <- (RM.r/!is.na(psp.bin[,pCensus][,-1]))/per.year
        born <- RM.r/per.year
        
        M.r <- (RM.m/!is.na(psp.bin[,pCensus][,-1]))/per.year
        dead <- RM.m/per.year
        alive <- !is.na(psp.bin[,pCensus][,-1])/per.year
        
        AGB <- apply(AGB.data[,-1], 2, sum, na.rm = T)
        rgr <- apply(t(apply(log(DBH.data), 1, diff, na.rm = T))/per.year,
                     MARGIN = 2, 
                     FUN = mean, na.rm  = T) * 100
        BA.sum <- apply(BA.data[,-1], 2, sum, na.rm = T)
        
        RM.r[RM.r!=1] <- NA
        RM.m <- RM.m * -1
        AGB.delta <- apply(t(apply(AGB.data, 1, diff)), 2, sum, na.rm = T) %>% data.frame %>% t /per.year
        R <- apply(X = AGB.data[-1]*RM.r-min.biomass2, 2, sum, na.rm = T) %>% data.frame %>% t /per.year
        M <- apply(X = AGB.data[-ncol(AGB.data)]*RM.m, 2, sum, na.rm = T) %>% data.frame %>% t /per.year; colnames(M) <- yr.delta
        
        
      } else {
        
        RM <- apply(apply(psp.bin[,pCensus], 2, function(x) !is.na(x)), 1, diff) %>% t
        RM.r <- apply(apply(RM, 2, function(x) x==1), 2, function(x) ifelse(x==1,1,NA))
        RM.m <- apply(apply(RM, 2, function(x) x==-1), 2, function(x) ifelse(x==1,1,NA))
        
        R.r <- apply(RM.r,2,sum,na.rm = T)/apply(!is.na(psp.bin[,pCensus])[,-1], 2, sum)/per.year
        born <- apply(RM.r,2,sum,na.rm = T)/apply(!is.na(psp.bin[,pCensus])[,-1], 2, sum)/per.year
        
        M.r <- apply(RM.m,2,sum,na.rm = T)/apply(!is.na(psp.bin[,pCensus])[,-1], 2, sum)/per.year
        dead <- apply(RM.m,2,sum,na.rm = T)/per.year
        alive <-apply(!is.na(psp.bin[,pCensus])[,-1], 2, sum)/per.year
        
        AGB <- apply(AGB.data[,-1], 2, sum, na.rm = T)
        rgr <- apply(t(apply(log(DBH.data), 1, diff, na.rm = T))/per.year,
                     MARGIN = 2, 
                     FUN = mean, na.rm  = T) * 100
        BA.sum <- apply(BA.data[,-1], 2, sum, na.rm = T)

        RM.r[RM.r!=1] <- NA
        RM.m <- RM.m * -1
        AGB.delta <- apply(t(apply(AGB.data, 1, diff)), 2, sum, na.rm = T) %>% data.frame %>% t /per.year
        R <- apply(X = AGB.data[-1]*RM.r-min.biomass2, 2, sum, na.rm = T) %>% data.frame %>% t /per.year
        M <- apply(X = AGB.data[-ncol(AGB.data)]*RM.m, 2, sum, na.rm = T) %>% data.frame %>% t /per.year; colnames(M) <- yr.delta
        
      }
      
      
      ## Assembling the data frame
      
      if (length(yr.delta)>0) {
        RM.df <- cbind(Year = yr.delta %>% substr(2,5) %>% as.numeric,
                       PSP = plots[p],
                       bin = bins[b],
                       harvest = harvest,
                       N.total = N.total[-1],
                       N = N[-1],
                       N.bin = N.bin[-1],
                       SR.total = SR.total[-1],
                       SR = SR[-1],
                       Nf.total = Nf.total[-1],
                       pf.total = pf.total[-1],
                       pf.BA.total = pf.BA.total[-1],
                       rgr = rgr,
                       AGB = AGB,
                       BA = BA.sum,
                       AGB.delta = AGB.delta %>% as.numeric,
                       R.r = R.r %>% as.numeric,
                       born = born %>% as.numeric,
                       R = R %>% as.numeric,
                       M.r = M.r %>% as.numeric,
                       dead = dead %>% as.numeric,
                       alive = alive %>% as.numeric,
                       M = M %>% as.numeric)
        
        dyndemo.psp <- rbind(dyndemo.psp, RM.df)}
      
    } # bin loop
    setTxtProgressBar(pb, p)
  } # PSP loop
  if (fix == 1) {dyndemo.psp.nfixer <- dyndemo.psp} else {dyndemo.psp.fixer <- dyndemo.psp}
  close(pb)
} # fixer switch loop


is.na(dyndemo.psp.fixer) <- sapply(dyndemo.psp.fixer, is.infinite)
is.na(dyndemo.psp.fixer) <- sapply(dyndemo.psp.fixer, is.nan)
is.na(dyndemo.psp.nfixer) <- sapply(dyndemo.psp.nfixer, is.infinite)
is.na(dyndemo.psp.nfixer) <- sapply(dyndemo.psp.nfixer, is.nan)

dyndemo <- rbind(cbind(dyndemo.psp.fixer, Fixer = rep('fixer')),
                 cbind(dyndemo.psp.nfixer, Fixer = rep('nonfixer')))

write.csv(dyndemo, "../Dynamics Paper 2019/Data/Fixer_Nonfixer_dynamics.csv", row.names = F)

