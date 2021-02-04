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
  
  if (fix == 1) {VMFR2 <- VMFR.nonfix} else {VMFR2 <- VMFR.fix}
  
  dyndemo.nobin.sp <- data.frame()
  cn <- which(names(VMFR2) %in% 'X1983'):dim(VMFR2)[2]
  
  plots <- unique(VMFR2$PSP)
  pb <- txtProgressBar(min = 1, max = length(plots), style = 3)
  for (p in seq(length(plots))) {
    PSP <- VMFR2 %>% filter(PSP == plots[p])
    PSP.total <- VMFR %>% filter(PSP == plots[p])
    BA.total <- BA.VMFR %>% filter(PSP == plots[p])
    
    subplots <- unique(PSP$Su.Pl.No)
    for (sp in seq(length(subplots))) {
      SP <- PSP %>% filter(Su.Pl.No == subplots[sp]) # subplot filter
      
      SP.total <- PSP.total %>% filter(Su.Pl.No == subplots[sp])
      BA.SP.total <- BA.total %>% filter(Su.Pl.No == subplots[sp])
      harvest <- SP$Harvest[1]
      
      N.sp <-  nrow(SP.total) # density
      pCensus <- names(SP.total[,cn])[apply(SP.total[,cn],2,sum,na.rm = T)>0] # possible census years
      if (length(pCensus)==0) {next}
      
      ## This section checks for anomolously high recruitment, likely caused by census error
      cc <- apply(SP.total[pCensus]>0, 2, sum, na.rm = T) %>% diff /N.sp
      if (N.sp > 5) {
        while(sum(cc*100 < -50)>0) {
          c.rm <- names(cc)[cc < -0.5]
          pCensus <- pCensus[-match(c.rm, pCensus)]
          if(length(pCensus)>1) {cc <- apply(SP.total[pCensus]>0, 2, sum, na.rm = T) %>% diff /N.sp} else {cc <- 1}
        }
      }
      
      year.vec <- pCensus %>% substr(2,5) %>% as.numeric
      per.year <- year.vec %>% diff
      
      
      if (ncol(SP.total[,pCensus] %>% data.frame)==1) {
        stem.total <- apply(SP.total[,pCensus] %>% data.frame, 2, function(x) !is.na(x))} else if (nrow(SP.total[,pCensus])==1) {
          stem.total <- !is.na(SP.total[,pCensus])} else {
            stem.total <- apply(SP.total[,pCensus] %>% data.frame, 2, function(x) !is.na(x))}
      stem.total <- stem.total %>% data.frame
      
      if (ncol(SP[,pCensus] %>% data.frame)==1) {
        stem <- apply(SP[,pCensus] %>% data.frame, 2, function(x) !is.na(x))} else if (nrow(SP[,pCensus])==1) {
          stem <- !is.na(SP[,pCensus])} else {
            stem <- apply(SP[,pCensus] %>% data.frame, 2, function(x) !is.na(x))}
      stem <- stem %>% data.frame
      
      stem.BA <- BA.SP.total[,pCensus] %>% data.frame
      
      N.sp.total <-  apply(stem.total, 2, sum) # density
      BA.sp <- apply(stem.BA, 2, sum, na.rm = T)
      N.sp <- apply(stem, 2, sum)
      
      Nf.sp.total <-  apply(stem.total*SP.total$Fixer, 2, sum, na.rm = T) # of fixers
      Nf.sp.BA <- apply(stem.BA*BA.SP.total$Fixer, 2, sum, na.rm = T)
      
      pf.sp.total <-  Nf.sp.total/N.sp.total # prop fixers
      pf.sp.BA.total <- Nf.sp.BA/BA.sp
      
      SR.sp.total <- apply(stem.total, 2, function(x) length(unique(SP.total$Spec.Code[x])))
      SR.sp <- apply(stem, 2, function(x) length(unique(SP$Spec.Code[x])))
      
      AGB.data <- SP[,pCensus] %>% data.frame
      yr.delta <- pCensus[-1]
      
      rgr <- gm.mean(SP$mRGR)*100 # relative growth rate at bin level
      
      ## Recruit - mortality rates ##
      if (length(pCensus)==1) {
        
        R.r <- NA
        M.r <- NA
        born <- NA
        alive <- NA
        dead <- NA
        
        AGB <- NA
        AGB.delta <- NA
        R <- NA
        M <- NA
        
      } else if (length(pCensus)==2 & nrow(SP[pCensus]) > 1) {
        
        RM <- apply(apply(SP[pCensus], 2, function(x) !is.na(x)), 1, diff) %>% data.frame
        RM.r <- apply(apply(RM, 2, function(x) x==1), 2, function(x) ifelse(x==1,1,NA)) %>% data.frame
        RM.m <- apply(apply(RM, 2, function(x) x==-1), 2, function(x) ifelse(x==1,1,NA)) %>% data.frame
        
        R.r <- (apply(RM.r,2,sum,na.rm = T)/sum(!is.na(SP[,pCensus])[,-1]))/per.year
        born <- apply(RM.r,2,sum,na.rm = T)/per.year
        
        M.r <- (apply(RM.m,2,sum,na.rm = T)/sum(!is.na(SP[,pCensus])[,-1]))/per.year
        dead <- apply(RM.m,2,sum,na.rm = T)/per.year
        alive <- sum(!is.na(SP[,pCensus])[,-1])/per.year
        
        AGB <- sum(AGB.data[,-1], na.rm = T)
        
        RM.r[RM.r!=1] <- NA
        RM.m <- RM.m * -1
        AGB.delta <- apply(t(apply(AGB.data, 1, diff)), 2, sum, na.rm = T) %>% data.frame %>% t /per.year
        R <- apply(X = AGB.data[-1]*RM.r-min.biomass2, 2, sum, na.rm = T) %>% data.frame %>% t /per.year
        M <- apply(X = AGB.data[-ncol(AGB.data)]*RM.m, 2, sum, na.rm = T) %>% data.frame %>% t /per.year; colnames(M) <- yr.delta
        
        
      } else if (nrow(SP[pCensus]) == 1) {
        
        RM <-  as.numeric(!is.na(SP[pCensus])) %>% diff
        
        RM.r <- RM; RM.r[RM!=1] <- 0
        RM.m <- RM; RM.m[RM!=-1] <- 0; RM.m <- RM.m*-1
        
        R.r <- (RM.r/!is.na(SP[pCensus][,-1]))/per.year
        born <- RM.r/per.year
        
        M.r <- (RM.m/!is.na(SP[pCensus][,-1]))/per.year
        dead <- RM.m/per.year
        alive <- !is.na(SP[pCensus][,-1])/per.year
        
        if (length(pCensus)==2) {AGB <- sum(AGB.data[,-1], na.rm = T)} else {AGB <- apply(AGB.data[,-1], 2, sum, na.rm = T)}
        
        RM.r[RM.r!=1] <- NA
        RM.m <- RM.m * -1
        AGB.delta <- apply(t(apply(AGB.data, 1, diff)), 2, sum, na.rm = T) %>% data.frame %>% t /per.year
        R <- apply(X = AGB.data[-1]*RM.r-min.biomass2, 2, sum, na.rm = T) %>% data.frame %>% t /per.year
        M <- apply(X = AGB.data[-ncol(AGB.data)]*RM.m, 2, sum, na.rm = T) %>% data.frame %>% t /per.year; colnames(M) <- yr.delta
        
      } else {
        
        RM <- apply(apply(SP[pCensus], 2, function(x) !is.na(x)), 1, diff) %>% t
        RM.r <- apply(apply(RM, 2, function(x) x==1), 2, function(x) ifelse(x==1,1,NA))
        RM.m <- apply(apply(RM, 2, function(x) x==-1), 2, function(x) ifelse(x==1,1,NA))
        
        R.r <- (apply(RM.r,2,sum,na.rm = T)/apply(!is.na(SP[pCensus])[,-1], 2, sum))/per.year
        born <- apply(RM.r,2,sum,na.rm = T)/per.year
        
        M.r <- (apply(RM.m,2,sum,na.rm = T)/apply(!is.na(SP[pCensus])[,-1], 2, sum))/per.year
        dead <- apply(RM.m,2,sum,na.rm = T)/per.year
        alive <- apply(!is.na(SP[pCensus])[,-1], 2, sum)/per.year
        
        AGB <- apply(AGB.data[,-1], 2, sum, na.rm = T)
        
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
                       SP = subplots[sp],
                       harvest = harvest,
                       N.sp = N.sp[-1],
                       SR.sp = SR.sp[-1],
                       SR.sp.total = SR.sp.total[-1],
                       N.sp.total = N.sp.total[-1],
                       Nf.sp.total = Nf.sp.total[-1],
                       pf.sp.total = pf.sp.total[-1],
                       pf.sp.BA.total = pf.sp.BA.total[-1],
                       rgr = rgr,
                       AGB = AGB,
                       AGB.delta = AGB.delta %>% as.numeric,
                       R.r = R.r %>% as.numeric,
                       born = born %>% as.numeric,
                       R = R %>% as.numeric,
                       M.r = M.r %>% as.numeric,
                       dead = dead %>% as.numeric,
                       alive = alive %>% as.numeric,
                       M = M %>% as.numeric)
        
        
        
        dyndemo.nobin.sp <- rbind(dyndemo.nobin.sp, RM.df)
      }
      
    } # SP loop
    setTxtProgressBar(pb, p)
  } # PSP loop
  close(pb)
  if (fix == 1) {dyndemo.sp.nobin.nfixer <- dyndemo.nobin.sp} else {dyndemo.sp.nobin.fixer <- dyndemo.nobin.sp}
} # fixer indicator loop


is.na(dyndemo.sp.nobin.fixer) <- sapply(dyndemo.sp.nobin.fixer, is.infinite)
is.na(dyndemo.sp.nobin.fixer) <- sapply(dyndemo.sp.nobin.fixer, is.nan)
is.na(dyndemo.sp.nobin.nfixer) <- sapply(dyndemo.sp.nobin.nfixer, is.infinite)
is.na(dyndemo.sp.nobin.nfixer) <- sapply(dyndemo.sp.nobin.nfixer, is.nan)

dyndemo.nobin <- rbind(cbind(dyndemo.sp.nobin.fixer, Fixer = rep('fixer')),
                       cbind(dyndemo.sp.nobin.nfixer, Fixer = rep('nonfixer')))

write.csv(dyndemo.nobin, "../Dynamics Paper 2019/Data/Fixer_Nonfixer_dynamics_nobin_SP.csv", row.names = F)



# Dataframe creation with Bin category ---------------------------------


for (fix in c(1,2)) {
  
  if (fix == 1) {VMFR2 <- VMFR.nonfix} else {VMFR2 <- VMFR.fix}
  
  dyndemo.sp <- data.frame()
  cn <- which(names(VMFR2) %in% 'X1983'):dim(VMFR2)[2]
  
  plots <- unique(VMFR2$PSP)
  pb <- txtProgressBar(min = 1, max = length(plots), style = 3)
  for (p in seq(length(plots))) {
    PSP <- VMFR2 %>% filter(PSP == plots[p])
    PSP.total <- VMFR %>% filter(PSP == plots[p])
    BA.total <- BA.VMFR %>% filter(PSP == plots[p])
    
    subplots <- unique(PSP$Su.Pl.No)
    for (sp in seq(length(subplots))) {
      SP <- PSP %>% filter(Su.Pl.No == subplots[sp]) # subplot filter
      harvest <- SP$Harvest[1]

      SP.total <- PSP.total %>% filter(Su.Pl.No == subplots[sp])
      BA.SP.total <- BA.total %>% filter(Su.Pl.No == subplots[sp])
      
      
      N.sp <-  nrow(SP.total) # density
      pCensus <- names(SP.total[,cn])[apply(SP.total[,cn],2,sum,na.rm = T)>0] # possible census years
      if (length(pCensus)==0) {next}
      
      ## This section checks for anomolously high recruitment, likely caused by census error
      cc <- apply(SP.total[pCensus]>0, 2, sum, na.rm = T) %>% diff /N.sp
      if (N.sp > 5) {
        while(sum(cc*100 < -50)>0) {
          c.rm <- names(cc)[cc < -0.5]
          pCensus <- pCensus[-match(c.rm, pCensus)]
          if(length(pCensus)>1) {cc <- apply(SP.total[pCensus]>0, 2, sum, na.rm = T) %>% diff /N.sp} else {cc <- 1}
        }
      }
      
      year.vec <- pCensus %>% substr(2,5) %>% as.numeric
      per.year <- year.vec %>% diff
      
      
      if (ncol(SP.total[,pCensus] %>% data.frame)==1) {
        stem.total <- apply(SP.total[,pCensus] %>% data.frame, 2, function(x) !is.na(x))} else if (nrow(SP.total[,pCensus])==1) {
          stem.total <- !is.na(SP.total[,pCensus])} else {
            stem.total <- apply(SP.total[,pCensus] %>% data.frame, 2, function(x) !is.na(x))}
      stem.total <- stem.total %>% data.frame
      
      if (ncol(SP[,pCensus] %>% data.frame)==1) {
        stem <- apply(SP[,pCensus] %>% data.frame, 2, function(x) !is.na(x))} else if (nrow(SP[,pCensus])==1) {
          stem <- !is.na(SP[,pCensus])} else {
            stem <- apply(SP[,pCensus] %>% data.frame, 2, function(x) !is.na(x))}
      stem <- stem %>% data.frame
      
      stem.BA <- BA.SP.total[,pCensus] %>% data.frame
      
      N.sp.total <-  apply(stem.total, 2, sum) # density
      BA.sp <- apply(stem.BA, 2, sum, na.rm = T)
      N.sp <- apply(stem, 2, sum)
      
      Nf.sp.total <-  apply(stem.total*SP.total$Fixer, 2, sum, na.rm = T) # of fixers
      Nf.sp.BA <- apply(stem.BA*BA.SP.total$Fixer, 2, sum, na.rm = T)
      
      pf.sp.total <-  Nf.sp.total/N.sp.total # prop fixers
      pf.sp.BA.total <- Nf.sp.BA/BA.sp
      
      SR.sp.total <- apply(stem.total, 2, function(x) length(unique(SP.total$Spec.Code[x])))
      SR.sp <- apply(stem, 2, function(x) length(unique(SP$Spec.Code[x])))
      
      
      bins <- unique(SP$DBH_bin)
      for (b in seq(length(bins))) {
        sp.bin <- SP %>% filter(DBH_bin == bins[b]) # filters by bin
        
        AGB.data <- sp.bin[,pCensus] %>% data.frame
        yr.delta <- pCensus[-1]
        
        rgr <- gm.mean(sp.bin$mRGR)*100 # relative growth rate at bin level
        
        if(nrow(AGB.data) == 1) {N.bin.sp <- rep(1, times = ncol(AGB.data))} else {N.bin.sp <-  apply(apply(AGB.data, 2, function(x) !is.na(x)), 2, sum)}
        
        ## Recruit - mortality rates ##
        if (length(pCensus)==1) {
          
          R.r <- NA
          M.r <- NA
          born <- NA
          alive <- NA
          dead <- NA
          
          AGB <- NA
          AGB.delta <- NA
          R <- NA
          M <- NA
          
        } else if (length(pCensus)==2 & nrow(sp.bin[pCensus]) > 1) {
          
          RM <- apply(apply(sp.bin[pCensus], 2, function(x) !is.na(x)), 1, diff) %>% data.frame
          RM.r <- apply(apply(RM, 2, function(x) x==1), 2, function(x) ifelse(x==1,1,NA)) %>% data.frame
          RM.m <- apply(apply(RM, 2, function(x) x==-1), 2, function(x) ifelse(x==1,1,NA)) %>% data.frame
          
          R.r <- (apply(RM.r,2,sum,na.rm = T)/sum(!is.na(sp.bin[,pCensus])[,-1]))/per.year
          born <- apply(RM.r,2,sum,na.rm = T)/per.year
          
          M.r <- (apply(RM.m,2,sum,na.rm = T)/sum(!is.na(sp.bin[,pCensus])[,-1]))/per.year
          dead <- apply(RM.m,2,sum,na.rm = T)/per.year
          alive <- sum(!is.na(sp.bin[,pCensus])[,-1])/per.year
          
          AGB <- sum(AGB.data[,-1], na.rm = T)
          
          RM.r[RM.r!=1] <- NA
          RM.m <- RM.m * -1
          AGB.delta <- apply(t(apply(AGB.data, 1, diff)), 2, sum, na.rm = T) %>% data.frame %>% t /per.year
          R <- apply(X = AGB.data[-1]*RM.r-min.biomass2, 2, sum, na.rm = T) %>% data.frame %>% t /per.year
          M <- apply(X = AGB.data[-ncol(AGB.data)]*RM.m, 2, sum, na.rm = T) %>% data.frame %>% t /per.year; colnames(M) <- yr.delta
          
          
        } else if (nrow(sp.bin[pCensus]) == 1) {
          
          RM <-  as.numeric(!is.na(sp.bin[pCensus])) %>% diff
          
          RM.r <- RM; RM.r[RM!=1] <- 0
          RM.m <- RM; RM.m[RM!=-1] <- 0; RM.m <- RM.m*-1
          
          R.r <- (RM.r/!is.na(sp.bin[pCensus][,-1]))/per.year
          born <- RM.r/per.year
          
          M.r <- (RM.m/!is.na(sp.bin[pCensus][,-1]))/per.year
          dead <- RM.m/per.year
          alive <- !is.na(sp.bin[pCensus][,-1])/per.year
          
          if (length(pCensus)==2) {AGB <- sum(AGB.data[,-1], na.rm = T)} else {AGB <- apply(AGB.data[,-1], 2, sum, na.rm = T)}
          
          RM.r[RM.r!=1] <- NA
          RM.m <- RM.m * -1
          AGB.delta <- apply(t(apply(AGB.data, 1, diff)), 2, sum, na.rm = T) %>% data.frame %>% t /per.year
          R <- apply(X = AGB.data[-1]*RM.r-min.biomass2, 2, sum, na.rm = T) %>% data.frame %>% t /per.year
          M <- apply(X = AGB.data[-ncol(AGB.data)]*RM.m, 2, sum, na.rm = T) %>% data.frame %>% t /per.year; colnames(M) <- yr.delta
          
        } else {
          
          RM <- apply(apply(sp.bin[pCensus], 2, function(x) !is.na(x)), 1, diff) %>% t
          RM.r <- apply(apply(RM, 2, function(x) x==1), 2, function(x) ifelse(x==1,1,NA))
          RM.m <- apply(apply(RM, 2, function(x) x==-1), 2, function(x) ifelse(x==1,1,NA))
          
          R.r <- (apply(RM.r,2,sum,na.rm = T)/apply(!is.na(sp.bin[pCensus])[,-1], 2, sum))/per.year
          born <- apply(RM.r,2,sum,na.rm = T)/per.year
          
          M.r <- (apply(RM.m,2,sum,na.rm = T)/apply(!is.na(sp.bin[pCensus])[,-1], 2, sum))/per.year
          dead <- apply(RM.m,2,sum,na.rm = T)/per.year
          alive <- apply(!is.na(sp.bin[pCensus])[,-1], 2, sum)/per.year
          
          AGB <- apply(AGB.data[,-1], 2, sum, na.rm = T)
          
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
                         SP = subplots[sp],
                         bin = bins[b],
                         harvest = harvest,
                         N.sp = N.sp[-1],
                         N.bin.sp = N.bin.sp[-1],
                         SR.sp = SR.sp[-1],
                         SR.sp.total = SR.sp.total[-1],
                         N.sp.total = N.sp.total[-1],
                         Nf.sp.total = Nf.sp.total[-1],
                         pf.sp.total = pf.sp.total[-1],
                         pf.sp.BA.total = pf.sp.BA.total[-1],
                         rgr = rgr,
                         AGB = AGB,
                         AGB.delta = AGB.delta %>% as.numeric,
                         R.r = R.r %>% as.numeric,
                         born = born %>% as.numeric,
                         R = R %>% as.numeric,
                         M.r = M.r %>% as.numeric,
                         dead = dead %>% as.numeric,
                         alive = alive %>% as.numeric,
                         M = M %>% as.numeric)
          
          
          
          dyndemo.sp <- rbind(dyndemo.sp, RM.df)
        }
        
      } # bin loop
    } # SP loop
    setTxtProgressBar(pb, p)
  } # PSP loop
  close(pb)
  if (fix == 1) {dyndemo.sp.nfixer <- dyndemo.sp} else {dyndemo.sp.fixer <- dyndemo.sp}
} # fixer indicator loop


is.na(dyndemo.sp.fixer) <- sapply(dyndemo.sp.fixer, is.infinite)
is.na(dyndemo.sp.fixer) <- sapply(dyndemo.sp.fixer, is.nan)
is.na(dyndemo.sp.nfixer) <- sapply(dyndemo.sp.nfixer, is.infinite)
is.na(dyndemo.sp.nfixer) <- sapply(dyndemo.sp.nfixer, is.nan)

dyndemo <- rbind(cbind(dyndemo.sp.fixer, Fixer = rep('fixer')),
                 cbind(dyndemo.sp.nfixer, Fixer = rep('nonfixer')))

write.csv(dyndemo, "../Dynamics Paper 2019/Data/Fixer_Nonfixer_dynamics_SP.csv", row.names = F)