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


# Dataframe creation with no Bin category ---------------------------------


dyndemo.psp.nobin <- data.frame()
cn <- which(names(VMFR) %in% 'X1983'):dim(VMFR)[2]

plots <- unique(VMFR$PSP)
pb <- txtProgressBar(min = 1, max = length(plots), style = 3)
for (p in seq(length(plots))) {
  PSP <- VMFR %>% filter(PSP == plots[p])
  BA <- BA.VMFR %>% filter(PSP == plots[p])
  DBH <- DBH.VMFR %>% filter(PSP == plots[p])
  harvest <- PSP$Harvest[1]
  N.max <-  nrow(PSP) 
  
  pCensus <- names(PSP[,cn])[apply(PSP[,cn],2,sum,na.rm = T)>0] # possible census years
  
  ## This section checks for anomolously high recruitment, likely caused by census error
  cc <- apply(PSP[pCensus]>0, 2, sum, na.rm = T) %>% diff /N.max
  while(sum(cc*100 < -50)>0) {
    c.rm <- names(cc)[cc < -0.5]
    pCensus <- pCensus[-match(c.rm, pCensus)]
    if(length(pCensus)>1) {cc <- apply(PSP[pCensus]>0, 2, sum, na.rm = T) %>% diff /N.max} else {cc <- 1}
  }
  year.vec <- pCensus %>% substr(2,5) %>% as.numeric
  per.year <- year.vec %>% diff
  yr.delta <- pCensus[-1]
  
  AGB.data <- PSP[,pCensus] %>% data.frame
  BA.data <- BA[,pCensus] %>% data.frame
  DBH.data <- DBH[,pCensus] %>% data.frame
  
  stem <- apply(AGB.data, 2, function(x) !is.na(x))
  # stem.BA <- BA.data[,pCensus]
  N.psp <-  apply(stem, 2, sum) # density
  BA.sum <- apply(BA.data, 2, sum, na.rm = T)
  Nf.psp <-  apply(stem*PSP$Fixer, 2, sum, na.rm = T) # of fixers
  WD.psp <- apply(PSP$Density * stem, 2, gm.mean, na.rm = T)
  BA.fix <- apply(BA.data*PSP$Fixer, 2, sum, na.rm = T)
  pf.psp <-  Nf.psp/N.psp # prop fixers
  pf.BA.psp <- BA.fix/BA.sum
  SR.psp <- apply(stem, 2, function(x) length(unique(PSP$Spec.Code[x])))
  
  
  ## Recruit - mortality rates ##
  if (length(pCensus)==1) {
    R.r <- NA
    M.r <- NA
    born <- NA
    alive <- NA
    dead <- NA
    
    rgr <- NA
    BA.sum <- NA
    AGB <- NA
    AGB.delta <- NA
    R <- NA
    M <- NA
    
    
  } else if (length(pCensus)==2 & nrow(AGB.data) > 1) {
    
    RM <- apply(apply(AGB.data, 2, function(x) !is.na(x)), 1, diff) %>% data.frame
    RM.r <- apply(apply(RM, 2, function(x) x==1), 2, function(x) ifelse(x==1,1,NA)) %>% data.frame
    RM.m <- apply(apply(RM, 2, function(x) x==-1), 2, function(x) ifelse(x==1,1,NA)) %>% data.frame
    
    R.r <- (apply(RM.r,2,sum,na.rm = T)/sum(!is.na(AGB.data)[,-1]))/per.year
    born <- apply(RM.r,2,sum,na.rm = T)/per.year
    
    M.r <- (apply(RM.m,2,sum,na.rm = T)/sum(!is.na(AGB.data)[,-1]))/per.year
    dead <- apply(RM.m,2,sum,na.rm = T)/per.year
    alive <- sum(!is.na(AGB.data)[,-1])/per.year
    
    AGB <- sum(AGB.data[,-1], na.rm = T)
    rgr <- NA
    BA.sum  <- sum(BA.data[,-1], na.rm = T)
    
    RM.r[RM.r!=1] <- NA
    RM.m <- RM.m * -1
    AGB.delta <- apply(t(apply(AGB.data, 1, diff)), 2, sum, na.rm = T) %>% data.frame %>% t /per.year
    R <- apply(X = AGB.data[-1]*RM.r-min.biomass2, 2, sum, na.rm = T) %>% data.frame %>% t /per.year
    M <- apply(X = AGB.data[-ncol(AGB.data)]*RM.m, 2, sum, na.rm = T) %>% data.frame %>% t /per.year; colnames(M) <- yr.delta
    
  } else if (nrow(AGB.data) == 1) {
    
    RM <-  as.numeric(!is.na(AGB.data)) %>% diff
    
    RM.r <- RM; RM.r[RM!=1] <- 0
    RM.m <- RM; RM.m[RM!=-1] <- 0; RM.m <- RM.m*dd-1
    
    R.r <- (RM.r/!is.na(AGB.data[,-1]))/per.yeard
    born <- RM.r/per.year
    
    M.r <- (RM.m/!is.na(AGB.data[,-1]))/per.year
    dead <- RM.m/per.year
    alive <- !is.na(AGB.data[,-1])/per.year
    
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
    
    RM <- apply(apply(AGB.data, 2, function(x) !is.na(x)), 1, diff) %>% t
    RM.r <- apply(apply(RM, 2, function(x) x==1), 2, function(x) ifelse(x==1,1,NA))
    RM.m <- apply(apply(RM, 2, function(x) x==-1), 2, function(x) ifelse(x==1,1,NA))
    
    R.r <- (apply(RM.r,2,sum,na.rm = T)/apply(!is.na(AGB.data)[,-1], 2, sum))/per.year
    born <- apply(RM.r,2,sum,na.rm = T)/per.year
    
    M.r <- (apply(RM.m,2,sum,na.rm = T)/apply(!is.na(AGB.data)[,-1], 2, sum))/per.year
    dead <- apply(RM.m,2,sum,na.rm = T)/per.year
    alive <- apply(!is.na(AGB.data)[,-1], 2, sum)/per.year
    
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
    RM.df <- cbind.data.frame(Year = yr.delta %>% substr(2,5) %>% as.numeric,
                              PSP = plots[p],
                              harvest = harvest,
                              N.psp = N.psp[-1],
                              Nf.psp = Nf.psp[-1],
                              WD.psp = WD.psp[-1],
                              pf.psp = pf.psp[-1],
                              pf.BA.psp = pf.BA.psp[-1],
                              SR.psp = SR.psp[-1],
                              rgr = rgr,
                              AGB = AGB,
                              BA = BA.sum,
                              AGB.delta = AGB.delta %>% as.numeric,
                              R = R %>% as.numeric,
                              R.r = R.r %>% as.numeric,
                              born = born %>% as.numeric,
                              alive = alive %>% as.numeric,
                              M = M %>% as.numeric,
                              M.r = M.r %>% as.numeric,
                              dead = dead %>% as.numeric)
    
    
    
    dyndemo.psp.nobin <- rbind(dyndemo.psp.nobin, RM.df)}
  
  setTxtProgressBar(pb, p)
} # close PSP loop
close(pb)

write.csv(dyndemo.psp.nobin, "../Dynamics Paper 2019/Data/PSP_dynamics_no_bin.csv", row.names = F)


# Dataframe creation with Bin Category ------------------------------------

dyndemo.psp <- data.frame()
cn <- which(names(VMFR) %in% 'X1983'):dim(VMFR)[2]

plots <- unique(VMFR$PSP)
pb <- txtProgressBar(min = 1, max = length(plots), style = 3)
for (p in seq(length(plots))) {
  PSP <- VMFR %>% filter(PSP == plots[p])
  BA <- BA.VMFR %>% filter(PSP == plots[p])
  rgr.psp <- gm.mean(PSP$mRGR)*100
  harvest <- PSP$Harvest[1]
  N.max <-  nrow(PSP) 
  
  pCensus <- names(PSP[,cn])[apply(PSP[,cn],2,sum,na.rm = T)>0] # possible census years
  
  ## This section checks for anomolously high recruitment, likely caused by census error
  cc <- apply(PSP[pCensus]>0, 2, sum, na.rm = T) %>% diff /N.max
  while(sum(cc*100 < -50)>0) {
    c.rm <- names(cc)[cc < -0.5]
    pCensus <- pCensus[-match(c.rm, pCensus)]
    if(length(pCensus)>1) {cc <- apply(PSP[pCensus]>0, 2, sum, na.rm = T) %>% diff /N.max} else {cc <- 1}
  }
  year.vec <- pCensus %>% substr(2,5) %>% as.numeric
  per.year <- year.vec %>% diff
  
  
  AGB.data.psp <- PSP[,pCensus] %>% data.frame
  BA.data.psp <- BA[,pCensus] %>% data.frame
  stem <- apply(AGB.data.psp, 2, function(x) !is.na(x))
  stem.BA <- BA.data.psp[,pCensus]
  N.psp <-  apply(stem, 2, sum) # density
  BA.sum <- apply(stem.BA, 2, sum, na.rm = T)
  Nf.psp <-  apply(stem*PSP$Fixer, 2, sum, na.rm = T) # of fixers
  BA.fix <- apply(BA.data.psp*PSP$Fixer, 2, sum, na.rm = T)
  pf.psp <-  Nf.psp/N.psp # prop fixers
  pf.BA.psp <- BA.fix/BA.sum
  SR.psp <- apply(stem, 2, function(x) length(unique(PSP$Spec.Code[x])))
  
  
  bins <- unique(PSP$DBH_bin)
  for (b in seq(length(bins))) {
    psp.bin <- PSP %>% filter(DBH_bin == bins[b]) # filters by bin
    BA.bin <- BA %>% filter(DBH_bin == bins[b])
    rgr.b <- gm.mean(psp.bin$mRGR)*100 # relative growth rate at bin level
    yr.delta <- pCensus[-1]
    AGB.data <- psp.bin[,pCensus] %>% data.frame
    BA.data <- BA.bin[,pCensus] %>% data.frame
    
    if(nrow(AGB.data) == 1) {N.bin <- rep(1, times = ncol(AGB.data))} else {N.bin <-  apply(apply(AGB.data, 2, function(x) !is.na(x)), 2, sum)}
    
    ## Recruit - mortality rates ##
    if (length(pCensus)==1) {
      R.r <- NA
      M.r <- NA
      born <- NA
      alive <- NA
      dead <- NA
      
      rgr <- NA
      AGB <- NA
      AGB.delta <- NA
      BA.sum <- NA
      R <- NA
      M <- NA
      
      
    } else if (length(pCensus)==2 & nrow(psp.bin[pCensus]) > 1) {
      
      RM <- apply(apply(AGB.data, 2, function(x) !is.na(x)), 1, diff) %>% data.frame
      RM.r <- apply(apply(RM, 2, function(x) x==1), 2, function(x) ifelse(x==1,1,NA)) %>% data.frame
      RM.m <- apply(apply(RM, 2, function(x) x==-1), 2, function(x) ifelse(x==1,1,NA)) %>% data.frame
      
      R.r <- (apply(RM.r,2,sum,na.rm = T)/sum(!is.na(AGB.data)[,-1]))/per.year
      born <- apply(RM.r,2,sum,na.rm = T)/per.year
      
      M.r <- (apply(RM.m,2,sum,na.rm = T)/sum(!is.na(AGB.data)[,-1]))/per.year
      dead <- apply(RM.m,2,sum,na.rm = T)/per.year
      alive <- sum(!is.na(AGB.data)[,-1])/per.year
      
      AGB <- sum(AGB.data[,-1], na.rm = T)
      rgr <- NA
      BA.sum <- sum(BA.data[,-1], na.rm = T)
      
      RM.r[RM.r!=1] <- NA
      RM.m <- RM.m * -1
      AGB.delta <- apply(t(apply(AGB.data, 1, diff)), 2, sum, na.rm = T) %>% data.frame %>% t /per.year
      R <- apply(X = AGB.data[-1]*RM.r-min.biomass2, 2, sum, na.rm = T) %>% data.frame %>% t /per.year
      M <- apply(X = AGB.data[-ncol(AGB.data)]*RM.m, 2, sum, na.rm = T) %>% data.frame %>% t /per.year; colnames(M) <- yr.delta
      
    } else if (nrow(AGB.data) == 1) {
      
      RM <-  as.numeric(!is.na(AGB.data)) %>% diff
      
      RM.r <- RM; RM.r[RM!=1] <- 0
      RM.m <- RM; RM.m[RM!=-1] <- 0; RM.m <- RM.m*-1
      
      R.r <- (RM.r/!is.na(AGB.data[,-1]))/per.year
      born <- RM.r/per.year
      
      M.r <- (RM.m/!is.na(AGB.data[,-1]))/per.year
      dead <- RM.m/per.year
      alive <- !is.na(AGB.data[,-1])/per.year
      
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
      
      RM <- apply(apply(AGB.data, 2, function(x) !is.na(x)), 1, diff) %>% t
      RM.r <- apply(apply(RM, 2, function(x) x==1), 2, function(x) ifelse(x==1,1,NA))
      RM.m <- apply(apply(RM, 2, function(x) x==-1), 2, function(x) ifelse(x==1,1,NA))
      
      R.r <- (apply(RM.r,2,sum,na.rm = T)/apply(!is.na(AGB.data)[,-1], 2, sum))/per.year
      born <- apply(RM.r,2,sum,na.rm = T)/per.year
      
      M.r <- (apply(RM.m,2,sum,na.rm = T)/apply(!is.na(AGB.data)[,-1], 2, sum))/per.year
      dead <- apply(RM.m,2,sum,na.rm = T)/per.year
      alive <- apply(!is.na(AGB.data)[,-1], 2, sum)/per.year
      
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
    
    #
    ## Assembling the data frame
    
    if (length(yr.delta)>0) {
      RM.df <- cbind(Year = yr.delta %>% substr(2,5) %>% as.numeric,
                     PSP = plots[p],
                     bin = bins[b],
                     harvest = harvest,
                     N.psp = N.psp[-1],
                     N.bin = N.bin[-1],
                     Nf.psp = Nf.psp[-1],
                     pf.psp = pf.psp[-1],
                     pf.BA.psp = pf.BA.psp[-1],
                     SR.psp = SR.psp[-1],
                     rgr = rgr,
                     AGB = AGB,
                     BA = BA.sum,
                     AGB.delta = AGB.delta %>% as.numeric,
                     R = R %>% as.numeric,
                     R.r = R.r %>% as.numeric,
                     born = born %>% as.numeric,
                     alive = alive %>% as.numeric,
                     M = M %>% as.numeric,
                     M.r = M.r %>% as.numeric,
                     dead = dead %>% as.numeric)
      
      
      
      dyndemo.psp <- rbind(dyndemo.psp, RM.df)}
    
  } # close bin loop
  setTxtProgressBar(pb, p)
} # close PSP loop
close(pb)

write.csv(dyndemo.psp, "../Dynamics Paper 2019/Data/PSP_dynamics.csv", row.names = F)
