## Dynamics demographics dataframe creator at the subplot level ##

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


dyndemo.nobin.sp <- data.frame()
cn <- which(names(VMFR) %in% 'X1983'):dim(VMFR)[2]

plots <- unique(VMFR$PSP)
pb <- txtProgressBar(min = 1, max = length(plots), style = 3)
for (p in seq(length(plots))) {
  PSP <- VMFR %>% filter(PSP == plots[p])
  BA <- BA.VMFR %>% filter(PSP == plots[p])
  DBH <- DBH.VMFR %>% filter(PSP == plots[p])
  
  subplots <- unique(PSP$Su.Pl.No)
  for (sp in seq(length(subplots))) {
    SP <- PSP %>% filter(Su.Pl.No == subplots[sp]) # subplot filter
    SP.BA <- BA %>% filter(Su.Pl.No == subplots[sp])
    SP.DBH <- DBH %>% filter(Su.Pl.No == subplots[sp])
    harvest <- SP$Harvest[1]
    N.max <-  nrow(SP) 
    
    pCensus <- names(SP[,cn])[apply(SP[,cn],2,sum,na.rm = T)>0] # possible census years
    if (length(pCensus)==0) {next}
    
    ## This section checks for anomolously high recruitment, likely caused by census error
    cc <- apply(SP[,pCensus]>0, 2, sum, na.rm = T) %>% diff /N.max
    if (N.max > 5) {
      while(sum(cc*100 < -50)>0) {
        c.rm <- names(cc)[cc < -0.5]
        pCensus <- pCensus[-match(c.rm, pCensus)]
        if(length(pCensus)>1) {cc <- apply(SP[pCensus]>0, 2, sum, na.rm = T) %>% diff /N.max} else {cc <- 1}
      }
    }
    year.vec <- pCensus %>% substr(2,5) %>% as.numeric
    per.year <- year.vec %>% diff
    yr.delta <- pCensus[-1]
    AGB.data <- SP[,pCensus] %>% data.frame
    BA.data <- SP.BA[,pCensus] %>% data.frame
    DBH.data <- SP.DBH[,pCensus] %>% data.frame
    
    if (ncol(SP[,pCensus] %>% data.frame)==1) {
      stem.total <- apply(SP[,pCensus] %>% data.frame, 2, function(x) !is.na(x))} else if (nrow(SP[,pCensus])==1) {
        stem.total <- !is.na(SP[,pCensus])} else {
          stem.total <- apply(SP[,pCensus] %>% data.frame, 2, function(x) !is.na(x))}
    stem.total <- stem.total %>% data.frame
    
    
    N.sp <-  apply(stem.total, 2, sum) # density
    Nf.sp <-  apply(stem.total*SP$Fixer, 2, sum, na.rm = T) # of fixers
    pf.sp <-  Nf.sp/N.sp # prop fixers
    BA.fix <- BA.data*SP$Fixer
    SR.sp <- apply(stem.total, 2, function(x) length(unique(SP$Spec.Code[x])))
    
    
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
      rgr <- NA
      BA.sum  <- sum(BA.data[,-1], na.rm = T)
      pf.sp.BA  <- sum(BA.fix[,-1], na.rm = T)/BA.sum
      
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
      
      
      if (length(pCensus)==2) {
        AGB <- sum(AGB.data[,-1], na.rm = T)
        rgr <- NA
        BA.sum <- sum(BA.data[,-1], na.rm = T)
        pf.sp.BA  <- sum(BA.fix[,-1], na.rm = T)/BA.sum
        
      } else {
        AGB <- apply(AGB.data[,-1], 2, sum, na.rm = T)
        rgr <- apply(t(apply(log(DBH.data), 1, diff, na.rm = T))/per.year,
                     MARGIN = 2, 
                     FUN = mean, na.rm  = T) * 100
        BA.sum <- apply(BA.data[,-1], 2, sum, na.rm = T)
        pf.sp.BA  <- apply(BA.fix[,-1], 2, sum, na.rm = T)/BA.sum
      }
      
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
      pf.sp.BA  <- apply(BA.fix[,-1], 2, sum, na.rm = T)/BA.sum
      
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
                     Nf.sp = Nf.sp[-1],
                     pf.sp = pf.sp[-1],
                     pf.BA.sp = pf.sp.BA,
                     SR.sp = SR.sp[-1],
                     rgr.sp = rgr,
                     AGB = AGB,
                     BA = BA.sum,
                     AGB.delta = AGB.delta %>% as.numeric,
                     R = R %>% as.numeric,
                     R.r = R.r %>% as.numeric,
                     born = born %>% as.numeric,
                     M.r = M.r %>% as.numeric,
                     dead = dead %>% as.numeric,
                     alive = alive %>% as.numeric,
                     M = M %>% as.numeric)
      
      
      
      dyndemo.nobin.sp <- rbind(dyndemo.nobin.sp, RM.df)
    }
    
  } # close SP loop
  setTxtProgressBar(pb, p)
} # close PSP loop
close(pb)


write.csv(dyndemo.nobin.sp, "../Dynamics Paper 2019/Data/SP_dynamics_nobin.csv", row.names = F)



# Dataframe creation with Bin Category ------------------------------------


dyndemo.sp <- data.frame()
cn <- which(names(VMFR) %in% 'X1983'):dim(VMFR)[2]

plots <- unique(VMFR$PSP)
pb <- txtProgressBar(min = 1, max = length(plots), style = 3)
for (p in seq(length(plots))) {
  PSP <- VMFR %>% filter(PSP == plots[p])
  BA <- BA.VMFR %>% filter(PSP == plots[p])
  DBH <- DBH.VMFR %>% filter(PSP == plots[p])
  
  subplots <- unique(PSP$Su.Pl.No)
  for (sp in seq(length(subplots))) {
    SP <- PSP %>% filter(Su.Pl.No == subplots[sp]) # subplot filter
    SP.BA <- BA %>% filter(Su.Pl.No == subplots[sp])
    SP.DBH <- DBH %>% filter(Su.Pl.No == subplots[sp])
    harvest <- SP$Harvest[1]
    N.max <-  nrow(SP) 
    
    pCensus <- names(SP[,cn])[apply(SP[,cn],2,sum,na.rm = T)>0] # possible census years
    if (length(pCensus)==0) {next}
    
    ## This section checks for anomolously high recruitment, likely caused by census error
    cc <- apply(SP[,pCensus]>0, 2, sum, na.rm = T) %>% diff /N.max
    if (N.max > 5) {
      while(sum(cc*100 < -50)>0) {
        c.rm <- names(cc)[cc < -0.5]
        pCensus <- pCensus[-match(c.rm, pCensus)]
        if(length(pCensus)>1) {cc <- apply(SP[pCensus]>0, 2, sum, na.rm = T) %>% diff /N.max} else {cc <- 1}
      }
    }
    year.vec <- pCensus %>% substr(2,5) %>% as.numeric
    per.year <- year.vec %>% diff
    
    if (ncol(SP[,pCensus] %>% data.frame)==1) {
      stem.total <- apply(SP[,pCensus] %>% data.frame, 2, function(x) !is.na(x))} else if (nrow(SP[,pCensus])==1) {
        stem.total <- !is.na(SP[,pCensus])} else {
          stem.total <- apply(SP[,pCensus] %>% data.frame, 2, function(x) !is.na(x))}
    stem.total <- stem.total %>% data.frame
    
    
    if (ncol(SP.BA[,pCensus] %>% data.frame)==1) {
      stem.BA <- apply(SP.BA[,pCensus] %>% data.frame, 2, function(x) !is.na(x))} else if (nrow(SP.BA[,pCensus])==1) {
        stem.BA <- !is.na(SP.BA[,pCensus])} else {
          stem.BA <- apply(SP.BA[,pCensus] %>% data.frame, 2, function(x) !is.na(x))}
    stem.BA <- stem.BA %>% data.frame
    
    N.sp <-  apply(stem.total, 2, sum) # density
    BA.sp <- apply(stem.BA, 2, sum, na.rm = T)
    
    Nf.sp <-  apply(stem.total*SP$Fixer, 2, sum, na.rm = T) # of fixers
    Nf.sp.BA <- apply(stem.BA*SP.BA$Fixer, 2, sum, na.rm = T)
    
    pf.sp <-  Nf.sp/N.sp # prop fixers
    pf.sp.BA <- Nf.sp.BA/BA.sp
    
    SR.sp <- apply(stem.total, 2, function(x) length(unique(SP$Spec.Code[x])))
    
    
    bins <- unique(SP$DBH_bin)
    for (b in seq(length(bins))) {
      sp.bin <- SP %>% filter(DBH_bin == bins[b]) # filters by bin
      BA.bin <- SP.BA %>% filter(DBH_bin == bins[b])
      
      yr.delta <- pCensus[-1]
      AGB.data <- sp.bin[,pCensus] %>% data.frame
      BA.data <- sp.bin[,pCensus] %>% data.frame
      
      
      if(nrow(AGB.data) == 1) {N.bin <- rep(1, times = ncol(AGB.data))} else {N.bin <-  apply(apply(AGB.data, 2, function(x) !is.na(x)), 2, sum)}
      
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
        rgr <- NA
        BA.sum  <- sum(BA.data[,-1], na.rm = T)

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
        
        if (length(pCensus)==2) {
          
          AGB <- sum(AGB.data[,-1], na.rm = T)
          rgr <- NA
          BA.sum <- sum(BA.data[,-1], na.rm = T)
          
        } else {
          AGB <- apply(AGB.data[,-1], 2, sum, na.rm = T)
          rgr <- apply(t(apply(log(DBH.data), 1, diff, na.rm = T))/per.year,
                       MARGIN = 2, 
                       FUN = mean, na.rm  = T) * 100
          BA.sum <- apply(BA.data[,-1], 2, sum, na.rm = T)
        }
        
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
        RM.df <- cbind(Year = yr.delta %>% substr(2,5) %>% as.numeric,
                       PSP = plots[p],
                       SP = subplots[sp],
                       bin = bins[b],
                       harvest = harvest,
                       N.sp = N.sp[-1],
                       N.bin = N.bin[-1],
                       Nf.sp = Nf.sp[-1],
                       pf.sp = pf.sp[-1],
                       pf.BA.sp = pf.sp.BA[-1],
                       SR.sp = SR.sp[-1],
                       rgr = rgr,
                       AGB = AGB,
                       BA = BA.sum,
                       AGB.delta = AGB.delta %>% as.numeric,
                       R = R %>% as.numeric,
                       M = M %>% as.numeric,
                       R.r = R.r %>% as.numeric,
                       born = born %>% as.numeric,
                       M.r = M.r %>% as.numeric,
                       dead = dead %>% as.numeric,
                       alive = alive %>% as.numeric,
                       T.r = (R.r%>% as.numeric + M.r%>% as.numeric)/2)
        
        
        
        dyndemo.sp <- rbind(dyndemo.sp, RM.df)
      }
      
    } # close bin loop
  } # close SP loop
  setTxtProgressBar(pb, p)
} # close PSP loop
close(pb)





write.csv(dyndemo.sp, "../Dynamics Paper 2019/Data/SP_dynamics.csv", row.names = F)
