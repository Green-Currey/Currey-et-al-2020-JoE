# Chisholm plots non-density corrected

source('~/R/startup.R')
#c:/users/bryce/onedrive/documents/VMFR/Dynamics Paper 2019/Data/

# Start Up ------------------------------------------------


bad.plots.25 <- c(33, 34, 37, 39, 49, 53, 59, 69, 71, 58, 79)




line.range.plot.fix.nd.sp <- data.frame(bin = rep(c('<30','30-60','>60'), times = 3),
                                        slope = rep(0),
                                        upr = rep(0),
                                        lwr = rep(0),
                                        var = rep(c('Productivity', 'Mortality', 'Net Biomass'), each = 3),
                                        sig = rep(0))

line.range.plot.fix.dc.sp <- data.frame(bin = rep(c('<30','30-60','>60'), times = 3),
                                        slope = rep(0),
                                        upr = rep(0),
                                        lwr = rep(0),
                                        var = rep(c('Productivity', 'Mortality', 'Net Biomass'), each = 3),
                                        sig = rep(0))



line.range.plot.non.nd.sp <- data.frame(bin = rep(c('<30','30-60','>60'), times = 3),
                                        slope = rep(0),
                                        upr = rep(0),
                                        lwr = rep(0),
                                        var = rep(c('Productivity', 'Mortality', 'Net Biomass'), each = 3),
                                        sig = rep(0))

line.range.plot.non.dc.sp <- data.frame(bin = rep(c('<30','30-60','>60'), times = 3),
                                        slope = rep(0),
                                        upr = rep(0),
                                        lwr = rep(0),
                                        var = rep(c('Productivity', 'Mortality', 'Net Biomass'), each = 3),
                                        sig = rep(0))







line.range.plot.fix.nd <- data.frame(bin = rep(c('<30','30-60','>60'), times = 3),
                                     slope = rep(0),
                                     upr = rep(0),
                                     lwr = rep(0),
                                     var = rep(c('Productivity', 'Mortality', 'Net Biomass'), each = 3),
                                     sig = rep(0))

line.range.plot.fix.dc <- data.frame(bin = rep(c('<30','30-60','>60'), times = 3),
                                     slope = rep(0),
                                     upr = rep(0),
                                     lwr = rep(0),
                                     var = rep(c('Productivity', 'Mortality', 'Net Biomass'), each = 3),
                                     sig = rep(0))



line.range.plot.non.nd <- data.frame(bin = rep(c('<30','30-60','>60'), times = 3),
                                     slope = rep(0),
                                     upr = rep(0),
                                     lwr = rep(0),
                                     var = rep(c('Productivity', 'Mortality', 'Net Biomass'), each = 3),
                                     sig = rep(0))

line.range.plot.non.dc <- data.frame(bin = rep(c('<30','30-60','>60'), times = 3),
                                     slope = rep(0),
                                     upr = rep(0),
                                     lwr = rep(0),
                                     var = rep(c('Productivity', 'Mortality', 'Net Biomass'), each = 3),
                                     sig = rep(0))




dc <- function(data, density) {
        return(residuals(loess(data ~ density, span = 1, degree = 2)))
} # density correction (loess span = 1, k = 2)

dif <- function(uncorrected, corrected) {
        
        dif <- (uncorrected['slope'] - corrected['slope'])/uncorrected['slope']
        sig <- corrected['sig'] + uncorrected['sig']
        return(dif[sig!=0])
        
}





# SUBPLOT LEVEL ANALYSES --------------------------------------------------


# ---- --------------------------------------------------------------------

# Fixer influence on FIXER Productivity (bin and plot) -----------------------------------------

vmfr <- read.csv('Fixer_Nonfixer_dynamics_SP.csv') %>%
        filter(Fixer == 'fixer') %>%
        mutate(P = (AGB.delta + R)/1000) %>%
        filter(!is.na(P)) %>% 
        filter(PSP %!in% bad.plots.25) %>%
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


mod.nd <- summary(lmer(slnP ~ pf.sp.BA.total * bin + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.fix.nd.sp$slope[1] <- mod.nd$coefficients[2,1]
line.range.plot.fix.nd.sp$slope[2] <- mod.nd$coefficients[5,1] + mod.nd$coefficients[2,1]
line.range.plot.fix.nd.sp$slope[3] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]

mod.dc <- summary(lmer(slnPdc~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.fix.dc.sp$slope[1] <- mod.dc$coefficients[1,1]
line.range.plot.fix.dc.sp$slope[2] <- mod.dc$coefficients[4,1] + mod.dc$coefficients[1,1]
line.range.plot.fix.dc.sp$slope[3] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]


CI.nd <- confint(lmer(slnP ~ pf.sp.BA.total * bin + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.fix.nd.sp$upr[1] <- CI.nd[6,2]
line.range.plot.fix.nd.sp$upr[2] <- CI.nd[9,2] + CI.nd[6,2]
line.range.plot.fix.nd.sp$upr[3] <- CI.nd[10,2] + CI.nd[6,2]
line.range.plot.fix.nd.sp$lwr[1] <- CI.nd[6,1]
line.range.plot.fix.nd.sp$lwr[2] <- CI.nd[9,1] + CI.nd[6,1]
line.range.plot.fix.nd.sp$lwr[3] <- CI.nd[10,1] + CI.nd[6,1]

CI.dc <- confint(lmer(slnPdc ~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.fix.dc.sp$upr[1] <- CI.dc[5,2]
line.range.plot.fix.dc.sp$upr[2] <- CI.dc[8,2] + CI.dc[5,2]
line.range.plot.fix.dc.sp$upr[3] <- CI.nd[10,2] + CI.nd[6,2]
line.range.plot.fix.dc.sp$lwr[1] <- CI.dc[5,1]
line.range.plot.fix.dc.sp$lwr[2] <- CI.dc[8,1] + CI.dc[5,1]
line.range.plot.fix.dc.sp$lwr[3] <- CI.nd[10,1] + CI.nd[6,1]


line.range.plot.fix.nd.sp$sig[1] <- 1
line.range.plot.fix.nd.sp$sig[2] <- 1
line.range.plot.fix.nd.sp$sig[3] <- 1

line.range.plot.fix.dc.sp$sig[1] <- 1
line.range.plot.fix.dc.sp$sig[2] <- 1
line.range.plot.fix.dc.sp$sig[3] <- 1


# ---- --------------------------------------------------------------------


# Fixer influence on FIXER incremental (bin and plot) -----------------------------------------

vmfr <- read.csv('Fixer_Nonfixer_dynamics_SP.csv') %>%
        filter(Fixer == 'fixer') %>%
        mutate(I = (AGB.delta + R + M)/1000) %>%
        filter(!is.na(I)) %>% 
        filter(PSP %!in% bad.plots.25) %>%
        group_by(bin) %>%
        filter(I > mean(I) - 2*sd(I) & I < mean(I) + 2*sd(I)) %>%
        mutate(sI = scale(I)) %>%
        mutate(sIdc = dc(sI,N.bin.sp)) %>%
        ungroup(bin) %>%
        filter(N.bin.sp > 0) %>%
        mutate(bin = factor(bin,  labels = c('<30 cm',
                                             "30-60 cm",
                                             ">60 cm")))


mod.nd <- summary(lmer(sI ~ pf.sp.BA.total * bin + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.fix.nd.sp$slope[7] <- mod.nd$coefficients[2,1]
line.range.plot.fix.nd.sp$slope[8] <- mod.nd$coefficients[5,1] + mod.nd$coefficients[2,1]
line.range.plot.fix.nd.sp$slope[9] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]

mod.dc <- summary(lmer(sIdc ~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.fix.dc.sp$slope[7] <- mod.dc$coefficients[1,1]
line.range.plot.fix.dc.sp$slope[8] <- mod.dc$coefficients[4,1] + mod.dc$coefficients[1,1]
line.range.plot.fix.dc.sp$slope[9] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]


CI.nd <- confint(lmer(sI ~ pf.sp.BA.total * bin + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.fix.nd.sp$upr[7] <- CI.nd[6,2]
line.range.plot.fix.nd.sp$upr[8] <- CI.nd[9,2] + CI.nd[6,2]
line.range.plot.fix.nd.sp$upr[9] <- CI.nd[10,2] + CI.nd[6,2]
line.range.plot.fix.nd.sp$lwr[7] <- CI.nd[6,1]
line.range.plot.fix.nd.sp$lwr[8] <- CI.nd[9,1] + CI.nd[6,1]
line.range.plot.fix.nd.sp$lwr[9] <- CI.nd[10,1] + CI.nd[6,1]

CI.dc <- confint(lmer(sIdc ~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.fix.dc.sp$upr[7] <- CI.dc[5,2]
line.range.plot.fix.dc.sp$upr[8] <- CI.dc[8,2] + CI.dc[5,2]
line.range.plot.fix.dc.sp$upr[9] <- CI.nd[10,2] + CI.nd[6,2]
line.range.plot.fix.dc.sp$lwr[7] <- CI.dc[5,1]
line.range.plot.fix.dc.sp$lwr[8] <- CI.dc[8,1] + CI.dc[5,1]
line.range.plot.fix.dc.sp$lwr[9] <- CI.nd[10,1] + CI.nd[6,1]


line.range.plot.fix.nd.sp$sig[7] <- 1
line.range.plot.fix.nd.sp$sig[8] <- 1
line.range.plot.fix.nd.sp$sig[9] <- 1

line.range.plot.fix.dc.sp$sig[7] <- 1
line.range.plot.fix.dc.sp$sig[8] <- 0
line.range.plot.fix.dc.sp$sig[9] <- 1


# ---- --------------------------------------------------------------------


# Fixer influence on FIXER mortality (bin and plot) -----------------------------------------

vmfr <- read.csv('Fixer_Nonfixer_dynamics_SP.csv') %>%
        filter(Fixer == 'fixer') %>%
        mutate(M = M/1000 * -1) %>%
        filter(!is.na(M)) %>% 
        filter(PSP %!in% bad.plots.25) %>%
        group_by(bin) %>%
        filter(M > mean(M) - 2*sd(M) & M < mean(M) + 2*sd(M)) %>%
        mutate(lnM = log(M+0.01)) %>%
        filter(!is.na(lnM)) %>%
        mutate(slnM = scale(lnM)) %>%
        mutate(slnMdc = dc(slnM,N.bin.sp)) %>%
        ungroup(bin) %>%
        filter(N.bin.sp > 0) %>%
        mutate(bin = factor(bin,  labels = c('<30 cm',
                                             "30-60 cm",
                                             ">60 cm")))


mod.nd <- summary(lmer(slnM ~ pf.sp.BA.total * bin + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.fix.nd.sp$slope[4] <- mod.nd$coefficients[2,1]
line.range.plot.fix.nd.sp$slope[5] <- mod.nd$coefficients[5,1] + mod.nd$coefficients[2,1]
line.range.plot.fix.nd.sp$slope[6] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]

mod.dc <- summary(lmer(slnMdc ~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.fix.dc.sp$slope[4] <- mod.dc$coefficients[1,1]
line.range.plot.fix.dc.sp$slope[5] <- mod.dc$coefficients[4,1] + mod.dc$coefficients[1,1]
line.range.plot.fix.dc.sp$slope[6] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]


CI.nd <- confint(lmer(slnM ~ pf.sp.BA.total * bin + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.fix.nd.sp$upr[4] <- CI.nd[6,2]
line.range.plot.fix.nd.sp$upr[5] <- CI.nd[9,2] + CI.nd[6,2]
line.range.plot.fix.nd.sp$upr[6] <- CI.nd[10,2] + CI.nd[6,2]
line.range.plot.fix.nd.sp$lwr[4] <- CI.nd[6,1]
line.range.plot.fix.nd.sp$lwr[5] <- CI.nd[9,1] + CI.nd[6,1]
line.range.plot.fix.nd.sp$lwr[6] <- CI.nd[10,1] + CI.nd[6,1]

CI.dc <- confint(lmer(slnMdc ~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.fix.dc.sp$upr[4] <- CI.dc[5,2]
line.range.plot.fix.dc.sp$upr[5] <- CI.dc[8,2] + CI.dc[5,2]
line.range.plot.fix.dc.sp$upr[6] <- CI.nd[10,2] + CI.nd[6,2]
line.range.plot.fix.dc.sp$lwr[4] <- CI.dc[5,1]
line.range.plot.fix.dc.sp$lwr[5] <- CI.dc[8,1] + CI.dc[5,1]
line.range.plot.fix.dc.sp$lwr[6] <- CI.nd[10,1] + CI.nd[6,1]


line.range.plot.fix.nd.sp$sig[4] <- 0
line.range.plot.fix.nd.sp$sig[5] <- 0
line.range.plot.fix.nd.sp$sig[6] <- 1

line.range.plot.fix.dc.sp$sig[4] <- 0
line.range.plot.fix.dc.sp$sig[5] <- 0
line.range.plot.fix.dc.sp$sig[6] <- 1


# ---- --------------------------------------------------------------------


# Fixer plots  -------------------------------------------
line.range.plot.fix.nd.sp <- line.range.plot.fix.nd.sp %>% 
        mutate(bin = factor(bin, levels = c('<30', '30-60', '>60'))) %>%
        mutate(var = factor(var, levels = c('Productivity', 'Mortality', 'Net Biomass')))

line.range.plot.fix.dc.sp <- line.range.plot.fix.dc.sp %>% 
        mutate(bin = factor(bin, levels = c('<30', '30-60', '>60'))) %>%
        mutate(var = factor(var, levels = c('Productivity', 'Mortality', 'Net Biomass')))

## ~~~~~ Line Plots ~~~~~##

lp.fix.sp <- ggplot() +
        geom_linerange(data = line.range.plot.fix.nd.sp, 
                       aes(x = bin, ymin = lwr, ymax = upr, group = bin),
                       linetype = 'solid', color = 'grey50', size = 4, alpha = 0.4) +
        
        geom_pointrange(data = line.range.plot.fix.dc.sp, 
                        aes(x = bin, y = slope, ymin = lwr, ymax = upr, group = bin, shape = factor(sig)),
                        linetype = 'solid', color = 'red', fatten = 3, size = 1.5, stroke = 1.1) +
        
        facet_grid(.~var) +
        geom_hline(yintercept = 0, linetype = 'longdash', color = 'grey50') +
        scale_shape_manual(values = c(1,16)) +
        # scale_y_continuous(limits = c(-2,2), breaks = c(-2, 0, 2)) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              axis.text.y = element_text(size = 22),
              axis.text.x = element_text(size = 18),
              axis.title = element_text(size = 22),
              strip.background = element_rect(fill = 'white', color = 'black', size = 1),
              strip.text = element_text(size = 22),
              legend.position = 'none') +
        labs(x = '', y = '')


# ---- --------------------------------------------------------------------


# Fixer influence on NONFIXER Productivity (bin and plot) -----------------------------------------

vmfr <- read.csv('Fixer_Nonfixer_dynamics_SP.csv') %>%
        filter(Fixer == 'nonfixer') %>%
        mutate(P = (AGB.delta + R)/1000) %>%
        filter(!is.na(P)) %>% 
        filter(PSP %!in% bad.plots.25) %>%
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


mod.nd <- summary(lmer(slnP ~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.non.nd.sp$slope[1] <- mod.nd$coefficients[2,1]
line.range.plot.non.nd.sp$slope[2] <- mod.nd$coefficients[5,1] + mod.nd$coefficients[2,1]
line.range.plot.non.nd.sp$slope[3] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]

mod.dc <- summary(lmer(slnPdc ~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.non.dc.sp$slope[1] <- mod.dc$coefficients[1,1]
line.range.plot.non.dc.sp$slope[2] <- mod.dc$coefficients[5,1] + mod.dc$coefficients[1,1]
line.range.plot.non.dc.sp$slope[3] <- mod.dc$coefficients[6,1] + mod.dc$coefficients[1,1]


CI.nd <- confint(lmer(slnP ~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.non.nd.sp$upr[1] <- CI.nd[6,2]
line.range.plot.non.nd.sp$upr[2] <- CI.nd[9,2] + CI.nd[6,2]
line.range.plot.non.nd.sp$upr[3] <- CI.nd[10,2] + CI.nd[6,2]
line.range.plot.non.nd.sp$lwr[1] <- CI.nd[6,1]
line.range.plot.non.nd.sp$lwr[2] <- CI.nd[9,1] + CI.nd[6,1]
line.range.plot.non.nd.sp$lwr[3] <- CI.nd[10,1] + CI.nd[6,1]

CI.dc <- confint(lmer(slnPdc ~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.non.dc.sp$upr[1] <- CI.dc[5,2]
line.range.plot.non.dc.sp$upr[2] <- CI.dc[9,2] + CI.dc[5,2]
line.range.plot.non.dc.sp$upr[3] <- CI.dc[10,2] + CI.dc[5,2]
line.range.plot.non.dc.sp$lwr[1] <- CI.dc[5,1]
line.range.plot.non.dc.sp$lwr[2] <- CI.dc[9,1] + CI.dc[5,1]
line.range.plot.non.dc.sp$lwr[3] <- CI.dc[10,1] + CI.dc[5,1]


line.range.plot.non.nd.sp$sig[1] <- 1
line.range.plot.non.nd.sp$sig[2] <- 1
line.range.plot.non.nd.sp$sig[3] <- 1

line.range.plot.non.dc.sp$sig[1] <- 1
line.range.plot.non.dc.sp$sig[2] <- 1
line.range.plot.non.dc.sp$sig[3] <- 1




# ---- --------------------------------------------------------------------


# Fixer influence on NONFIXER incremental (bin and plot) -----------------------------------------

vmfr <- read.csv('Fixer_Nonfixer_dynamics_SP.csv') %>%
        filter(Fixer == 'nonfixer') %>%
        mutate(I = (AGB.delta + R + M)/1000) %>%
        filter(!is.na(I)) %>% 
        filter(PSP %!in% bad.plots.25) %>%
        group_by(bin) %>%
        filter(I > mean(I) - 2*sd(I) & I < mean(I) + 2*sd(I)) %>%
        mutate(sI = scale(I)) %>%
        mutate(sIdc = dc(sI,N.bin.sp)) %>%
        ungroup(bin) %>%
        filter(N.bin.sp > 0) %>%
        mutate(bin = factor(bin,  labels = c('<30 cm',
                                             "30-60 cm",
                                             ">60 cm")))


mod.nd <- summary(lmer(sI ~ pf.sp.BA.total * bin + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.non.nd.sp$slope[7] <- mod.nd$coefficients[2,1]
line.range.plot.non.nd.sp$slope[8] <- mod.nd$coefficients[5,1] + mod.nd$coefficients[2,1]
line.range.plot.non.nd.sp$slope[9] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]

mod.dc <- summary(lmer(sIdc ~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.non.dc.sp$slope[7] <- mod.dc$coefficients[1,1]
line.range.plot.non.dc.sp$slope[8] <- mod.dc$coefficients[5,1] + mod.dc$coefficients[1,1]
line.range.plot.non.dc.sp$slope[9] <- mod.dc$coefficients[6,1] + mod.dc$coefficients[1,1]


CI.nd <- confint(lmer(sI ~ pf.sp.BA.total * bin + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.non.nd.sp$upr[7] <- CI.nd[6,2]
line.range.plot.non.nd.sp$upr[8] <- CI.nd[9,2] + CI.nd[6,2]
line.range.plot.non.nd.sp$upr[9] <- CI.nd[10,2] + CI.nd[6,2]
line.range.plot.non.nd.sp$lwr[7] <- CI.nd[6,1]
line.range.plot.non.nd.sp$lwr[8] <- CI.nd[9,1] + CI.nd[6,1]
line.range.plot.non.nd.sp$lwr[9] <- CI.nd[10,1] + CI.nd[6,1]

CI.dc <- confint(lmer(sIdc ~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.non.dc.sp$upr[7] <- CI.dc[5,2]
line.range.plot.non.dc.sp$upr[8] <- CI.dc[9,2] + CI.dc[5,2]
line.range.plot.non.dc.sp$upr[9] <- CI.dc[10,2] + CI.dc[5,2]
line.range.plot.non.dc.sp$lwr[7] <- CI.dc[5,1]
line.range.plot.non.dc.sp$lwr[8] <- CI.dc[9,1] + CI.dc[5,1]
line.range.plot.non.dc.sp$lwr[9] <- CI.dc[10,1] + CI.dc[5,1]


line.range.plot.non.nd.sp$sig[7] <- 0
line.range.plot.non.nd.sp$sig[8] <- 0
line.range.plot.non.nd.sp$sig[9] <- 1

line.range.plot.non.dc.sp$sig[7] <- 0
line.range.plot.non.dc.sp$sig[8] <- 0
line.range.plot.non.dc.sp$sig[9] <- 1



# ---- --------------------------------------------------------------------


# Fixer influence on NONFIXER mortality (bin and plot) -----------------------------------------

vmfr <- read.csv('Fixer_Nonfixer_dynamics_SP.csv') %>%
        filter(Fixer == 'nonfixer') %>%
        mutate(M = M/1000 * -1) %>%
        filter(!is.na(M)) %>% 
        filter(PSP %!in% bad.plots.25) %>%
        group_by(bin) %>%
        filter(M > mean(M) - 2*sd(M) & M < mean(M) + 2*sd(M)) %>%
        mutate(lnM = log(M+0.01)) %>%
        filter(!is.na(lnM)) %>%
        mutate(slnM = scale(lnM)) %>%
        mutate(slnMdc = dc(slnM,N.bin.sp)) %>%
        ungroup(bin) %>%
        filter(N.bin.sp > 0) %>%
        mutate(bin = factor(bin,  labels = c('<30 cm',
                                             "30-60 cm",
                                             ">60 cm")))

mod.nd <- summary(lmer(slnM ~ pf.sp.BA.total * bin + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.non.nd.sp$slope[4] <- mod.nd$coefficients[2,1]
line.range.plot.non.nd.sp$slope[5] <- mod.nd$coefficients[5,1] + mod.nd$coefficients[2,1]
line.range.plot.non.nd.sp$slope[6] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]

mod.dc <- summary(lmer(slnMdc ~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.non.dc.sp$slope[4] <- mod.dc$coefficients[1,1]
line.range.plot.non.dc.sp$slope[5] <- mod.dc$coefficients[5,1] + mod.dc$coefficients[1,1]
line.range.plot.non.dc.sp$slope[6] <- mod.dc$coefficients[6,1] + mod.dc$coefficients[1,1]


CI.nd <- confint(lmer(slnM ~ pf.sp.BA.total * bin + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.non.nd.sp$upr[4] <- CI.nd[6,2]
line.range.plot.non.nd.sp$upr[5] <- CI.nd[9,2] + CI.nd[6,2]
line.range.plot.non.nd.sp$upr[6] <- CI.nd[10,2] + CI.nd[6,2]
line.range.plot.non.nd.sp$lwr[4] <- CI.nd[6,1]
line.range.plot.non.nd.sp$lwr[5] <- CI.nd[9,1] + CI.nd[6,1]
line.range.plot.non.nd.sp$lwr[6] <- CI.nd[10,1] + CI.nd[6,1]

CI.dc <- confint(lmer(slnMdc ~ pf.sp.BA.total * bin - 1 + (1|PSP/SP) + (1|Year), data = vmfr))
line.range.plot.non.dc.sp$upr[4] <- CI.dc[5,2]
line.range.plot.non.dc.sp$upr[5] <- CI.dc[9,2] + CI.dc[5,2]
line.range.plot.non.dc.sp$upr[6] <- CI.dc[10,2] + CI.dc[5,2]
line.range.plot.non.dc.sp$lwr[4] <- CI.dc[5,1]
line.range.plot.non.dc.sp$lwr[5] <- CI.dc[9,1] + CI.dc[5,1]
line.range.plot.non.dc.sp$lwr[6] <- CI.dc[10,1] + CI.dc[5,1]


line.range.plot.non.nd.sp$sig[4] <- 0
line.range.plot.non.nd.sp$sig[5] <- 0
line.range.plot.non.nd.sp$sig[6] <- 1

line.range.plot.non.dc.sp$sig[4] <- 0
line.range.plot.non.dc.sp$sig[5] <- 0
line.range.plot.non.dc.sp$sig[6] <- 1



# ---- --------------------------------------------------------------------


# Nonfixer Plots  -------------------------------------------

line.range.plot.non.nd.sp <- line.range.plot.non.nd.sp %>% 
        mutate(bin = factor(bin, levels = c('<30', '30-60', '>60'))) %>%
        mutate(var = factor(var, levels = c('Productivity', 'Mortality', 'Net Biomass')))

line.range.plot.non.dc.sp <- line.range.plot.non.dc.sp %>% 
        mutate(bin = factor(bin, levels = c('<30', '30-60', '>60'))) %>%
        mutate(var = factor(var, levels = c('Productivity', 'Mortality', 'Net Biomass')))

## ~~~~~ Line Plots ~~~~~##

lp.non.sp <- ggplot() +
        geom_linerange(data = line.range.plot.non.nd.sp, 
                       aes(x = bin, ymin = lwr, ymax = upr, group = bin),
                       linetype = 'solid', color = 'grey50', size = 4, alpha = 0.4) +
        
        geom_pointrange(data = line.range.plot.non.dc.sp, 
                        aes(x = bin, y = slope, ymin = lwr, ymax = upr, group = bin, shape = factor(sig)),
                        linetype = 'solid', color = 'blue', fatten = 3, size = 1.5, stroke = 1.1) +
        
        facet_grid(.~var) +
        geom_hline(yintercept = 0, linetype = 'longdash', color = 'grey50') +
        scale_shape_manual(values = c(1,16)) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              axis.text.y = element_text(size = 22),
              axis.text.x = element_text(size = 18),
              axis.title = element_text(size = 22),
              strip.background = element_rect(fill = 'white', color = 'black', size = 1),
              strip.text = element_text(size = 22),
              legend.position = 'none') +
        labs(x = '', y = '')




# ---- --------------------------------------------------------------------

# PLOT LEVEL ANALYSES -----------------------------------------------------
# ---- --------------------------------------------------------------------

# Fixer influence on FIXER Productivity (bin and plot) -----------------------------------------

vmfr <- read.csv('Fixer_Nonfixer_dynamics.csv') %>%
        filter(Fixer == 'fixer') %>%
        mutate(P = (AGB.delta + R)/1000) %>%
        filter(!is.na(P)) %>% 
        filter(PSP %!in% bad.plots.25) %>%
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


mod.nd <- summary(lmer(slnP ~ pf.BA.total * bin + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.fix.nd$slope[1] <- mod.nd$coefficients[2,1]
line.range.plot.fix.nd$slope[2] <- mod.nd$coefficients[5,1] + mod.nd$coefficients[2,1]
line.range.plot.fix.nd$slope[3] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]

mod.dc <- summary(lmer(slnPdc~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.fix.dc$slope[1] <- mod.dc$coefficients[1,1]
line.range.plot.fix.dc$slope[2] <- mod.dc$coefficients[5,1] + mod.dc$coefficients[1,1]
line.range.plot.fix.dc$slope[3] <- mod.dc$coefficients[6,1] + mod.dc$coefficients[1,1]


CI.nd <- confint(lmer(slnP ~ pf.BA.total * bin + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.fix.nd$upr[1] <- CI.nd[5,2]
line.range.plot.fix.nd$upr[2] <- CI.nd[8,2] + CI.nd[5,2]
line.range.plot.fix.nd$upr[3] <- CI.nd[9,2] + CI.nd[5,2]
line.range.plot.fix.nd$lwr[1] <- CI.nd[5,1]
line.range.plot.fix.nd$lwr[2] <- CI.nd[8,1] + CI.nd[5,1]
line.range.plot.fix.nd$lwr[3] <- CI.nd[9,1] + CI.nd[5,1]

CI.dc <- confint(lmer(slnPdc ~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.fix.dc$upr[1] <- CI.dc[4,2]
line.range.plot.fix.dc$upr[2] <- CI.dc[8,2] + CI.dc[4,2]
line.range.plot.fix.dc$upr[3] <- CI.dc[9,2] + CI.dc[4,2]
line.range.plot.fix.dc$lwr[1] <- CI.dc[4,1]
line.range.plot.fix.dc$lwr[2] <- CI.dc[8,1] + CI.dc[4,1]
line.range.plot.fix.dc$lwr[3] <- CI.dc[9,1] + CI.dc[4,1]


line.range.plot.fix.nd$sig[1] <- 1
line.range.plot.fix.nd$sig[2] <- 1
line.range.plot.fix.nd$sig[3] <- 0

line.range.plot.fix.dc$sig[1] <- 0
line.range.plot.fix.dc$sig[2] <- 0
line.range.plot.fix.dc$sig[3] <- 0


# ---- --------------------------------------------------------------------


# Fixer influence on FIXER incremental (bin and plot) -----------------------------------------

vmfr <- read.csv('Fixer_Nonfixer_dynamics.csv') %>%
        filter(Fixer == 'fixer') %>%
        mutate(I = (AGB.delta + R + M)/1000) %>%
        filter(!is.na(I)) %>% 
        filter(PSP %!in% bad.plots.25) %>%
        group_by(bin) %>%
        filter(I > mean(I) - 2*sd(I) & I < mean(I) + 2*sd(I)) %>%
        mutate(sI = scale(I)) %>%
        mutate(sIdc = dc(sI,N.bin)) %>%
        ungroup(bin) %>%
        filter(N.bin > 0) %>%
        mutate(bin = factor(bin,  labels = c('<30 cm',
                                             "30-60 cm",
                                             ">60 cm")))


mod.nd <- summary(lmer(sI ~ pf.BA.total * bin + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.fix.nd$slope[7] <- mod.nd$coefficients[2,1]
line.range.plot.fix.nd$slope[8] <- mod.nd$coefficients[5,1] + mod.nd$coefficients[2,1]
line.range.plot.fix.nd$slope[9] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]

mod.dc <- summary(lmer(sIdc ~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.fix.dc$slope[7] <- mod.dc$coefficients[1,1]
line.range.plot.fix.dc$slope[8] <- mod.dc$coefficients[5,1] + mod.dc$coefficients[1,1]
line.range.plot.fix.dc$slope[9] <- mod.dc$coefficients[6,1] + mod.dc$coefficients[1,1]


CI.nd <- confint(lmer(sI ~ pf.BA.total * bin + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.fix.nd$upr[7] <- CI.nd[5,2]
line.range.plot.fix.nd$upr[8] <- CI.nd[8,2] + CI.nd[5,2]
line.range.plot.fix.nd$upr[9] <- CI.nd[9,2] + CI.nd[5,2]
line.range.plot.fix.nd$lwr[7] <- CI.nd[5,1]
line.range.plot.fix.nd$lwr[8] <- CI.nd[8,1] + CI.nd[5,1]
line.range.plot.fix.nd$lwr[9] <- CI.nd[9,1] + CI.nd[5,1]

CI.dc <- confint(lmer(sIdc ~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.fix.dc$upr[7] <- CI.dc[4,2]
line.range.plot.fix.dc$upr[8] <- CI.dc[8,2] + CI.dc[4,2]
line.range.plot.fix.dc$upr[9] <- CI.dc[9,2] + CI.dc[4,2]
line.range.plot.fix.dc$lwr[7] <- CI.dc[4,1]
line.range.plot.fix.dc$lwr[8] <- CI.dc[8,1] + CI.dc[4,1]
line.range.plot.fix.dc$lwr[9] <- CI.dc[9,1] + CI.dc[4,1]


line.range.plot.fix.nd$sig[7] <- 1
line.range.plot.fix.nd$sig[8] <- 0
line.range.plot.fix.nd$sig[9] <- 0

line.range.plot.fix.dc$sig[7] <- 0
line.range.plot.fix.dc$sig[8] <- 0
line.range.plot.fix.dc$sig[9] <- 0


# ---- --------------------------------------------------------------------


# Fixer influence on FIXER mortality (bin and plot) -----------------------------------------

vmfr <- read.csv('Fixer_Nonfixer_dynamics.csv') %>%
        filter(Fixer == 'fixer') %>%
        mutate(M = M/1000 * -1) %>%
        filter(!is.na(M)) %>% 
        filter(PSP %!in% bad.plots.25) %>%
        group_by(bin) %>%
        filter(M > mean(M) - 2*sd(M) & M < mean(M) + 2*sd(M)) %>%
        mutate(lnM = log(M+0.01)) %>%
        filter(!is.na(lnM)) %>%
        mutate(slnM = scale(lnM)) %>%
        mutate(slnMdc = dc(slnM,N.bin)) %>%
        ungroup(bin) %>%
        filter(N.bin > 0) %>%
        mutate(bin = factor(bin,  labels = c('<30 cm',
                                             "30-60 cm",
                                             ">60 cm")))


mod.nd <- summary(lmer(slnM ~ pf.BA.total * bin + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.fix.nd$slope[4] <- mod.nd$coefficients[2,1]
line.range.plot.fix.nd$slope[5] <- mod.nd$coefficients[5,1] + mod.nd$coefficients[2,1]
line.range.plot.fix.nd$slope[6] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]

mod.dc <- summary(lmer(slnMdc ~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.fix.dc$slope[4] <- mod.dc$coefficients[1,1]
line.range.plot.fix.dc$slope[5] <- mod.dc$coefficients[5,1] + mod.dc$coefficients[1,1]
line.range.plot.fix.dc$slope[6] <- mod.dc$coefficients[6,1] + mod.dc$coefficients[1,1]


CI.nd <- confint(lmer(slnM ~ pf.BA.total * bin + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.fix.nd$upr[4] <- CI.nd[5,2]
line.range.plot.fix.nd$upr[5] <- CI.nd[8,2] + CI.nd[5,2]
line.range.plot.fix.nd$upr[6] <- CI.nd[9,2] + CI.nd[5,2]
line.range.plot.fix.nd$lwr[4] <- CI.nd[5,1]
line.range.plot.fix.nd$lwr[5] <- CI.nd[8,1] + CI.nd[5,1]
line.range.plot.fix.nd$lwr[6] <- CI.nd[9,1] + CI.nd[5,1]

CI.dc <- confint(lmer(slnMdc ~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.fix.dc$upr[4] <- CI.dc[4,2]
line.range.plot.fix.dc$upr[5] <- CI.dc[8,2] + CI.dc[4,2]
line.range.plot.fix.dc$upr[6] <- CI.dc[9,2] + CI.dc[4,2]
line.range.plot.fix.dc$lwr[4] <- CI.dc[4,1]
line.range.plot.fix.dc$lwr[5] <- CI.dc[8,1] + CI.dc[4,1]
line.range.plot.fix.dc$lwr[6] <- CI.dc[9,1] + CI.dc[4,1]


line.range.plot.fix.nd$sig[4] <- 1
line.range.plot.fix.nd$sig[5] <- 1
line.range.plot.fix.nd$sig[6] <- 0

line.range.plot.fix.dc$sig[4] <- 0
line.range.plot.fix.dc$sig[5] <- 0
line.range.plot.fix.dc$sig[6] <- 0


# ---- --------------------------------------------------------------------


# Fixer plots  -------------------------------------------
line.range.plot.fix.nd <- line.range.plot.fix.nd %>% 
        mutate(bin = factor(bin, levels = c('<30', '30-60', '>60'))) %>%
        mutate(var = factor(var, levels = c('Productivity', 'Mortality', 'Net Biomass')))

line.range.plot.fix.dc <- line.range.plot.fix.dc %>% 
        mutate(bin = factor(bin, levels = c('<30', '30-60', '>60'))) %>%
        mutate(var = factor(var, levels = c('Productivity', 'Mortality', 'Net Biomass')))

## ~~~~~ Line Plots ~~~~~##

lp.fix.psp <- ggplot() +
        geom_linerange(data = line.range.plot.fix.nd, 
                       aes(x = bin, ymin = lwr, ymax = upr, group = bin),
                       linetype = 'solid', color = 'grey50', size = 4, alpha = 0.4) +
        
        geom_pointrange(data = line.range.plot.fix.dc, 
                        aes(x = bin, y = slope, ymin = lwr, ymax = upr, group = bin, shape = factor(sig)),
                        linetype = 'solid', color = 'red', fatten = 3, size = 1.5, stroke = 1.1) +
        
        facet_grid(.~var) +
        geom_hline(yintercept = 0, linetype = 'longdash', color = 'grey50') +
        scale_shape_manual(values = c(1,16)) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              axis.text.y = element_text(size = 22),
              axis.text.x = element_text(size = 18),
              axis.title = element_text(size = 22),
              strip.background = element_rect(fill = 'white', color = 'black', size = 1),
              strip.text = element_text(size = 22),
              legend.position = 'none') +
        labs(x = '', y = '')


# ---- --------------------------------------------------------------------


# Fixer influence on NONFIXER Productivity (bin and plot) -----------------------------------------

vmfr <- read.csv('Fixer_Nonfixer_dynamics.csv') %>%
        filter(Fixer == 'nonfixer') %>%
        mutate(P = (AGB.delta + R)/1000) %>%
        filter(!is.na(P)) %>% 
        filter(PSP %!in% bad.plots.25) %>%
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


mod.nd <- summary(lmer(slnP ~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.non.nd$slope[1] <- mod.nd$coefficients[2,1]
line.range.plot.non.nd$slope[2] <- mod.nd$coefficients[5,1] + mod.nd$coefficients[2,1]
line.range.plot.non.nd$slope[3] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]

mod.dc <- summary(lmer(slnPdc ~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.non.dc$slope[1] <- mod.dc$coefficients[1,1]
line.range.plot.non.dc$slope[2] <- mod.dc$coefficients[5,1] + mod.dc$coefficients[1,1]
line.range.plot.non.dc$slope[3] <- mod.dc$coefficients[6,1] + mod.dc$coefficients[1,1]


CI.nd <- confint(lmer(slnP ~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.non.nd$upr[1] <- CI.nd[5,2]
line.range.plot.non.nd$upr[2] <- CI.nd[8,2] + CI.nd[5,2]
line.range.plot.non.nd$upr[3] <- CI.nd[9,2] + CI.nd[5,2]
line.range.plot.non.nd$lwr[1] <- CI.nd[5,1]
line.range.plot.non.nd$lwr[2] <- CI.nd[8,1] + CI.nd[5,1]
line.range.plot.non.nd$lwr[3] <- CI.nd[9,1] + CI.nd[5,1]

CI.dc <- confint(lmer(slnPdc ~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.non.dc$upr[1] <- CI.dc[4,2]
line.range.plot.non.dc$upr[2] <- CI.dc[8,2] + CI.dc[4,2]
line.range.plot.non.dc$upr[3] <- CI.dc[9,2] + CI.dc[4,2]
line.range.plot.non.dc$lwr[1] <- CI.dc[4,1]
line.range.plot.non.dc$lwr[2] <- CI.dc[8,1] + CI.dc[4,1]
line.range.plot.non.dc$lwr[3] <- CI.dc[9,1] + CI.dc[4,1]


line.range.plot.non.nd$sig[1] <- 0
line.range.plot.non.nd$sig[2] <- 1
line.range.plot.non.nd$sig[3] <- 1

line.range.plot.non.dc$sig[1] <- 0
line.range.plot.non.dc$sig[2] <- 0
line.range.plot.non.dc$sig[3] <- 0




# ---- --------------------------------------------------------------------


# Fixer influence on NONFIXER incremental (bin and plot) -----------------------------------------

vmfr <- read.csv('Fixer_Nonfixer_dynamics.csv') %>%
        filter(Fixer == 'nonfixer') %>%
        mutate(I = (AGB.delta + R + M)/1000) %>%
        filter(!is.na(I)) %>% 
        filter(PSP %!in% bad.plots.25) %>%
        group_by(bin) %>%
        filter(I > mean(I) - 2*sd(I) & I < mean(I) + 2*sd(I)) %>%
        mutate(sI = scale(I)) %>%
        mutate(sIdc = dc(sI,N.bin)) %>%
        ungroup(bin) %>%
        filter(N.bin > 0) %>%
        mutate(bin = factor(bin,  labels = c('<30 cm',
                                             "30-60 cm",
                                             ">60 cm")))


mod.nd <- summary(lmer(sI ~ pf.BA.total * bin + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.non.nd$slope[7] <- mod.nd$coefficients[2,1]
line.range.plot.non.nd$slope[8] <- mod.nd$coefficients[5,1] + mod.nd$coefficients[2,1]
line.range.plot.non.nd$slope[9] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]

mod.dc <- summary(lmer(sIdc ~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.non.dc$slope[7] <- mod.dc$coefficients[1,1]
line.range.plot.non.dc$slope[8] <- mod.dc$coefficients[5,1] + mod.dc$coefficients[1,1]
line.range.plot.non.dc$slope[9] <- mod.dc$coefficients[6,1] + mod.dc$coefficients[1,1]


CI.nd <- confint(lmer(sI ~ pf.BA.total * bin + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.non.nd$upr[7] <- CI.nd[5,2]
line.range.plot.non.nd$upr[8] <- CI.nd[8,2] + CI.nd[5,2]
line.range.plot.non.nd$upr[9] <- CI.nd[9,2] + CI.nd[5,2]
line.range.plot.non.nd$lwr[7] <- CI.nd[5,1]
line.range.plot.non.nd$lwr[8] <- CI.nd[8,1] + CI.nd[5,1]
line.range.plot.non.nd$lwr[9] <- CI.nd[9,1] + CI.nd[5,1]

CI.dc <- confint(lmer(sIdc ~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.non.dc$upr[7] <- CI.dc[4,2]
line.range.plot.non.dc$upr[8] <- CI.dc[8,2] + CI.dc[4,2]
line.range.plot.non.dc$upr[9] <- CI.dc[9,2] + CI.dc[4,2]
line.range.plot.non.dc$lwr[7] <- CI.dc[4,1]
line.range.plot.non.dc$lwr[8] <- CI.dc[8,1] + CI.dc[4,1]
line.range.plot.non.dc$lwr[9] <- CI.dc[9,1] + CI.dc[4,1]


line.range.plot.non.nd$sig[7] <- 0
line.range.plot.non.nd$sig[8] <- 0
line.range.plot.non.nd$sig[9] <- 0

line.range.plot.non.dc$sig[7] <- 0
line.range.plot.non.dc$sig[8] <- 0
line.range.plot.non.dc$sig[9] <- 0



# ---- --------------------------------------------------------------------


# Fixer influence on NONFIXER mortality (bin and plot) -----------------------------------------

vmfr <- read.csv('Fixer_Nonfixer_dynamics.csv') %>%
        filter(Fixer == 'nonfixer') %>%
        mutate(M = M/1000 * -1) %>%
        filter(!is.na(M)) %>% 
        filter(PSP %!in% bad.plots.25) %>%
        group_by(bin) %>%
        filter(M > mean(M) - 2*sd(M) & M < mean(M) + 2*sd(M)) %>%
        mutate(lnM = log(M+0.01)) %>%
        filter(!is.na(lnM)) %>%
        mutate(slnM = scale(lnM)) %>%
        mutate(slnMdc = dc(slnM,N.bin)) %>%
        ungroup(bin) %>%
        filter(N.bin > 0) %>%
        mutate(bin = factor(bin,  labels = c('<30 cm',
                                             "30-60 cm",
                                             ">60 cm")))

mod.nd <- summary(lmer(slnM ~ pf.BA.total * bin + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.non.nd$slope[4] <- mod.nd$coefficients[2,1]
line.range.plot.non.nd$slope[5] <- mod.nd$coefficients[5,1] + mod.nd$coefficients[2,1]
line.range.plot.non.nd$slope[6] <- mod.nd$coefficients[6,1] + mod.nd$coefficients[2,1]

mod.dc <- summary(lmer(slnMdc ~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.non.dc$slope[4] <- mod.dc$coefficients[1,1]
line.range.plot.non.dc$slope[5] <- mod.dc$coefficients[5,1] + mod.dc$coefficients[1,1]
line.range.plot.non.dc$slope[6] <- mod.dc$coefficients[6,1] + mod.dc$coefficients[1,1]


CI.nd <- confint(lmer(slnM ~ pf.BA.total * bin + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.non.nd$upr[4] <- CI.nd[5,2]
line.range.plot.non.nd$upr[5] <- CI.nd[8,2] + CI.nd[5,2]
line.range.plot.non.nd$upr[6] <- CI.nd[9,2] + CI.nd[5,2]
line.range.plot.non.nd$lwr[4] <- CI.nd[5,1]
line.range.plot.non.nd$lwr[5] <- CI.nd[8,1] + CI.nd[5,1]
line.range.plot.non.nd$lwr[6] <- CI.nd[9,1] + CI.nd[5,1]

CI.dc <- confint(lmer(slnMdc ~ pf.BA.total * bin - 1 + (1|PSP) + (1|Year), data = vmfr))
line.range.plot.non.dc$upr[4] <- CI.dc[4,2]
line.range.plot.non.dc$upr[5] <- CI.dc[8,2] + CI.dc[4,2]
line.range.plot.non.dc$upr[6] <- CI.dc[9,2] + CI.dc[4,2]
line.range.plot.non.dc$lwr[4] <- CI.dc[4,1]
line.range.plot.non.dc$lwr[5] <- CI.dc[8,1] + CI.dc[4,1]
line.range.plot.non.dc$lwr[6] <- CI.dc[9,1] + CI.dc[4,1]


line.range.plot.non.nd$sig[4] <- 0
line.range.plot.non.nd$sig[5] <- 0
line.range.plot.non.nd$sig[6] <- 0

line.range.plot.non.dc$sig[4] <- 0
line.range.plot.non.dc$sig[5] <- 0
line.range.plot.non.dc$sig[6] <- 0



# ---- --------------------------------------------------------------------


# Nonfixer Plots  -------------------------------------------

line.range.plot.non.nd <- line.range.plot.non.nd %>% 
        mutate(bin = factor(bin, levels = c('<30', '30-60', '>60'))) %>%
        mutate(var = factor(var, levels = c('Productivity', 'Mortality', 'Net Biomass')))

line.range.plot.non.dc <- line.range.plot.non.dc %>% 
        mutate(bin = factor(bin, levels = c('<30', '30-60', '>60'))) %>%
        mutate(var = factor(var, levels = c('Productivity', 'Mortality', 'Net Biomass')))

## ~~~~~ Line Plots ~~~~~##

lp.non.psp <- ggplot() +
        geom_linerange(data = line.range.plot.non.nd, 
                       aes(x = bin, ymin = lwr, ymax = upr, group = bin),
                       linetype = 'solid', color = 'grey50', size = 4, alpha = 0.4) +
        
        geom_pointrange(data = line.range.plot.non.dc, 
                        aes(x = bin, y = slope, ymin = lwr, ymax = upr, group = bin, shape = factor(sig)),
                        linetype = 'solid', color = 'blue', fatten = 3, size = 1.5, stroke = 1.1) +
        
        facet_grid(.~var) +
        geom_hline(yintercept = 0, linetype = 'longdash', color = 'grey50') +
        scale_shape_manual(values = c(1,16)) +
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              axis.text.y = element_text(size = 22),
              axis.text.x = element_text(size = 18),
              axis.title = element_text(size = 22),
              strip.background = element_rect(fill = 'white', color = 'black', size = 1),
              strip.text = element_text(size = 22),
              legend.position = 'none') +
        labs(x = '', y = '')

# ---- --------------------------------------------------------------------


# Plotting ----------------------------------------------------------------

## Line Plot ##

tiff('c:/users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Figures/LinePlot_combined.tif',
     width = 14, height = 10, units = 'in', res = 400)

ggarrange(lp.non.sp, lp.non.psp, lp.fix.sp, lp.fix.psp, nrow = 2, ncol = 2, align = 'hv')

dev.off()


# ---- --------------------------------------------------------------------




# Denisty correction analysis ---------------------------------------------

dif(line.range.plot.non.nd, line.range.plot.non.dc)
dif(line.range.plot.fix.nd, line.range.plot.fix.dc)
dif(line.range.plot.all.nd, line.range.plot.all.dc)
