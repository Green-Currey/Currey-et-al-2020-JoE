source('~/R/startup.R')
#C:/Users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Data/



# ~~~~ Figure 1 ----------------------------------------------------------------
# Change in productivity over time (all bins) ----------------------------------


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


# Mixed effects model & summary
mod <- lmer(P ~ Year + (1|harvest/PSP), data = P.data)
mod %>% summary
ci.list1 <- ci.bands(P.data$Year, P.data$P, mod)

P1 <- ggplot() +
     geom_line(data = P.data, aes(x = Year, y = P, group = PSP), alpha = 0.2) +
     
     annotate('segment',
              x = ci.list1$x[1], xend = ci.list1$x[2],
              y = ci.list1$y[1], yend = ci.list1$y[2],
              size = 1.5, color = 'grey40', alpha = 1, linetype = 'longdash') +
     geom_ribbon(aes(x = ci.list1$ci.df$x, ymin = ci.list1$ci.df$lwr, ymax = ci.list1$ci.df$upr),
                 alpha = 0.3, fill = 'grey30') +
     
     theme(panel.background = element_blank(),
           axis.line = element_line(),
           axis.text = element_text(size = 24), 
           axis.ticks.x = element_blank(),
           panel.grid.major.x = element_line(color = 'grey80'),
           axis.line.x = element_blank(),
           axis.text.x = element_blank(),
           axis.title = element_blank()) +
     scale_x_continuous(breaks = c(1990, 2000, 2010), labels = c('1990', '2000','2010')) +
     labs(y = bquote('Productivity (Mg'~ha^-1~yr^-1*')'))

figure(P1)



mod <- lmer(P ~ Year + (1|harvest/PSP/SP), data = P.sp)
mod %>% summary
ci.list1.sp <- ci.bands(P.sp$Year, P.sp$P, mod)

P2 <- ggplot() +
     geom_point(data = P.sp, aes(x = Year, y = P), alpha = 0) +
     annotate('segment',
              x = ci.list1.sp$x[1], xend = ci.list1.sp$x[2],
              y = ci.list1.sp$y[1], yend = ci.list1.sp$y[2],
              size = 1.5, color = 'red3') +
     geom_ribbon(aes(x = ci.list1.sp$ci.df$x, ymin = ci.list1.sp$ci.df$lwr, ymax = ci.list1.sp$ci.df$upr),
                 alpha = 0.3, fill = 'red3') +
     theme(panel.background = element_blank(),
           axis.line.y = element_line(color = 'red3'),
           axis.text.y = element_text(size = 24, color = 'red3'), 
           axis.ticks.x = element_blank(),
           # panel.grid.major.x = element_line(color = 'grey80'),
           axis.line.x = element_blank(),
           axis.text.x = element_blank(),
           axis.title = element_blank()) +
     scale_x_continuous(breaks = c(1990, 2000, 2010), labels = c('1990', '2000','2010')) +
     scale_y_continuous(position = 'right', limits = c(0.05,1.5))

figure(P2)


# Change in Mortality over time (all bins) ----------------------------------

# data

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


# Mixed effects model & summary
mod <- lmer(M ~ Year + (1|harvest/PSP), data = M.data)
mod %>% summary
ci.list3 <- ci.bands(M.data$Year, M.data$M, mod)



M1 <- ggplot() +
     geom_line(data = M.data, aes(x = Year, y = M, group = PSP), alpha = 0.2) +
     
     annotate('segment',
              x = ci.list3$x[1], xend = ci.list3$x[2],
              y = ci.list3$y[1], yend = ci.list3$y[2],
              size = 1.5, color = 'grey40', alpha = 1) +
     geom_ribbon(aes(x = ci.list3$ci.df$x, ymin = ci.list3$ci.df$lwr, ymax = ci.list3$ci.df$upr),
                 alpha = 0.3, fill = 'grey30') +
     
     theme(panel.background = element_blank(),
           axis.line = element_line(),
           axis.text = element_text(size = 24), 
           axis.ticks.x = element_blank(),
           panel.grid.major.x = element_line(color = 'grey80'),
           axis.line.x = element_blank(),
           axis.text.x = element_blank(),
           axis.title = element_blank()) +
     scale_x_continuous(breaks = c(1990, 2000, 2010), labels = c('1990', '2000','2010')) +
     labs(y = bquote('Biomass Mortality (Mg'~ha^-1~yr^-1*')')) + ylim(NA,13)

figure(M1)




mod <- lmer(M ~ Year + (1|harvest/PSP/SP), data = M.sp)
mod %>% summary
ci.list3.sp <- ci.bands(M.sp$Year, M.sp$M, mod)

M2 <- ggplot() +
     geom_line(data = M.sp, aes(x = Year, y = M, group = PSP), alpha = 0) +
     
     annotate('segment',
              x = ci.list3.sp$x[1], xend = ci.list3.sp$x[2],
              y = ci.list3.sp$y[1], yend = ci.list3.sp$y[2],
              size = 1.5, color = 'red3', alpha = 1) +
     geom_ribbon(aes(x = ci.list3.sp$ci.df$x, ymin = ci.list3.sp$ci.df$lwr, ymax = ci.list3.sp$ci.df$upr),
                 alpha = 0.3, fill = 'red3') +
     
     theme(panel.background = element_blank(),
           axis.line.y = element_line(color = 'red3'),
           axis.text.y = element_text(size = 24, color = 'red3'), 
           axis.ticks.x = element_blank(),
           # panel.grid.major.x = element_line(color = 'grey80'),
           axis.line.x = element_blank(),
           axis.text.x = element_blank(),
           axis.title = element_blank()) +
     scale_x_continuous(breaks = c(1990, 2000, 2010), labels = c('1990', '2000','2010')) +
     scale_y_continuous(position = 'right', limits = c(NA, 0.6))

figure(M2)


# Change in Net AGB over time (all bins) ----------------------------------

# data

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



# Mixed effects model & summary
mod <- lmer(I ~ Year + (1|harvest/PSP), data = I.data)
mod %>% summary

ci.list2 <- ci.bands(I.data$Year, I.data$I, mod)



# plot

I1 <- ggplot() +
     geom_line(data = I.data, aes(x = Year, y = I, group = PSP), alpha = 0.2) +
     
     annotate('segment',
              x = ci.list2$x[1], xend = ci.list2$x[2],
              y = ci.list2$y[1], yend = ci.list2$y[2],
              size = 1.5, color = 'grey40', alpha = 1) +
     geom_ribbon(aes(x = ci.list2$ci.df$x, ymin = ci.list2$ci.df$lwr, ymax = ci.list2$ci.df$upr),
                 alpha = 0.3, fill = 'grey30') +
     
     geom_hline(yintercept = 0, color = 'grey50', linetype = 'longdash') +
     theme(panel.background = element_blank(),
           axis.line = element_line(),
           axis.text = element_text(size = 24), 
           axis.title = element_blank(),
           axis.ticks.x = element_blank(),
           panel.grid.major.x = element_line(color = 'grey80')) +
     scale_x_continuous(breaks = c(1990, 2000, 2010), labels = c('1990', '2000','2010')) +
     labs(y = bquote('Net AGB (Mg'~ha^-1~yr^-1*')')) + ylim(-5,10)

figure(I1)



mod <- lmer(I ~ Year + (1|harvest/PSP/SP), data = I.sp)
mod %>% summary
ci.list2.sp <- ci.bands(I.sp$Year, I.sp$I, mod)


I2 <- ggplot() +
     geom_line(data = I.sp, aes(x = Year, y = I, group = PSP), alpha = 0) +
     
     annotate('segment',
              x = ci.list2.sp$x[1], xend = ci.list2.sp$x[2],
              y = ci.list2.sp$y[1], yend = ci.list2.sp$y[2],
              size = 1.5, color = 'red3', alpha = 1) +
     geom_ribbon(aes(x = ci.list2.sp$ci.df$x, ymin = ci.list2.sp$ci.df$lwr, ymax = ci.list2.sp$ci.df$upr),
                 alpha = 0.3, fill = 'red3') +
     
     theme(panel.background = element_blank(),
           axis.line.y = element_line(color = 'red3'),
           axis.text.y = element_text(size = 24, color = 'red3'), 
           axis.ticks.x = element_blank(),
           # panel.grid.major.x = element_line(color = 'grey80'),
           axis.line.x = element_blank(),
           axis.text.x = element_text(size = 24),
           axis.title = element_blank()) +
     scale_x_continuous(breaks = c(1990, 2000, 2010), labels = c('', '','')) +
     scale_y_continuous(position = 'right', limits = c(-0.5,1))


figure(I2)


# Plotting ----------------------------------------------------------------

figure(
     ggarrange(P1, 
               M1, 
               I1,
               nrow = 3, align = 'hv') %>%
          annotate_figure(bottom = text_grob('Year', color = 'black', size = 25)),
     path.name = 'c:/users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Figures/PMI_figure_harvest',
     save = T,
     width = 8,
     height = 8
)


figure(
     ggarrange(P2, 
               M2, 
               I2,
               nrow = 3, align = 'hv') %>%
          annotate_figure(bottom = text_grob(' ', color = 'black', size = 25)),
     path.name = 'c:/users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Figures/PMI_figure2_harvest',
     save = T,
     width = 8,
     height = 8
)






# ----- -------------------------------------------------------------------


# ~~~~ Supplemental Figure -----------------------------------------------------
# Change in biomass over time (all bins) ----------------------------------

anomolies.25 <- c(33, 34, 37, 39, 49, 53, 59, 69, 71, 58, 79)

# data
AGB.data <- read.csv('../../Data/VMFR_corrected_AGB.csv') %>%
     filter(PSP %!in% anomolies.25) %>%
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
     filter(PSP %!in% anomolies.25) %>%
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




mod <- lmer(AGB ~ Year + (1|harvest/PSP), data = AGB.data)
mod %>% summary
ci.list1 <- ci.bands(AGB.data$Year, AGB.data$AGB, mod)



# plot

AGB1 <- ggplot(data = AGB.data) +
     geom_line(aes(x = Year, y = AGB, group = PSP), alpha = 0.2) +
     
     annotate('segment',
              x = ci.list1$x[1], xend = ci.list1$x[2],
              y = ci.list1$y[1], yend = ci.list1$y[2],
              size = 1.5, color = 'grey40', alpha = 1, linetype = 'solid') +
     geom_ribbon(aes(x = ci.list1$ci.df$x, ymin = ci.list1$ci.df$lwr, ymax = ci.list1$ci.df$upr),
                 alpha = 0.3, fill = 'grey30') +
     
     theme(panel.background = element_blank(),
           axis.line = element_line(),
           axis.text = element_text(size = 24), 
           axis.ticks.x = element_blank(),
           panel.grid.major.x = element_line(color = 'grey80'),
           axis.line.x = element_blank(),
           axis.text.x = element_blank(),
           axis.title = element_blank()) +
     scale_x_continuous(breaks = c(1990, 2000, 2010), labels = c('1990', '2000','2010')) +
     labs(y = bquote('Biomass (Mg'~ha^-1*')'))

figure(AGB1)

mod <- lmer(AGB ~ Year + (1|harvest/PSP/Su.Pl.No), data = AGB.sp)
mod %>% summary
ci.list1.sp <- ci.bands(AGB.sp$Year, AGB.sp$AGB, mod)


AGB2 <- ggplot() +
     geom_point(data = AGB.sp, aes(x = Year, y = AGB), alpha = 0) +
     
     annotate('segment',
              x = ci.list1.sp$x[1], xend = ci.list1.sp$x[2],
              y = ci.list1.sp$y[1], yend = ci.list1.sp$y[2],
              size = 1.5, color = 'red3') +
     geom_ribbon(aes(x = ci.list1.sp$ci.df$x, ymin = ci.list1.sp$ci.df$lwr, ymax = ci.list1.sp$ci.df$upr),
                 alpha = 0.3, fill = 'red3') +
     
     theme(panel.background = element_blank(),
           axis.line.y = element_line(color = 'red3'),
           axis.text.y = element_text(size = 24, color = 'red3'), 
           axis.ticks.x = element_blank(),
           axis.line.x = element_blank(),
           axis.text.x = element_blank(),
           axis.title = element_blank()) +
     scale_x_continuous(breaks = c(1990, 2000, 2010), labels = c('1990', '2000','2010')) +
     scale_y_continuous(position = 'right', limits = c(3, 40))

figure(AGB2)





# Change in Basal Area over time --------------------------------------------------------------

# data

ba.data <- read.csv('PSP_dynamics_no_bin.csv')
ba.sp <- read.csv('SP_dynamics_nobin.csv') %>%
     mutate(unique.id = paste0(PSP,'.',SP))

mod <- lmer(BA ~ Year + (1|harvest/PSP), data = ba.data)
mod %>% summary
ci.list3 <- ci.bands(ba.data$Year, ba.data$Year, mod)


BA1 <- ggplot(ba.data) +
     geom_line(aes(x = Year, y = BA, group = PSP), alpha = 0.2) +
     
     annotate('segment',
              x = ci.list3$x[1], xend = ci.list3$x[2],
              y = ci.list3$y[1], yend = ci.list3$y[2],
              size = 1.5, color = 'grey40', alpha = 1) +
     geom_ribbon(aes(x = ci.list3$ci.df$x, ymin = ci.list3$ci.df$lwr, ymax = ci.list3$ci.df$upr),
                 alpha = 0.3, fill = 'grey30') +
     
     theme(panel.background = element_blank(),
           axis.line = element_line(),
           axis.text = element_text(size = 24), 
           axis.ticks.x = element_blank(),
           panel.grid.major.x = element_line(color = 'grey80'),
           axis.line.x = element_blank(),
           axis.text.x = element_blank(),
           axis.title = element_blank()) +
     scale_x_continuous(breaks = c(1990, 2000, 2010), labels = c('1990', '2000','2010')) +
     labs(y = bquote('Basal Area ('*m^2~ha^-1*')'))
figure(BA1)


mod <- lmer(BA ~ Year + (1|harvest/PSP/SP), data = ba.sp)
mod %>% summary
ci.list3.sp <- ci.bands(ba.sp$Year, ba.sp$Year, mod)


BA2 <- ggplot() +
     geom_line(data = ba.sp, aes(x = Year, y = BA, group = PSP), alpha = 0) +
     
     annotate('segment',
              x = ci.list3.sp$x[1], xend = ci.list3.sp$x[2],
              y = ci.list3.sp$y[1], yend = ci.list3.sp$y[2],
              size = 1.5, color = 'red3', alpha = 1) +
     geom_ribbon(aes(x = ci.list3.sp$ci.df$x, ymin = ci.list3.sp$ci.df$lwr, ymax = ci.list3.sp$ci.df$upr),
                 alpha = 0.3, fill = 'red3') +
     
     theme(panel.background = element_blank(),
           axis.line.y = element_line(color = 'red3'),
           axis.text.y = element_text(size = 24, color = 'red3'), 
           axis.ticks.x = element_blank(),
           axis.line.x = element_blank(),
           axis.text.x = element_blank(),
           axis.title = element_blank()) +
     scale_x_continuous(breaks = c(1990, 2000, 2010), labels = c('1990', '2000','2010')) +
     scale_y_continuous(position = 'right', limits = c(.5, 4))

figure(BA2)

# Change in density over time (all bins) ----------------------------------

# data
d.data <- read.csv('PSP_dynamics_no_bin.csv')

d.sp <- read.csv('SP_dynamics_nobin.csv') %>%
     mutate(unique.id = paste0(PSP,'.',SP))

mod <- lmer(N.psp ~ Year + (1|harvest/PSP), data = d.data)
mod %>% summary
ci.list2 <- ci.bands(d.data$Year, d.data$N.psp, mod)



D1 <- ggplot(d.data) +
     geom_line(aes(x = Year, y = N.psp, group = PSP), alpha = 0.2) +
     
     annotate('segment',
              x = ci.list2$x[1], xend = ci.list2$x[2],
              y = ci.list2$y[1], yend = ci.list2$y[2],
              size = 1.5, color = 'grey40', alpha = 1) +
     geom_ribbon(aes(x = ci.list2$ci.df$x, ymin = ci.list2$ci.df$lwr, ymax = ci.list2$ci.df$upr),
                 alpha = 0.3, fill = 'grey30') +
     
     theme(panel.background = element_blank(),
           axis.line = element_line(),
           axis.text = element_text(size = 24), 
           axis.title = element_blank(),
           axis.ticks.x = element_blank(),
           panel.grid.major.x = element_line(color = 'grey80')) +
     scale_x_continuous(breaks = c(1990, 2000, 2010), labels = c('1990', '2000','2010')) +
     labs(y = bquote('Density (stems'~ha^-1*')'), x = 'Year')

figure(D1)



mod <- lmer(N.sp ~ Year + (1|harvest/PSP/SP), data = d.sp)
mod %>% summary
ci.list2.sp <- ci.bands(d.sp$Year, d.sp$N.sp, mod)



D2 <- ggplot() +
     geom_line(data = d.sp, aes(x = Year, y = N.sp, group = PSP), alpha = 0) +
     
     annotate('segment',
              x = ci.list2.sp$x[1], xend = ci.list2.sp$x[2],
              y = ci.list2.sp$y[1], yend = ci.list2.sp$y[2],
              size = 1.5, color = 'red3', alpha = 1) +
     geom_ribbon(aes(x = ci.list2.sp$ci.df$x, ymin = ci.list2.sp$ci.df$lwr, ymax = ci.list2.sp$ci.df$upr),
                 alpha = 0.3, fill = 'red3') +
     
     theme(panel.background = element_blank(),
           axis.line.y = element_line(color = 'red3'),
           axis.text.y = element_text(size = 24, color = 'red3'), 
           axis.ticks.x = element_blank(),
           # panel.grid.major.x = element_line(color = 'grey80'),
           axis.line.x = element_blank(),
           axis.text.x = element_text(size = 24),
           axis.title = element_blank()) +
     scale_x_continuous(breaks = c(1990, 2000, 2010), labels = c('', '','')) +
     scale_y_continuous(position = 'right', limits = c(2, NA))

figure(D2)


# Plotting ----------------------------------------------------------------

figure(
     ggarrange(AGB1, 
               BA1, 
               D1,
               nrow = 3, align = 'hv') %>%
          annotate_figure(bottom = text_grob('Year', color = 'black', size = 25)),
     path.name = 'c:/Users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Figures/AGB_BA_D_figure_harvest',
     save = T,
     width = 8,
     height = 8
)


figure(
     ggarrange(AGB2, 
               BA2, 
               D2,
               nrow = 3, align = 'hv') %>%
          annotate_figure(bottom = text_grob(' ', color = 'black', size = 25)),
     path.name = 'c:/Users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Figures/AGB_BA_D_figure2_harvest',
     save = T,
     width = 8,
     height = 8
)






