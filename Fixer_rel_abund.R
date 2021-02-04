
source('~/R/startup.R')
#C:/Users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Data/

bad.plots.25 <- c(33, 34, 37, 39, 49, 53, 59, 69, 71, 58, 79)



fRA <- read.csv('PSP_dynamics_no_bin.csv') %>% 
  mutate(fra = pf.psp*100) %>%
  mutate(time = Year - 1982) %>%
  filter(PSP %!in% bad.plots.25) 

lmer(fra ~ time + (1|PSP), data = fRA) %>% summary

# plot


tiff('c:/users/bryce/OneDrive/Documents/VMFR/Dynamics Paper 2019/Figures/FRA_over_time.tif',
     width = 10, height = 5, units = 'in', res = 1400)

ggplot() +
  geom_line(data = fRA, aes(x = Year, y = fra, group = PSP), alpha = 0.2) +
  geom_smooth(data = fRA, aes(x = Year, y = fra), se = T, alpha = 0.7, method = 'lm', linetype = 'solid', color = 'red3', size = 2) +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 18),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_line(color = 'grey80')) +
  geom_hline(yintercept = 24.6, linetype = 'longdash', size = 1.5, alpha = 0.5) + 
  coord_cartesian(ylim = c(0,50)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010), labels = c('1990', '2000','2010')) +
  labs(y = bquote('N-Fixer Relative Abundance (% '~ha^-1*')'))

dev.off()



fRA <- read.csv('SP_dynamics_nobin.csv') %>% 
  mutate(fra = pf.sp*100) %>%
  filter(PSP %!in% bad.plots.25) %>%
  filter(fra>0)

fRA$fra %>% summary

