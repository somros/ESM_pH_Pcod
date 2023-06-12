library(dplyr)

dat <- read.csv('esm_ph_300.csv')

dat1 <- dat %>%
  filter(NMFS_AREA %in% c(610, 620) & esm == 'GFDL')

# write tab for Andre
write.csv(dat1, 'gfdl_ph_610_620_300m.csv', row.names = F)

dat <- read.csv('esm_ph.csv')

dat1 <- dat %>%
  filter(NMFS_AREA %in% c(610, 620) & esm == 'GFDL')

# write tab for Andre
write.csv(dat1, 'gfdl_ph_610_620_1000m.csv', row.names = F)




dat <- read.csv('esm_ph_debug_alldepth.csv')


p <- dat %>%
  filter(NMFS_AREA == 610, time == max(dat %>% filter(esm=='GFDL') %>% pull(time))) %>%
  ggplot(aes(x = -lev, y = ph, group = idx, color = idx))+
  geom_point()+
  coord_flip()+
  theme_bw()+
  labs(title = 'Vertical profiles of pH for ESM cells in NMFS area 610 (October 2100)')+
  facet_grid(esm~run, scales = 'free')
p

ggsave('vertical_profiles_610.png', p, width = 8, height = 10)

esm_ph <- read.csv('esm_ph.csv')
esm_ph1 <- read.csv('esm_ph_300.csv')


p <- esm_ph1 %>%
  # mutate(season = case_when(
  #   month %in% c(3,4,5) ~ 4, # mid-month
  #   month %in% c(12,1,2) ~ 1,
  #   month %in% c(6,7,8) ~ 7,
  #   month %in% c(9,10,11) ~ 10,
  # )) %>%
  # group_by(esm, run, slice, lev, NMFS_AREA, idx, year) %>%
  # summarise(ph2 = mean(ph)) %>%
  # ungroup() %>%
  mutate(date = as.Date(paste(year, month, '01', sep = '-'))) %>%
  filter(NMFS_AREA %in% c(610, 620), esm == 'GFDL') %>%
  ggplot(aes(x = date, y = ph_mean, color = run))+
  geom_line(linewidth = 0.8)+
  #geom_label_repel(aes(group = idx, label = ifelse(date == maxdate, lev, '')))+
  theme_bw()+
  labs(x = '', y = 'pH', title = 'Monthly surface and bottom GFDL pH in NMFS areas 610-620 (depth <300 m)')+
  facet_grid(slice~NMFS_AREA, scales = 'free')
p
ggsave('model_comparison_610.png', p, width = 10, height = 8)



test <- dat %>%
  filter(year %in% c(2014,2015), esm == 'GFDL') %>%
  mutate(date = as.Date(paste(year, month, '01', sep = '-'))) 

test %>%
  ggplot(aes(x = date, y = ph_mean, group = run, color = run))+
  geom_line()+
  theme_bw()+
  labs(x = '', y = 'pH', title = 'NMFS area 650')+
  facet_grid(NMFS_AREA~slice)

# in GFDL the jump seems to be very small and there do not seem to be gaps
# MIROC is more difficult

# reproduce what andre did
dat1 <- dat %>%
  filter(month %in% c(6:8), NMFS_AREA %in% c(610,620)) %>%
  group_by(esm, run, slice, year) %>%
  summarise(index = mean(ph_mean)) %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year, '07', '15', sep = '-')))

dat1 %>% ggplot()+
  geom_line(aes(x = date, y = index, color = run), linewidth = 0.8)+
  scale_y_continuous(limits = c(6,9), breaks = c(6, 7.5, 9))+
  theme_bw()+
  facet_wrap(slice~esm)

dat1 %>%
  filter(slice == 'surface') %>% 
  ggplot()+
  geom_line(aes(x = date, y = index, color = esm), linewidth = 0.8)+
  scale_y_continuous(limits = c(6,9), breaks = c(6, 7.5, 9))+
  theme_bw()+
  facet_wrap(~run)






# compare 300 to 1000
# pH
library(ggplot2)

dat300 <- read.csv('gfdl_ph_610_620_300m.csv')
dat1000 <- read.csv('gfdl_ph_610_620_1000m.csv')

dat300 <- dat300 %>% mutate(m = 300)
dat1000 <- dat1000 %>% mutate(m = 1000)

datall <- rbind(dat300,dat1000)

datall <- datall %>% mutate(date = as.Date(paste(year, month, '15', sep = '-')))

datall %>% 
  filter(NMFS_AREA == 610) %>%
  ggplot(aes(x = date, y = ph_mean, color = run))+
  geom_line()+
  theme_bw()+
  facet_grid(slice~m)

