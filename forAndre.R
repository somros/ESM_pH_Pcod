# Prepare pH data for Andre
library(tidyverse)

dat1000 <- read.csv('esm_ph.csv')
dat300 <- read.csv('esm_ph_300.csv')

# keep only GFDL
# keep 610 and 620 and 630
# get weighted average of 610, 620, and 630

# start from 1000
# areas m^2 of 0-1000 m shelf by NMFS area
# 610 63986698621 
# 620 69583703140
# 630 105918077937
# 640 37270389681
# 650 43952466109

dat1000 <- dat1000 %>%
  filter(NMFS_AREA %in% c(610,620,630) & esm == 'GFDL') %>%
  pivot_wider(values_from = ph_mean, names_from = NMFS_AREA) %>%
  rename(pH_610 = `610`, pH_620 = `620`, pH_630 = `630`) %>%
  mutate(pH_610_620_630 = (pH_610 * 63986698621 + 
                             pH_620 * 69583703140 +
                             pH_630 * 105918077937) / (63986698621 + 69583703140 + 105918077937))


# view
dat1000 %>%
  ggplot(aes(x = as.Date(paste(year, month, 15, sep = '-')), y = pH_610_620_630, color = run))+
  geom_line()+
  theme_bw()+
  facet_wrap(~slice)

# write out for Andre
write.csv(dat1000, 'gfdl_ph_610_620_630_1000m.csv', row.names = F)

# 300 m
# areas m^2 of 0-300 m shelf by NMFS area
# 610 57225003746
# 620 62597059226
# 630 98582220025
# 640 32560976631
# 650 36726651409
dat300 <- dat300 %>%
  filter(NMFS_AREA %in% c(610,620,630) & esm == 'GFDL') %>%
  pivot_wider(values_from = ph_mean, names_from = NMFS_AREA) %>%
  rename(pH_610 = `610`, pH_620 = `620`, pH_630 = `630`) %>%
  mutate(pH_610_620_630 = (pH_610 * 57225003746 + 
                             pH_620 * 62597059226 +
                             pH_630 * 98582220025) / (57225003746 + 62597059226 + 98582220025))

# view
dat300 %>%
  ggplot(aes(x = as.Date(paste(year, month, 15, sep = '-')), y = pH_610_620_630, color = run))+
  geom_line()+
  theme_bw()+
  facet_wrap(~slice)

# write out for Andre
write.csv(dat300, 'gfdl_ph_610_620_630_300m.csv', row.names = F)
