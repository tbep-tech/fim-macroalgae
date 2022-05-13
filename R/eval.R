library(tidyverse)
library(sf)
library(lubridate)

data(algdat)

ddg <- 0.4

thm <- theme_minimal() + 
  theme(
    panel.grid.minor = element_blank()
  )

# # take yr, mo median by bay segment
# algdat <- algdat %>% 
#   group_by(bay_segment, Gear, yr, mo) %>% 
#   summarise(
#     cpue_gper100m2 = median(cpue_gper100m2), 
#     .groups = 'drop'
#   ) 

toplo1 <- algdat %>% 
  group_by(mo, bay_segment, Gear) %>% 
  summarise(
    cnt = n(), 
    medv = median(cpue_gper100m2, na.rm = T), 
    lov = quantile(cpue_gper100m2, 0.25, na.rm = T),
    hiv = quantile(cpue_gper100m2, 0.75, na.rm = T), 
    .groups = 'drop'
  )
  
ggplot(toplo1, aes(x = mo, y = medv, group = Gear, color = Gear)) +
  geom_point(position = position_dodge(width = ddg), size = 3) +
  geom_line(position = position_dodge(width = ddg)) + 
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0, position = position_dodge(width = ddg)) +
  # geom_violin() + 
  scale_y_log10() +
  facet_wrap(~bay_segment) + 
  thm

toplo2 <- algdat

ggplot(toplo2, aes(x = mo, y = 1 + cpue_gper100m2, fill = Gear)) +
  geom_violin(draw_quantiles = 0.5, position = position_dodge(width = 0.3)) +
  # geom_boxplot() + 
  scale_y_log10() +
  facet_wrap(~bay_segment) + 
  thm
  
toplo3 <- algdat %>% 
  group_by(yr, bay_segment, Gear) %>% 
  summarise(
    cnt = n(), 
    medv = median(cpue_gper100m2, na.rm = T), 
    lov = quantile(cpue_gper100m2, 0.25, na.rm = T),
    hiv = quantile(cpue_gper100m2, 0.75, na.rm = T), 
    .groups = 'drop'
  )

ggplot(toplo3, aes(x = yr, y = medv, group = Gear, color = Gear)) +
  geom_point(position = position_dodge(width = ddg), size = 3) +
  geom_line(position = position_dodge(width = ddg)) +
  # geom_smooth(method = 'lm', se = F, linetype = 'dashed') +
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0, position = position_dodge(width = ddg)) +
  # scale_y_log10() +
  scale_y_continuous(limits = c(0, 1)) + 
  facet_wrap(~bay_segment) + 
  thm

toplo4 <- algdat

ggplot(toplo4, aes(x = factor(yr), y = 1 + cpue_gper100m2, fill = Gear)) +
  geom_violin(draw_quantiles = 0.5, position = position_dodge(width = 0.3)) +
  scale_y_log10() +
  facet_wrap(~bay_segment) + 
  thm

# # take yr, mo median by bay segment
# tomod <- algdat %>%
#   group_by(bay_segment, Gear, yr, mo) %>%
#   summarise(
#     cpue_gper100m2 = median(cpue_gper100m2),
#     .groups = 'drop'
#   ) %>% 
#   mutate(
#     dy = 1
#   ) %>% 
#   unite('date', yr, mo, dy, sep = '-') %>% 
#   mutate(
#     date = ymd(date), 
#     doy = yday(date), 
#     yr = year(date),
#     cont_year = decimal_date(date)
#   ) 

tomod <- algdat
tmp <- tomod %>% 
  filter(bay_segment == 'LTB') %>% 
  filter(Gear == '21.3m') %>% 
  mutate(cont_year = decimal_date(date))

mod <- gam(cpue_gper100m2 ~ s(cont_year, k = 20) + s(doy, bs = 'cc') + ti(cont_year, doy, bs = c('tp', 'cc')), data = tmp)

toplo <- tmp %>% 
  mutate(
    prd = predict(mod, newdata = ., na.action = )
  )

ggplot(toplo, aes(x = doy, y = prd, group = yr, color = yr)) + 
  geom_line()

ggplot(toplo, aes(x = date, y = prd, group = yr)) + 
  geom_line() + 
  geom_point(aes(y = cpue_gper100m2)) + 
  scale_y_log10()
