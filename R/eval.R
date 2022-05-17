library(tidyverse)
library(sf)
library(lubridate)
library(mgcv)
library(wqtrends)
library(hrbrthemes)
library(here)
library(showtext)
library(RColorBrewer)

data(algdat)

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

showtext_auto()
showtext_opts(dpi = 300)

# observed plots ----------------------------------------------------------

ddg <- 0.4

thm <- theme_ipsum(base_family = fml, plot_margin = margin(10, 10, 10, 10)) + 
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 12), 
    axis.title.y = element_text(hjust = 0.5, size = 12), 
    legend.position = 'top'
  )

toplo1 <- algdat %>% 
  group_by(mo, bay_segment, Gear) %>% 
  summarise(
    cnt = n(), 
    medv = median(cpue_gper100m2, na.rm = T), 
    lov = quantile(cpue_gper100m2, 0.25, na.rm = T),
    hiv = quantile(cpue_gper100m2, 0.75, na.rm = T), 
    .groups = 'drop'
  )
  
p <- ggplot(toplo1, aes(x = mo, y = medv, group = Gear, color = Gear)) +
  geom_point(position = position_dodge(width = ddg), size = 2) +
  geom_line(position = position_dodge(width = ddg)) + 
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0, position = position_dodge(width = ddg), size = 0.25) +
  # geom_violin() + 
  scale_y_log10() +
  facet_wrap(~bay_segment) + 
  thm + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(
    x = NULL, 
    y = 'median CPUE (25th/75th %tile)', 
    caption = 'CPUE as gallons / 100m2'
  )

jpeg(here('figs/obsmo.jpeg'), height = 5, width = 6, family = fml, units = 'in', res = 500)
print(p)
dev.off()

toplo2 <- algdat %>% 
  group_by(yr, bay_segment, Gear) %>% 
  summarise(
    cnt = n(), 
    medv = median(cpue_gper100m2, na.rm = T), 
    lov = quantile(cpue_gper100m2, 0.25, na.rm = T),
    hiv = quantile(cpue_gper100m2, 0.75, na.rm = T), 
    .groups = 'drop'
  )

p <- ggplot(toplo2, aes(x = yr, y = medv, group = Gear, color = Gear)) +
  geom_point(position = position_dodge(width = ddg), size = 1) +
  geom_line(position = position_dodge(width = ddg)) +
  # geom_smooth(method = 'lm', se = F, linetype = 'solid') +
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0, position = position_dodge(width = ddg), size = 0.25) +
  # scale_y_log10() +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~bay_segment) + 
  thm +
  labs(
    x = NULL, 
    y = 'median CPUE (25th/75th %tile)', 
    caption = 'CPUE as gallons / 100m2'
  )

jpeg(here('figs/obsyr.jpeg'), height = 5, width = 6, family = fml, units = 'in', res = 500)
print(p)
dev.off()

p <- ggplot(toplo2, aes(x = yr, y = medv, group = Gear, color = Gear)) +
  geom_point(position = position_dodge(width = 0), size = 1) +
  # geom_line(position = position_dodge(width = ddg)) +
  geom_smooth(method = 'lm', se = F, linetype = 'solid') +
  # geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0, position = position_dodge(width = ddg)) +
  # scale_y_log10() +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~bay_segment) + 
  thm +
  labs(
    x = NULL, 
    y = 'median CPUE (25th/75th %tile)', 
    caption = 'CPUE as gallons / 100m2'
  )

jpeg(here('figs/obsyrlm.jpeg'), height = 5, width = 6, family = fml, units = 'in', res = 500)
print(p)
dev.off()

# GAMs --------------------------------------------------------------------

mods <- algdat %>% 
  mutate(
    cont_year = decimal_date(date)
  ) %>% 
  group_by(bay_segment, Gear) %>% 
  nest() %>% 
  mutate(
    gammod = purrr::map(data, function(x){
      
      gam(log10(1 + cpue_gper100m2) ~ s(cont_year, k = 20) + s(doy, bs = 'cc'), data = x)
      
    }), 
    gamprd = purrr::pmap(list(data, gammod), function(data, gammod){

      prddat <- range(data$date) %>% 
        {seq.Date(floor_date(.[1], 'year'), ceiling_date(.[2], 'year'), by = 'day')} %>% 
        tibble(date = .) %>% 
        mutate(
          cont_year = decimal_date(date), 
          doy = yday(date)
        ) 
    
      out <- prddat %>% 
        mutate(
          prd = 10^predict(gammod, newdata = .) - 1, 
          prdse = 10^predict(gammod, newdata = ., se.fit = T)$se.fit - 1, 
          intercept = 10^gammod$coefficients[1] - 1
          ) %>% 
        bind_cols(10^predict(gammod, newdata= prddat, type = 'terms') - 1) %>% 
        bind_cols(10^predict(gammod, newdata= prddat, type = 'terms', se.fit = T)$se.fit - 1, .name_repair = 'minimal')
      
      names(out)[duplicated(names(out))] <- paste0(names(out)[duplicated(names(out))], '_se')
  
      return(out)
      
    })
  ) 

modprd <- mods %>%
  select(-data, -gammod) %>% 
  unnest('gamprd') %>% 
  left_join(algdat, by = c('date', 'Gear', 'bay_segment', 'doy')) %>% 
  mutate(
    Year = year(date), 
    mo = month(date), 
    doylb = date
  )
year(modprd$doylb) <- 0

modprf <- mods %>% 
  select(Gear, bay_segment, gammod) %>% 
  mutate(
    prf = purrr::map(gammod, anlz_fit)
  ) %>% 
  select(-gammod) %>% 
  unnest('prf')

thm <- theme_ipsum(base_family = fml, plot_margin = margin(10, 10, 10, 10)) + 
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 12), 
    axis.title.y = element_text(hjust = 0.5, size = 12), 
    strip.text = element_text(hjust = 0.5), 
    legend.position = 'top',
    legend.key.width = unit(dev.size()[1] / 10, "inches")
  )

colyrs <- c(rep('lightgrey', 3), brewer.pal(9, 'Greys')[5:8])

p <- ggplot(modprd, aes(x = doylb, y = prd, group = Year, color = Year)) + 
  geom_line() + 
  # scale_y_log10() + 
  scale_color_gradientn(colors = colyrs) +
  scale_x_date(date_labels = '%b', breaks = ymd('0000-01-01', '0000-04-01', '0000-07-01', '0000-10-01')) +
  facet_grid(Gear ~ bay_segment, scales = 'free_y') + 
  thm + 
  labs(
    x = NULL, 
    y = 'Predicted CPUE', 
    caption = 'CPUE as gallons / 100m2'
  )

jpeg(here('figs/gampreddoy.jpeg'), height = 5.5, width = 8, family = fml, units = 'in', res = 300)
print(p)
dev.off()

p <- ggplot(modprd %>% filter(Gear == '183m'), aes(x = date, y = prd)) + 
  geom_line(color = 'tomato1') + 
  geom_point(aes(y = cpue_gper100m2), size = 0.25) +
  scale_y_log10() +
  facet_grid(bay_segment ~ .) + 
  thm + 
  labs(
    subtitle = '183m',
    x = NULL, 
    y = 'Predicted CPUE', 
    caption = 'CPUE as gallons / 100m2'
  )

jpeg(here('figs/gam183predann.jpeg'), height = 5, width = 8, family = fml, units = 'in', res = 300)
print(p)
dev.off()

p <- ggplot(modprd %>% filter(Gear == '21.3m'), aes(x = date, y = prd)) + 
  geom_line(color = 'tomato1') + 
  geom_point(aes(y = cpue_gper100m2), size = 0.25) +
  scale_y_log10() +
  facet_grid(bay_segment ~ .) + 
  thm + 
  labs(
    subtitle = '21.3m',
    x = NULL, 
    y = 'Predicted CPUE', 
    caption = 'CPUE as gallons / 100m2'
  )

jpeg(here('figs/gam213predann.jpeg'), height = 5, width = 8, family = fml, units = 'in', res = 300)
print(p)
dev.off()

p <- ggplot(modprd %>% filter(Gear == '183m'), aes(x = date, y = `s(cont_year)`)) + 
  geom_ribbon(aes(ymin = `s(cont_year)` - `s(cont_year)_se`, ymax = `s(cont_year)` + `s(cont_year)_se`), fill = 'grey', alpha = 0.3) +
  geom_line(col = 'tomato1') + 
  facet_grid(bay_segment ~ .) + 
  thm +
  labs(
    subtitle = '183m',
    x = NULL
  )

jpeg(here('figs/gam183ann.jpeg'), height = 5, width = 8, family = fml, units = 'in', res = 300)
print(p)
dev.off()

p <- ggplot(modprd %>% filter(Gear == '21.3m'), aes(x = date, y = `s(cont_year)`)) + 
  geom_ribbon(aes(ymin = `s(cont_year)` - `s(cont_year)_se`, ymax = `s(cont_year)` + `s(cont_year)_se`), fill = 'grey', alpha = 0.3) +
  geom_line(col = 'tomato1') + 
  facet_grid(bay_segment ~ .) + 
  thm +
  labs(
    subtitle = '21.3m',
    x = NULL
  )

jpeg(here('figs/gam213ann.jpeg'), height = 5, width = 8, family = fml, units = 'in', res = 300)
print(p)
dev.off()

p <- ggplot(modprd, aes(x = doylb, y = `s(doy)`)) + 
  geom_ribbon(aes(ymin = `s(doy)` - `s(doy)_se`, ymax = `s(doy)` + `s(doy)_se`), fill = 'grey', alpha = 0.3) +
  geom_line(col = 'tomato1') + 
  scale_x_date(date_labels = '%b', breaks = ymd('0000-01-01', '0000-04-01', '0000-07-01', '0000-10-01')) +
  facet_grid(Gear ~ bay_segment, scales = 'free_y') + 
  thm+ 
  labs(
    x = NULL
  )

jpeg(here('figs/gamseas.jpeg'), height = 5.5, width = 8, family = fml, units = 'in', res = 300)
print(p)
dev.off()
