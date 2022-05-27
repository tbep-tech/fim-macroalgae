library(tidyverse)
library(sf)
library(lubridate)
library(mgcv)
library(wqtrends)
library(hrbrthemes)
library(here)
library(showtext)
library(RColorBrewer)
library(tbeptools)
library(wqtrends)
library(flextable)

data(algdat)
data(alldat)

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"
 
showtext_auto()
showtext_opts(dpi = 300)

thm <- theme_ipsum(base_family = fml, plot_margin = margin(10, 10, 10, 10)) + 
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 12), 
    axis.title.y = element_text(hjust = 0.5, size = 12), 
    legend.position = 'top'
  )

# barplot counts of species info in transect data -------------------------

sgdat <- read_transect(raw = T)

toplo <- sgdat %>% 
  pull(Species) %>%  
  table %>% 
  data.frame %>% 
  rename(
    Description = '.', 
    Count = Freq
  ) %>% 
  arrange(Count) %>% 
  filter(!Description == 'No Cover') %>% 
  mutate(
    Description = factor(Description, levels = Description), 
    lab = format(Count, format = 'd', big.mark = ','), 
    sav = ifelse(Description %in% c('Halodule', 'Halophila spp.', 'Ruppia', 'Syringodium', 'Thalassia'), 'SAV', 'non-SAV'),
    sav = factor(sav, levels = c('SAV', 'non-SAV'))
  )

p <- ggplot(toplo, aes(y = Description, x = Count)) + 
  geom_bar(stat = 'identity', alpha = 0.7, aes(color = sav, fill = sav), size = 0.7, width = 0.7) + 
  scale_color_manual(values = c('#00806E', '#958984')) +
  scale_fill_manual(values = c('#00806E', '#958984')) +
  geom_text(aes(label = lab), nudge_x = 200, hjust = 0) + 
  scale_x_continuous(limits = c(0, max(toplo$Count) * 1.1)) + 
  thm + 
  theme(
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line()
  ) +
  labs(
    y = NULL, 
    color = NULL,
    fill = NULL,
    title = 'Count of records',
    subtitle = 'Tampa Bay Annual Transect Monitoring, 1998 to 2021'
  )

jpeg(here('figs/trncounts.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
print(p)
dev.off()

# bar plot counts of relevant macroalgae descriptions ---------------------

# macroalgae relevant descriptions im fimraw
dscrp <- c('Syringodium spp.', 'Thalassia spp.', 'Ruppia spp.', 'Halophila spp.', 'Halophila decipiens (paddle grass)', 'Halophila engelmanii  (star grass)', 'Halodule spp.', 'Acanthophora spp.', 'Algae: Drift', 'Algae: Filamentous green', 'Algae: Filamentous red', 'Algae: Mixed', 'Caulerpa spp.', 'Dapis/Lyngbya spp. (filamentous cyanobacteria)', 'Gracilaria spp.', 'Ulva spp.')

toplo <- alldat %>% 
  filter(Description %in% dscrp) %>% 
  filter(Gear %in% c(20, 160)) %>%
  pull(Description) %>% 
  table %>% 
  data.frame %>% 
  rename(
    Description = '.', 
    Count = Freq
  ) %>% 
  arrange(Count) %>% 
  mutate(
    Description = factor(Description, levels = Description), 
    lab = format(Count, format = 'd', big.mark = ','), 
    sav = ifelse(Description %in% c('Syringodium spp.', 'Thalassia spp.', 'Ruppia spp.', 'Halophila spp.', 'Halophila decipiens (paddle grass)', 'Halophila engelmanii  (star grass)', 'Halodule spp.'), 'SAV', 'non-SAV'), 
    sav = factor(sav, levels = c('SAV', 'non-SAV'))
  )

p <- ggplot(toplo, aes(y = Description, x = Count)) + 
  geom_bar(stat = 'identity', alpha = 0.7, aes(color = sav, fill = sav), size = 0.7, width = 0.7) + 
  scale_color_manual(values = c('#00806E', '#958984')) +
  scale_fill_manual(values = c('#00806E', '#958984')) +
  geom_text(aes(label = lab), nudge_x = 200, hjust = 0) + 
  scale_x_continuous(limits = c(0, max(toplo$Count) * 1.1)) + 
  thm + 
  theme(
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line()
  ) +
  labs(
    y = NULL, 
    color = NULL,
    fill = NULL,
    title = 'Count of records',
    subtitle = 'FIM bycatch for Tampa Bay, 1998 to 2020'
  )

jpeg(here('figs/fimcounts.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
print(p)
dev.off()

# transect fo -------------------------------------------------------------

sgdat <- read_transect()

# get complete data, fill all species not found with zero
datcmp <- sgdat %>%
  dplyr::filter(var %in% 'Abundance') %>%
  dplyr::mutate(
    Savspecies = dplyr::case_when(
      grepl('Caulerpa', Savspecies) ~ 'Caulerpa',
      grepl('^DR|^DA|^DG', Savspecies) ~ 'Macroalgae',
      T ~ Savspecies
    )
  ) %>%
  dplyr::select(Date, Transect, Site, Savspecies, bb = aveval) %>%
  dplyr::group_by(Date, Transect, Site, Savspecies) %>%
  dplyr::summarise(bb = mean(bb, na.rm = T), .groups = 'drop') %>%
  dplyr::ungroup() %>%
  tidyr::complete(Savspecies, tidyr::nesting(Date, Transect, Site), fill = list(bb = 0))

# make no cover a five for bb if nothing else found
datcmp <- datcmp %>%
  dplyr::group_by(Transect, Date, Site) %>%
  dplyr::mutate(
    bb = dplyr::case_when(
      Savspecies == 'No Cover' ~ 0,
      T ~ bb
    ),
    bb = dplyr::case_when(
      sum(bb[!Savspecies %in% 'No Cover']) == 0 & Savspecies == 'No Cover' ~ 5,
      T ~ bb
    )
  )

# summarise fo/bb by unique sites per date/transect, this is better than commented code
transectocc <- datcmp %>%
  dplyr::group_by(Date, Transect, Savspecies) %>%
  dplyr::summarise(
    nsites = length(unique(Site)),
    foest = sum(bb > 0, na.rm = T) / nsites,
    bbest = sum(bb, na.rm = T) / nsites
  )

yrrng <- c(1998, 2021)

bay_segment <- c('OTB', 'HB', 'MTB', 'LTB')

sf::st_crs(trnpts) <- 4326

# pts by segment
trnptsshed <- trnpts %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::select(Transect = TRAN_ID, bay_segment) %>%
  unique

# fo by species
filtdat <- transectocc %>%
  dplyr::left_join(trnptsshed, by = 'Transect') %>%
  dplyr::mutate(
    yr = lubridate::year(Date)
  ) %>%
  dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
  dplyr::filter(bay_segment %in% !!bay_segment)

# retain results across segments
out <- filtdat %>%
  dplyr::filter(Savspecies %in% c('Caulerpa', 'Macroalgae')) %>%
  dplyr::group_by(yr, bay_segment, Savspecies) %>%
  dplyr::summarise(
    foest = mean(foest, na.rm = T),
    nsites = sum(nsites, na.rm = T),
    .groups = 'drop'
  ) %>%
  dplyr::mutate(
    bay_segment = factor(bay_segment, levels = !!bay_segment)
  )

p <- ggplot(out, aes(x = yr, y = 100 * foest, color = Savspecies)) +
  geom_line() + 
  geom_point(size = 2) +
  scale_color_manual(values = c('#00806E', '#958984')) +
  facet_wrap(~bay_segment, ncol = 2) + 
  thm + 
  labs(
    y = '% frequency occurrence', 
    x = NULL, 
    color = NULL, 
    caption = 'Freq. occurrence as total estimate across transects by year/segment'
  )

jpeg(here('figs/trnfo.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
print(p)
dev.off()

# observed plots ----------------------------------------------------------

ddg <- 0.4

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
  scale_color_manual(values = c('#00806E', '#958984')) +
  scale_y_log10() +
  facet_wrap(~bay_segment) + 
  thm + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(
    x = NULL, 
    y = 'median CPUE (25th/75th %tile)', 
    caption = 'CPUE as gallons / 100m2'
  )

jpeg(here('figs/obsmo.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
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
  scale_color_manual(values = c('#00806E', '#958984')) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~bay_segment) + 
  thm +
  labs(
    x = NULL, 
    y = 'median CPUE (25th/75th %tile)', 
    caption = 'CPUE as gallons / 100m2'
  )

jpeg(here('figs/obsyr.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
print(p)
dev.off()

p <- ggplot(toplo2, aes(x = yr, y = medv, group = Gear, color = Gear)) +
  geom_point(position = position_dodge(width = 0), size = 1) +
  # geom_line(position = position_dodge(width = ddg)) +
  geom_smooth(method = 'lm', se = F, linetype = 'solid') +
  # geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0, position = position_dodge(width = ddg)) +
  # scale_y_log10() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c('#00806E', '#958984')) +
  facet_wrap(~bay_segment) + 
  thm +
  labs(
    x = NULL, 
    y = 'median CPUE (25th/75th %tile)', 
    caption = 'CPUE as gallons / 100m2'
  )

jpeg(here('figs/obsyrlm.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
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
      
      gam(log10(1 + cpue_gper100m2) ~ s(cont_year, k = 20) + s(doy, bs = 'cc') + ti(cont_year, doy, bs = c('tp', 'cc')), data = x)
      
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

colyrs <- c(rep('lightgrey', 3), brewer.pal(9, 'Greens')[4:8])

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

jpeg(here('figs/gampreddoy.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
print(p)
dev.off()

p <- ggplot(modprd, aes(x = date, y = prd)) + 
  geom_line(color = '#00806E') + 
  geom_point(aes(y = cpue_gper100m2), size = 0.25) +
  scale_y_log10() +
  # scale_y_continuous(limits = c(0, 2)) +
  facet_grid(Gear ~ bay_segment) + 
  thm + 
  theme(axis.text.x = element_text(size = 9)) +
  labs(
    x = NULL, 
    y = 'Predicted CPUE', 
    caption = 'CPUE as gallons / 100m2'
  )

jpeg(here('figs/gampredann.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
print(p)
dev.off()
p <- ggplot(modprd, aes(x = date, y = `s(cont_year)`)) + 
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = `s(cont_year)` - `s(cont_year)_se`, ymax = `s(cont_year)` + `s(cont_year)_se`), fill = 'grey', alpha = 0.3) +
  geom_line(col = '#00806E') + 
  facet_grid(Gear ~ bay_segment) + 
  thm +
  theme(axis.text.x = element_text(size = 9)) +
  labs(
    x = NULL, 
    y = 's(annual)'
  )

jpeg(here('figs/gamann.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
print(p)
dev.off()

p <- ggplot(modprd, aes(x = doylb, y = `s(doy)`)) + 
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = `s(doy)` - `s(doy)_se`, ymax = `s(doy)` + `s(doy)_se`), fill = 'grey', alpha = 0.3) +
  geom_line(col = '#00806E') + 
  scale_x_date(date_labels = '%b', breaks = ymd('0000-01-01', '0000-04-01', '0000-07-01', '0000-10-01')) +
  facet_grid(Gear ~ bay_segment, scales = 'free_y') + 
  thm+ 
  labs(
    x = NULL, 
    y = 's(day of year)'
  )

jpeg(here('figs/gamseas.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
print(p)
dev.off()


# gam mixmeta trends ------------------------------------------------------

mods <- mods %>% 
  mutate(
    gammod = purrr::map(gammod, function(x){
      x$trans = 'log10'
      return(x)
    })
  )

mixln_fun <- function(mixmet, mod, yrstr, yrend){
  
  # subtitle info
  pval <- coefficients(summary(mixmet)) %>% data.frame %>% .[2, 4] %>% anlz_pvalformat()
  
  dispersion <- summary(mod)$dispersion
  
  # backtransform mixmeta predictions
  out <- data.frame(
    yr = seq(yrstr, yrend, length = 50)
  ) %>% 
    dplyr::mutate( 
      met = predict(mixmet, newdata = data.frame(yr = yr)), 
      se = predict(mixmet, newdata = data.frame(yr = yr), se = T)[, 2], 
      bt_lwr = 10^((met - 1.96 * se) + log(10) * dispersion / 2),
      bt_upr = 10^((met + 1.96 * se) + log(10) * dispersion / 2),
      bt_met = 10^(met + log(10) * dispersion / 2), 
      pval = pval
    )
  
  return(out)
  
}

ests <- tibble(
  yrstr = 2010, 
  yrend = 2020, 
  doystr = c(1, 1, 182, 60), 
  doyend = c(338, 181, 338, 212), 
  period = c('Jan - Dec', 'Jan - Jun', 'Jul - Dec', 'Mar - Jul')
) %>% 
  crossing(., mods) %>% 
  mutate(
    avgest = purrr::pmap(list(gammod, doystr, doyend), anlz_avgseason),
    mixmet = purrr::pmap(list(avgest, yrstr, yrend), anlz_mixmeta),
    trndln = purrr::pmap(list(mixmet, gammod, yrstr, yrend), mixln_fun)
  ) %>% 
  select(-data, -gammod, -gamprd, -mixmet) 

avgest <- ests %>% 
  select(-trndln) %>% 
  unnest('avgest')
trndln <- ests %>% 
  select(-avgest) %>% 
  unnest('trndln')

col1 <- '#958984'
col2 <- '#00806E'

pr <- 'Jan - Dec'
toplo1 <- avgest %>% 
  filter(period == pr)
toplo2 <- trndln %>% 
  filter(period == pr) %>% 
  mutate(
    pval = case_when(
      pval != 'ns' ~ 'p < 0.05', 
      T ~ pval
    ), 
    pval = factor(pval, levels = c('p < 0.05', 'ns'))
  )

# plot output
p <- ggplot(data = toplo1, aes(x = yr, y = bt_met)) + 
  geom_point(colour = col1) +
  geom_errorbar(aes(ymin = bt_lwr, ymax = bt_upr), colour = col1) +
  thm +
  theme(axis.text.x = element_text(size = 9)) +
  geom_ribbon(data = toplo2, aes(ymin = bt_lwr, ymax = bt_upr, fill = pval), alpha = 0.4) +
  geom_line(data = toplo2, aes(color = pval)) +
  scale_fill_manual(values = c(col2, NA)) + 
  scale_color_manual(values = c(col2, NA)) +
  facet_grid(Gear ~ bay_segment, scales = 'free_y') +
  labs(
    title = 'Estimated trends, 2010-2020',
    subtitle = paste(pr, 'CPUE averages'),
    y = 'Predicted CPUE', 
    x = NULL, 
    caption = 'CPUE as gallons / 100m2', 
    color = NULL, 
    fill = NULL
  )

jpeg(here('figs/gamtrnd1.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
print(p)
dev.off()

pr <- 'Jan - Jun'
toplo1 <- avgest %>% 
  filter(period == pr)
toplo2 <- trndln %>% 
  filter(period == pr) %>% 
  mutate(
    pval = case_when(
      pval != 'ns' ~ 'p < 0.05', 
      T ~ pval
    ), 
    pval = factor(pval, levels = c('p < 0.05', 'ns'))
  )

# plot output
p <- ggplot(data = toplo1, aes(x = yr, y = bt_met)) + 
  geom_point(colour = col1) +
  geom_errorbar(aes(ymin = bt_lwr, ymax = bt_upr), colour = col1) +
  thm +
  theme(axis.text.x = element_text(size = 9)) +
  geom_ribbon(data = toplo2, aes(ymin = bt_lwr, ymax = bt_upr, fill = pval), alpha = 0.4) +
  geom_line(data = toplo2, aes(color = pval)) +
  scale_fill_manual(values = c(col2, NA)) + 
  scale_color_manual(values = c(col2, NA)) +
  facet_grid(Gear ~ bay_segment, scales = 'free_y') +
  labs(
    title = 'Estimated trends, 2010-2020',
    subtitle = paste(pr, 'CPUE averages'),
    y = 'Predicted CPUE', 
    x = NULL, 
    caption = 'CPUE as gallons / 100m2', 
    color = NULL, 
    fill = NULL
  )

jpeg(here('figs/gamtrnd2.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
print(p)
dev.off()

pr <- 'Jul - Dec'
toplo1 <- avgest %>% 
  filter(period == pr)
toplo2 <- trndln %>% 
  filter(period == pr) %>% 
  mutate(
    pval = case_when(
      pval != 'ns' ~ 'p < 0.05', 
      T ~ pval
    ), 
    pval = factor(pval, levels = c('p < 0.05', 'ns'))
  )

# plot output
p <- ggplot(data = toplo1, aes(x = yr, y = bt_met)) + 
  geom_point(colour = col1) +
  geom_errorbar(aes(ymin = bt_lwr, ymax = bt_upr), colour = col1) +
  thm +
  theme(axis.text.x = element_text(size = 9)) +
  geom_ribbon(data = toplo2, aes(ymin = bt_lwr, ymax = bt_upr, fill = pval), alpha = 0.4) +
  geom_line(data = toplo2, aes(color = pval)) +
  scale_fill_manual(values = c(col2, NA)) + 
  scale_color_manual(values = c(col2, NA)) +
  facet_grid(Gear ~ bay_segment, scales = 'free_y') +
  labs(
    title = 'Estimated trends, 2010-2020',
    subtitle = paste(pr, 'CPUE averages'),
    y = 'Predicted CPUE', 
    x = NULL, 
    caption = 'CPUE as gallons / 100m2', 
    color = NULL, 
    fill = NULL
  )

jpeg(here('figs/gamtrnd3.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
print(p)
dev.off()

pr <- 'Mar - Jul'
toplo1 <- avgest %>% 
  filter(period == pr)
toplo2 <- trndln %>% 
  filter(period == pr) %>% 
  mutate(
    pval = case_when(
      pval != 'ns' ~ 'p < 0.05', 
      T ~ pval
    ), 
    pval = factor(pval, levels = c('p < 0.05', 'ns'))
  )

# plot output
p <- ggplot(data = toplo1, aes(x = yr, y = bt_met)) + 
  geom_point(colour = col1) +
  geom_errorbar(aes(ymin = bt_lwr, ymax = bt_upr), colour = col1) +
  thm +
  theme(axis.text.x = element_text(size = 9)) +
  geom_ribbon(data = toplo2, aes(ymin = bt_lwr, ymax = bt_upr, fill = pval), alpha = 0.4) +
  geom_line(data = toplo2, aes(color = pval)) +
  scale_fill_manual(values = c(col2, NA)) + 
  scale_color_manual(values = c(col2, NA)) +
  facet_grid(Gear ~ bay_segment, scales = 'free_y') +
  labs(
    title = 'Estimated trends, 2010-2020',
    subtitle = paste(pr, 'CPUE averages'),
    y = 'Predicted CPUE', 
    x = NULL, 
    caption = 'CPUE as gallons / 100m2', 
    color = NULL, 
    fill = NULL
  )

jpeg(here('figs/gamtrnd4.jpeg'), height = 5, width = 10, family = fml, units = 'in', res = 300)
print(p)
dev.off()

# gam model fits ----------------------------------------------------------

fits <- mods %>% 
  mutate(
    fit = purrr::map(gammod, anlz_fit)
  ) %>% 
  select(Gear, `Bay segment` = bay_segment, fit) %>% 
  mutate(`Bay segment` = factor(`Bay segment`, levels = c('OTB', 'HB', 'MTB', 'LTB'))) %>% 
  arrange(Gear, `Bay segment`) %>% 
  unnest('fit') %>% 
  as_grouped_data(groups = 'Gear')
  
tab <- flextable(fits) %>% 
  colformat_double(digits = 2)

save_as_image(tab, path = 'figs/tab.png')
