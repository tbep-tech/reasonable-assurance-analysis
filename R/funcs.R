# get legend from an existing ggplot object
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

show_rathrplot <- function(datin, bay_segment = c('OTB', 'HB', 'MTB', 'LTB', 'BCBS', 'TCB', 'MR'), thr = c('chla', 'la'), trgs = NULL, yrrng = c(1975, 2019),
                           family = NA, labelexp = TRUE, txtlab = TRUE, thrs = FALSE, partialyr = FALSE){
  
  
  maxyr <- yrrng[2]
  
  # default targets from data file
  if(is.null(trgs))
    trgs <- targets
  
  # yrrng must be in ascending order
  if(yrrng[1] >= yrrng[2])
    stop('yrrng argument must be in ascending order, e.g., c(1975, 2019)')
  
  # segment
  bay_segment <- match.arg(bay_segment)
  
  # wq to plot
  thr <- match.arg(thr)
  
  # colors
  cols <- c("Annual Mean"="red", "Management Target"="blue", "+1 se (small exceedance)"="blue", "+2 se (large exceedance)"="blue")
  
  # averages
  aves <- raanlz_avedat(datin, partialyr = partialyr)

  # axis label
  if(labelexp)
    axlab <- dplyr::case_when(
      thr == 'chla' ~ expression("Mean Ann. Chl-a ("~ mu * "g\u00B7L"^-1 *")")
    )
  if(!labelexp)
    axlab <- dplyr::case_when(
      thr == 'chla' ~ "Mean Ann. Chl-a (ug/L)"
    )
  
  # get lines to plot
  toln <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment)
  trgnum <- toln %>% dplyr::pull(!!paste0(thr, '_target'))
  smlnum <- toln %>% dplyr::pull(!!paste0(thr, '_smallex'))
  thrnum <- toln %>% dplyr::pull(!!paste0(thr, '_thresh'))
  
  
  # change label location if thrs is true
  if(!thrs)
    num <- trgnum
  if(thrs)
    num <- thrnum
  
  # threshold label
  if(labelexp)
    trglab <- dplyr::case_when(
      thr == 'chla' ~ paste(num, "~ mu * g%.%L^{-1}")
    )
  if(!labelexp)
    trglab <- dplyr::case_when(
      thr == 'chla' ~ paste(num, "ug/L")
    )
  
  # bay segment plot title
  ttl <- trgs %>%
    dplyr::filter(bay_segment %in% !!bay_segment) %>%
    dplyr::pull(name)
  
  if(partialyr)
    ttl <- paste0(ttl, '*')
  
  # get data to plo
  toplo <- aves$ann %>%
    dplyr::filter(grepl(paste0('_', thr, '$'), var)) %>%
    mutate(var = 'yval') %>%
    dplyr::filter(bay_segment == !!bay_segment) %>%
    dplyr::filter(yr >= yrrng[1] & yr <= yrrng[2]) %>%
    tidyr::spread(var, val)
  
  p <- ggplot(toplo) +
    geom_rect(xmin = 2017, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'grey', alpha = 0.6) + 
    geom_point(data = toplo, aes(x = yr, y = yval, colour = "Annual Mean"), size = 3) +
    geom_line(data = toplo, aes(x = yr, y = yval, colour = "Annual Mean"), linetype = 'solid', size = 0.75) +
    labs(y = axlab, title = ttl) +
    scale_x_continuous(breaks = seq(1975, maxyr, by = 5)) +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          plot.background = element_rect(fill = NA, color = NA),
          legend.position = 'top',#c(0.85, 0.95),
          legend.background = element_rect(fill=NA),
          legend.key = element_rect(fill = '#ECECEC'),
          legend.title = element_blank(),
          axis.text.y = element_text(colour = 'black', size = 14),
          axis.title = element_blank(),
          plot.title = element_text(size = 22, colour = 'black'),
          legend.text = element_text(size = 16, colour = 'black'),
          axis.text.x = element_text(colour = 'black', angle = 0, size = 14, hjust = 0.5),
          text = element_text(family)
    )
  
  # all targets/thresholds
  if(!thrs)
    p <- p +
    geom_hline(aes(yintercept = trgnum, colour = 'Management Target')) +
    geom_hline(aes(yintercept = smlnum, colour = '+1 se (small exceedance)'), linetype = 'dashed') +
    geom_hline(aes(yintercept = thrnum, colour = '+2 se (large exceedance)'), linetype = 'dotted') +
    scale_colour_manual(values = cols, labels = factor(names(cols), levels = names(cols))) +
    guides(colour = guide_legend(
      override.aes = list(
        shape = c(19, NA, NA, NA),
        colour = cols,
        linetype = c('solid', 'solid', 'dashed', 'dotted'),
        size = c(0.75, 0.5, 0.5, 0.5)
      )
    ))
  
  # thresholds only
  if(thrs)
    p <- p +
    geom_hline(aes(yintercept = thrnum, colour = '+2 se (large exceedance)'), linetype = 'dotted') +
    scale_colour_manual(values = cols[c(1, 4)], labels = factor(names(cols[c(1, 4)]), levels = names(cols[c(1, 4)]))) +
    guides(colour = guide_legend(
      override.aes = list(
        shape = c(19, NA),
        colour = cols[c(1, 4)],
        linetype = c('solid', 'dotted'),
        size = c(0.75, 0.5)
      )
    ))
  
  if(txtlab & !thrs)
    p <- p +
    geom_text(aes(yrrng[1], num, label = trglab), parse = labelexp, hjust = 0.2, vjust = 1, family = family, colour = 'blue')
  
  if(txtlab & thrs)
    p <- p +
    geom_text(aes(yrrng[1], max(yval), label = trglab), parse = labelexp, hjust = 0.2, vjust = 1, family = family, colour = 'blue')
  
  
  if(partialyr)
    p <- p +
    labs(caption = paste0('*Incomplete data for ', max(yrrng), ' estimated by five year average'))
  
  return(p)
  
}

raanlz_avedat <- function(datin, partialyr = FALSE){
  
  # year month averages
  # long format, separate bay_segment for MTB into sub segs
  # mtb year month averages are weighted
  moout <- datin %>%
    dplyr::select(yr, mo, bay_segment, station, chla) %>%
    tidyr::gather('var', 'val', chla) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      bay_segment = dplyr::case_when(
        station %in% c(9, 11, 81, 84) ~ "MT1",
        station %in% c(13, 14, 32, 33) ~ "MT2",
        station %in% c(16, 19, 28, 82) ~ "MT3",
        TRUE ~ bay_segment
      )
    ) %>%
    dplyr::group_by(bay_segment, yr, mo, var) %>%
    dplyr::summarise(val = mean(val), .groups = 'drop') %>%
    drop_na() %>%
    dplyr::mutate(
      val = dplyr::case_when(
        bay_segment %in% "MT1" ~ val * 2108.7,
        bay_segment %in% "MT2" ~ val * 1041.9,
        bay_segment %in% "MT3" ~ val * 974.6,
        TRUE ~ val
      ),
      bay_segment = dplyr::case_when(
        bay_segment %in% c('MT1', 'MT2', 'MT3') ~ 'MTB',
        TRUE ~ bay_segment
      )
    ) %>%
    dplyr::group_by(bay_segment, yr, mo, var) %>%
    dplyr::summarise(
      val = sum(val), 
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      val = dplyr::case_when(
        bay_segment %in% 'MTB' ~ val / 4125.2,
        TRUE ~ val
      )
    ) %>%
    dplyr::filter(!is.na(val)) %>%
    dplyr::filter(!is.infinite(val)) %>%
    dplyr::arrange(var, yr, mo, bay_segment)
  
  # add partial year
  if(partialyr){
    
    # years to averge, last five complete
    maxyr <- max(moout$yr)
    yrfl <- c(maxyr - 5, maxyr - 1)
    
    # months to fill
    mofl <- moout %>%
      dplyr::filter(yr %in% maxyr) %>%
      dplyr::pull(mo) %>%
      unique %>%
      setdiff(1:12, .)
    
    # month averages
    moave <- moout %>%
      dplyr::filter(yr >= yrfl[1] & yr <= yrfl[2]) %>%
      dplyr::group_by(bay_segment, mo, var) %>%
      summarise(
        val = mean(val, na.rm = TRUE), 
        .groups = 'drop'
        ) %>%
      dplyr::filter(mo %in% mofl) %>%
      dplyr::mutate(yr = maxyr)
    
    # join missing months to
    moout <- moout %>%
      dplyr::bind_rows(moave) %>%
      dplyr::arrange(var, yr, mo, bay_segment)
    
  }
  
  # annual data
  anout <- moout %>%
    dplyr::group_by(yr, bay_segment, var) %>%
    dplyr::summarise(
      val = mean(val), 
      .groups = 'drop'
      ) %>%
    dplyr::mutate(
      var = dplyr::case_when(
        var == 'chla' ~ 'mean_chla',
        TRUE ~ var
      )
    ) %>%
    tidyr::spread('var', 'val') %>%
    tidyr::gather('var', 'val', mean_chla) %>%
    dplyr::filter(!is.na(val)) %>%
    dplyr::filter(!is.infinite(val)) %>%
    dplyr::arrange(var, yr, bay_segment)
  
  # mo dat to light attenuation
  moout <- moout %>%
    dplyr::mutate(
      var = dplyr::case_when(
        var == 'chla' ~ 'mean_chla'
      )
    )
  
  # combine all
  out <- list(
    ann = anout,
    mos = moout
  )
  
  return(out)
  
}

# calculate difference between updated and original dps data for hfc/city of tampa
# tn, tp, tss, bod, hy
# tn, tp, tss, bod as tons
# hy as 10^6 m^3
#
# results as per month or per year based on agg fun
# total logical indicating if diffs are separated as reuse/end of pipe or total of both
# optional varsel as character string for one to many loading variables to return only as diff in wide format
dpsdiff_fun <- function(dpsupdate, annual = F, total = F, varsel = NULL){
  
  ##
  # original data
  # domestic point source prior to 2017-2021 RA 
  dpsmosdat1 <- read_sas(here('data-raw/dps0420monthentbas.sas7bdat')) %>% 
    filter(Year < 2017)
  dpsmosdat2 <- read_sas(here('data-raw/dps1721monthentbas.sas7bdat')) 
  
  olddat <- bind_rows(dpsmosdat1, dpsmosdat2) %>% 
    filter(entity == 'Tampa') %>% 
    filter(Year > 2011) %>%  # earliest year in updated data is 2012
    mutate(
      source = case_when(
        grepl('REUSE$', source2) ~ 'DPS - reuse', 
        grepl('SW$', source2) ~ 'DPS - end of pipe'
      )
    ) %>% 
    select(
      Year,
      Month,
      entity, 
      source, 
      tn_load = tnloadtons, 
      tp_load = tploadtons, 
      tss_load = tssloadtons, 
      bod_load = bodloadtons,
      hy_load = h2oload10e6m3
    )
  
  ##
  # hfc/city of tampa updated data
  newdat <- dpsupdate %>% 
    filter(Year < 2022)
  
  ##
  # combine
  
  # prep for plot
  cmbdat <- full_join(olddat, newdat, by = c('Year', 'Month', 'entity', 'source'), suffix = c('.old', '.new')) %>% 
    pivot_longer(names_to = 'var', values_to = 'val', -c(Year, Month, entity, source, bay_segment)) %>%
    separate(var, into = c('var', 'type'), sep = '\\.') %>% 
    pivot_wider(names_from = type, values_from = val) %>% 
    rename(year = Year, month = Month)
  
  # get total dps if true
  if(total)
    cmbdat <- cmbdat %>% 
    mutate(
      source = 'DPS'
    ) %>% 
    summarise(
      old = sum(old, na.rm = T), 
      new = sum(new, na.rm = T), 
      .by = c(year, month, entity, source, bay_segment, var)
    )
  
  # get annual loads if true
  if(annual)
    cmbdat <- cmbdat %>% 
    summarize(
      old = sum(old, na.rm = T), 
      new = sum(new, na.rm = T),
      .by = c(year, entity, source, bay_segment, var)
    )
  
  # get differece between new and original data
  out <- cmbdat %>% 
    mutate(
      diffv = new - old
    ) 
  
  # subset variables, return only diff in wide format
  if(!is.null(varsel)){
    
    if(any(!varsel %in% c('tn_load', 'tp_load', 'tss_load', 'bod_load', 'hy_load')))
      stop('varsel must be one to many of tn_load, tp_load, tss_load, bod_load, hy_load')
    
    out <- out %>%
      filter(var %in% varsel) %>%
      select(-new, -old) %>%
      mutate(var = paste0(var, '_diffv')) %>%
      pivot_wider(names_from = 'var', values_from = 'diffv')
    
  }
  
  return(out)
  
}
