
# preamble ----------------------------------------------------------------
library(hrbrthemes)
library(scales)
library(egg)
library(ggchicklet)
library(tidytext)
library(data.table)
library(tidyverse)
library(cowplot)
library(showtext)
library(readxl)
library(see)
library(emmeans)


showtext_auto()
sysfonts::font_add_google("Roboto Condensed")


# load data and carpentry -------------------------------------------------
dx <- fread("Data/rl_germany_eivs.csv")
head(dx)

# how many species have eiv values----
nrow(dx)

dx %>% filter(!is.na(L)) %>% nrow
dx %>% filter(!is.na(T)) %>% nrow
dx %>% filter(!is.na(C)) %>% nrow
dx %>% filter(!is.na(M)) %>% nrow
dx %>% filter(!is.na(R)) %>% nrow
dx %>% filter(!is.na(N)) %>% nrow

# how many species are threatened 2018----
# total
dx %>% nrow

# not assessed
dx %>% filter(rl2018 == "nb") %>% nrow +
  dx %>% filter(rl2018 == "D") %>% nrow

# threatened or extinct
dx %>% filter(rl2018 == "0") %>% nrow +
  dx %>% filter(rl2018 == "1") %>% nrow +
  dx %>% filter(rl2018 == "2") %>% nrow +
  dx %>% filter(rl2018 == "3") %>% nrow +
  dx %>% filter(rl2018 == "G") %>% nrow

1359 / (5312 - 990)
#31.4 percent of assessed species threatened

# how many species are threatened 1998----
# total
dx %>% nrow

# not assessed
dx %>% filter(rl1998 == "nb") %>% nrow +
  dx %>% filter(rl1998 == "D") %>% nrow +
  dx %>% filter(rl1998 == "kN") %>% nrow +
  dx %>% filter(rl1998 == "fehlt") %>% nrow


# threatened or extinct
dx %>% filter(rl1998 == "0") %>% nrow +
  dx %>% filter(rl1998 == "1") %>% nrow +
  dx %>% filter(rl1998 == "2") %>% nrow +
  dx %>% filter(rl1998 == "3") %>% nrow +
  dx %>% filter(rl1998 == "G") %>% nrow

973 / (5312 - 1460)
#25.3 percent of assessed species threatened

# change in overall threat status
0.3144378 - 0.2525961


# how many species in each eiv class? ----
dx_overall <- bind_rows(
  list(
    # L Number
    dx %>% group_by(L) %>% count() %>% gather("EIV",  "number", -n) %>% select(EIV, number, n),
    # T Number
    dx %>% group_by(T) %>% count() %>% gather("EIV",  "number", -n) %>% select(EIV, number, n),
    # C Number
    dx %>% group_by(C) %>% count() %>% gather("EIV",  "number", -n) %>% select(EIV, number, n),
    # M Number
    dx %>% group_by(M) %>% count() %>% gather("EIV",  "number", -n) %>% select(EIV, number, n),
    # R Number
    dx %>% group_by(R) %>% count() %>% gather("EIV",  "number", -n) %>% select(EIV, number, n),
    # N Number
    dx %>% group_by(N) %>% count() %>% gather("EIV",  "number", -n) %>% select(EIV, number, n)
  )
) %>% na.omit



# analysis for 2018 -------------------------------------------------------

# calculate proportion of threatened species in each eiv number class: 2018----
# how many species threatened in each eiv class? 2018
# add threatened cat
dx2018 <- dx %>% replace_na(list(rl2018 = "ne")) %>%
  mutate(
    threatened = ifelse(
      rl2018 == "0" |
        rl2018 == "1" |
        rl2018 == "2" |
        rl2018 == "3" |
        rl2018 == "G" ,
      "threatened",
      ifelse(
        rl2018 == "nb" |
          rl2018 == "D" |
          rl2018 == "ne" ,
        "not_evaluated",
        "not_threatened"
      )
    )
  )

dx2018_percent <- bind_rows(
  list(
    # L Number
    dx2018 %>% group_by(L) %>% count(threatened) %>% na.omit %>% spread(threatened, n) %>%
      gather("EIV",  "number",-threatened,-not_threatened,-not_evaluated) %>%
      select(EIV, number, not_threatened, threatened, not_evaluated) %>%
      replace(is.na(.), 0) %>%
      mutate(percentage = threatened / (not_threatened + threatened)) ,
    # T Number
    dx2018 %>% group_by(T) %>% count(threatened) %>% na.omit %>% spread(threatened, n) %>%
      gather("EIV",  "number",-threatened,-not_threatened,-not_evaluated) %>%
      select(EIV, number, not_threatened, threatened, not_evaluated) %>%
      replace(is.na(.), 0) %>%
      mutate(percentage = threatened / (not_threatened + threatened)) ,
    # C Number
    dx2018 %>% group_by(C) %>% count(threatened) %>% na.omit %>% spread(threatened, n) %>%
      gather("EIV",  "number",-threatened,-not_threatened,-not_evaluated) %>%
      select(EIV, number, not_threatened, threatened, not_evaluated) %>%
      replace(is.na(.), 0) %>%
      mutate(percentage = threatened / (not_threatened + threatened)),
    # M Number
    dx2018 %>% group_by(M) %>% count(threatened) %>% na.omit %>% spread(threatened, n) %>%
      gather("EIV",  "number",-threatened,-not_threatened,-not_evaluated) %>%
      select(EIV, number, not_threatened, threatened, not_evaluated) %>%
      replace(is.na(.), 0) %>%
      mutate(percentage = threatened / (not_threatened + threatened)) ,
    # R Number
    dx2018 %>% group_by(R) %>% count(threatened) %>% na.omit %>% spread(threatened, n) %>%
      gather("EIV",  "number",-threatened,-not_threatened,-not_evaluated) %>%
      select(EIV, number, not_threatened, threatened, not_evaluated) %>%
      replace(is.na(.), 0) %>%
      mutate(percentage = threatened / (not_threatened + threatened)) ,
    # N Number
    dx2018 %>% group_by(N) %>% count(threatened) %>% na.omit %>% spread(threatened, n) %>%
      gather("EIV",  "number",-threatened,-not_threatened,-not_evaluated) %>%
      select(EIV, number, not_threatened, threatened, not_evaluated) %>%
      replace(is.na(.), 0) %>%
      mutate(percentage = threatened / (not_threatened + threatened))
  )
)

dx2018_total <- full_join(dx_overall, dx2018_percent)

# calculate the number of evaluated species with eiv value in each class
# also calculate the percentage of threat. species by dividing by n (all species
# with eivs)
dx2018_total <- dx2018_total %>%
  mutate(n_eval = not_threatened + threatened,
         percentage_total = threatened / n)


# total number of species with eiv value for each eiv class
ntot2018 <-
  dx2018_total %>% group_by(EIV) %>% summarize(ntotal = sum(n_eval))


# figure 1a ----

group.colors <- c(
  C = "#ff5700",
  L = "#ffd700",
  M = "#00a8ff",
  N = "#ff5700",
  R = "#00ff57",
  T = "#ff0029"
)

to_string <- as_labeller(
  c(
    `L` = "Light",
    `C` = "Continentality",
    `M` = "Moisture",
    `N` = "Nutrients",
    `T` = "Temperature",
    `R` = "Reaction"
  )
)

data.segm1 <- data.frame(
  x = 5.6,
  y = 0.76,
  xend = 6.7,
  yend = 0.7,
  EIV = "L"
)
data.segm2 <- data.frame(
  x = 8,
  y = 0.37,
  xend = 7.9,
  yend = 0.15,
  EIV = "L"
)
data.segm3 <- data.frame(
  x = 4,
  y = 0.45,
  xend = 5,
  yend = 0.318,
  EIV = "L"
)

(
  ggplot(data = dx2018_total %>% filter(EIV == "L" |
                                          EIV == "N"), aes(x = number, col = EIV)) +
    facet_wrap(
      ~ factor(EIV, levels = c("L", "N")),
      scales = "free_x",
      labeller = to_string
    ) +
    geom_hline(yintercept = 0.314) +
    
    geom_curve(
      data = data.segm1,
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend
      ),
      arrow = arrow(length = unit(0.07, "inch")),
      size = 0.7,
      color = "grey30",
      curvature = -0.3,
      inherit.aes = FALSE
    ) +
    geom_curve(
      data = data.segm2,
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend
      ),
      arrow = arrow(length = unit(0.07, "inch")),
      size = 0.7,
      color = "grey30",
      curvature = -0.3,
      inherit.aes = FALSE
    ) +
    geom_curve(
      data = data.segm3,
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend
      ),
      arrow = arrow(length = unit(0.07, "inch")),
      size = 0.7,
      color = "grey30",
      curvature = -0.3,
      inherit.aes = FALSE
    ) +
    
    geom_text(
      data = data.segm1,
      aes(x = 4, y = 0.76),
      label = "All spp.",
      inherit.aes = F,
      hjust = 0,
      size = 3,
      family = "Roboto Condensed"
    ) +
    geom_text(
      data = data.segm2,
      aes(x = 5.4, y = 0.05),
      label = "Percent\nthreatened spp.",
      inherit.aes = F,
      hjust = 0,
      size = 3,
      family = "Roboto Condensed"
    ) +
    geom_text(
      data = data.segm1,
      aes(x = 1.8, y = 0.45),
      label = "Avg. threat\nstatus (31%)",
      inherit.aes = F,
      hjust = 0,
      size = 3,
      family = "Roboto Condensed"
    ) +
    geom_text(
      data = ntot2018 %>% filter(EIV == "L" |
                                   EIV == "N"),
      aes(label = paste("n =", ntotal)),
      x = -Inf,
      y = Inf,
      hjust = -0.1,
      vjust = 1,
      inherit.aes = FALSE,
      size = 3,
      fontface = 3,
      family = "Roboto Condensed"
    ) +
    geom_line(aes(y = n_eval / 1000), lty = 2) +
    geom_point(aes(y = n_eval / 1000), pch = 2) +
    geom_line(aes(y = percentage)) +
    geom_point(aes(y = percentage)) +
    scale_color_manual(values = group.colors) +
    scale_fill_manual(values = group.colors) +
    scale_y_continuous(
      name = "Percent threatened spp.\nper indicator value (2018)",
      sec.axis = sec_axis( ~ . * 1000,
                           name = "All spp. per indicator value")
    ) +
    scale_x_continuous(
      breaks = function(x)
        seq(ceiling(x[1]), floor(x[2]), by = 1)
    ) +
    theme_ipsum(
      grid = "",
      axis_title_size = 12,
      plot_title_size = 12,
      axis_title_just = "mm",
      base_family = "Roboto Condensed"
    ) +
    theme(
      plot.margin =  unit(rep(0.1, 4), "cm"),
      text = element_text(size = 14),
      axis.line = element_line(),
      legend.position = "none"
    ) +
    labs(x = "Ellenberg indicator values") -> fig1
)




# analysis for 1998 -------------------------------------------------------


# calculate proportion of threatened species in each eiv number class: 1998
# how many species threatened in each eiv class? 1998
# add threatened cat
dx1998 <- dx %>% mutate(
  threatened = ifelse(
    rl1998 == "0" |
      rl1998 == "1" |
      rl1998 == "2" |
      rl1998 == "3" |
      rl1998 == "G" ,
    "threatened",
    ifelse(
      rl1998 == "nb" |
        rl1998 == "D" |
        rl1998 == "kN" |
        rl1998 == "fehlt",
      "not_evaluated",
      "not_threatened"
    )
  )
) %>%
  filter(!is.na(threatened))


dx1998_percent <- bind_rows(
  list(
    # L Number
    dx1998 %>% group_by(L) %>% count(threatened) %>% na.omit %>% spread(threatened, n) %>%
      gather("EIV",  "number",-threatened,-not_threatened,-not_evaluated) %>%
      select(EIV, number, not_threatened, threatened, not_evaluated) %>%
      replace(is.na(.), 0) %>%
      mutate(percentage = threatened / (not_threatened + threatened)) ,
    # T Number
    dx1998 %>% group_by(T) %>% count(threatened) %>% na.omit %>% spread(threatened, n) %>%
      gather("EIV",  "number",-threatened,-not_threatened,-not_evaluated) %>%
      select(EIV, number, not_threatened, threatened, not_evaluated) %>%
      replace(is.na(.), 0) %>%
      mutate(percentage = threatened / (not_threatened + threatened)) ,
    # C Number
    dx1998 %>% group_by(C) %>% count(threatened) %>% na.omit %>% spread(threatened, n) %>%
      gather("EIV",  "number",-threatened,-not_threatened,-not_evaluated) %>%
      select(EIV, number, not_threatened, threatened, not_evaluated) %>%
      replace(is.na(.), 0) %>%
      mutate(percentage = threatened / (not_threatened + threatened)),
    # M Number
    dx1998 %>% group_by(M) %>% count(threatened) %>% na.omit %>% spread(threatened, n) %>%
      gather("EIV",  "number",-threatened,-not_threatened,-not_evaluated) %>%
      select(EIV, number, not_threatened, threatened, not_evaluated) %>%
      replace(is.na(.), 0) %>%
      mutate(percentage = threatened / (not_threatened + threatened)) ,
    # R Number
    dx1998 %>% group_by(R) %>% count(threatened) %>% na.omit %>% spread(threatened, n) %>%
      gather("EIV",  "number",-threatened,-not_threatened,-not_evaluated) %>%
      select(EIV, number, not_threatened, threatened, not_evaluated) %>%
      replace(is.na(.), 0) %>%
      mutate(percentage = threatened / (not_threatened + threatened)) ,
    # N Number
    dx1998 %>% group_by(N) %>% count(threatened) %>% na.omit %>% spread(threatened, n) %>%
      gather("EIV",  "number",-threatened,-not_threatened,-not_evaluated) %>%
      select(EIV, number, not_threatened, threatened, not_evaluated) %>%
      replace(is.na(.), 0) %>%
      mutate(percentage = threatened / (not_threatened + threatened))
  )
)


dx1998_total <- full_join(dx_overall, dx1998_percent)

# calculate the number of evaluated species with eiv value in each class
dx1998_total <- dx1998_total %>%
  mutate(n_eval = not_threatened + threatened,
         percentage_total = threatened / n)

# total number of species with eiv value for each eiv class
ntot1998 <-
  dx1998_total %>% group_by(EIV) %>% summarize(ntotal = sum(n_eval))


# Quantify percentage changes over time -----------------------------------

# calculate difference to 2018
diff <- full_join(
  dx1998_total %>%
    select(EIV, number, percentage, percentage_total) %>%
    rename(per1998 = percentage, per1998_total = percentage_total),
  
  dx2018_total %>% select(EIV, number, percentage, percentage_total) %>%
    rename(per2018 = percentage, per2018_total = percentage_total)
) %>%
  mutate(delta = per2018 - per1998,
         delta_total = per2018_total - per1998_total)


diff_ntot <-
  full_join(ntot2018, ntot1998 %>% rename(ntotal1998 = ntotal))


data.segm4 <- data.frame(
  x = 2,
  y = 0.09,
  xend = 2.5,
  yend = 0.062,
  EIV = "L"
)

# figure 1b ----

(
  ggplot(data = diff  %>% filter(EIV == "L" |
                                   EIV == "N"), aes(x = number, col = EIV)) +
    facet_wrap(
      ~ factor(EIV, levels = c("L", "N")),
      scales = "free_x",
      labeller = to_string
    ) +
    
    geom_curve(
      data = data.segm4,
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend
      ),
      arrow = arrow(length = unit(0.07, "inch")),
      size = 0.7,
      color = "grey30",
      curvature = 0.3,
      inherit.aes = FALSE
    ) +
    geom_text(
      data = data.segm1,
      aes(x = 1.9, y = 0.10),
      label = "Change in avg.\nthreat status (+6.2%)",
      inherit.aes = F,
      hjust = 0,
      vjust = 0,
      size = 3,
      family = "Roboto Condensed"
    ) +
    geom_point(aes(y = delta)) +
    geom_hline(yintercept = 0, lty = 3) +
    geom_hline(yintercept = 0.0618) +
    geom_line(aes(y = delta)) +
    scale_color_manual(values = group.colors) +
    scale_fill_manual(values = group.colors) +
    scale_x_continuous(
      breaks = function(x)
        seq(ceiling(x[1]), floor(x[2]), by = 1)
    ) +
    theme_ipsum(
      grid = "",
      axis_title_size = 12,
      plot_title_size = 12,
      axis_title_just = "mm",
      base_family = "Roboto Condensed"
    ) +
    theme(
      plot.margin =  unit(rep(0.1, 4), "cm"),
      strip.text = element_blank(),
      text = element_text(size = 14),
      axis.line = element_line(),
      legend.position = "none"
    ) +
    labs(x = "Ellenberg indicator values",
         y = "Change relative to 1998") -> fig2
)



# full data frame ---------------------------------------------------------

eiv_rl_total <- full_join(
  dx1998_total %>%
    select(
      EIV,
      number,
      n,
      threatened,
      not_threatened,
      not_evaluated,
      percentage,
      percentage_total
    ) %>%
    mutate(n_assessed1998 = threatened + not_threatened) %>%
    rename(
      threatened1998 = threatened,
      not_threatened1998 = not_threatened,
      not_evaluated1998 = not_evaluated,
      per1998 = percentage,
      per1998_total = percentage_total,
      n_total = n
    ),
  
  dx2018_total %>%
    select(
      EIV,
      number,
      threatened,
      not_threatened,
      not_evaluated,
      percentage,
      percentage_total
    ) %>%
    mutate(n_assessed2018 = threatened + not_threatened) %>%
    rename(
      threatened2018 = threatened,
      not_threatened2018 = not_threatened,
      not_evaluated2018 = not_evaluated,
      per2018 = percentage,
      per2018_total = percentage_total
    ),
) %>%
  mutate(delta = per2018 - per1998,
         delta_total = per2018_total - per1998_total) %>%
  select(
    EIV,
    number,
    n_total,
    n_assessed1998,
    n_assessed2018,
    threatened1998,
    threatened2018,
    not_threatened1998,
    not_threatened2018,
    not_evaluated1998,
    not_evaluated2018,
    per1998,
    per2018,
    per1998_total,
    per2018_total,
    delta,
    delta_total
  )

View(eiv_rl_total)
write.table(eiv_rl_total,
            "eiv_rl_total.csv",
            row.names = FALSE,
            sep = ",")


# what is the percentage of eivs to all threatened sp. ------------------------

# how many plants in Germany grow in low, mid, high eiv conditions.
eivcat <-
  eiv_rl_total %>% select(EIV, number, n_total, threatened1998, threatened2018) %>%
  mutate(
    cat = case_when(
      EIV == "L" & number <= 3 ~ "Shade\n(1-3)",
      EIV == "L" & number >= 3 & number <= 6 ~ "Half shade\n(4-6)",
      EIV == "L" & number >= 7 ~ "Full light\n(7-9)",
      
      EIV == "M" & number <= 3 ~ "Dry (1-3)",
      EIV == "M" & number >= 3 & number <= 6 ~ "Moist (4-6)",
      EIV == "M" & number >= 7 & number <= 9 ~ "Wet (7-9)",
      EIV == "M" & number >= 10 ~ "Water (10-12)",
      
      EIV == "T" & number <= 3 ~ "Alpine (1-3)",
      EIV == "T" & number >= 3 & number <= 6 ~ "Temperate (4-6)",
      EIV == "T" & number >= 7 ~ "Mediterranean (7-9)",
      
      EIV == "R" & number <= 3 ~ "Acidic (1-3)",
      EIV == "R" & number >= 3 & number <= 6 ~ "Neutral (4-6)",
      EIV == "R" & number >= 7 ~ "Alkaline (7-9)",
      
      EIV == "C" & number <= 3 ~ "Oceanic (1-3)",
      EIV == "C" & number >= 3 & number <= 6 ~ "Intermediate (4-6)",
      EIV == "C" & number >= 7 ~ "Continental (7-9)",
      
      EIV == "N" & number <= 3 ~ "N-poor\n(1-3)",
      EIV == "N" & number >= 3 & number <= 6 ~ "Intermediate\n(4-6)",
      EIV == "N" & number >= 7 ~ "N-rich\n(7-9)"
      
    )
  ) %>% group_by(EIV, cat) %>%
  summarise(
    nsum = sum(n_total),
    nthreat1998 = sum(threatened1998),
    nthreat2018 = sum(threatened2018)
  )


# how many species on each red list as threatened
# of these how many have eiv values
# 2018
dthreat <- dx %>% filter(rl2018 == "0" |
                           rl2018 == "1" |
                           rl2018 == "2" |
                           rl2018 == "3" |
                           rl2018 == "G") %>%
  select(submitted_name_rl, L, T, C, M, R, N) 

t2018_wn <- bind_rows(list(
  c(
    EIV = "L",
    t2018_wn = dthreat %>% filter(!is.na(L)) %>% nrow
  ),
  c(
    EIV = "T",
    t2018_wn = dthreat %>% filter(!is.na(T)) %>% nrow
  ),
  c(
    EIV = "C",
    t2018_wn = dthreat %>% filter(!is.na(C)) %>% nrow
  ),
  c(
    EIV = "M",
    t2018_wn = dthreat %>% filter(!is.na(M)) %>% nrow
  ),
  c(
    EIV = "R",
    t2018_wn = dthreat %>% filter(!is.na(R)) %>% nrow
  ),
  c(
    EIV = "N",
    t2018_wn = dthreat %>% filter(!is.na(N)) %>% nrow
  )
)) %>% mutate_at(c(2), as.numeric)


# 1998
dthreat <- dx %>% filter(rl1998 == "0" |
                           rl1998 == "1" |
                           rl1998 == "2" |
                           rl1998 == "3" |
                           rl1998 == "G") %>%
  select(submitted_name_rl, L, T, C, M, R, N) 

t1998_wn <- bind_rows(list(
  c(
    EIV = "L",
    t1998_wn = dthreat %>% filter(!is.na(L)) %>% nrow
  ),
  c(
    EIV = "T",
    t1998_wn = dthreat %>% filter(!is.na(T)) %>% nrow
  ),
  c(
    EIV = "C",
    t1998_wn = dthreat %>% filter(!is.na(C)) %>% nrow
  ),
  c(
    EIV = "M",
    t1998_wn = dthreat %>% filter(!is.na(M)) %>% nrow
  ),
  c(
    EIV = "R",
    t1998_wn = dthreat %>% filter(!is.na(R)) %>% nrow
  ),
  c(
    EIV = "N",
    t1998_wn = dthreat %>% filter(!is.na(N)) %>% nrow
  )
)) %>% mutate_at(c(2), as.numeric)


eiv_overall <- full_join(eivcat,
                         eivcat %>% group_by(EIV) %>% summarise(ntotal = sum(nsum))) %>%
  full_join(t2018_wn) %>%
  full_join(t1998_wn) %>%
  mutate(perc_overall = nsum / ntotal) %>%
  mutate(
    t2018 = 1358,
    t1998 = 973,
    perc_threat1998 = nthreat1998 / t1998_wn ,
    perc_threat2018 = nthreat2018 / t2018_wn
  )


# figure 1c ----
(
  eiv_overall %>% filter(EIV == "L" | EIV == "N") %>%
    mutate(cat = factor(
      cat,
      levels = c(
        "Shade\n(1-3)",
        "Half shade\n(4-6)",
        "Full light\n(7-9)",
        "N-poor\n(1-3)",
        "Intermediate\n(4-6)",
        "N-rich\n(7-9)"
      )
    )) %>%
    ggplot(aes(
      x = cat,
      y =  perc_threat2018 * 100, fill = EIV
    )) +
    facet_wrap(
      ~ factor(EIV, levels = c('L', "N")),
      scales = "free_x",
      labeller = to_string
    ) +
    scale_x_reordered() +
    geom_chicklet(width = 0.75) +
    geom_text(
      aes(label = paste("n =", t2018_wn, "(1359)")),
      x = -Inf,
      y = Inf,
      hjust = -0.1,
      vjust = 1,
      size = 3,
      fontface = 3,
      check_overlap = TRUE,
      family = "Roboto Condensed"
    ) +
    geom_text(
      aes(label = paste(round(perc_threat2018, 2) * 100, "%")),
      vjust = -.2,
      fontface = "italic",
      family = "Roboto Condensed"
    ) +
    theme_ipsum(
      base_family = "Roboto Condensed",
      grid = "X",
      axis_title_size = 12,
      plot_title_size = 12,
      axis_title_just = "mm"
    ) +
    scale_color_manual(values = group.colors) +
    scale_fill_manual(values = group.colors) +
    theme(
      plot.margin =  unit(rep(0.1, 4), "cm"),
      strip.text = element_blank(),
      text = element_text(size = 14),
      axis.line = element_line(),
      legend.position = "none"
    ) +
    ylim(0, 85) +
    labs(x = "",
         y = "Percent of Red List (2018)") -> fig3
)



# combine figure 1, 2 and 3---------------------------------------------

plot_grid(fig1, fig2, fig3, nrow = 3, align = "hv", labels = c("a", "b", "c"))

ggsave(
  "Figures/figure_1_restore.pdf",
  width = 6.47,
  height = 7.77,
  dpi = 600,
  bg = "white"
)



# smon data  --------------------------------------------------------------

# sMon
eichenberg <- read_excel("Data/eichenberg.xlsx")

# growth form
gf <- read_excel("Data/41559_2018_787_MOESM3_ESM.xlsx", skip = 1)
gf$species <- gsub("_", " ", gf$Species)


# join the data
d <- left_join(eichenberg, gf, by = c("TaxonName" = "species"))

# select relevant columns
d <- d %>% select(
  TaxonName,
  Floristic_status,
  SOPSpect1,
  `t3-t1`,
  `Included for trend analysis`,
  Growth_form,
  Life_cycle,
  Lifeform
)

# many rows missing
d %>% nrow
d %>% na.omit %>% nrow
d %>% count(Growth_form)


# option to fill missing values with genera growth form
# gf data frame
gf$Genera <- word(gf$species, 1)
gf_genera <-
  gf %>% select(Genera, Growth_form, Life_cycle, Lifeform)
gf_genera <- gf %>% group_by(Genera) %>%
  count(Lifeform) %>%
  group_by(Genera) %>%
  filter(n == max(n)) %>%
  slice(1) %>% rename(Lifeform_genera = Lifeform)

d$Genera <- word(d$TaxonName, 1)

d <- left_join(d, gf_genera)
# now replace missing values with the genera life form
d <-
  d %>% mutate(life_form_all = ifelse(is.na(Lifeform) == T, Lifeform_genera, Lifeform))

d <-
  d %>% mutate(
    life_form_all = ifelse(
      life_form_all == "Unclassified herb",
      Lifeform_genera,
      life_form_all
    )
  )
d <-
  d %>% mutate(
    life_form_all = ifelse(
      life_form_all == "Unclassified perennial",
      Lifeform_genera,
      life_form_all
    )
  )
d <- d %>% mutate(life_form_all = ifelse(life_form_all == "Annual",
                                         "Annual herb", life_form_all))
d %>% select(TaxonName, life_form_all) %>% nrow
d %>% select(TaxonName, life_form_all) %>% na.omit %>% nrow

d <- d %>% filter(`Included for trend analysis` == 1)
stat_box_data <- function(y, upper_limit = max(d$`t3-t1`)) {
  return(data.frame(y = 0.95 * upper_limit,
                    label = paste('n =', length(y))))
}

(
  d %>%
    filter(!is.na(life_form_all)) %>%
    filter(life_form_all != "Unclassified herb") %>%
    filter(life_form_all != "Unclassified perennial") %>%
    ggplot(aes(
      x = life_form_all, y = `t3-t1`, fill = life_form_all
    )) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_violinhalf(position = position_nudge(x = 0.2, y = 0), alpha =
                      0.5) +
    geom_point(position = position_jitter(width = 0.1), alpha = 0.1)   +
    geom_boxplot(
      outlier.shape = NA,
      width = 0.2,
      alpha = 0.5
    ) +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      size = 3,
      color = "black",
      fill = "#ff0028"
    ) +
    stat_summary(
      fun.data = stat_box_data,
      geom = "text",
      fontface = "italic",
      hjust = 1,
      family = "Roboto Condensed"
    ) +
    coord_flip() +
    scale_fill_manual(values = c("#d700ff", "#ffd700", "#00ffd7")) +
    scale_color_manual(values = c("#d700ff", "#ffd700", "#00ffd7")) +
    theme_ipsum(
      grid = "X",
      axis_title_size = 12,
      plot_title_size = 12,
      axis_title_just = "mm",
      base_family = "Roboto Condensed"
    ) +
    theme(
      text = element_text(size = 14),
      plot.margin = unit(rep(0.2, 4), "cm"),
      legend.position = "none",
      legend.title = element_blank() ,
      axis.line = element_line()
    ) +
    labs(y = "Percentage change in occupancy across Germany\n  1997–2017 vs. 1960–1987",
         x = "") -> p1
)


# estimated means
mod <- lm(
  `t3-t1` ~ life_form_all + SOPSpect1,
  data = d  %>%
    filter(life_form_all != "Unclassified herb") %>%
    filter(life_form_all != "Unclassified perennial")
)
summary(mod)
est_mod <- emmeans(mod, pairwise ~ life_form_all)

(
  plot(est_mod[[1]], col = "#898676") +
    geom_vline(xintercept = 0, lty = 2) +
    theme_ipsum(
      grid = "X",
      axis_title_size = 12,
      plot_title_size = 12,
      axis_title_just = "mm",
      base_family = "Roboto Condensed"
    ) +
    theme(
      text = element_text(size = 14),
      plot.margin = unit(c(0.2, 0.2, 0.2, 0.6), "cm"),
      legend.position = "none",
      axis.title.y = element_blank() ,
      axis.text.y = element_blank() ,
      legend.title = element_blank() ,
      axis.line = element_line()
    ) +
    labs(x = "Estimated mean\npercentage change",
         y = "Life form") -> p2
)

plot_grid(p1, p2, rel_widths = c(1,0.5), labels = c("a", "b"))

ggsave(
  "Figures/figure_2_restore.pdf",
  width = 7.71,
  height = 4.91,
  dpi = 600,
  bg = "white"
)


# extra analyses ----------------------------------------------------------

# average threat change in species below L7
eiv_rl_total <- read_csv("Data/eiv_rl_total.csv")

eiv_rl_total %>% 
  filter(EIV == "L") %>% 
  filter(number < 7) %>% 
  select(delta) %>% 
  summarize(mean(delta))

eiv_rl_total %>% 
  filter(EIV == "L") %>% 
  filter(number >= 7) %>% 
  select(delta) %>% 
  summarize(mean(delta))
