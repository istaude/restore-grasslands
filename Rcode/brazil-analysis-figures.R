

# load data ---------------------------------------------------------------
d1 <- read_excel("Data/merge_ameacaRS_lista.xlsx", skip = 1)
d2 <- read_excel("Data/merge_ameacaRS2002_lista.xlsx")

# select and rename columns
d1 <-
  d1 %>% select(species = spp2,
                habitat = Habitatfinal,
                rl_cat = `Categoria    ...9`)
d2 <-
  d2 %>% select(species = spp2,
                habitat = Habitatfinal,
                rl_cat = `Categoria SEMA 2002`)

# data cleaning
d1 <- d1 %>% filter(habitat != "NA")
d2 <- d2 %>% filter(habitat != "NA")
# remove synonyms
d2 %>% count(species) %>% arrange(desc(n))
d2 <- d2 %>% distinct

# classify threatened status
d1 <- d1 %>% mutate(
  threatened = ifelse(
    rl_cat == "RE" |
      rl_cat == "CR" |
      rl_cat == "VU" |
      rl_cat == "EN" |
      rl_cat == "EX",
    "yes",
    "no"
  )
)

# how many species?
nrow(d1)
# in SEMA everything is threatened


# calculations and plotting for 2014 list ---------------------------------

# calculate threat status in 2014
d1_summarized <- d1 %>% count(habitat, threatened) %>%
  pivot_wider(values_from = "n",
              names_from = "threatened",
              values_fill = 0) %>%
  mutate(total = yes + no,
         prop_threatened = yes / total)


# total threat status
d1_summarized %>% summarise(sum(yes) / sum(total))

# different habitat classfication
(
  part1 <- d1_summarized %>%
    mutate(
      open_habitat = ifelse(
        habitat == "coastal_vegetation" |
          habitat == "dune" |
          habitat == "mainlygrassland" |
          habitat == "onlygrassland" |
          habitat == "outcrop" ,
        "open",
        "closed"
      )
    ) %>%
    filter(habitat != "aquatic") %>% filter(habitat != "antropic") %>% filter(habitat != "forest_and_grassland") %>%
    arrange(open_habitat) %>%
    group_by(open_habitat) %>%
    summarize(
      yes = sum(yes),
      no = sum(no),
      total = sum(total)
    ) %>%
    mutate(prop_threatened = yes / total) %>%
    rename(habitat = open_habitat)
)


# create one data frame
(part2 <-
    d1_summarized %>% filter(habitat == "onlygrassland" |
                               habitat == "onlyforest"))
(
  part3 <- d1_summarized %>%
    summarise(
      yes = sum(yes),
      no = sum(no),
      total = sum(total)
    ) %>%
    mutate(prop_threatened = yes / total, habitat = "all")
)

d <- bind_rows(list(part1, part2, part3)) %>%
  mutate(habitat = factor(
    habitat,
    levels = c("open", "closed", "onlygrassland", "onlyforest"),
    labels = c("open habitat", "closed habitat", " only grassland", "only forest")
  )) %>%
  mutate(cat = c(
    rep("Open vs closed habitat", 2),
    rep("Grassland vs forest", 2),
    "all"
  ))


# plot
(
  ggplot(data = d %>% filter(habitat != "all"), aes(x = habitat, col = habitat)) +
    facet_wrap(~ factor(
      cat,
      levels = c("Open vs closed habitat", "Grassland vs forest", "all")
    ),
    scales = "free_x") +
    geom_hline(yintercept = 0.667, lty = 2) +
    geom_point(aes(y = total / 1000), pch = 17, size = 4) +
    geom_point(aes(y = prop_threatened, size = 4)) +
    scale_y_continuous(
      name = "Percent threatened sp. (2014)",
      sec.axis = sec_axis(~ . * 1000,
                          name = "Total number of assessed spp.")
    ) +
    theme_ipsum(
      grid = "",
      axis_title_size = 12,
      axis_text_size = 10,
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
    labs(x = "") +
    scale_color_manual(values = c(
      "#f9f978", "#9ed39e", "#f9f978", "#9ed39e"
    )) -> p
)

# add description
data.segm1 <- data.frame(x=1.3,y=0.8,xend=1,yend=0.76,
                         cat="Open vs closed habitat")
data.segm2 <- data.frame(x=.7, y=0.44,xend=0.9,yend=0.51,
                         cat="Open vs closed habitat")
data.segm3 <- data.frame(x=1.85,y=0.7,xend=1.7,yend=0.676,
                         cat="Open vs closed habitat")

(
  p +
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
      curvature = 0.3,
      inherit.aes = FALSE
    )  +
    geom_text(
      data = data.segm1,
      aes(x = 1.35, y = 0.78),
      label = "Threatened spp.\npercent",
      inherit.aes = F,
      hjust = 0,
      size = 3,
      family = "Roboto Condensed"
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
      curvature = -0.2,
      inherit.aes = FALSE
    ) +
    geom_text(
      data = data.segm2,
      aes(x = .6, y = 0.4),
      label = "Total number\nassessed spp.",
      inherit.aes = F,
      hjust = 0,
      size = 3,
      family = "Roboto Condensed"
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
      curvature = 0,
      inherit.aes = FALSE
    ) +
    geom_text(
      data = data.segm1,
      aes(x = 1.9, y = 0.71),
      label = "Avg. threat\nstatus (67%)",
      inherit.aes = F,
      hjust = 0,
      size = 3,
      family = "Roboto Condensed"
    ) -> p1
)

# plot overall proportion on red list
dtotal <- d1_summarized %>%
  mutate(
    open_habitat = ifelse(
      habitat == "coastal_vegetation" |
        habitat == "dune" |
        habitat == "mainlygrassland" |
        habitat == "onlygrassland" |
        habitat == "outcrop" ,
      "open",
      ifelse(
        habitat == "forest_and_grassland",
        "both",
        ifelse(habitat == "aquatic" |
                 habitat == "antropic", "other", "closed")
      )
    )
  ) %>%
  group_by(open_habitat) %>%
  summarise(yes = sum(yes),
            no = sum(no),
            total = sum(total))


# compute percentages
dtotal$fraction <- dtotal$yes / sum(dtotal$yes)
dtotal$ymax <- cumsum(dtotal$fraction)
dtotal$ymin <- c(0, head(dtotal$ymax, n = -1))
dtotal$labelPosition <- (dtotal$ymax + dtotal$ymin) / 2
dtotal$label <-
  paste0(dtotal$open_habitat, "\n", percent(round(dtotal$fraction, 2)))
dtotal$labelPosition[1] <- 0.04

(
  ggplot(
    dtotal,
    aes(
      ymax = ymax,
      ymin = ymin,
      xmax = 4,
      xmin = 3,
      fill = open_habitat
    )
  ) +
    geom_rect() +
    geom_text(
      x = 2,
      aes(y = labelPosition, label = label),
      size = 3.5,
      family = "Roboto Condensed"
    ) + # x here controls label position (inner / outer)
    scale_fill_manual(values = c(
      "#7878f9", "#9ed39e", "#f9f978", "#f97878"
    )) +
    coord_polar(theta = "y") +
    xlim(c(-1, 4)) +
    theme_void() +
    labs(title = "Percent endangered spp. in RL", family = "Roboto Condensed") +
    theme(
      plot.title = element_text(hjust = 0.5),
      text = element_text(family = "Roboto Condensed"),
      legend.position = "none",
      aspect.ratio = 1
    ) -> p2
)


plot_grid(p1, p2, nrow = 1, rel_widths = c(1, 0.6), labels = c("a", "b")) 
ggsave(
  "Figures/figure_3_restore.pdf",
  width = 8.37,
  height = 2.85
) 

