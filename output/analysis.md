---
title: analysis
output:
  html_document:
    keep_md: true
    toc: true
    toc_float: true
    highlight: tango
params:
  show_results: FALSE
---





<!-- # data entry -->


```r
df <- read_csv(here::here("data", "community-table.csv"))
```

```
## 
## ── Column specification ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
## cols(
##   .default = col_double(),
##   plot = col_character()
## )
## ℹ Use `spec()` for the full column specifications.
```

```r
xy <- read_csv(here::here("data", "community-geo-coordinates.csv"))
```

```
## 
## ── Column specification ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
## cols(
##   Long = col_double(),
##   Lat = col_double()
## )
```

<!-- # map for enp -->


```r
if (!file.exists(here::here("output", "figures", "map_emas.tiff"))) {

  southam <- sf::read_sf(here::here("data", "shapefiles", "south-america", "sa.shp"))

  cerrado <- sf::read_sf(here::here("data", "shapefiles", "cerrado", "cerrado.shp"))

  enp <- sf::read_sf(here::here("data", "shapefiles", "emas-national-park", "enp.shp"))

  theme_set(
    theme_bw(base_size = 16, base_family = "Times") +
    theme(
      plot.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "black"),
      axis.text.x = element_text(colour = "black"),
      axis.text.y = element_text(colour = "black"),
      axis.title = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  )

  enp_center <- enp %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_coordinates() %>%
    as_tibble() %>%
    head(1)

  southam_enp_center <-
    ggplot() +
    geom_sf(data = southam, fill = "grey", lwd = 0) +
    geom_sf(data = cerrado, fill = "grey20", lwd = 0) +
    xlim(c(-90, -30)) +
    geom_point(
      data = enp_center,
      aes(x = X, y = Y),
      size = 2,
      pch = 21,
      colour = "black",
      fill = "white"
    ) +
    labs(x = "",
         y = "") +
    theme_bw() +
    theme(
      panel.border = element_rect(fill = "transparent", color = "transparent"),
      axis.text = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "transparent", color = "transparent"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  southam_enp_center

  enp_points <-
    ggplot() +
    geom_sf(
      data = enp,
      fill = "grey",
      lwd = 0
    ) +
    geom_point(
      data = xy,
      aes(x = Long, y = Lat),
      size = 2,
      pch = 22,
      colour = "black",
      fill = "white"
    ) +
    north(
      data = enp,
      location = "topleft",
      symbol = 12
    ) +
    scalebar(
      data = enp,
      location = "bottomleft",
      dist = 10,
      dist_unit = "km",
      model = "WGS84",
      transform = TRUE,
      st.dist = 0.03,
      st.size = 3
    ) +
    scale_x_continuous(breaks = seq(-53.1, -52.7, by = 0.1))

  enp_points

  map_emas <- ggdraw() +
    draw_plot(enp_points) +
    draw_plot(southam_enp_center, x = 0.62, y = 0.70, width = 0.3, height = 0.3)

  map_emas

  save_plot(map_emas, format = "tiff", trim = TRUE, overwrite = TRUE)

}
```

<!-- # t test analysis and plot -->


```r
df_pa <-
  df %>%
  column_to_rownames("plot") %>%
  decostand(., "pa")

richness <-
  data.frame("spp_richness" = rowSums(df_pa)) %>%
  rownames_to_column("pair") %>%
  mutate(
    condition = str_remove(pair, pattern = "beta[0-9]."),
    pair = str_extract(pair, "beta[0-9].")
  )

sampling_units <- richness %>% distinct(pair) %>% pull()

richness_wider <-
  richness %>%
  pivot_wider(
    names_from = "condition",
    values_from = "spp_richness"
  )

n_higher_richness <-
  richness_wider %>%
  filter(unburned >= burned) %>%
  nrow(.)

t_test_result <-
  richness_wider %>%
  rename_if(is.numeric, paste0, "_richness") %>%
  {
    t.test(
      x = .$unburned_richness,
      y = .$burned_richness,
      paired = TRUE
    )
  }

not_nodf <-
  richness_wider %>%
  filter(burned > unburned)

if (!file.exists(here::here("output", "figures", "paired_richness.tiff"))) {

  paired_richness <-
    richness %>%
    filter(!pair %in% not_nodf$pair) %>%
    ggplot(aes(x = condition, y = spp_richness, group = pair)) +
    geom_line(aes(group = pair), size = 1, alpha = 0.5) +
    geom_line(data = richness %>% filter(pair %in% not_nodf$pair), linetype = "dashed", size = 1, alpha = 0.5) +
    geom_point(size = 4, alpha = 0.5) +
    geom_point(data = richness %>% filter(pair %in% not_nodf$pair), size = 4, alpha = 0.5) +
    scale_x_discrete(
      limits = c("unburned", "burned"),
      labels = c("Natural fire frequency", "High fire frequency")
    ) +
    # scale_y_continuous(
    #   breaks = scales::pretty_breaks(30)
    #   ) +
    labs(
      x = "",
      y = "Species richness"
    ) +
    ylim(c(10, 25)) +
    theme_bw(base_size = 16, base_family = "Times") +
    theme(
      axis.title.x = element_text(size = 16, colour = "black"),
      axis.title.y = element_text(size = 16, colour = "black"),
      axis.text.x = element_text(size = 12, colour = "black"),
      axis.text.y = element_text(size = 12, colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      panel.background = element_rect(colour = "black")
    )

  paired_richness

  save_plot(paired_richness, format = "tiff", trim = TRUE, overwrite = TRUE)

}
```

<!-- # data wrangling for ancova and permanova/permdisp -->


```r
df_list <- df %>%
  mutate(fire_type = str_remove(plot, "beta[0-9].")) %>%
  column_to_rownames("plot") %>%
  split(.$fire_type) %>%
  map(~select(.x, -fire_type)) %>%
  map(~.x[, colSums(.x) != 0])

all_similarities <-
  bind_rows(
    df_list %>%
    map(~make_similarity(.x, "Sorensen", xy)) %>%
    enframe() %>%
    unnest(cols = c(value)),
    df_list %>%
    map(~make_similarity(.x, "Simpson (turnover)", xy)) %>%
    enframe() %>%
    unnest(cols = c(value)),
    df_list %>%
    map(~make_similarity(.x, "Richness difference (nestedness)", xy)) %>%
    enframe() %>%
    unnest(cols = c(value)),
    df_list %>%
    map(~make_similarity(.x, "Bray-Curtis", xy)) %>%
    enframe() %>%
    unnest(cols = c(value))
  ) %>%
  rename(factor_type = name) %>%
  mutate(
    factor_type = case_when(
      factor_type == "burned" ~ "High fire frequency",
      factor_type == "unburned" ~ "Natural fire frequency"
    ),
    similarity_type = factor(
      similarity_type,
      levels = c(
        "Sorensen",
        "Simpson (turnover)",
        "Richness difference (nestedness)",
        "Bray-Curtis"
      )
    )
  ) %>%
  arrange(similarity_type)
```

<!-- # ancova -->


```r
if (!file.exists(here::here("output", "figures", "distance_decay_plate.tiff"))) {

  distance_decay_plate <-
    all_similarities %>%
    mutate(
      similarity_type = factor(
        similarity_type,
        levels = c(
          "Sorensen",
          "Simpson (turnover)",
          "Richness difference (nestedness)",
          "Bray-Curtis"
        )
      )
    ) %>%
    arrange(similarity_type) %>%
    ggplot(
      aes(
        x = spatial_distance,
        y = similarity_value,
        color = factor_type
      )
    ) +
    geom_point(
      size = 2,
      alpha = 0.6
    ) +
    scale_color_manual(
      values = c(
        "red",
        "blue"
      )
    ) +
    facet_wrap(~similarity_type) +
    labs(
      x = "Spatial distance",
      y = "Similarity"
    ) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_bw(base_size = 16, base_family = "Times") +
    theme(
      axis.title.x = element_text(size = 14, colour = "black"),
      axis.title.y = element_text(size = 14, colour = "black"),
      axis.text.x = element_text(size = 10, colour = "black"),
      axis.text.y = element_text(size = 10, colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.position = c(0.38, 0.97),
      legend.background = element_rect(fill = alpha("white", 0.0)),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 8),
      panel.background = element_rect(colour = "black")
    ) +
    guides(color = guide_legend(keyheight = 0.7, override.aes = list(fill = NA)))

  distance_decay_plate

  save_plot(distance_decay_plate, format = "tiff", trim = TRUE, overwrite = TRUE)

}

regression_test_by_permutation <- here::here("output", "tables",
                                             "regression_test_by_permutation.csv")

if (!file.exists(regression_test_by_permutation)) {

  sim_types <- unique(all_similarities$similarity_type)

  result.regre.perm <- matrix(, 4, 4)
  colnames(result.regre.perm) <- c("dif.int1_int2", "p.i", "dif.slo1_slo2", "p.s")
  rownames(result.regre.perm) <- sim_types

  for (i in seq_along(sim_types)) {

    tmpdf <- all_similarities %>% filter(similarity_type == sim_types[i])

    vec_y1 <-
      tmpdf %>%
      filter(factor_type == "High fire frequency") %>%
      pull(similarity_value)

    vec_y2 <-
      tmpdf %>%
      filter(factor_type == "Natural fire frequency") %>%
      pull(similarity_value)

    vec_xy <-
      tmpdf %>%
      filter(factor_type == "High fire frequency") %>%
      pull(spatial_distance)

    set.seed(2020)

    result.regre.perm[sim_types[i], ] <- dif.coefs(y1 = vec_y1, y2 = vec_y2, x = vec_xy)

  }

  result.regre.perm %>%
    as.data.frame() %>%
    rownames_to_column("similarity_type") %>%
    write_csv(regression_test_by_permutation)

}
```

<!-- # manova -->


```r
df_clean <-
  df %>%
  column_to_rownames("plot")

groups_fire <-
  df %>%
  mutate(plot = str_remove(plot, "beta[0-9].")) %>%
  pull(plot) %>%
  as.factor()

pairs_fire <-
  df %>%
  mutate(plot = str_extract(plot, "beta[0-9].")) %>%
  pull(plot) %>%
  as.factor()

df_clean_list <-
  list(
    "Bray-Curtis" = vegdist(df_clean, "bray"),
    "Richness difference (nestedness)" = betapart::beta.pair(x = decostand(df_clean, "pa"), index.family = "sorensen")$beta.sne,
    "Simpson (turnover)" = beta.pair(x = decostand(df_clean, "pa"), index.family = "sorensen")$beta.sim,
    "Sorensen" = beta.pair(x = decostand(df_clean, "pa"), index.family = "sorensen")$beta.sor
  )

set.seed(2020)
permanovas <- df_clean_list %>%
  map(~adonis2(formula = as.dist(.x) ~ groups_fire + pairs_fire))

permanovas
```

```
## $`Bray-Curtis`
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 999
## 
## adonis2(formula = as.dist(.x) ~ groups_fire + pairs_fire)
##             Df SumOfSqs      R2      F Pr(>F)    
## groups_fire  1   0.2706 0.03932 3.1927  0.002 ** 
## pairs_fire  19   5.0007 0.72665 3.1050  0.001 ***
## Residual    19   1.6105 0.23402                  
## Total       39   6.8818 1.00000                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## $`Richness difference (nestedness)`
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 999
## 
## adonis2(formula = as.dist(.x) ~ groups_fire + pairs_fire)
##             Df SumOfSqs      R2      F Pr(>F)  
## groups_fire  1 0.012687 0.07943 4.4047  0.068 .
## pairs_fire  19 0.092307 0.57792 1.6866  0.169  
## Residual    19 0.054729 0.34265                
## Total       39 0.159723 1.00000                
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## $`Simpson (turnover)`
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 999
## 
## adonis2(formula = as.dist(.x) ~ groups_fire + pairs_fire)
##             Df SumOfSqs      R2      F Pr(>F)    
## groups_fire  1   0.1406 0.04166 3.5061  0.011 *  
## pairs_fire  19   2.4720 0.73256 3.2445  0.001 ***
## Residual    19   0.7619 0.22578                  
## Total       39   3.3745 1.00000                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## $Sorensen
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 999
## 
## adonis2(formula = as.dist(.x) ~ groups_fire + pairs_fire)
##             Df SumOfSqs      R2      F Pr(>F)    
## groups_fire  1   0.2068 0.04617 3.5476  0.005 ** 
## pairs_fire  19   3.1654 0.70659 2.8578  0.001 ***
## Residual    19   1.1076 0.24725                  
## Total       39   4.4798 1.00000                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
permanova_table <- here::here("output", "tables", "permanova_table.csv")

if (!file.exists(permanova_table)) {

  suppressWarnings({

    # because broom do not recognize columns
    # names, but the values are ok.
    permanova_tidy_results <-
      permanovas %>%
      map(~tidy(.x)) %>%
      enframe() %>%
      unnest(cols = c(value)) %>%
      mutate_if(is.numeric, ~round(., 2)) %>%
      mutate(
        name = factor(
          name,
          levels = c(
            "Sorensen",
            "Simpson (turnover)",
            "Richness difference (nestedness)",
            "Bray-Curtis"
          )
        )
      ) %>%
      arrange(name) %>%
      mutate(
        name = c(
          levels(.$name)[1], NA, NA, NA,
          levels(.$name)[2], NA, NA, NA,
          levels(.$name)[3], NA, NA, NA,
          levels(.$name)[4], NA, NA, NA
        )
      ) %>%
      filter(term %in% c("groups_fire", "pairs_fire")) %>%
      mutate(
        p_value = if_else(
          condition = p.value < 0.001 | p.value == 0,
          true = paste("<0.001"),
          false = paste(format(p.value, digits = 4, nsmall = 3))
        )
      ) %>%
      mutate(
        term = case_when(
          term == "groups_fire" ~ "Fire frequency",
          term == "pairs_fire" ~ "Pairs",
          TRUE ~ as.character(term)
        ),
        df = as.character(df)
      ) %>%
      mutate_if(is.numeric, ~format(., digits = 3, nsmall = 3))

      permanova_tidy_results

      permanova_tidy_results %>%
        write_csv(permanova_table)

  })

} else {

  permanova_table <- read_csv(permanova_table)

}
```

```
## 
## ── Column specification ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
## cols(
##   name = col_character(),
##   term = col_character(),
##   df = col_double(),
##   SumOfSqs = col_double(),
##   R2 = col_double(),
##   statistic = col_double(),
##   p.value = col_double(),
##   p_value = col_character()
## )
```

```r
permdisps <- df_clean_list %>%
  map(~betadisper(.x, groups_fire))
```

```
## Warning in betadisper(.x, groups_fire): some squared distances are negative and
## changed to zero

## Warning in betadisper(.x, groups_fire): some squared distances are negative and
## changed to zero
```

```r
permdisps
```

```
## $`Bray-Curtis`
## 
## 	Homogeneity of multivariate dispersions
## 
## Call: betadisper(d = .x, group = groups_fire)
## 
## No. of Positive Eigenvalues: 25
## No. of Negative Eigenvalues: 14
## 
## Average distance to median:
##   burned unburned 
##   0.3851   0.4165 
## 
## Eigenvalues for PCoA axes:
## (Showing 8 of 39 eigenvalues)
##  PCoA1  PCoA2  PCoA3  PCoA4  PCoA5  PCoA6  PCoA7  PCoA8 
## 1.4365 1.1823 0.7829 0.6157 0.4817 0.4028 0.3391 0.3252 
## 
## $`Richness difference (nestedness)`
## 
## 	Homogeneity of multivariate dispersions
## 
## Call: betadisper(d = .x, group = groups_fire)
## 
## No. of Positive Eigenvalues: 22
## No. of Negative Eigenvalues: 17
## 
## Average distance to median:
##   burned unburned 
##  0.05260  0.04288 
## 
## Eigenvalues for PCoA axes:
## (Showing 8 of 39 eigenvalues)
##    PCoA1    PCoA2    PCoA3    PCoA4    PCoA5    PCoA6    PCoA7    PCoA8 
## 0.181628 0.021463 0.017745 0.015175 0.014390 0.010806 0.006766 0.005984 
## 
## $`Simpson (turnover)`
## 
## 	Homogeneity of multivariate dispersions
## 
## Call: betadisper(d = .x, group = groups_fire)
## 
## No. of Positive Eigenvalues: 22
## No. of Negative Eigenvalues: 17
## 
## Average distance to median:
##   burned unburned 
##   0.2523   0.2890 
## 
## Eigenvalues for PCoA axes:
## (Showing 8 of 39 eigenvalues)
##  PCoA1  PCoA2  PCoA3  PCoA4  PCoA5  PCoA6  PCoA7  PCoA8 
## 1.1979 0.5616 0.4481 0.3864 0.3396 0.3167 0.2474 0.2150 
## 
## $Sorensen
## 
## 	Homogeneity of multivariate dispersions
## 
## Call: betadisper(d = .x, group = groups_fire)
## 
## No. of Positive Eigenvalues: 23
## No. of Negative Eigenvalues: 16
## 
## Average distance to median:
##   burned unburned 
##   0.3078   0.3328 
## 
## Eigenvalues for PCoA axes:
## (Showing 8 of 39 eigenvalues)
##  PCoA1  PCoA2  PCoA3  PCoA4  PCoA5  PCoA6  PCoA7  PCoA8 
## 1.2211 0.7442 0.4701 0.4222 0.3690 0.3356 0.2781 0.2561
```

```r
permdisp_table <- here::here("output", "tables", "permdisp_table.csv")

if (!file.exists(permdisp_table)) {

  permdisp_tidy_results <-
    permdisps %>%
    map(~permutest(.x)) %>%
    map(~.x$tab) %>%
    map(~rownames_to_column(.x, "term")) %>%
    enframe() %>%
    unnest(cols = c(value)) %>%
    filter(term == "Groups") %>%
    mutate(
      name = factor(
        name,
        levels = c(
          "Sorensen",
          "Simpson (turnover)",
          "Richness difference (nestedness)",
          "Bray-Curtis"
        )
      )
    ) %>%
    arrange(name) %>%
    mutate(
      term = "Fire frequency",
      Df = as.character(Df)
    ) %>%
    mutate_if(is.numeric, ~format(., digits = 3, nsmall = 3))

  permdisp_tidy_results

  permdisp_tidy_results %>%
    write_csv(permdisp_table)

} else {

  permdisp_table <- read_csv(permdisp_table)

}
```

```
## 
## ── Column specification ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
## cols(
##   name = col_character(),
##   term = col_character(),
##   Df = col_double(),
##   `Sum Sq` = col_double(),
##   `Mean Sq` = col_double(),
##   F = col_double(),
##   N.Perm = col_double(),
##   `Pr(>F)` = col_double()
## )
```

```r
permdisp_vectors <- permdisps %>%
  map(~get_permdisp_vectors(.x)) %>%
  enframe() %>%
  unnest(cols = c(value)) %>%
  mutate(
    name = factor(
      name,
      levels = c(
        "Sorensen",
        "Simpson (turnover)",
        "Richness difference (nestedness)",
        "Bray-Curtis"
      )
    )
  ) %>%
  arrange(name)

permdisp_polygons <- permdisps %>%
  map(~get_permdisp_vectors(.x)) %>%
  map(~get_permdisp_polygons(.x)) %>%
  enframe() %>%
  unnest(cols = c(value)) %>%
  mutate(
    name = factor(
      name,
      levels = c(
        "Sorensen",
        "Simpson (turnover)",
        "Richness difference (nestedness)",
        "Bray-Curtis"
      )
    )
  ) %>%
  arrange(name)

permdisps %>%
  map(~(.x$eig[1] / sum(.x$eig)) * 100) %>%
  enframe() %>%
  unnest(cols = c(value))
```

```
## # A tibble: 4 x 2
##   name                             value
##   <chr>                            <dbl>
## 1 Bray-Curtis                       20.9
## 2 Richness difference (nestedness) 114. 
## 3 Simpson (turnover)                35.5
## 4 Sorensen                          27.3
```

```r
permdisps %>%
  map(~(.x$eig[2] / sum(.x$eig)) * 100) %>%
  enframe() %>%
  unnest(cols = c(value))
```

```
## # A tibble: 4 x 2
##   name                             value
##   <chr>                            <dbl>
## 1 Bray-Curtis                       17.2
## 2 Richness difference (nestedness)  13.4
## 3 Simpson (turnover)                16.6
## 4 Sorensen                          16.6
```

```r
if (!file.exists(here::here("output", "figures", "pcoa_plate.tiff"))) {

  pcoa_plate <-
    ggplot() +
    geom_polygon(
      data = permdisp_polygons,
      aes(
        x = PCoA1,
        y = PCoA2,
        fill = veg_type,
        group = veg_type
      ),
      alpha = 0.10
    ) +
    scale_fill_manual(
      values = c(
        "High fire frequency" = "red",
        "Natural fire frequency"  = "blue"
      )
    ) +
    geom_point(
      data = permdisp_vectors,
      aes(
        x = PCoA1,
        y = PCoA2,
        colour = veg_type
      ),
      size = 3
    ) +
    scale_colour_manual(
      values = c(
        "High fire frequency" = "red",
        "Natural fire frequency"  = "blue"
      )
    ) +
    theme_bw(base_size = 16, base_family = "Times") +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      legend.title = element_blank(),
      legend.position = c(0.40, 0.65),
      legend.background = element_rect(fill = alpha("white", 0.0)),
      legend.box.background = element_blank(),
      legend.text = element_text(size = 6)
    ) +
    guides(color = guide_legend(keyheight = 0.1)) +
    labs(
      colour = "Vegetation condition",
      fill   = "Vegetation condition"
    ) +
    facet_wrap(~name, scale = "free") +
    geom_line(
      data = permdisp_vectors,
      aes(
        x = PCoA1,
        y = PCoA2,
        group = factor(pair)
      )
    )

  pcoa_plate

  save_plot(pcoa_plate, format = "tiff", trim = TRUE, overwrite = TRUE)

}
```

<!-- # get taxa info -->


```r
taxa_info <- here::here("output", "tables", "taxa_info.csv")

df_com <-
  df %>%
  column_to_rownames("plot")

if (!file.exists(taxa_info)) {

  taxa_query <-
    df_com %>%
    names() %>%
    get.taxa()

  taxa_query %>%
    write_csv(taxa_info)

} else {

  taxa_query <-
    read_csv(taxa_info)

}
```

```
## 
## ── Column specification ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
## cols(
##   id = col_double(),
##   scientific.name = col_character(),
##   accepted.name = col_logical(),
##   family = col_character(),
##   taxon.rank = col_character(),
##   taxon.status = col_character(),
##   search.str = col_character(),
##   threat.status = col_character(),
##   notes = col_character(),
##   original.search = col_character()
## )
```

```r
abundances_between_treatments <- here::here("output", "tables", "abundances_between_treatments.csv")

if (!file.exists(abundances_between_treatments)) {

  compare_abund <-
    df_com %>%
    rownames_to_column("treatment") %>%
    pivot_longer(cols = -treatment, names_to = "species", values_to = "abundance") %>%
    mutate(treatment = str_remove(treatment, "beta[0-9].")) %>%
    group_by(treatment, species) %>%
    summarise(abundance = sum(abundance, na.rm = TRUE)) %>%
    pivot_wider(names_from = treatment, values_from = abundance) %>%
    mutate_if(is.numeric, list(~ na_if(., "unknown"))) %>%
    full_join(taxa_query, by = c("species" =  "original.search")) %>%
    mutate(species = ifelse(is.na(scientific.name), species, scientific.name)) %>%
    select(species, burned, unburned) %>%
    mutate(species = case_when(
             species == "Solanum L." ~ "Solanum sp.",
             species == "Cipo escandente" ~ "Sp 1",
             species == "Asteraceae Bercht. & J.Presl" ~ "Sp 2",
             TRUE ~ as.character(species)
           )
           ) %>%
    arrange(species)


  compare_abund %>%
    write_csv(abundances_between_treatments)

} else {

  compare_abund <-
    read_csv(abundances_between_treatments)

}
```

```
## 
## ── Column specification ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
## cols(
##   species = col_character(),
##   burned = col_double(),
##   unburned = col_double()
## )
```

```r
compare_abund %>%
  as.data.frame()
```

```
##                                                     species burned unburned
## 1          Agonandra brasiliensis Miers ex Benth. & Hook.f.      0        2
## 2    Anadenanthera peregrina var. falcata (Benth.) Altschul     44       38
## 3                                  Annona crassiflora Mart.     35       30
## 4                     Aspidosperma tomentosum Mart. & Zucc.      2       16
## 5                              Bowdichia virgilioides Kunth      0        1
## 6                            Byrsonima coccolobifolia Kunth      7        3
## 7                             Caryocar brasiliense Cambess.      0        5
## 8                                   Casearia sylvestris Sw.      6       13
## 9                                Connarus suberosus Planch.     23       42
## 10                   Cybistax antisyphilitica (Mart.) Mart.      1        2
## 11                             Davilla elliptica A.St.-Hil.      1        9
## 12         Didymopanax macrocarpus (Cham. & Schltdl.) Seem.      1        3
## 13           Didymopanax vinosus (Cham. & Schltdl.) Marchal      0        1
## 14                               Dimorphandra mollis Benth.      8        5
## 15                    Diospyros lasiocalyx (Mart.) B.Walln.     38       59
## 16                            Eremanthus glomerulatus Less.     32       63
## 17                 Eriotheca gracilipes (K.Schum.) A.Robyns      4       10
## 18       Eriotheca pubescens (Mart. & Zucc.) Schott & Endl.     13       17
## 19                          Erythroxylum engleri O.E.Schulz     20       15
## 20                        Erythroxylum suberosum A.St.-Hil.     21       22
## 21                             Erythroxylum tortuosum Mart.      2        8
## 22                                    Eugenia aurata O.Berg      0        3
## 23                          Eugenia punicifolia (Kunth) DC.      1        2
## 24                            Guapira noxia (Netto) Lundell      8        4
## 25                                 Hancornia speciosa Gomes      1        1
## 26                    Handroanthus ochraceus (Cham.) Mattos     22       60
## 27                Himatanthus obovatus (Müll. Arg.) Woodson      5        3
## 28                     Hymenaea stigonocarpa Mart. ex Hayne      2        2
## 29                        Kielmeyera coriacea Mart. & Zucc.     36       37
## 30                     Kielmeyera grandiflora (Wawra) Saddi      3        2
## 31                              Lafoensia pacari A.St.-Hil.      0        2
## 32                             Leptolobium dasycarpum Vogel     81       42
## 33                             Machaerium acutifolium Vogel      1        4
## 34                            Miconia albicans (Sw.) Triana      0       45
## 35                                  Miconia ferruginata DC.      0        2
## 36                        Miconia ligustroides (DC.) Naudin      0        2
## 37                                    Myrcia bella Cambess.      9        5
## 38                          Myrcia camapuanensis N.Silveira      6        4
## 39                            Myrcia guianensis (Aubl.) DC.      6        1
## 40                                    Myrcia variabilis DC.      2        1
## 41                   Ouratea hexasperma (A.St.-Hil.) Baill.      7       10
## 42                        Ouratea spectabilis (Mart.) Engl.     29       31
## 43                                  Palicourea rigida Kunth     61       26
## 44                   Piptocarpha rotundifolia (Less.) Baker     84       63
## 45                            Plathymenia reticulata Benth.     12        4
## 46                                Plenckia populnea Reissek      1        1
## 47                        Pouteria ramiflora (Mart.) Radlk.    153       90
## 48                            Pouteria torta (Mart.) Radlk.    151      136
## 49                            Psidium laruotteanum Cambess.    104      118
## 50                       Pterodon pubescens (Benth.) Benth.      0        1
## 51                                  Qualea parviflora Mart.      2        2
## 52                                    Rourea induta Planch.      1        0
## 53                                              Solanum sp.      2        2
## 54                                                     Sp 1      0        1
## 55                                                     Sp 2      0        1
## 56              Stryphnodendron adstringens (Mart.) Coville    141      114
## 57                      Stryphnodendron rotundifolium Mart.      2        8
## 58 Tabebuia aurea (Silva Manso) Benth. & Hook.f. ex S.Moore      8        4
## 59                                 Vochysia cinnamomea Pohl      1        2
```

```r
compare_abund %>%
  mutate_if(is.numeric, ~ ifelse(. != 0, 1, 0)) %>%
  select(-species) %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  summarise(sum(value), mean(value), sd(value))
```

```
## # A tibble: 2 x 4
##   name     `sum(value)` `mean(value)` `sd(value)`
##   <chr>           <dbl>         <dbl>       <dbl>
## 1 burned             47         0.797       0.406
## 2 unburned           58         0.983       0.130
```

```r
richness_stats <-
  full_join(
    x = richness %>%
      group_by(condition) %>%
      summarise(mean_spp = mean(spp_richness), sd_spp = sd(spp_richness)),
    y = df_com %>%
      rownames_to_column("condition") %>%
      mutate(condition = str_remove(condition, "beta[0-9].")) %>%
      pivot_longer(-condition) %>%
      filter(value != 0) %>%
      select(-value) %>%
      distinct() %>%
      group_by(condition) %>%
      summarise(count_spp = n()),
    by = "condition"
  )

burned_richness <- richness_stats %>% filter(condition == "burned") %>% pull(count_spp)
unburned_richness <- richness_stats %>% filter(condition == "unburned") %>% pull(count_spp)
```
