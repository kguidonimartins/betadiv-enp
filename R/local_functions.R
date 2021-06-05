check_require <- function(pkg) {
  full_pkgname <- pkg
  pkgname <- basename(full_pkgname)

  if (!requireNamespace(pkgname, quietly = TRUE)) {
    usethis::ui_stop(
      "Package {usethis::ui_field(pkgname)} needed for this function to work!
       Solution: You can install it with `misc::ipak(\"{ui_field(full_pkgname)}\")`"
    )
  }
}

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(sapply(pkg, require, character.only = TRUE))
}

prefer <- function(pkg_name, quiet = TRUE) {
  for (pkg in seq_along(pkg_name)) {
    for (func in getNamespaceExports(pkg_name[pkg])) {
      conflicted::conflict_prefer(name = func, winner = pkg_name[pkg], quiet = quiet)
    }
  }
}

save_plot <- function(object, filename = NULL, dir_to_save = NULL, width = NA, height = NA, format = NULL, units = NULL, dpi = NULL, overwrite = FALSE, trim = FALSE) {
  default_format <- "png"
  default_units <- "cm"
  default_dpi <- 300
  default_filename <- deparse(substitute(object))

  if (!is.null(format)) default_format <- format
  if (!is.null(units)) default_units <- units
  if (!is.null(dpi)) default_dpi <- dpi
  if (!is.null(filename)) default_filename <- filename

  if (is.null(dir_to_save)) {
    dir_to_save <- here::here("output/figures")
  }

  name_to_save <- paste0(dir_to_save, "/", default_filename, ".", default_format)
  args <- list(
    plot = object,
    file = name_to_save,
    units = default_units,
    dpi = default_dpi,
    width = width,
    height = height
  )
  if (default_format == "pdf") args[["useDingbats"]] <- FALSE
  if (!fs::file_exists(name_to_save)) {
    usethis::ui_todo("Saving {usethis::ui_field(here::here(name_to_save))}...")
    do.call(ggplot2::ggsave, args)
    usethis::ui_done("{usethis::ui_field(here::here(name_to_save))} saved!")
  } else if (overwrite) {
    usethis::ui_todo("Overwriting {usethis::ui_field(here::here(name_to_save))}...")
    unlink(name_to_save)
    do.call(ggplot2::ggsave, args)
    usethis::ui_done("{usethis::ui_field(here::here(name_to_save))} saved!")
  } else {
    usethis::ui_info("File {usethis::ui_field(here::here(name_to_save))} already exists! Use overwrite = TRUE.")
  }
  if (trim) {
    trim_fig(figure_path = name_to_save, overwrite = overwrite)
  }
}


trim_fig <- function(figure_path, overwrite = FALSE) {
  check_require("magick")
  if (!fs::file_exists(figure_path)) {
    usethis::ui_stop("{usethis::ui_field(figure_path)} does not exists!")
  }
  fig_dirname <- dirname(figure_path)
  fig_name <- basename(figure_path)
  fig_dir_trim <- paste0(fig_dirname, "/", "trim/")
  fs::dir_create(fig_dir_trim)
  name_to_save <- paste0(fig_dir_trim, fig_name)
  make_trim_fig <- function(figure_path, name_to_save) {
    fig <- magick::image_read(figure_path)
    fig_trim <- magick::image_trim(fig)
    magick::image_write(image = fig_trim, path = name_to_save)
  }
  if (!fs::file_exists(name_to_save)) {
    usethis::ui_todo("Saving {usethis::ui_field(name_to_save)}...")
    make_trim_fig(figure_path = figure_path, name_to_save = name_to_save)
    usethis::ui_done("{usethis::ui_field(name_to_save)} saved!")
  } else if (overwrite) {
    usethis::ui_todo("Overwriting {usethis::ui_field(name_to_save)}...")
    unlink(name_to_save)
    make_trim_fig(figure_path = figure_path, name_to_save = name_to_save)
    usethis::ui_done("{usethis::ui_field(name_to_save)} saved!")
  } else {
    usethis::ui_info("File {usethis::ui_field(name_to_save)} already exists! Use overwrite = TRUE.")
  }
}


update_description <- function(pkg_list) {
  has_tidy <- grepl(pattern = "tidyverse", x = pkg_list)

  if (sum(has_tidy) >= 1) {
    pkg_list <- pkg_list[!pkg_list %in% "tidyverse"]
    pkg_list <- c(pkg_list, c("ggplot2", "tibble", "tidyr", "readr", "purrr", "dplyr", "stringr", "forcats"))
  }

  pkg_list_github <- pkg_list[grep(pattern = "/", x = pkg_list)]
  pkg_list_cran <- pkg_list[!pkg_list %in% pkg_list_github]

  if (!file.exists(here::here("DESCRIPTION"))) {
    usethis::use_description(check_name = FALSE)
  }

  suppressMessages({
    pkg_list_cran %>%
      map(~ use_package(package = .x, type = "Imports"))
  })

  message(glue("Consider include {pkg_list_github} into Remotes section of DESCRIPTION file"))
}

write_reg_frml_wout_interaction <- function(df) {
  m <- lm(
    formula = bray_curtis ~ factor_type + dist_geo,
    data = bray_all_plus_xy
  )
  eq <- substitute(
    # italic(y) == a + b %.% italic(x) + c %.% italic(z)*","~~italic(r)^2~"="~r2,
    # put signs by hand
    italic(Y) == a - b * italic(X) - c * italic(Z) * ";" ~ ~ italic(R)^2 ~ "=" ~ r2, #* ";"~~italic(p)~"<"~pvalue,
    list(
      a = format(unname(abs(coef(m)[1])), digits = 2),
      b = format(unname(abs(coef(m)[3])), digits = 2),
      c = format(unname(abs(coef(m)[2])), digits = 2),
      r2 = format(summary(m)$r.squared, digits = 2)
      # pvalue = ifelse(summary(m)$coefficients[2,'Pr(>|t|)'] < 0.001, paste("0.001"), pvalue)
    )
  )
  as.character(as.expression(eq))
}

write_reg_frml_with_interaction <- function(df) {
  m <- lmp(
    formula = bray_curtis ~ factor_type * dist_geo,
    perm = "Prob",
    data = bray_all_plus_xy
  )
  eq <- substitute(
    # italic(y) == a + b %.% italic(x) + c %.% italic(z)*","~~italic(r)^2~"="~r2,
    # put signs by hand
    italic(Y) == a - b * italic(X) - c * italic(Z) - d * italic(XZ) * ";" ~ ~ italic(R)^2 ~ "=" ~ r2, #* ";"~~italic(p)~"<"~pvalue,
    list(
      a = format(unname(abs(coef(m)[1])), digits = 2),
      b = format(unname(abs(coef(m)[3])), digits = 2),
      c = format(unname(abs(coef(m)[2])), digits = 1),
      d = format(unname(abs(coef(m)[4])), digits = 2),
      r2 = format(summary(m)$r.squared, digits = 2)
      # pvalue = ifelse(summary(m)$coefficients[2,'Pr(>|t|)'] < 0.001, paste("0.001"), pvalue)
    )
  )
  as.character(as.expression(eq))
}

#' NODF function
#'
#' @param rich A vector of supposed community with the highest richness
#' @param poor A vector of supposed community with the lowest richness
#'
#' @return None
nodf <- function(rich, poor) {
  comm_rich <- sum(rich)
  comm_poor <- sum(poor)
  verif <- comm_rich > comm_poor
  if (verif == FALSE) {
    resp <- 0
  }
  if (verif == TRUE) {
    part1 <- rich + poor
    part2 <- part1 == 2
    part3 <- as.numeric(part2)
    part4 <- sum(part3)
    resp <- (part4 / comm_poor)
  }
  return(resp)
}


nodf_aleat <- function(rich, poor, aleat = 999) {
  # make sure that selected columns are vectors
  rich <- as.vector(as.matrix(rich))
  poor <- as.vector(as.matrix(poor))

  # calculate nodf (see previous function)
  nodf_obs <- nodf(rich, poor)

  # create a vector to store null nodf values
  nodf_null <- numeric(aleat)

  # create vectors to separate communites
  sep_rich <- 1:length(rich)
  sep_poor <- (length(rich) + 1):(length(rich) * 2)

  # concatenate rich and poor vectors
  bind_comm <- c(rich, poor)

  for (i in 1:aleat) {
    # sample communities
    sample_comm <- sample(bind_comm)

    # separate rich community
    comm_rich <- sum(sample_comm[sep_rich])

    # separate poor community
    comm_poor <- sum(sample_comm[sep_poor])

    # begin checking
    verif <- comm_rich > comm_poor
    if (verif == FALSE) {
      resp <- 0
    }
    if (verif == TRUE) {
      sum_vec_rich_poor <- rich + poor
      identify_rich_comm <- as.numeric(sum_vec_rich_poor == 2)
      sum_rich_comm <- sum(identify_rich_comm)
      nodf_null[aleat] <- (sum_rich_comm / comm_poor)
    }
  }

  # make sure that we are testing extreme values,
  # regardless positive or negative distribution
  abs_nodf_obs <- abs(nodf_obs)
  abs_nodf_aleat <- abs(nodf_null)

  # how many random values are greater than the observed values?
  highest <- abs_nodf_aleat >= abs_nodf_obs
  extreme <- sum(highest)

  # sum up 1 to both extreme and random values, to make sure that the absence of
  # extreme values doesn't create p-value equal to 0.
  p <- (extreme + 1) / (aleat + 1)

  # create report
  results <- numeric(2)
  names(results) <- c("NODF observed", "Probability")
  results["NODF observed"] <- nodf_obs
  results["Probability"] <- p

  return(results)
}

get_authority <- function(x) {
  sapply(x, function(y) paste(unlist(strsplit(y, " "))[-c(1,2)], collapse = " "))
}

get_genus_epithet <- function(x) {
  sapply(x, function(y) paste(unlist(strsplit(y, " "))[c(1,2)], collapse = " "))
}


dif.coefs <- function(y1, y2, x, aleat = 9999) {
  # y1, y2 and x are dissimilarities/distances in vector form.
  coef1 <- lm(y1 ~ x)$coef
  coef2 <- lm(y2 ~ x)$coef
  dif.obs <- coef1 - coef2

  dif.perm <- matrix(, nrow = aleat, ncol = 2)
  dat <- cbind(c(y1, y2), c(x, x))
  colnames(dat) <- c("y", "x")
  n <- length(y1)
  for (i in 1:aleat) {
    dat.temp <- dat[sample(1:(2 * n)), ]
    y1.temp <- dat.temp[1:n, ]
    y2.temp <- dat.temp[(n + 1):(2 * n), ]
    coef1.temp <- lm(y1.temp[, 1] ~ y1.temp[, 2])$coef
    coef2.temp <- lm(y2.temp[, 1] ~ y2.temp[, 2])$coef
    dif.perm[i, ] <- coef1.temp - coef2.temp
  }

  dif.obs.abs <- abs(dif.obs)
  dif.perm.abs <- abs(dif.perm)

  # intercepto
  extrem.i <- sum(dif.perm.abs[, 1] >= dif.obs.abs[1])
  p.i <- (extrem.i + 1) / (aleat + 1)

  # slope
  extrem.s <- sum(dif.perm.abs[, 2] >= dif.obs.abs[2])
  p.s <- (extrem.s + 1) / (aleat + 1)

  resu <- c(dif.obs[1], p.i, dif.obs[2], p.s)
  names(resu) <- c("dif.int1_int2", "p.i", "dif.slo1_slo2", "p.s")
  return(resu)
}

make_similarity <- function(data, type, xy) {

  if (type == "Sorensen") {
    similarity <-
      1 - betapart::beta.pair(
                            x = decostand(data, "pa"),
                            index.family = "sorensen"
                            )$beta.sor
  }

  if (type == "Simpson (turnover)") {
    similarity <-
      1 - betapart::beta.pair(
                            x = decostand(data, "pa"),
                            index.family = "sorensen"
                            )$beta.sim
  }

  if (type == "Richness difference (nestedness)") {
    similarity <-
      1 - betapart::beta.pair(
                            x = decostand(data, "pa"),
                            index.family = "sorensen"
                            )$beta.sne
  }

  if (type == "Bray-Curtis") {
    similarity <- 1 - vegdist(data, "bray")
  }

  spatial_distance <- dist(xy)

  similarity_dataframe <-
    bind_cols(
      similarity_value = similarity,
      spatial_distance = spatial_distance
    ) %>%
    mutate(similarity_type = type) %>%
    mutate_if(is.numeric, ~ as.numeric(.x))

  return(similarity_dataframe)

}

get_permdisp_vectors <- function(permdisp_obj) {

  veg_type <- rep(c("High fire frequency", "Natural fire frequency"), 20)

  betadisper_vectors <-
    permdisp_obj$vectors %>%
    as.data.frame() %>%
    select(PCoA1, PCoA2) %>%
    rownames_to_column(var = "sites") %>%
    bind_cols(veg_type = veg_type) %>%
    mutate(pair = str_extract(string = sites, pattern = "beta[0-9]."))

  return(betadisper_vectors)

}

get_permdisp_polygons <- function(permdisp_vectors) {

  veg_type <- rep(c("High fire frequency", "Natural fire frequency"), 20)

  betadisper_polygons <-
    permdisp_vectors %>%
    group_by(veg_type) %>%
    do(.[chull(.[2:3]), ]) %>%
    ungroup()

  return(betadisper_polygons)

}


