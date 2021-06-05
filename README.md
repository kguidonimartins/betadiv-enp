# betadiv-enp


<!-- TODO: add badges -->

Code, data, and author's manuscript accompanying the publication:

#### Karlo G. Guidoni-Martins [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-8458-8467), Leandro Maracahipes [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-6148-3291), Adriano S. Melo [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-4695-2854), Marcus V. Cianciaruso [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-5866-5345). *Annual fires reduce local species richness but do not homogenize the composition of savanna woody species*.  Published in *Flora*, 2 June 2021 <https://doi.org/>

## Overview

This repository host the code, data, and manuscript source of the publication.

We adopt a routine to ensure that all statistical analyses and the production of the figures using the R environment are reproducible. Thus, users interested in checking these steps have **three alternatives**:

1. Clone or download this repository, and build the manuscript with `devtools::install(); rmarkdown::render("main-script.Rmd")`;

2. Browse to [GitHub Actions](https://github.com/kguidonimartins/betadiv-enp/actions) to review the build of the latest version of the manuscript and get the latest uploaded artifact (rendered versions of both the manuscript and the analysis report);

3. Pull the [docker image](https://hub.docker.com/r/kguidonimartins/betadiv-enp) based on this repository that delivers the rendered versions of both the manuscript and the analysis report.

### Repository structure

The tree below represent how files are organized (only the relevant files are shown).

```bash
betadiv-enp/
├── data/           # stores all the data used in the manuscript
├── main-script.Rmd # main text of the publication
├── manuscript/     # stores manuscript helper as citation style and bibliography
├── output/         # stores tables, figures, and appendices.
└── R/              # stores analysis routines and custom functions
```