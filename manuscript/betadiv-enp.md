---
title: "Annual fires reduce local species richness but do not homogenize the composition of savanna woody species"
author:
- Karlo G. Guidoni-Martins^a^\*, Leandro Maracahipes^b,c^, Adriano S. Melo^d^, Marcus V. Cianciaruso^e^
- ^a^Programa de Pós-Graduação em Ecologia e Evolução, Universidade Federal de Goiás, CP 131, Goiânia, GO, CEP 74001-970, Brazil
- ^b^Instituto de Pesquisa Ambiental da Amazônia (IPAM), Brasília, DF, Asa Norte CLN 211 B1 B Sala 201 - Asa Norte, CEP 70863-520, Brazil
- ^c^Instituto de Biologia, University of Campinas (UNICAMP), Campinas, SP 13083-970, Brazil
- ^d^Departamento de Ecologia, IB, Universidade Federal do Rio Grande do Sul, Av. Bento Gonçalves 9500, Porto Alegre, RS, CEP 91501-970, Brazil
- ^e^Departamento de Ecologia, ICB, Universidade Federal de Goiás, CP 131, Goiânia, GO, CEP 74001-970, Brazil
- <br/>
- <br/>
- <br/>
- \*Corresponding author
- \***kguidonimartins@gmail.com** (K G. Guidoni-Martins), lmaracahipes@gmail.com (L. Maracahipes), asm.adrimelo@gmail.com (A.S. Melo), cianciaruso@gmail.com (M.V. Cianciaruso)
output:
  word_document:
    reference_docx: manuscript/sources/template.docx
    keep_md: true
bibliography: manuscript/sources/bibliography.bib
csl: manuscript/sources/flora.csl
---





<!--
The following chunk (analysis_and_figures) need to be empty.
R/analysis.Rmd run all analysis and create figures.
Some objects created there are used here to extract results
and embed them in the main text.
-->



















<!-- # data entry -->



<!-- # map for enp -->



<!-- # t test analysis and plot -->



<!-- # data wrangling for ancova and permanova/permdisp -->



<!-- # ancova -->



<!-- # manova -->



<!-- # get taxa info -->










\
\
\
\
\
\
\
\
\
\


# ABSTRACT


Savanna woody species evolved with fire. However, the effect of changes in the fire regimes, which tend to increase in the next decades, is not yet fully understood. Here, we tested the effects of increased fire frequency on species richness and the composition of woody plant communities. Specifically, we tested if a high frequency fire management reduces local species richness of communities and homogenizes species composition. We sampled woody plants in 40 sites, distributed in 20 pairs, in Emas National Park (Brazil). Each pair included sites with high fire frequency (firebreaks that are burned annually since 1994) and with natural fire frequency (occurring every 3-5 years). We used a paired t-test to assess effects of the two fire frequency treatments on species richness and permutation tests to compare distance decay in similarity composition and relative abundances. We found that sites with high fire frequency had lower species richness compared to their paired sites subjected to natural fire frequency. However, similarities in species composition among sites with high fire frequency were only slightly higher than among sites subjected to natural fire frequency. The rate of distance decay in similarity was similar on the two treatments. At local scales, the high frequency of fire reduced species richness, an impact that in the long run may be irreversible. However, at broader spatial scales, effects were small as two treatments included similar sets of species. Prescribed fires have the potential to modify the natural dynamics of the woody communities in the savannas we studied. In our study, fire appears to act as a hierarchical filter, selecting species at broader spatial scales and nesting communities at local scales.


*Keywords:* beta diversity; Cerrado; fire management; Neotropical savanna


# 1. Introduction


Understanding how biological communities respond to disturbances has been one of the main goals of Ecology [@Grime_1973; @Connell_1978; @McLauchlan2020; @Pausas2017a; @Pausas2019]. Fire is one of the principal types of disturbance and along with other factors such as grazing, climate, and soil plays a fundamental role shaping biodiversity in savanna ecosystems [@Buisson_2018; @Pausas2019]. Indeed, there is evidence that fire is the main factor that maintains alternative states of Neotropical savannas [@Dantas2016; @Pausas2020]. Despite our knowledge about the potential of fire in altering structure and dynamics of plant communities in savannas [@Cianciaruso2012; @Dantas2013a; @Govender_2006; @Higgins_2000; @Lehmann_2014; @Maracahipes2014; @Silva2010], studies about the role of frequent fires on the spatial variation of species composition in Neotropical savannas are still lacking. Identifying how fire regimes controls the spatial structure of communities is essential to predict changes in both the species richness and composition in future climatic scenarios in which the frequency, duration, extent, and intensity of anthropogenic and wildfires are expected to increase [@Bowman2009; @Dale2001; @Field2014].


Fire promotes landscape heterogeneity [@Pausas2017c] and can lead to differences or similarities in species composition among communities (beta-diversity), via distinct mechanisms [@Dantas2016; @Myers2015]. For example, convergence of species composition may emerge when fire selects similar sets of species [tolerant species; @Laliberte2013a]. In this case, communities subjected to frequent disturbances may be composed by a subset of species from undisturbed communities [a pattern known as nestedness; @Ulrich2012] or by exclusively fire-resistant species. On the other hand, divergence in species composition may emerge when a disturbance interacts with environmental filters along environmental gradients [@Myers2015]. In the case of fire, communities along gradients may have distinct species compositions because fire effects depend on local conditions [for example, the amount of standing dry biomass [@Gomes2018] which can be controlled by soil characteristics along gradients].


Changes in beta-diversity in consequence of different fire regimes are important information for fire management in protected areas. Tropical grasslands and savannas naturally burn every 2-5 years, mostly due to lightning strikes in the beginning of the rainy season [@Franca2007a; @Ramos-Neto2000]. To prevent fire propagation inside protected areas large strips of vegetation (firebreaks) are burned annually, often during the dry season [@Ramos-Neto2000]. This resembles the anthropogenic fire regimes that occur over large areas with native vegetation in the Cerrado [@Miranda_2009]. The effects of the specific fire regime in firebreaks thus allow us to predict the consequences of high fire frequency on plant communities. Even if savanna plant species are adapted to natural fire that occur in the beginning of the rainy season [October -- May; @Bond2001; @Gignoux1997; @Gignoux2016a; @Pausas2017a], both high fire frequencies and burning during the dry season may deplete carbohydrate and nutrients which are essential to post-fire regrowth [@Miyanishi1986] and increase the mortality of savanna woody species [@Hoffmann1996]. As a consequence, recurrent anthropogenic fires should select those woody species that are extremely resistant or resilient to high fire frequencies [@Cianciaruso2012].


Additionally, the spatial scale of fire occurrence is important [@Bond2008; @Pausas2017]. For example, high fire frequency occurring in large patches homogenizes species composition [@Pausas2008] and drives the functional clustering of plants [@Silva2010]. Large and frequent fires affect the regional species pool by reducing both the chance of species to colonize open areas and the survival of species [@Dickson2008; @Lloret2005]. Thus, the similarity of species composition of communities affected by large and frequent fires is expected to be high [@Mouquet2003]. Conversely, fires occurring at local scales and in high frequencies create a mosaic landscape with habitat patches differing in frequency and time since the last fire [@Burkle2015]. In this scenario, annually burned patches are expected to present lower woody species richness in relation to patches experiencing natural fires [which occur in the Cerrado every 2-5 years due to lightning strikes; @Pereira_J_nior_2014; @Ramos-Neto2000]. Patches under natural fires are exposed to both different fire frequencies and intensities, which selects different species groups resistant to these conditions. Contrarily, firebreaks are burned annually during the dry season with the aid of drip torches. The differences between natural fires and high frequency anthropogenic fires should lead to distinct beta diversity patterns. Annual dry season fires employed in firebreaks should homogenize species composition and cause reduced beta diversity compared to areas with natural fire regimes. Identifying these patterns in different spatial scales can reveal the processes that maintain beta diversity and improve our knowledge on the consequences of annually burned firebreaks on woody species diversity.


We tested the effect of high fire frequency (firebreaks burned annually during the dry season) on species richness and community structure of savanna woody species. We expect that species richness of the communities under high fire frequency is lower than those under natural fire regimes (areas naturally burned every 2-5 years by lightning strikes, mainly during the transition between dry to wet season). Also, high fire frequency should homogenize communities and, thus, beta diversity among communities subjected to high fire frequency should be lower than that among natural fire regimes.


# 2. Material and Methods


## *2.1 Study sites and sampling*

We conducted the study at Emas National Park (ENP), one of the largest Cerrado reserves located in the Brazilian Central Plateau (Figure  1). The park covers an area of 132,941 hectares and the vegetation is a mosaic of open savannas (68.1% of its area), woodland savannas (25.1%), wet grasslands (4.9%), and riparian and semi-deciduous forest formations (1.2%). Climate is classified as tropical humid, with dry winters (June -- September) and wet summers (October -- May). The annual rainfall varies from 1,200 to 2,000 mm, and the average annual temperature is 24.6 °C [@Ramos-Neto2000]. We conducted our sampling in areas of open savannas.


The ENP was created in 1961, but was burned annually in the dry season between 1963 and 1984 to promote forage regrowth, which in that period was exploited for cattle ranching [@Franca2007a]. In 1984 the park was fenced and a fire exclusion policy was applied [@Ramos-Neto2000]. Consequently, several catastrophic wildfires occurred due to biomass accumulation. Since 1994, a network of 340 km of firebreaks is burned annually to control the spreading of fire inside the park [@Ramos-Neto2000]. However, the firebreaks did not avoid all the uncontrolled wildfires and extensive wildfires still occur in the park. For instance, in years of extreme drought, as in 2005 and 2010 wildfires burned, respectively, 50% and 98% of the total area of the park [@Silva2011].


During the wet season of 2017, we sampled 40 sites, distributed in 20 pairs along 35 km of firebreaks (Figure  1). Each pair included woody plant assemblages (shrubs and trees) subjected to two distinct fire treatments: a high fire frequency (50 m wide firebreaks burned annually since 1994 at the beginning of the dry season) and a moderate fire frequency (natural fires, burned every 3-5 years, mostly at the transition between dry and wet seasons). On average, the number of fires between 1994 and 2016 in these sites was 5.86 ± 0.52 (mean ± standard deviation). Distance between pairs varied between 1 and 34 km. In each sample site, we recorded 60 woody plant individuals. Each sample site consisted of 15 points systematically spaced 20 m from each other along a line of 300 m. In each point, we used the point-quarter method [@Mueller-Dombois1974] to sample four living woody individuals in the shrub-tree layer with at least 10 cm of diameter at ground level.


## *2.2 Data analyses*


We tested the effect of fire frequency on species richness using a paired t-test. As the number of individuals in each sample was fixed (60 individuals), standardization of sample effort using rarefaction was unnecessary. Our hypothesis regarding species richness is that high fire frequency reduces species richness and, thus, a one-tailed test was employed.


We assessed resemblance among communities subjected to natural and high fire frequencies using a permutational multivariate analysis of variance [PERMANOVA; @Anderson2001]. This analysis is based on the partitioning of the sum of squares of dissimilarity matrices between treatments (fire frequency) and among sample pairs, as the latter takes into account the spatial structure of the sampling design. If high fire frequency changes species composition, we expect to observe a large difference in species composition between fire treatments. Dissimilarities were obtained either for species composition and community structure (species composition and relative abundances) using Sorensen and Bray-Curtis indices, respectively. Our field sampling avoided differences in abundance among samples as well as large discrepancy in abundance values. Accordingly, Bray-Curtis dissimilarities were obtained using raw abundance data. We decomposed Sorensen's total dissimilarity into turnover and species richness differences [@Baselga2010]. Turnover represents species replacement without the influence of species richness among sampling sites along our spatial gradient. When two communities have the same number of species, the dissimilarity emerges from differences in species identity between those communities. In this case, Sorensen's total dissimilarity is equal to Simpson's dissimilarity [@Baselga2010]. Thus, we refer to our results based on turnover as Simpson (turnover). In contrast, species richness difference informs that communities differ in species richness. For instance, even if all species in one site are present in the other site, differences may occur as the species rich site includes species not observed in the species poor site. When that difference occurs orderly (i.e., when one community has a subset of species richness from another community), this phenomenon is classified as nestedness [@Baselga2010]. Nestedness is calculated by subtracting Simpson's dissimilarity from Sorensen's total dissimilarity [@Baselga2010]. We refer to our results based on species richness difference as richness difference (nestedness).


We used two approaches to test the effect of fire frequency on beta diversity among communities. First, we used the multivariate dispersion analysis to verify the homogeneity of dissimilarity values within the
treatments [PERMDISP; @Anderson2006]. If high fire frequencies homogenize communities, we may expect lower overall dissimilarity among sites subjected to high fire frequency (i.e., low dispersion in the ordination) compared to those dissimilarities among sites subjected to natural fire regimes. PERMDISP does not allow inclusion of a second predictor and, thus, it was not possible to take spatial effects into account. In our second approach, however, we took space into account by using distance decay in similarity analyses. We regressed (Ordinary Least Square) the similarities (1 - dissimilarity) against the Euclidean distances of spatial coordinates pairwise sites within each fire frequency to assess how similarities within each fire treatment decay with distance and, thus, only within-treatment similarities were used. Because similarity values were not independent to each other (samples were used multiple times), we used a permutation procedure to estimate probabilities that differences in intercept and slope between the two fire treatments were produced by chance. In our permutation procedure, the triangular matrices of similarities calculated separately for the two treatments were unfolded into a single vector. The same procedure was repeated for spatial distances, which were paired with their respective similarities. We adjusted linear regression models (similarities against spatial distances) for each treatment group, obtained the coefficients for intercept and slope and recorded the differences between the two treatments. Next, we permuted labels of the two treatment groups, maintaining each similarity value paired with its respective spatial distance. Regressions were fit and differences in coefficients of permuted groups recorded. We repeated this procedure 9999 times and estimated the chance the observed differences in coefficients were generated simply by chance as the proportion of differences obtained in the 9999 permutation that were equal or more extreme than those obtained using the original data (two-tailed test). If high fire frequency homogenizes species composition, we expect to observe a flat relationship between similarities within high fire frequency and spatial distances while observing a distance-decay relationship between similarities within natural fire frequency and spatial distances. Thus, our regression results should present significant differences between intercept and slope coefficients of fire treatments. PERMDISP and the similarity decay analyses were done using the Sorensen and Bray-Curtis dissimilarities as well as the Simpson (turnover) and species richness difference (nestedness) components of the former.


All analyses were done using the R environment [@r-core-team]. Multivariate analyses (dissimilarity matrices, PERMANOVA, and PERMDISP) were done using the *vegan* and *betapart* packages [@vegan; @betapart]. We wrote R scripts to compute the permutation tests.


# 3. Results








We recorded a total of 2400 tree and shrub individuals (60 individuals in each of the 40 sampling units) distributed among 59 woody species (Table A1). A total of 58 and 47 woody species were sampled in sites with the natural fire and high fire frequencies treatments, respectively. Most samples under natural fire frequency had higher woody species richness than their counterparts under high fire frequency (16 out of 20 pairs; *t*~19~ = 2.56, *p* = 0.009; Figure  2). Thus, we corroborate the hypothesis that communities under high fire frequency have lower species richness at both local (paired sites) and regional (overall species richness in the two treatments) scales.


We found that, after taking into account spatial effects using the paired sampling design, fire frequency changed community composition and structure (Table  1). Yet, the effects were small compared to the high variation in species composition and structure due to space (Figure  3). In fact, communities under high fire frequency were almost totally superposed on the multivariate space defined by communities under natural fire frequency, indicating they have only slightly dissimilar species composition (Figure  3).


Dispersion of samples in the ordination space was similar for the two treatments, indicating they have overall similar beta diversity (Table  2; Figure  3). The slopes of the distance decay of similarity were similar for the two fire frequency treatments (Table  3; Figure  4). Similarity in species composition among samples subjected to high fire regime was slightly higher than among those samples subjected to natural fire frequency except for the species richness difference components of Sorensen index (Figure  4; Table  3). Yet, species composition changed more in function of the spatial distance than in function of fire frequency (Figure  4). Unexpectedly, similarity due to differences in species richness increased with the distance (Figure  4).


# 4. Discussion


We investigated differences between sites with high fire frequency (annual fires) and with natural fire frequency (burned every 3 to 5 years) regarding woody species richness and woody species community composition in a Neotropical savanna. We found that high fire frequency reduces woody species richness locally, leading to a subset of species from sites with natural fire frequency (Figure  2). Also, total number of woody species observed in all high fire frequency samples (47) was lower than that observed in natural fire regime (58). Similarity between samples in the high fire treatment tended to be slightly higher and significant than between those under natural fire frequency when controlled for spatial distances. Yet, even if firebreaks are burned during the dry season and at much higher frequency than natural fires, this did not affect the overall woody species composition at the larger scales, encompassing all samples irrespective of spatial distance (Figure  3. The decay of similarity of the woody species composition with the spatial distance was similar for the two fire frequencies (Figure  4).


These results reveal that the fire management done in ENP may have small effects on the homogenization of woody species composition (i.e., species composition under different fire frequencies are similar). However, annual burns in the firebreaks reduce woody species richness at local and regional scales. This indicates that high fire frequency resulted in local communities that were a subset of the nearby communities under natural fire frequency. Cerrado woody species have fire-adaptative traits that allow their establishment, persistence, and reproduction even under severe fire conditions [@Batalha2011a; @Cianciaruso2012]. However, repeated annual fires at the dry season can extirpate species from local communities by preventing individuals to attain high heights, stem diameter, and thick bark, important traits related to fire-escape in savannas [@Hoffmann2012; @Maracahipes2018; @Medeiros2008]. In addition, as most of the species concentrates seed maturation in the beginning of the wet season, dry season fire can have negative impacts for their population dynamics [@Hoffmann1998; @Hoffmann2002a]. Because frequent fire reduces the plant height and basal area [@Medeiros2008], our results suggest that the species impoverishment in areas under high fire frequency maybe a consequence of tree topkill, in which vegetative and reproductive structures of woody species are constantly removed by fire [@Hoffmann2009; @Souchie2017]. Even if savanna woody species have traits that enable them to resist fire, these traits usually are not present in saplings or juveniles. For example, in early ontogenetic stages, woody plants do not have sufficient thick bark to escape fire [@Maracahipes2018; @Pausas2015c]. Our sampling strategy, which recorded individuals with a diameter above 10 cm, likely did not include those juveniles. However, the fact that the fire management is conducted during the dry season may result in harsher conditions to species establishment and survival [@Rissi_2017]. During the dry season, high air temperatures coupled with low soil humidity can make fires more severe for species in the firebreaks, so we cannot expect high seedling recruitment of woody species. In the long term, some species submitted to these conditions can no longer persist and establish in these areas due to the mismatch between anthropogenic fire regime and their natural population dynamics [@Hoffmann2002a].


At the regional scale, we did not observe differences in the community structure (species composition and relative abundances) between the areas with natural and high fire frequency (Figure  3). Cerrado woody species have evolved under fire [@Pausas2017c; @Simon2012], but we expected a negative effect of the high fire frequency for the beta diversity. Our results reinforce previous findings [@Cianciaruso2012; @Scalon2019; @Simon2012] about the convergence of species strategies to deal with fire; surprisingly, even in high fire frequency, the differences in community structure did not change. However, both modeling and observational studies have found structural changes of the communities when under different fire frequency [@Dantas2013a; @Hoffmann1999]. For instance, individuals tend to have reduced stem size due to the repeated topkilling under high fire frequency. Furthermore, as stem size is closely related to plant survival, reproduction, and growth [@Bond1996], it is probable that despite surviving in the firebreaks these individuals are not reproducing [@Zirondi_2021]. The effects of high fire frequency on woody species reproduction is still an open question that deserves future investigation to best understand the consequences of fire management and species response to anthropogenic fires that are becoming larger and more frequent.


Contrary to our expectation, fire had no effect in changing woody community structure at our larger scale (entire study). The decay of similarity of species composition among communities along spatial distances is known for this savanna [@Dantas2015] and for others vegetation types [@Nekola1999; @Reilly2006]. Our results indicate that the decay rate (slope) of these relationships were similar for the two fire treatments and the mean similarity were only slightly higher in the high fire frequency. Other factors can act shaping the beta diversity along spatial gradients. Soil features, nutrient and water availability, and topography can interact with fire frequency generating changes in species composition [@Batalha2011a; @Dantas2015]. Also, since fire occurs in a patchy way, communities protected from fire are a source of new individuals that can colonize the new areas opened by fire, thus influencing the species turnover [@Catano2017; @Reilly2006a]. Due the effect of recurrent fire on the sexual reproduction of shrubs and trees [@Hoffmann2002a], this distance-decay pattern also can be generated by dispersal limitation (species recruitment mostly by resprouting).


# 5. Conclusions


Understanding the effect of fire on diversity is important to both theoretical and practical perspectives. Identifying how ecological communities respond to fire can help us to predict future scenarios in which the frequency of this disturbance is likely to increase. Also, this knowledge can serve to inform management of fire-prone vegetation. Here, we found that fire had an important effect on species richness of woody plants at local and regional scales. Yet, fire had only subtle effects on the homogenization of this savanna. Thus, fire seems to act as a hierarchical filter, selecting species from the species pool at regional scales and decreasing species richness at local scales. It should be noted, however, that our high frequency fire samples were immersed in a landscape of natural fire regimes. Increased fire frequencies expected to occur under future climate and that will affect vast areas should produce stronger effects.


# Acknowledgements


ASM and MVC received fellowship from Conselho Nacional de Desenvolvimento Científico e Tecnológico (CNPq, no. 307587/2017-7 and 306590/2018-2). This study was partially funded by Coordenação de Aperfeiçoamento de Pessoal de Nível Superior - Brasil (CAPES) - Finance Code 001, CNPq -- PELD Sítio 13 (441214/2016-9) and FAPEG. We thank Cleber ten Caten and Ismaiara Vaz for helping us in the field. We also thank the two anonymous reviewers and the subject editor for suggestions to improve our manuscript.


# Research data


Data, text, and code to reproduce analyses, figures, and the entire manuscript itself are available at the following link: https://github.com/kguidonimartins/betadiv-enp


# References


<div id="refs"></div>


# Tables





Table  1: Results of PERMANOVA analyses. After taking into account spatial effects using the paired sampling design, fire frequency has a small effect in community composition and structure. No effect was found on richness difference.

|Similarity                       |Predictor      | Degrees of freedom|  *F*| R^2^| *P*-value|
|:--------------------------------|:--------------|------------------:|----:|----:|---------:|
|Sorensen                         |Fire frequency |                  1| 3.55| 0.05|    <0.001|
|                                 |Pairs          |                 19| 2.86| 0.71|    <0.001|
|Simpson (turnover)               |Fire frequency |                  1| 3.51| 0.04|     0.010|
|                                 |Pairs          |                 19| 3.24| 0.73|    <0.001|
|Richness difference (nestedness) |Fire frequency |                  1| 4.40| 0.08|     0.070|
|                                 |Pairs          |                 19| 1.69| 0.58|     0.170|
|Bray-Curtis                      |Fire frequency |                  1| 3.19| 0.04|    <0.001|
|                                 |Pairs          |                 19| 3.11| 0.73|    <0.001|


Table  2: Results of PERMDISP analyses. Dispersion of dissimilarities did not differ between fire frequencies. Degrees of freedom is 1 for the fire frequency and 38 for the residuals.

|Similarity                       |*F*   | *P*-value|
|:--------------------------------|:-----|---------:|
|Sorensen                         |1.359 |     0.266|
|Simpson (turnover)               |1.643 |     0.189|
|Richness difference (nestedness) |0.626 |     0.454|
|Bray-Curtis                      |1.967 |     0.163|


Table  3: Results of the permutation procedure to compare intercepts and slopes of the distance decay in similarity relationships in natural and high fire frequencies treatments. Similarity in both treatments tended to decrease with distance. No effect was found on richness difference.

|Similarity                       | Intercept difference| *P*-value intercept| Slope difference| *P*-value slope|
|:--------------------------------|--------------------:|-------------------:|----------------:|---------------:|
|Sorensen                         |                0.044|               0.010|           -0.039|           0.706|
|Simpson (turnover)               |                0.062|               0.001|           -0.060|           0.614|
|Richness difference (nestedness) |               -0.018|               0.095|            0.021|           0.709|
|Bray-Curtis                      |                0.036|               0.049|            0.101|           0.347|


# Captions


Figure  1: Location of the sampling pairs (white squares) in the Emas National Park (ENP), GO, Brazil. Pairs were located at least 1 km apart. The figure at the top right indicates the location of ENP (white circle) within the Cerrado biome (dark gray) in South America (light gray).


Figure  2: Paired comparison of woody species richness between communities under natural fire frequency and high frequency of fire. Dashed line represents pairs of sites in which woody species richness was greater in higher fire frequency than in natural fire frequency. Circles representing sampling sites with the same woody species richness are overlaid (darker gray). In general, communities under natural fire frequency were richer than communities under high fire frequency.


Figure  3: PCoA ordination of woody plant community structure in samples subjected to high and natural fire frequencies. Lines connect pairs of sampling units (each sampling unit under a different fire frequency). There is no difference in dissimilarities' dispersion between natural and high fire frequency, regardless of the dissimilarity index.


Figure  4: Similarity decay of woody species communities along our spatial gradient. Each point represents a comparison pair of the distance matrix. Similarities in both high and natural fire regimes decreased in the same rate with spatial distance, except for the Richness difference.


# Figures


![](manuscript/betadiv-enp_files/figure-docx/print_map-1.png)<!-- -->


![](manuscript/betadiv-enp_files/figure-docx/print_richness-1.png)<!-- -->


![](manuscript/betadiv-enp_files/figure-docx/print_pcoa-1.png)<!-- -->


![](manuscript/betadiv-enp_files/figure-docx/print_distance_decay-1.png)<!-- -->


# Appendix


Table A1: Species abundances in the high and natural fire frequency regimes.

|Species                                                    | High fire frequency (firebreaks) | Natural fire frequency |
|:----------------------------------------------------------|:--------------------------------:|:----------------------:|
|*Agonandra brasiliensis* Miers ex Benth. & Hook.f.         |                0                 |           2            |
|*Anadenanthera peregrina* var. falcata (Benth.) Altschul   |                44                |           38           |
|*Annona crassiflora* Mart.                                 |                35                |           30           |
|*Aspidosperma tomentosum* Mart. & Zucc.                    |                2                 |           16           |
|*Bowdichia virgilioides* Kunth                             |                0                 |           1            |
|*Byrsonima coccolobifolia* Kunth                           |                7                 |           3            |
|*Caryocar brasiliense* Cambess.                            |                0                 |           5            |
|*Casearia sylvestris* Sw.                                  |                6                 |           13           |
|*Connarus suberosus* Planch.                               |                23                |           42           |
|*Cybistax antisyphilitica* (Mart.) Mart.                   |                1                 |           2            |
|*Davilla elliptica* A.St.-Hil.                             |                1                 |           9            |
|*Didymopanax macrocarpus* (Cham. & Schltdl.) Seem.         |                1                 |           3            |
|*Didymopanax vinosus* (Cham. & Schltdl.) Marchal           |                0                 |           1            |
|*Dimorphandra mollis* Benth.                               |                8                 |           5            |
|*Diospyros lasiocalyx* (Mart.) B.Walln.                    |                38                |           59           |
|*Eremanthus glomerulatus* Less.                            |                32                |           63           |
|*Eriotheca gracilipes* (K.Schum.) A.Robyns                 |                4                 |           10           |
|*Eriotheca pubescens* (Mart. & Zucc.) Schott & Endl.       |                13                |           17           |
|*Erythroxylum engleri* O.E.Schulz                          |                20                |           15           |
|*Erythroxylum suberosum* A.St.-Hil.                        |                21                |           22           |
|*Erythroxylum tortuosum* Mart.                             |                2                 |           8            |
|*Eugenia aurata* O.Berg                                    |                0                 |           3            |
|*Eugenia punicifolia* (Kunth) DC.                          |                1                 |           2            |
|*Guapira noxia* (Netto) Lundell                            |                8                 |           4            |
|*Hancornia speciosa* Gomes                                 |                1                 |           1            |
|*Handroanthus ochraceus* (Cham.) Mattos                    |                22                |           60           |
|*Himatanthus obovatus* (Müll. Arg.) Woodson                |                5                 |           3            |
|*Hymenaea stigonocarpa* Mart. ex Hayne                     |                2                 |           2            |
|*Kielmeyera coriacea* Mart. & Zucc.                        |                36                |           37           |
|*Kielmeyera grandiflora* (Wawra) Saddi                     |                3                 |           2            |
|*Lafoensia pacari* A.St.-Hil.                              |                0                 |           2            |
|*Leptolobium dasycarpum* Vogel                             |                81                |           42           |
|*Machaerium acutifolium* Vogel                             |                1                 |           4            |
|*Miconia albicans* (Sw.) Triana                            |                0                 |           45           |
|*Miconia ferruginata* DC.                                  |                0                 |           2            |
|*Miconia ligustroides* (DC.) Naudin                        |                0                 |           2            |
|*Myrcia bella* Cambess.                                    |                9                 |           5            |
|*Myrcia camapuanensis* N.Silveira                          |                6                 |           4            |
|*Myrcia guianensis* (Aubl.) DC.                            |                6                 |           1            |
|*Myrcia variabilis* DC.                                    |                2                 |           1            |
|*Ouratea hexasperma* (A.St.-Hil.) Baill.                   |                7                 |           10           |
|*Ouratea spectabilis* (Mart.) Engl.                        |                29                |           31           |
|*Palicourea rigida* Kunth                                  |                61                |           26           |
|*Piptocarpha rotundifolia* (Less.) Baker                   |                84                |           63           |
|*Plathymenia reticulata* Benth.                            |                12                |           4            |
|*Plenckia populnea* Reissek                                |                1                 |           1            |
|*Pouteria ramiflora* (Mart.) Radlk.                        |               153                |           90           |
|*Pouteria torta* (Mart.) Radlk.                            |               151                |          136           |
|*Psidium laruotteanum* Cambess.                            |               104                |          118           |
|*Pterodon pubescens* (Benth.) Benth.                       |                0                 |           1            |
|*Qualea parviflora* Mart.                                  |                2                 |           2            |
|*Rourea induta* Planch.                                    |                1                 |           0            |
|Solanum sp.                                                |                2                 |           2            |
|Sp 1                                                       |                0                 |           1            |
|Sp 2                                                       |                0                 |           1            |
|*Stryphnodendron adstringens* (Mart.) Coville              |               141                |          114           |
|*Stryphnodendron rotundifolium* Mart.                      |                2                 |           8            |
|*Tabebuia aurea* (Silva Manso) Benth. & Hook.f. ex S.Moore |                8                 |           4            |
|*Vochysia cinnamomea* Pohl                                 |                1                 |           2            |
