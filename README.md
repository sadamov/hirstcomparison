## Setup
This project uses renv. To install all packages needed in your local R-environment simply run: `renv::restore()`
If you are working on your MCH-laptop you might want to set the renv cache-path to another location than the home-folder, otherwise you will reach the quota. Feel free to use my cache with downloaded packages, by pasting the following lines of code into your .Renviron file.
`# Set the renv cache to prod-zue otherwise my home-drive exceeds storage limits RENV_PATHS_CACHE="/prod/zue/climate/others/ads/share/renv/cache"` 
If you are using renv you might not be able to work with APK-tools for the time being.

## Branches
There is a master and develop branch, both were identical when I completed the project end of June 2020. Feel free to pull the latest code from either one.

## Vignettes
Both vignettes are quite similar in terms of what methods are applied. 
Hirstcomparison: Comparison of three Hirst-type traps located in Payerne in the blooming season of 2013.
Polenocomparison: Comparison of three Poleno-type traps and one Hirst-type traps located in Payerne in the blooming season of 2020.
The Poleno vignette is not quite up to the standard of the Hirst one, the classification of pollentaxa was no final at the time. Hence it is suggested to look at the Hirst vignette to get the latest methods and plots.
This analysis served multiple purposes:

- Better understanding the Measurement and Sampling variability of Hirst and Poleno traps
- Evaluating the necessities for the parallel measurement campaign, when the new Poleno devices are introduced
- Creating plots for one or more papers

## Data
The data is currently not stored on Gitlab and everyone using the codes need to copy the data manually into their respective "ext-data"-folder. You can find all the data here: "M:\zue-prod\climate\others\ads\hirstcomparison\ext-data".
The data was manually prepared by colleagues from the MDS team in Payerne. Please refer to them in case of questions about any data-prep that took place before the vignette.

- Hirst-trap Data from 2013 provided by Fiona Tummon 
  - PAYX_2013dates_10min.csv: Timestamps of the measurements.
  - PAYXlineX_2013_10min.csv: 10-Minute Pollen Concentrations Averages per trap and counted line (manually under microscope).
- Poleno and Hirst Data from 2020 provided by Gian Liebherr
  - second_pX.csv: Pollen counts by the three Poleno devices with labeling of the species of a work-in-progress NNet-Classifier.
  - pollen_2020_dwh.csv: The official pollen measurements from MCH retrieved using the climap software (all pollen - station Payerne).

## The Analysis
Below follows a short descpriction of the analysis carried out in the vignettes. 
As the blooming period strongly depends on the investigated plant species, a simple universal definition was chosen. Only time steps where at least one of the three traps measured a specific pollen type were considered in the analysis. Time steps during which no pollen of that specific type were measured by any of the traps, are usually not of great interest. Therefore, the measurements of all other time steps were set to NA after the temporal average was calculated.

Out of the 20 species mentioned in section 2.2, the top five pollen species were selected, based on their calculated Seasonal Pollen Index (SPI). In addition, five concentration classes were defined for the analysis: [0-9) pollen grains/m3, [10-19) pollen grains/m3, [20-49) pollen grains/m3, [50-99) pollen grains/m3, [100-299) grains/m3, and >=300 pollen grains/m3, with the aim of better understanding whether different pollen concentrations have an impact on sampling and measurement error. The measurements of all species were combined to create reasonably large concentration groups. The mean of all traps was calculated, to define which group the measurements for that time step fall within. 
Furthermore, to investigate the impact of pollen size on sampling and measurement error, five groups of pollen were defined according to their average sizes (Beug, XXXX):
Group 1: Urtica, Castanea
Group 2: Alnus, Betula, Corylus, Fraxinus, Platanus, Rumex, Salix
Group 3: Plantago, Poaceae, Populus, Taxus, Cupressus, Ulmus
Group 4: Carpinus, Fagus, Juglans, Quercus
Group 5: Pinus, Picea
After an initial residual analysis, the measurements were converted into logarithmic concentrations for the statistical comparison.

Several metrics were calculated to compare the measurements of the three traps: the frequency of occurrence for each pollen was obtained by counting the number of days that each particular pollen was detected; the seasonal mean was calculated by averaging daily values for each pollen for all the days that it occurred; and the SPI was calculated by integrating the concentrations of a particular pollen over the entire season (Mandrioli et al., 1998). Average and maximum concentrations were also calculated, as well as the standard deviations for both daily and hourly values.
To further investigate the difference of the measurements from the three traps, the relative differences from the common mean were calculated. Therefore, only time steps where all three traps measured pollen were considered, otherwise the common mean cannot be calculated.

The analysis was conducted in R and figures were produced using the package ggplot2 (Wickham H., 2016). To compare the three traps with each other, a variety of statistical techniques and visualization tools were used. The goal was to investigate several methods and compare the findings of them. If most techniques produce similar findings, that finding can be considered more robust. 
Two visual methods were used to compare the traps pairwise. First, simple scatterplots were displayed to compare the measurements of two traps for each time step. Locally weighted regression (LOESS) was performed to better identify biases in the measurements. Second, Bland-Altman plots were used, to identify outliers and investigate biases between the traps further (Bland and Altman, 1986). This method has the benefit that is that does not impose any assumptions on the underlying data and it is broadly used in various scientific areas.
In addition to the visual methods a broad range of statistical methodologies were applied to investigate the (dis-)similarities between the three traps. The correlation between the measurements from traps were calculated pairwise with three different approaches: Pearson (1895) correlation coefficients for linear correlation between traps, Spearman (1904) and Kendall (1938) for rank-based correlation. The latter two are non-parametric tests and do not assume normality of the data. As more than two traps were being compared, an analysis of variance (ANOVA) and corresponding pairwise contrasts can be calculated. Thereby, the explained variance of the factor trap (i.e. which trap measured a certain concentration) was compared to the overall variance in the data. First, a parametric one-way ANOVA (Fisher, 1918) was conducted. Second, robust pendants to the parametric one way ANOVA were investigated. The parametric one-way ANOVA imposes several assumptions on the underlying data: independent and normally distributed errors with a constant variance and a mean of zero. The robust Kruskal-Wallis rank test (Kruskal and Wallis, 1952), on the other hand, has less strict assumptions: the samples must be mutually independent, measurements must be continuous on at least an ordinal scale and all traps should have the same shape distributions. Depending on the temporal averaging window, certain assumptions were violated and the appropriate choice of the statistical procedure is important. 
Both, ANOVA and Kruskal-Wallis, are considered omnibus tests, testing whether the variance in a sample explained by the factor trap is greater than the unexplained variance. If the resulting explained variance is low, one can assume that the dissimilarities between the traps are low. Both omnibus tests can be followed-up by multiple pairwise tests between the traps. For the standard one-way ANOVA Tukey’s honestly significant difference test (Tukey, 1949) was applied, to measure the differences of the mean. This original version of TukeyHSD assumes normally distributed data with constant variance for all traps. The rank-based Kruskal-Wallis test was followed-up by Dunn’s test (Dunn, 1964) and the Conover-Iman (1979) test. Additionally, the nparcomp (Konietschke et al., 2015) package is used to create plots that show the probability that one trap will measure higher than another will. Its nparcomp-function provides the functionality to calculate confidence intervals for those estimators. For all pairwise tests, the resulting p-Values were adjusted for multiple testing using the Holm (1979) method.
So far, a fixed effect model set-up was assumed. In a final step, a random effect model was applied to make conclusions for a larger population of traps based on the three traps observed in this study. To do so, a technique called restricted maximum likelihood (Bartlett M., 1937) was applied for both parametric and robust rank-based methods.
