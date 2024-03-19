This repository contains the analysis files used for the ICPhS 2023 paper *Testing the Locus of Speech-Act Meaning in English Intonation* and the related ASA 2023 poster *Tonal Center of Gravity Predicts Variation in the Interpretation of Rising and Falling Intonation in American English*. The OSF page, which also contains the stimuli, proceedings papers, and posters is available here: https://osf.io/8hrfv/

Some notes:
 - Writeups/models.qmd runs all models and the performance metrics used for the ASA poster
 - Writeups/figures.qmd runs creates all the other figures used in the ASA poster and ICPhS paper
 - cmdstanr doesn't play well with renv for whatever reason. Install instructions here: https://mc-stan.org/cmdstanr/

```r
# install.packages("remotes")
remotes::install_github("stan-dev/cmdstanr")
```

Otherwise, you can run the following:

```r
renv::restore() # Install all packages
library(targets)
library(tarchetypes)

tar_make() # Run analyses and render figures
```
Explanation of directories:

 - Data: Contains data from each experiment
 - Figures: Figures used in the paper available as .svg and .pdf files
 - Helpers: .R files containing various helper functions. Some are redundant with the sosprosody package and others aren't used in this portion of the project.
 - Models: Models reported in the paper, as .rds files
 - Writeups: .qmd and standalone .html files for running the models and generating the figures. Re-rendering the .qmd files will overwrite the .html files.
 - renv: renv environment for the packages used in this analysis. Please refer to this page for more information on using renv.

## Citations

If you use any of the code contained in this repository, please cite our paper:

Sostarics, T., & Cole, J. (2023). Testing the Locus of Speech-Act Meaning in English Intonation. In R. Skarnitzl & J. Volín (Eds.), Proceedings of the 20th International Congress of Phonetic Sciences (pp. 1240–1244). Guarant International

```
@inproceedings{sostarics2023icphs,
  author={Sostarics, Thomas and Cole, Jennifer},
  title={{Testing the Locus of Speech-Act Meaning in English Intonation}},
  year={2023},
  booktitle={{Proceedings of the 20th International Congress of Phonetic Sciences}},
  pages={1240--1244},
  publisher={Guarant International},
  editor={Radek Skarnitzl and Jan Vol\'in}
}
```

Specific reference to the ASA poster should be made with:

Sostarics, T., & Cole, J. (2023). Tonal center of gravity predicts variation in the interpretation of rising and falling intonation in American English. The Journal of the Acoustical Society of America, 153(3_supplement), A78–A78. [https://doi.org/10.1121/10.0018225](https://doi.org/10.1121/10.0018225)

```
@article{sostaricscole2023asa,
    author = {Sostarics, Thomas and Cole, Jennifer},
    title = "{Tonal center of gravity predicts variation in the interpretation of rising and falling intonation in American English}",
    journal = {The Journal of the Acoustical Society of America},
    volume = {153},
    number = {3\_supplement},
    pages = {A78-A78},
    year = {2023},
    month = {03},
    issn = {0001-4966},
    doi = {10.1121/10.0018225},
    url = {https://doi.org/10.1121/10.0018225},
}
```
