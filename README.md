# TEST GROUND for agents setup

# OLD >>>>

# Scripts and data for a behavioural experiment and computational models

# to study how human cognition uses both causal selection and causal inference to generate explanations for observed phenomena

## Authors

- Stephanie Droop (stephanie.droop@ed.ac.uk)

## Languages

All scripts are in R, v.4.1. Packages needed are below with citations.

## How to run

- or **analysis** go to the `Scripts` folder and run `master.R`, or if you know what script you are looking for you can go straight there and run that only. See list of scripts below.
- This structure assumed you have a renv and .rproj doc with here set to the .rproj.
- To see and try the behavioural experiment go to https://eco.ppls.ed.ac.uk/~s0342840/collidern/collidertop.html. Code for the task interface and structure of running it in JavaScript in in the folder `Other/hostExperiment`. To click through the experiment: at the comprehension check enter Yes, No, True, 12.

## Files, folders, model

### FOLDER Scripts

- `master.R` - top level analysis script. **Go here first**

It will first source a few scripts holding static utilities:

- `cesmUtils.R` - functions to run the CESM model
- `optimUtils.R` - functions to fit and optimise models
- `plotUtils.R` - functions to generate plots

The masterscript sources scripts in the following order:

a) Processing behavioural experiment data (see folder `Other/hostExperiment` for the JS code of experiment):

- `01preprocess.R` - Get the participants' behavioural experiment data ready. Saves as `Data.Rdata` in folder `Data`

2. Set up Collider worlds (see note on worlds below) and get CESM (Quillien+Lucas23) model predictions for those for the _causal selection_ part of the model:

_Btw in the middle of a reconstruction of files and names at 21 Sept 2025_

- `02setParams.R` - short script to set the manipulated probabilities of the variables
- `03getPreds.R` - get CESM model predictions. Output `all.rda`
- `04processPreds.R` - put model predictions in a user-friendly format, renames variables, splits out node values 0 and 1. Input: `all.rda`, output `modelproc.rda`

3. Other modelling

- `05getLesions.R` calculates the _inference_, _computational kindness_ and _actual causality_ parts of the full models, then progressively lesions. Input `modelproc.rda` and `Data.rdata`, output `modelAndDataUnfit.rda`
- `06optimise.Rmd` fits and optimises models. Input `modelAndDataUnfit.rda`, output `fit16mpn.csv`.
- `07processForPlot.Rmd` - an intermediate file for the clumsy relabelling, to stop the actual plotting file getting unwieldy. Input: `fit16mpn.csv`; output `fitforplot16mpn.csv`
- `08samplePreds.Rmd` - get an actual sample from model for each participant trial. Input: `Data.Rdata` and `fitforplot16mpn.csv`, output: a glmer result in the console and reported in the Unobserved variables section of the paper and `forplotbyppt.csv`. (TO DO: should this not be used for the freestanding analyses scripts too)
- `09testSamp.R` - Test the matched sampled explanations against participants for our theory metrics
- `10reportFigs.R` generates plots on a pattern with functions from `plotUtils.R`. Most are 'nice to have' and not reported. Input `fitforplot16mpn`, output figures saved in folder `Other`, `Plots`. _TO DO_: 1) add code to save plots as .png too for wider comms.
- `11reportFigs2.R` individual plots using customised calls, for reporting.
- `12fitByppt.R` how many ppts are best fit by each model. Input `Data.Rdata` and `fitforplot16mpn`. Output: table of results in the console

4. Free-standing analyses for reporting

- `coverTest.Rmd` - Freestanding analysis to check if cover story affects answers (it doesn't - except lightly in 2/36 conditions, due to noise)
- `demogs.R` - demographics on participant behavioural data
- `abnormalInflation.Rmd` - check for presence of abnormal inflation and deflation (documented behavioural phenomena of causal selection) are found in our data, which would be support for causal selection theory. [Preliminary analysis finds no evidence, unlike in the cogsci paper]. Input `Data.Rdata`, output: table of results in the console.
- `chisq.r` - check whether participants answer non-uniformly in each world. Input `modelAndDataUnfitpn.csv`, output: table of results in the console.

### FOLDER Experiment

Holds the Javascript and html to run the behavioural experiment, which is an online interface with a task like a game. Participants from Prolific were paid to complete the task in early July 2024.

### FOLDER Data, pilot_data

Participant data from the behavioural experiment.

## Glossary

Some everyday words have a special sense in this project.

- `world` - a setting of observed node variables A and B, and outcome E set by deterministic structural equations. A single iteration of how things started off and turned out. Each node can take 0 or 1. Sometimes represented by the values of A,B,E in order, eg. 110 means A=1, B=1, E=0, ie. that A and B both happened and the effect didn't.

## Package citations

_ellmer_ **light use!**
Wickham H, Cheng J, Jacobs A, Aden-Buie G, Schloerke B (2025). _ellmer: Chat with Large Language
Models_. R package version 0.3.2, <https://CRAN.R-project.org/package=ellmer>.

_gander_ **light use!**
Couch S (2025). _gander: High Performance, Low Friction Large Language Model Chat_. R package version
0.1.0, <https://CRAN.R-project.org/package=gander>.

_ggnewscale_
Campitelli E (2024). _ggnewscale: Multiple Fill and Colour Scales in 'ggplot2'_. R package version
0.5.0, <https://CRAN.R-project.org/package=ggnewscale>.

_ggplot2_
H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.

_here_
Müller K (2025). _here: A Simpler Way to Find Your Files_. R package version 1.0.2,
<https://CRAN.R-project.org/package=here>.

_knitr_
Xie Y (2023). _knitr: A General-Purpose Package for Dynamic Report Generation in R_. R package version
1.45, <https://yihui.org/knitr/>.

_lme4_
Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker (2015). Fitting Linear Mixed-Effects Models
Using lme4. Journal of Statistical Software, 67(1), 1-48. doi:10.18637/jss.v067.i01.

_lmerTest_
Kuznetsova A, Brockhoff PB, Christensen RHB (2017). “lmerTest Package: Tests in Linear Mixed Effects
Models.” _Journal of Statistical Software_, _82_(13), 1-26. doi:10.18637/jss.v082.i13
<https://doi.org/10.18637/jss.v082.i13>.

_RColorBrewer_
Neuwirth E (2022). _RColorBrewer: ColorBrewer Palettes_. R package version 1.1-3,
<https://CRAN.R-project.org/package=RColorBrewer>.

_renv_
Ushey K, Wickham H (2025). _renv: Project Environments_. R package version 1.1.5,
<https://CRAN.R-project.org/package=renv>.

_rjson_
Couture-Beil A (2024). _rjson: JSON for R_. R package version 0.2.23,
<https://CRAN.R-project.org/package=rjson>.

_rmarkdown_
Allaire J, Xie Y, Dervieux C, McPherson J, Luraschi J, Ushey K, Atkins A, Wickham H, Cheng J, Chang W,
Iannone R (2024). _rmarkdown: Dynamic Documents for R_. R package version 2.29,
<https://github.com/rstudio/rmarkdown>.

_stringr_
Wickham H (2023). _stringr: Simple, Consistent Wrappers for Common String Operations_. R package version
1.5.1, <https://CRAN.R-project.org/package=stringr>.

_tidyverse_
Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J,
Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K,
Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source
Software_, _4_(43), 1686. doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.

_xtable_
Dahl D, Scott D, Roosen C, Magnusson A, Swinton J (2019). _xtable: Export Tables to LaTeX or HTML_. R
package version 1.8-4, <https://CRAN.R-project.org/package=xtable>.
