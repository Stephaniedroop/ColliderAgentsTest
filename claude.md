# ColliderAgentsTest

R-based PhD research project investigating how human cognition uses causal selection and causal inference to generate explanations. Intended for journal publication.

## Research Focus

- Studies causal reasoning using "collider" structures (two causes A and B independently affect outcome E)
- Compares human participant responses with CESM (Causal Explanation Selection Model, Quillien & Lucas 2023) predictions
- Tests "abnormal inflation/deflation" and "computational kindness" in causal explanations

## Project Structure

- `Scripts/` - 22 R scripts for analysis pipeline
- `Scripts/Standalone/` - Independent analyses (demographics, chi-square, cover story effects)
- `Data/` - Participant data, model outputs, fit tables
- `Other/Reports/` - Generated reports
- `renv/` - R environment management

## Analysis Pipeline

Run via `Scripts/master.R`:

1. **01preprocess.R** - Collate participant CSVs from behavioural experiment
2. **02setParams.R** - Set causal model base rates
3. **03getPreds.R** - Run CESM model predictions
4. **04processPreds.R** - Process predictions for analysis
5. **05getLesions.R** - Test model components (inference, computational kindness, actual causality)
6. **06optimise.R** - Fit models to participant data
7. **07processForPlot.R** - Prepare data for visualization
8. **08samplePreds.R** - Sample explanations from model
9. **09testSamp.R** - Test sampled explanations against participants
10. **10-11reportFigs.R** - Generate plots
11. **12-14fitByppt.R** - Per-participant model fitting

## Key Terminology

- **World**: Configuration of nodes A, B, E (each 0 or 1). E.g., "110" = A happened, B happened, E didn't
- **CESM**: Core causal explanation selection model being tested
- **Collider**: Causal structure where multiple causes affect one outcome

## Utility Scripts

- `cesmUtils.R` - CESM model functions
- `optimUtils.R` - Optimization and likelihood functions
- `plotUtils.R` - ggplot comparison functions

## Tech Stack

R v4.1, tidyverse, ggplot2, lme4/lmerTest, renv
