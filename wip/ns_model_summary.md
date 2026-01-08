# Necessity-Sufficiency Model Implementation Summary

## Overview

This document summarises the implementation and results of the Icard et al. (2017) Necessity-Sufficiency (N/S) model for causal explanation selection, as an alternative to the CESM model.

## Theoretical Background

The N/S model (Icard, Kominsky & Knobe, 2017) proposes that causal judgments depend on two counterfactual measures:

- **Necessity**: "If C hadn't occurred, would E not have occurred?" - P(E=0 | do(C=0))
- **Sufficiency**: "If C had occurred, would E have occurred?" - P(E=1 | do(C=1))

Icard's causal strength measure κ combines these:

```
κ_P(C,E) = P(C=0) · Necessity + P(C=1) · Sufficiency
```

This differs from CESM in that normality effects emerge from the base rate weighting (P(C=0) vs P(C=1)) rather than a separate stability parameter.

## Implementation Details

### Scripts Created

| Script | Purpose |
|--------|---------|
| `Scripts/nsUtils.R` | Core N/S computation functions |
| `Scripts/03getNSPreds.R` | Generate predictions for all 36 trial combinations |
| `Scripts/06processNSPreds.R` | Process predictions and merge with participant data |
| `Scripts/07fitNSModels.R` | Fit models and calculate likelihoods |

### Model Variants Tested

We tested 10 N/S model variants using different combination methods:

1. **Kappa (κ)** - Icard's original formulation: P(C=0)·N + P(C=1)·S
2. **Product** - Simple multiplication: N × S
3. **Average** - Simple mean: (N + S) / 2
4. **Necessity only** - Just the necessity component
5. **Sufficiency only** - Just the sufficiency component

Each was tested with and without the "Actual causation" constraint (where a cause must match the effect value).

### Data Structure

- **36 trials**: 3 parameter groups × 12 trial types (5 conjunctive, 7 disjunctive)
- **8 response options per trial**: A=0, A=1, B=0, B=1, Au=0, Au=1, Bu=0, Bu=1
- **288 total predictions** merged with 2,580 participant responses

## Results

### Aggregate Model Comparison (BIC)

| Rank | Model | BIC | Log-likelihood | τ | ε |
|------|-------|-----|----------------|---|---|
| 1 | ns_product_full | 8691.33 | -4337.81 | 0.314 | 0.382 |
| 2 | ns_average_full | 8691.71 | -4338.00 | 0.163 | 0.382 |
| 3 | ns_average_noAct | 8692.80 | -4338.54 | 0.166 | 0.352 |
| 4 | ns_necessity_full | 8697.46 | -4340.87 | 0.319 | 0.381 |
| 5 | ns_product_noAct | 8718.00 | -4351.14 | 0.357 | 0.209 |
| 6 | ns_necessity_noAct | 8720.30 | -4352.29 | 0.358 | 0.208 |
| 7 | ns_kappa_full | 8899.05 | -4441.67 | 0.549 | 0.379 |
| 8 | ns_kappa_noAct | 8983.22 | -4483.75 | 0.425 | 0.213 |
| 9 | ns_sufficiency_full | 9027.47 | -4505.88 | 0.456 | 0.379 |
| 10 | ns_sufficiency_noAct | 9462.50 | -4723.39 | 0.630 | 0.212 |
| 11 | baseline (uniform) | 10729.92 | -5364.96 | - | - |

### Key Findings

1. **Best combination method**: The simple **product (N × S)** marginally outperforms averaging, and substantially outperforms Icard's original kappa formulation.

2. **Actual causation helps**: Models with the Actual constraint ("_full") generally outperform those without ("_noAct"), though the difference is small for product/average.

3. **Kappa underperforms**: Surprisingly, Icard's original κ formulation (BIC=8899) performs ~200 BIC points worse than N×S product (BIC=8691). This suggests the base-rate weighting in κ may not match human judgments well.

4. **Necessity > Sufficiency**: Necessity-only models substantially outperform sufficiency-only models, suggesting humans weight "whether the cause was necessary" more heavily than "whether it would have been sufficient".

5. **All models beat baseline**: Every N/S variant substantially outperforms random responding (ΔBIC > 1200).

### Individual Participant Analysis

Distribution of best-fitting model across 215 participants:

| Model | N participants |
|-------|----------------|
| ns_average_full | 40 |
| ns_necessity_noAct | 30 |
| ns_product_noAct | 25 |
| ns_sufficiency_full | 21 |
| ns_kappa_full | 20 |
| ns_kappa_noAct | 20 |
| ns_necessity_full | 18 |
| ns_average_noAct | 18 |
| ns_product_full | 14 |
| ns_sufficiency_noAct | 9 |

Notable: There is considerable individual variation. While ns_average_full wins most often, all model variants are best for some participants.

## Comparison with CESM

To fully evaluate N/S against CESM, the next step would be to compare BIC values directly. Key differences:

| Aspect | CESM | N/S Model |
|--------|------|-----------|
| Counterfactual basis | Effect size in counterfactual sampling | Necessity and sufficiency probabilities |
| Normality | Stability parameter (s=0.7) | Base rate weighting in κ |
| Parameters | τ, ε, κ (kindness) | τ, ε |
| Computational kindness | Explicit ig (information gain) term | Not included |

## Files Generated

- `Data/modelData/nsPreds.rda` - Raw N/S predictions (288 rows)
- `Data/modelData/nsModelAndData.rda` - Processed predictions with participant counts
- `Data/modelData/nsFits.rda` - All fitting results
- `Data/modelData/nsAggregateFits.csv` - Aggregate fit summary
- `Data/modelData/nsBICSummary.csv` - Individual participant BIC summary

## References

Icard, T. F., Kominsky, J. F., & Knobe, J. (2017). Normality and actual causal strength. *Cognition*, 161, 80-93.

Quillien, T., & Lucas, C. G. (2023). Counterfactuals and the logic of causal selection. *Psychological Review*.
