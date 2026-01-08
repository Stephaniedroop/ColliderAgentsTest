# Next Steps for N/S Model Analysis

## Immediate Priorities

### 1. Direct CESM Comparison

Compare the best N/S model (ns_product_full, BIC=8691) against the best CESM model to determine which framework better explains human causal judgments.

**Action**: Load CESM fitting results and create side-by-side comparison table.

### 2. Generate Comparison Plots

Create visualisations showing:
- Predicted vs observed response distributions by trial type
- Model residuals across conditions
- Parameter recovery plots

### 3. Statistical Testing

- Likelihood ratio tests between nested models
- Vuong test for non-nested model comparison (N/S vs CESM)
- Bootstrap confidence intervals on BIC differences

## Medium-term Extensions

### 4. Hybrid Models

Test whether combining N/S with CESM components improves fit:
- N/S + computational kindness (information gain)
- CESM + necessity weighting
- Mixture models allowing both mechanisms

### 5. Individual Differences Analysis

- Cluster participants by best-fitting model
- Correlate model preference with demographics
- Test for stable individual strategies

### 6. Lesion Analysis for N/S

Systematically test which N/S components are necessary:
- Full model vs no-Actual
- Necessity only vs Sufficiency only
- Base-rate weighting (kappa) vs simple product

## Long-term Goals

### 7. Theoretical Integration

Address why N×S product outperforms Icard's original κ:
- Is base-rate weighting incorrect?
- Do humans use multiplicative rather than additive combination?
- Role of normalisation in counterfactual reasoning

### 8. Manuscript Preparation

- Write up N/S comparison for thesis chapter
- Prepare figures for publication
- Draft discussion of theoretical implications

## Questions to Address

1. **Why does product beat kappa?** The original Icard formulation weights by base rates, but simple multiplication works better. This suggests humans may not incorporate base rates in the way Icard proposed.

2. **Why does necessity dominate sufficiency?** Necessity-only models substantially outperform sufficiency-only. Is this a general feature of causal cognition or specific to our stimuli?

3. **What explains individual variation?** Some participants are best fit by kappa, others by product, others by sufficiency. What drives these differences?

4. **How does N/S relate to CESM's counterfactual sampling?** Both invoke counterfactuals but in different ways. Can we derive one from the other?

## Technical Debt

- [ ] Add error handling for edge cases in posterior computation
- [ ] Optimise fitting for large-scale individual analysis
- [ ] Add unit tests for core N/S functions
- [ ] Document parameter interpretation more thoroughly
