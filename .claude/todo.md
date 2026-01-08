# Implementation Plan: Necessity/Sufficiency Model

## Overview
Implement the Icard et al. (2017) sampling-based necessity/sufficiency model to generate causal strength predictions for the collider experiment, then fit to participant data.

## Background
- **Source**: Icard, Kominsky & Knobe (2017) "Normality and actual causal strength", Cognition
- **Approach**: Probabilistic sampling from graphical causal models
- **Key concepts**: Necessity (counterfactual dependence), Sufficiency (robust causation across background conditions)

## Task Breakdown

### Phase 1: Model Implementation
- [ ] **1.1** Create `Scripts/nsUtils.R` - utility functions for necessity/sufficiency model
  - Implement necessity score calculation via counterfactual sampling
  - Implement sufficiency score calculation via counterfactual sampling
  - Follow structure similar to `cesmUtils.R` for consistency

- [ ] **1.2** Define necessity and sufficiency formally (from Icard 2017, p.15):
  - **Necessity Strength**: P^ν_{X=0}(Y=0) - probability effect wouldn't occur if cause didn't
  - **Sufficiency Strength**: P^σ_{X=1}(Y=1) - probability effect would occur if cause did
  - **Combined measure κ**: `κ_P(X,Y) = P(X=0)·NecessityStrength + P(X=1)·SufficiencyStrength`
  - **NO stability parameter needed** - normality effects come from sampling X proportional to P(X)

### Phase 2: Generate Predictions
- [ ] **2.1** Create `Scripts/03getNSPreds.R` - script to generate N/S predictions
  - Loop over 3 parameter groups × 2 structures (conj/disj) × trial types
  - Compute scores for all 8 response options per trial
  - Output: 36 combinations × 8 options = 288 predictions per model variant

- [ ] **2.2** Implement multiple combination methods:
  - Product: N × S
  - Average: (N + S) / 2
  - Weighted: w₁N + w₂S (weights to be optimized)
  - Necessity only
  - Sufficiency only

### Phase 3: Data Processing
- [ ] **3.1** Create `Scripts/04processNSPreds.R`
  - Format predictions to match structure of `modelproc.rda`
  - Include node3 labels (A=0, A=1, B=0, B=1, Au=0, Au=1, Bu=0, Bu=1)
  - Merge with existing data structures

- [ ] **3.2** Create `Scripts/05getNSLesions.R`
  - Apply same lesion logic as CESM (Actual, Inference, etc.)
  - Combine with participant data

### Phase 4: Model Fitting
- [ ] **4.1** Extend `optimUtils.R` or create `optimUtilsNS.R`
  - Likelihood function for N/S models
  - Optimization routine (epsilon, tau parameters)
  - For weighted combination: add weight parameters

- [ ] **4.2** Create `Scripts/06optimiseNS.R`
  - Fit all N/S model variants to participant data
  - Compare BIC/log-likelihood across models
  - Output fit statistics table

### Phase 5: Comparison & Reporting
- [ ] **5.1** Create comparison table: CESM vs N/S models
  - Log-likelihood, BIC, AIC
  - Parameter estimates
  - Best-fitting model identification

- [ ] **5.2** Generate diagnostic plots
  - Predicted vs observed proportions
  - Model comparison visualizations

## File Structure
```
Scripts/
├── nsUtils.R           # N/S model utility functions (NEW)
├── 03getNSPreds.R      # Generate N/S predictions (NEW)
├── 04processNSPreds.R  # Process N/S predictions (NEW)
├── 05getNSLesions.R    # Apply lesions to N/S model (NEW)
├── 06optimiseNS.R      # Fit N/S models (NEW)
└── optimUtilsNS.R      # Optimization utilities for N/S (NEW)

Data/modelData/
├── nsPreds.rda         # Raw N/S predictions (NEW)
├── nsModelproc.rda     # Processed N/S predictions (NEW)
└── nsModelAndData.rda  # N/S with participant data (NEW)
```

## Key Decisions Made
1. Using Icard et al. 2017 sampling-based formulation
2. Testing multiple combination methods (product, average, weighted, N-only, S-only)
3. Computing scores for all 8 response options
4. Following existing codebase conventions (R, tidyverse, here package)
5. **NO stability parameter** - Icard's model differs from CESM:
   - CESM uses stability s=0.7 to keep counterfactuals "close" to actual world
   - Icard's κ samples X from prior P(X), then deterministically computes effect under intervention
   - Normality effects emerge from sampling propensity, not from stability

## Dependencies
- Existing: tidyverse, here, renv
- Data: params.rdata, Data.Rdata

## Status
- [x] Phase 0: Requirements clarification
- [ ] Phase 1: Model implementation
- [ ] Phase 2: Prediction generation
- [ ] Phase 3: Data processing
- [ ] Phase 4: Model fitting
- [ ] Phase 5: Comparison & reporting
