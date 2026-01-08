# N/S Model Implementation Notes

Technical documentation for the Necessity-Sufficiency model implementation.

## File Structure

```
Scripts/
├── nsUtils.R           # Core utility functions
├── 03getNSPreds.R      # Generate raw predictions
├── 06processNSPreds.R  # Process and merge with data
└── 07fitNSModels.R     # Model fitting

Data/modelData/
├── nsPreds.rda         # Raw predictions (288 rows)
├── nsPreds.csv         # CSV version
├── nsModelAndData.rda  # Merged with participant data
├── nsModelAndData.csv  # CSV version
├── nsFits.rda          # All fitting results
├── nsAggregateFits.csv # Aggregate BIC table
└── nsBICSummary.csv    # Per-participant summary
```

## Core Functions (nsUtils.R)

### `compute_necessity(cause, cause_value, A, Au, B, Bu, E, structure, params)`

Computes P(E=0 | do(C=0)) - the probability the effect wouldn't have occurred if the cause hadn't.

For conjunctive structure: E = (A ∧ Au) ∧ (B ∧ Bu)
For disjunctive structure: E = (A ∧ Au) ∨ (B ∧ Bu)

### `compute_sufficiency(cause, cause_value, A, Au, B, Bu, E, structure, params)`

Computes P(E=1 | do(C=1)) - the probability the effect would have occurred if the cause had.

### `compute_kappa(cause, cause_value, ...)`

Implements Icard's κ measure:
```
κ = P(C=0) × Necessity + P(C=1) × Sufficiency
```

### `compute_posterior(A, B, E, structure, params)`

Computes posterior distribution over unobserved variables (Au, Bu) given the observed world state (A, B, E).

### `get_ns_predictions(params, structure, trial_info)`

Main function that generates predictions for a single trial. Returns scores for all 8 response options.

## Trial Combinations

The 36 trials arise from:

- **3 parameter groups**: Different base rates for P(A), P(Au), P(B), P(Bu)
- **12 trial types**:
  - Conjunctive (c1-c5): E = (A ∧ Au) ∧ (B ∧ Bu)
  - Disjunctive (d1-d7): E = (A ∧ Au) ∨ (B ∧ Bu)

### Parameter Groups

| Group | P(A) | P(Au) | P(B) | P(Bu) |
|-------|------|-------|------|-------|
| 1 | 0.9 | 0.5 | 0.2 | 0.5 |
| 2 | 0.5 | 0.1 | 0.5 | 0.8 |
| 3 | 0.8 | 0.5 | 0.1 | 0.7 |

### Trial Types

| Trial | A | B | E | Structure |
|-------|---|---|---|-----------|
| c1 | 0 | 0 | 0 | conjunctive |
| c2 | 0 | 1 | 0 | conjunctive |
| c3 | 1 | 0 | 0 | conjunctive |
| c4 | 1 | 1 | 0 | conjunctive |
| c5 | 1 | 1 | 1 | conjunctive |
| d1 | 0 | 0 | 0 | disjunctive |
| d2 | 0 | 1 | 0 | disjunctive |
| d3 | 0 | 1 | 1 | disjunctive |
| d4 | 1 | 0 | 0 | disjunctive |
| d5 | 1 | 0 | 1 | disjunctive |
| d6 | 1 | 1 | 0 | disjunctive |
| d7 | 1 | 1 | 1 | disjunctive |

## Model Fitting

### Parameters

- **τ (tau)**: Temperature parameter for softmax. Lower = more deterministic.
- **ε (epsilon)**: Noise/lapse rate. Probability of random responding.

### Likelihood Function

For each trial, model predictions are converted to choice probabilities via:

```r
# Softmax with temperature
logits <- raw_scores / tau
probs <- exp(logits) / sum(exp(logits))

# Epsilon mixing
final_probs <- epsilon * (1/8) + (1 - epsilon) * probs
```

Log-likelihood is computed as:
```r
logl <- sum(log(final_probs) * observed_counts)
```

### BIC Calculation

```r
BIC <- -2 * logl + n_params * log(n_observations)
```

Where n_params = 2 (τ and ε) and n_observations = total participant responses.

## Handling Impossible Responses

Some responses contradict the observed world state (e.g., responding "A=1" when A=0 was observed). These are handled by:

1. Setting model scores to -Inf
2. Softmax naturally assigns near-zero probability
3. The epsilon term still allows some probability mass (capturing response errors)

## Actual Causation Constraint

The "Actual" flag marks whether a cause value matches the effect:
- If E=1, then C=1 is "actual"
- If E=0, then C=0 is "actual"

For "_full" models, non-actual causes receive -Inf scores.
For "_noAct" models, this constraint is removed.

## Running the Analysis

```r
# Generate predictions
source('Scripts/03getNSPreds.R')

# Process and merge
source('Scripts/06processNSPreds.R')

# Fit models
source('Scripts/07fitNSModels.R')
```

Or use R --vanilla to bypass renv issues:
```bash
R --vanilla --no-save -e "source('Scripts/07fitNSModels.R')"
```

## Known Issues

1. **renv/ellmer conflict**: The .Rprofile loads ellmer which may not be installed. Use `R --vanilla` to bypass.

2. **Posterior computation for deducible cases**: When Au or Bu can be logically deduced from (A, B, E), the posterior is deterministic (0 or 1).

## Future Extensions

1. **Add computational kindness**: Include information gain (ig) term like CESM
2. **Compare with CESM directly**: Generate comparison plots and tables
3. **Test hybrid models**: Combine N/S with CESM components
4. **Per-participant parameter estimation**: Currently using fixed parameters across all participants for aggregate fit
