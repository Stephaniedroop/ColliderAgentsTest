# Icard, Kominsky & Knobe (2017) - Summary

## Citation
Icard, T.F., Kominsky, J.F., & Knobe, J. (2017). Normality and actual causal strength. *Cognition*, 161, 80-93.

## Core Idea
People's judgments of actual causation are influenced by how **normal** they regard events. The paper proposes a measure of **actual causal strength** (κ) based on probabilistic sampling that captures normality effects.

## Key Definitions

### Actual Necessity (p. 12)
> "If C had not occurred, E also would not have occurred."

### Robust Sufficiency (p. 12)
> "Given that C occurred, E would have occurred even if background conditions had been slightly different."

## The Causal Strength Measure κ (Equation 1, p. 15)

The causal strength of X=1 on Y=1 is:

```
κ_P(X,Y) = P(X=0) · P^ν_{X=0}(Y=0) + P(X=1) · P^σ_{X=1}(Y=1)
```

Where:
- `P^ν_{X=0}(Y=0)` = **Necessity strength**: P(Y≠y | do(X≠x)...) - probability effect wouldn't occur if cause didn't
- `P^σ_{X=1}(Y=1)` = **Sufficiency strength**: P(Y=y | do(X=x)...) - probability effect would occur if cause did

**In words**: Causal strength is the weighted sum of necessity and sufficiency, weighted by P(X=0) and P(X=1) respectively.

## Sampling Algorithm (p. 15)

```
Initialize N = 0, and for k ≤ K:
    Sample a value X^(k) from P.
    If X^(k) = 0, sample Y^(k) from P^ν_{X=0}. Let N = N + (1 - Y^(k)).
    If X^(k) = 1, sample Y^(k) from P^σ_{X=1}. Let N = N + Y^(k).
Return N/K.
```

This converges to κ as K → ∞.

## Necessity/Sufficiency in Collider Structures (Table 2, p. 14)

For the unshielded collider when C = A = E = 1:

|                              | Disjunctive | Conjunctive |
|------------------------------|-------------|-------------|
| Necessity Strength P^ν_{C=0}(E=0) | 0           | 1           |
| Sufficiency Strength P^σ_{C=1}(E=1) | 1           | P(A)        |

## Key Predictions (Table 5, p. 23)

How κ_P(C,E) changes as a function of P(C) or P(A):

|              | Disjunctive           | Conjunctive          |
|--------------|-----------------------|----------------------|
| Change in P(C) | + (Abnormal Deflation) | - (Abnormal Inflation) |
| Change in P(A) | * (No Supersession)    | + (Supersession)      |

## Four Effects Explained

1. **Abnormal Inflation** (Conjunctive): Abnormal causes rated as MORE causal
2. **Supersession** (Conjunctive): Normality of other factor affects causal ratings
3. **No Supersession with Disjunction**: Supersession doesn't occur in disjunctive cases
4. **Abnormal Deflation** (Disjunctive, novel prediction): Abnormal causes rated as LESS causal

## Formulas for Specific Cases (pp. 15-16)

### Disjunctive case (C = A = E = 1):
```
κ_P(C,E) = P(~C) · P(~E | do(~C), A) + P(C) · P(E | do(C))
         = P(~C) · 0 + P(C) · 1
         = P(C)
```
**No supersession**: κ does not depend on P(A) at all.

### Conjunctive case (C = A = E = 1):
```
κ_P(C,E) = P(C) · P(A) - P(C) + 1
```
- **Supersession**: Monotonically increasing in P(A)
- **Abnormal Inflation**: As P(C) increases, second term decreases more than first increases (when P(A) < 1)

## Implications for Implementation

### Does N/S need a stability parameter?

**NO** - unlike CESM which uses stability s=0.7 to keep counterfactuals "close" to actuality, the Icard model:

1. Samples X from prior P (normality built into sampling probabilities)
2. Then deterministically computes Y under intervention do(X=x)
3. Normality effects come from **sampling propensity** being proportional to P(X)

The normality effects emerge from how often each X value is sampled, not from keeping counterfactuals similar to actuality.

### Key difference from CESM:
- **CESM**: Uses stability parameter, computes correlation between C and E across sampled worlds
- **Icard κ**: No stability, tests necessity OR sufficiency depending on sampled X value, counts successes

## Relevant for Your Project

Your collider structure with A, Au, B, Bu, E maps to their "unshielded collider" with additional latent variables. The model predicts:

- In conjunctive structures: Abnormal inflation (rarer causes judged more causal)
- In disjunctive structures: Abnormal deflation (rarer causes judged LESS causal)

Your `abnormalInflation.R` script tests for this - the paper notes deflation is a novel prediction that distinguishes this model from alternatives.
