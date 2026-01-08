# 36 World/Parameter Combinations

## Overview
36 combinations = 3 parameter groups × 12 trial types (5 conjunctive + 7 disjunctive)

---

## Parameter Groups (pgroup 1-3)

Each parameter group specifies prior probabilities for 4 variables:
- **pA**: P(A=1) - probability observed cause A occurs
- **peA**: P(Au=1) - probability unobserved/exogenous cause Au occurs
- **pB**: P(B=1) - probability observed cause B occurs
- **peB**: P(Bu=1) - probability unobserved/exogenous cause Bu occurs

### Parameter Group 1
| Variable | P(=0) | P(=1) |
|----------|-------|-------|
| pA       | 0.9   | 0.1   |
| peA      | 0.5   | 0.5   |
| pB       | 0.2   | 0.8   |
| peB      | 0.5   | 0.5   |

**Interpretation**: A is rare (10%), B is common (80%), exogenous variables are 50/50

### Parameter Group 2
| Variable | P(=0) | P(=1) |
|----------|-------|-------|
| pA       | 0.5   | 0.5   |
| peA      | 0.9   | 0.1   |
| pB       | 0.5   | 0.5   |
| peB      | 0.2   | 0.8   |

**Interpretation**: A and B are 50/50, Au is rare (10%), Bu is common (80%)

### Parameter Group 3
| Variable | P(=0) | P(=1) |
|----------|-------|-------|
| pA       | 0.9   | 0.1   |
| peA      | 0.3   | 0.7   |
| pB       | 0.2   | 0.8   |
| peB      | 0.5   | 0.5   |

**Interpretation**: A rare, B common, Au common (70%), Bu 50/50

---

## Trial Types (12 total)

### Conjunctive Structure (c1-c5)
**Rule**: E = (A AND Au) AND (B AND Bu)
Effect occurs only if BOTH pathways are active (A×Au AND B×Bu)

| Trial | A | B | E | Description |
|-------|---|---|---|-------------|
| c1    | 0 | 0 | 0 | Neither cause, no effect |
| c2    | 0 | 1 | 0 | Only B, no effect |
| c3    | 1 | 0 | 0 | Only A, no effect |
| c4    | 1 | 1 | 0 | Both causes, but no effect (implies Au=0 or Bu=0) |
| c5    | 1 | 1 | 1 | Both causes, effect occurred (implies Au=1 AND Bu=1) |

### Disjunctive Structure (d1-d7)
**Rule**: E = (A AND Au) OR (B AND Bu)
Effect occurs if EITHER pathway is active

| Trial | A | B | E | Description |
|-------|---|---|---|-------------|
| d1    | 0 | 0 | 0 | Neither cause, no effect |
| d2    | 0 | 1 | 0 | Only B present, no effect (implies Bu=0) |
| d3    | 0 | 1 | 1 | Only B present, effect (implies Bu=1) |
| d4    | 1 | 0 | 0 | Only A present, no effect (implies Au=0) |
| d5    | 1 | 0 | 1 | Only A present, effect (implies Au=1) |
| d6    | 1 | 1 | 0 | Both present, no effect (implies Au=0 AND Bu=0) |
| d7    | 1 | 1 | 1 | Both present, effect (at least one pathway active) |

---

## Response Options (8 per trial)

For each trial, participants can select one of 8 causal explanations:
1. **A=0** - "A not occurring caused E"
2. **A=1** - "A occurring caused E"
3. **B=0** - "B not occurring caused E"
4. **B=1** - "B occurring caused E"
5. **Au=0** - "Au not occurring caused E"
6. **Au=1** - "Au occurring caused E"
7. **Bu=0** - "Bu not occurring caused E"
8. **Bu=1** - "Bu occurring caused E"

---

## Full Combination Matrix

| pgroup | trialtype | structure   | A | B | E | trial_id |
|--------|-----------|-------------|---|---|---|----------|
| 1      | c1        | conjunctive | 0 | 0 | 0 | 1_c1     |
| 1      | c2        | conjunctive | 0 | 1 | 0 | 1_c2     |
| 1      | c3        | conjunctive | 1 | 0 | 0 | 1_c3     |
| 1      | c4        | conjunctive | 1 | 1 | 0 | 1_c4     |
| 1      | c5        | conjunctive | 1 | 1 | 1 | 1_c5     |
| 1      | d1        | disjunctive | 0 | 0 | 0 | 1_d1     |
| 1      | d2        | disjunctive | 0 | 1 | 0 | 1_d2     |
| 1      | d3        | disjunctive | 0 | 1 | 1 | 1_d3     |
| 1      | d4        | disjunctive | 1 | 0 | 0 | 1_d4     |
| 1      | d5        | disjunctive | 1 | 0 | 1 | 1_d5     |
| 1      | d6        | disjunctive | 1 | 1 | 0 | 1_d6     |
| 1      | d7        | disjunctive | 1 | 1 | 1 | 1_d7     |
| 2      | c1        | conjunctive | 0 | 0 | 0 | 2_c1     |
| 2      | c2        | conjunctive | 0 | 1 | 0 | 2_c2     |
| 2      | c3        | conjunctive | 1 | 0 | 0 | 2_c3     |
| 2      | c4        | conjunctive | 1 | 1 | 0 | 2_c4     |
| 2      | c5        | conjunctive | 1 | 1 | 1 | 2_c5     |
| 2      | d1        | disjunctive | 0 | 0 | 0 | 2_d1     |
| 2      | d2        | disjunctive | 0 | 1 | 0 | 2_d2     |
| 2      | d3        | disjunctive | 0 | 1 | 1 | 2_d3     |
| 2      | d4        | disjunctive | 1 | 0 | 0 | 2_d4     |
| 2      | d5        | disjunctive | 1 | 0 | 1 | 2_d5     |
| 2      | d6        | disjunctive | 1 | 1 | 0 | 2_d6     |
| 2      | d7        | disjunctive | 1 | 1 | 1 | 2_d7     |
| 3      | c1        | conjunctive | 0 | 0 | 0 | 3_c1     |
| 3      | c2        | conjunctive | 0 | 1 | 0 | 3_c2     |
| 3      | c3        | conjunctive | 1 | 0 | 0 | 3_c3     |
| 3      | c4        | conjunctive | 1 | 1 | 0 | 3_c4     |
| 3      | c5        | conjunctive | 1 | 1 | 1 | 3_c5     |
| 3      | d1        | disjunctive | 0 | 0 | 0 | 3_d1     |
| 3      | d2        | disjunctive | 0 | 1 | 0 | 3_d2     |
| 3      | d3        | disjunctive | 0 | 1 | 1 | 3_d3     |
| 3      | d4        | disjunctive | 1 | 0 | 0 | 3_d4     |
| 3      | d5        | disjunctive | 1 | 0 | 1 | 3_d5     |
| 3      | d6        | disjunctive | 1 | 1 | 0 | 3_d6     |
| 3      | d7        | disjunctive | 1 | 1 | 1 | 3_d7     |

**Total**: 36 combinations × 8 response options = **288 model predictions**

---

## Causal Structure Diagram

```
     A ----→ Au
              \
               \
                ↘
                 E
                ↗
               /
              /
     B ----→ Bu

Conjunctive: E = (A ∧ Au) ∧ (B ∧ Bu)
Disjunctive: E = (A ∧ Au) ∨ (B ∧ Bu)
```

Each observed variable (A, B) gates its corresponding unobserved variable (Au, Bu).
- If A=0, then Au cannot contribute to E regardless of Au's value
- If A=1, then Au's value determines whether that pathway is active

---

## Notes for Implementation

1. **Posterior inference**: In some trials, the unobserved variables can be inferred:
   - c5 (E=1 in conjunctive): Au=1 AND Bu=1 (certain)
   - d2 (B=1, E=0 in disjunctive): Bu=0 (certain)
   - d3 (B=1, E=1, A=0 in disjunctive): Bu=1 (certain)
   - d4 (A=1, E=0 in disjunctive): Au=0 (certain)
   - d5 (A=1, E=1, B=0 in disjunctive): Au=1 (certain)
   - d6 (A=1, B=1, E=0 in disjunctive): Au=0 AND Bu=0 (certain)

2. **Actual causation**: A cause is "actual" if its value matches the effect's value AND it's on an active pathway

3. **Invalid responses**: Some response options are logically impossible (e.g., selecting "A=1 caused E" when A=0 in that trial)
