# Choice-Set Size Benchmark Test

This repository contains the code and supporting materials for the paper:

**“Preference Stability in Environmental Valuation: Evidence from a Simulation-Based Benchmark Test”**

The study examines how the number of alternatives presented in a discrete choice experiment affects observed choices, estimated preferences, cost sensitivity and willingness-to-pay estimates. It uses data from a split-sample environmental valuation survey concerning improvements to the Baltic Sea.

## Study overview

Respondents were randomly assigned to one of five treatments:

- `2Alt`: status quo plus one policy alternative
- `3Alt`: status quo plus two policy alternatives
- `4Alt`: status quo plus three policy alternatives
- `5Alt`: status quo plus four policy alternatives
- `6Alt`: status quo plus five policy alternatives

The analysis estimates multinomial logit, random-parameter logit and latent-class models separately by treatment.

The repository also implements a simulation-based benchmark test. Preferences estimated from the binary `2Alt` treatment are applied to the realised design matrices of the other treatments. The resulting simulated choices are compared with the observed choices to assess whether stable benchmark preferences can reproduce the attribute-level choice frequencies in larger choice sets.
