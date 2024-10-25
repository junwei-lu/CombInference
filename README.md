# CombInference Package

## Overview

**CombInference** is an R package designed to implement a novel framework for combinatorial inference in graphical models, with False Discovery Rate (FDR) control across multiple scales. The package applies methods introduced in the paper: 

**"ARCH: Large-scale knowledge graph via aggregated narrative codified health records analysis"**.

The core functionality of this package includes:
1. Graphical feature selection.
2. Persistent homology analysis with uniform FDR (uFDR) control.
3. Integration of the KHAN algorithm for homological feature selection.

## Key Features

- **Graphical Feature Selection**: Implementing selection of graph features like hubs, cycles, and edges, based on single-edge p-values with FDR control.
- **Persistent Homology**: Efficient analysis of persistent homological features in graphs, across continuous filtration levels.
- **KHAN Algorithm**: Adaptive selection of homological features using a discrete Gram-Schmidt procedure to ensure statistical and computational efficiency.

## Installation

To install the CombInference package, run:

```bash
install.packages("CombInference")
```

# Usage 

```r
# Example call to the moonshoot function:
result <- moonshoot(
  X = your_data_matrix,
  SigmaHat = your_covariance_matrix,
  ThetaHat = your_precision_matrix,
  q = 0.05,
  numB = 1000,
  V0 = your_vertex_matrix,
  possibleSet = your_possible_set_list,
  prescreen = TRUE
)
```
