# CombInference Package

## Overview

**CombInference** is an R package designed to implement a novel framework for combinatorial inference in graphical models, with False Discovery Rate (FDR) control across multiple scales. The package applies methods introduced in the paper: 

**Lu Zhang. Junwei Lu. "StarTrek: Combinatorial variable selection with false discovery rate control." Ann. Statist. 52 (1) 78 - 102, February 2024. https://doi.org/10.1214/23-AOS2296**.

The core functionality of this package includes:
1. Graphical feature selection.
2. Persistent homology analysis with uniform FDR (uFDR) control.
3. Integration of the KHAN algorithm for homological feature selection.

![overview](https://github.com/junwei-lu/CombInference/tree/main/img/combinference.png)

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

---

## Citation

After using the `CombInference` R package, please cite both of the following:



		@article{zhang2024startrek,
			title={StarTrek: Combinatorial variable selection with false discovery rate control},
			author={Zhang, Lu and Lu, Junwei},
			journal={The Annals of Statistics},
			volume={52},
			number={1},
			pages={78--102},
			year={2024},
			publisher={Institute of Mathematical Statistics}
		}

---
