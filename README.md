# sound-symbolism-cogsci2024
Code associated with the paper "Exploring the evolutionary dynamics of sound symbolism".

This repository is intended to be cloned inside another repository (https://github.com/chundrac/linguisticCharacterMatrices).

Models were run on the University of Zurich's Science Cluster using the Slurm workload management system and the Rstan anaconda environment (https://anaconda.org/conda-forge/r-rstan).

The following shell script runs all data processing and model fitting scripts:

```
./run_individual_models.sh
./run_pairwise_models.sh
```
