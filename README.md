# The Social Costs of Hydrofluorocarbons and the Benefits from Their Expedited Phasedown

This repo contains data and code for Tan, Rennels, and Parthum (2023), "The Social Costs of Hydrofluorocarbons and the Benefits from Their Expedited Phasedown". 

> **Abstract:** 
> Hydrofluorocarbons are a potent greenhouse gas, yet there remains a lack of quantitative estimates of their social cost. The present study addresses this gap by directly calculating the social cost of hydrofluorocarbons (SC-HFCs) using perturbations of exogenous inputs to integrated assessment models. We first develop a set of direct estimates of the SC-HFCs using methods currently adopted by the United States Government, and then derive updated estimates that incorporate recent advances in climate science and economics. We compare our estimates with commonly used social cost approximations based on global warming potentials to show that the latter is a poor proxy for direct calculation of hydrofluorocarbon emissions impacts using IAMs. Applying our SC-HFCs to the Kigali Amendment, a global agreement to phase down HFCs, we estimate that it provides $37 trillion (2020USD) in climate benefits over its lifetime. Expediting the phasedown could increase the estimated climate benefits to $41 trillion (2020USD).

## Requirements
1. *Julia* is free and available for download [here](https://julialang.org/). Estimation of the SC-HFCs for the MimiGIVE and MimiIWG damage modules was performed on Julia 1.8. While newer versions of *Julia* are compatible with all the code (e.g. 1.9), the random number generators were updated and results might not be identical due to random differences in the random parameters underlying the Monte Carlo runs of MimiGIVE and MimiIWG. Install *Julia* and ensure that it can be invoked (ran) from where the replication repository is to be cloned ("in your path"). 

2. *R* is free and available for download [here](https://www.r-project.org/). The *RStudio* integrated development environment is useful for replication, it is free and available for download [here](https://www.rstudio.com/products/rstudio/). *R* is used to collect the estimates from each damage module and create a table of annual SC-HFCs. 

3. Optional: *Github* is free and available for download [here](https://github.com/git-guides/install-git). *Github* is used to house this repository and by installing and using it to clone the repository one will simplify the replication procedure. However, a user could also simply download a zipped file version of this repository, unzip in the desired location and follow the replication procedures outlined below.

4. Optional: *Visual Studio Code* is free and available for download [here](https://code.visualstudio.com/). However, all model replication can be ran directly through a terminal. 

# Estimating the SC-HFCs
Estimation of the two models and their SC-HFCs is outlined below. For convenience, this repository already includes the completed model runs (in each models `output` subdirectory), and the full distributions of their estimates (in each model's `output\scghgs` subdirectory).

We also include estimation of a paired MimiIWG-FAIRv162 model for sensitivity analysis. This is a requested sensitivity analysis -- not a main result -- see paper for modeling choices and assumptions.

## Getting Started
Begin by cloning this repository. This can be done by clicking on the green "code" button in this repository and following those instructions, or by navigating in the terminal via the command line to the desired location of the cloned repository and then typing: 

```
git clone https://github.com/bryanparthum/schfc-paper.git
```

Alternatively, you can make a `fork` of this repository. This allows for development on the `fork` while preserving its relationship with this repository.

## The Greenhouse Gas Impact Value Estimator (MimiGIVE)
Replicating the estimates from MimiGIVE can be done by following the steps outlined here and assumes that the user has downloaded and installed *Julia*. Begin by opening a terminal and navigating via the command line to the location of the cloned repository (as outlined [above](#getting-started)). Then, navigate to the [code](MimiGIVE/code) subdirectory by typing:

```
cd MimiGIVE
```

From within the `MimiGIVE` directory, navigate to the desired script. This can be done by typing:

```
cd code 
```

The directory: `schfc-paper\MimiGIVE\code` should be the current location in the terminal. This directory includes the replication script: `1_estimate_scghgs.jl`. 

### Running the Model
The replication script `1_estimate_scghgs.jl` contains several important parameters that need to be set prior to the user executing the below commands. These parameters depend on how many processors the user has available, the number of gases and emissions years they would like to run, and the number of Monte Carlo simulations. These parameters are in lines 19 through 52. To prevent triggering too many processors from being automatically called on a user's machine, we set the number of processors equal to 1 for the default.

Once the parameters have been established by the user, on the command line, type: 

```
julia 1_estimate_give_schfc.jl
```

**Note:** Estimation time for MimiGIVE varies by machine. Using 10,000 Monte Carlo draws for each `gas + emissions year` pair (one pair per processor) takes approximate 10 processor hours to run. With 12 gases and 9 emissions years, the replication of MimiGIVE will take approximately 1,080 processor hours. Users should plan to allocate 5GB of memory per processor. 

## MimiIWG 
Replicating the the SC-HFCs from `MimiIWG` can be done in the same way as MimiGIVE as outlined above in [Running the Scripts](#running-the-scripts). Instead of navigating to the `GIVE` directory, navigate to the `MimiIWG` directory (in the terminal, the same was as instructed for GIVE) and execute with the replication script: `1_estimate_scghgs.jl`. 

**Note:** Estimation time for MimiIWG varies by the integrated assessment model (IAM). Using 10,000 Monte Carlo draws for each `gas + IAM` pair (one pair per processor) takes approximately 4 hours for DICE, 8 hours for FUND, and 10 hours for PAGE (varies by machine).  In general, running all 12 gases and 3 IAM pairs (36 in total) will take approximately 1,250 processor hours. Users should plan to allocate 4GB of memory per processor. 

## MimiIWG_FAIRv162
Replicating the the SC-HFCs from `MimiIWG` paired with the `FAIRv1.6.2` climate model can be done in the same way as MimiGIVE as outlined above in [Running the Scripts](#running-the-scripts). Instead of navigating to the `GIVE` directory, navigate to the `MimiIWG_FAIRv162` directory (in the terminal, the same was as instructed for GIVE) and execute with the replication script: `1_estimate_scghgs.jl`. Again this is a requested sensitivity analysis -- not a main result -- see paper for modeling choices and assumptions.

**Note:** Estimation time for MimiIWG varies by the integrated assessment model (IAM). Using 10,000 Monte Carlo draws for each `gas + IAM` pair (one pair per processor) takes approximately 4 hours for DICE, 8 hours for FUND, and 10 hours for PAGE (varies by machine).  In general, running all 12 gases and 3 IAM pairs (36 in total) will take approximately 1,250 processor hours. Users should plan to allocate 4GB of memory per processor. 

## Processing Model Outputs and Producing the Annual SC-HFCs
While this repository already includes all estimates resulting from running the models outlined above, located in the `output` subdirectory under each model's folder, a user can replicate the cleaning process by using the *R* code provided in each model's `code` subdirectory. Begin by navigating to the `paper` directory in the file explorer or equivalent. Open the *R* project under each model's subdirectory (i.e., `MimiGIVE.Rproj` and `MimiIWG.Rproj`). Then, navigate to the `code` subdirectory and open `2_process_scghgs.R` for MimiGIVE. For MimiIWG and MimiIWG_FAIRv162, first open `2_process_page_discontinuities.R` and then `3_process_scghgs.R`. All remaining steps are documented in the code. 

For users that are interested in using Github integration for passing large files between systems (>100mb), we have provided the code `4_chunk_large_files_for_git.R` (in the `MimiGIVE` subdirectory) and `5_chunk_large_files_for_git.R` (in the `MimiIWG` subdirectory). This is particularly useful if users are estimating the models on large cloud-based systems with many CPUs to increase processing time, but who wish to perform the analysis on smaller, local machines, for example. For users who are performing replication and the analysis all on the same machine, these scripts will also provide the necessary inputs into the plotting of the `save_list` components (e.g., the temperature anomalies, radiative forcings, and forcing scaler) as estimated within the `3_recover_data_for_figures.jl` (in the `MimiGIVE` subdirectory) and `4_recover_data_for_figures.jl` (in the `MimiIWG` subdirectory) scripts.

# Estimating the Climate Benefits
The third subdirectory in this repo is titled: `paper`. The code within performs the remaining portions of the analysis, including replicating the total climate benefits and accompanying figures. Scripts are named according to their function, and code is documented with motivation at each step. The analysis was written in *R* using *Rstudio*, and users will likely find that performing the replication is best done within *Rstudio* as well. 

## License
The software code contained within this repository is made available under the [MIT license](http://opensource.org/licenses/mit-license.php). The data and figures are made available under the [Creative Commons Attribution 4.0](https://creativecommons.org/licenses/by/4.0/) license.
