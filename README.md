# Link_updating
This repository contains codes for the simulations and figures obtained in the manuscript: Link updating strategies influence consensus decisions as a function of the direction of communication. 
Find the preprint here: https://doi.org/10.48550/arXiv.2212.02880

The authors of the manuscript are: Sharaj Kunjar*, Ariana Strandburg-Peshkin, Mohit Kumar Jolly, Sumantra Sarkar, Pranav Minasandra, Helge Giese and Nico Gradwohl.
*author of correspondence (sharaj.kunjar@gmail.com)

The packages used in the codes are: ggplot2, dplyr, ggpubr, deSolve and scales

Structure of the repository:
*Incoming_model.R and Outgoing_model.R contain the functions that generate all the simulations on the mathematical models described in the manuscript.
*CSpeed.R contains the function that provides convergence speeds to consensus given the time evolution of the state variables.
*If_boundary.R contains the function that checks whether the state variable lie inside the imposed boundary conditions.
*Synth_data folder contains all the generated synthetic data that is used to obtain the figures. The names of the R dataframes correspond to the names of the R scripts that generated them.
*R Scripts and dataframes matched to the figure labels in the manuscript:
- Figure 3: Stub_out.RData
- Figure 4AB: LinkStrat_out.RData
- Figure 4CD: Disagreement_out.RData
- Figure 5AB: LinkStrat_speed.RData
- Figure 5C: Inertia_speed.RData
- Figure 5D: Stub_speed.RData
- Figure S1: Stub_out_Sup.RData
- Figure S2: Disagreement_out_Sup.RData
- Figure S3: Disagreement_speed_Sup.RData
- Figure S4: Inertia_out_Sup.RData
- Figure S5A: Inertia_speed_Sup.RData
- Figure S5B: Stub_speed_Sup.RData

Slight cosmetic changes have been made to the figures obtained for better visualisation. The codes are a work in progress right now for journal submission.