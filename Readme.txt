This repository is for open access to source codes and data in the paper "Multitasking and the evolution of optimal clutch size in fluctuating environment", which is currently under revision in the process of publishing to "Ecology and Evolution" journal.

1. In C_codes folder, since random number generators (dSfmt for uniform distribution random number, gen_Beta for Beta distribution random number) are included, please **DO NOT** change the relative location of these files. The main file is "clutch_season_length.c" in clutch_size folder, and "0508_xxx.c" in FigS2 folder. Execution commands are included in the files (in the form of comments, at the beginning of each file). The environment for simulation is clang-800.0.42.1 (compiler) in Mac OSX 10.12.3 (Operating system). 

2. In R_codes folder, the R file serves the same purpose as the "clutch_season_length.c". That is, comparing the offspring of each breeding strategy under various length of season. Because the early stage of this project is written in R, we leave this file as a alternative way of simulation.

3. In Simulation_data folder, we rearrange files by the sequence of each figure. Please note that some folders only have execution file, lacking real data. This is because the data were instantly plotted after simulation without saving. 

(Last modified: May30, 2018 by Ming Liu)
