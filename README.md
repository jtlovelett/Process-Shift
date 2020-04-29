# Overview
Write an overview
# Running a model
## Workflow
* Connect to remote cluster system
* use sbatch to launch .sh file for desired model 
* .sh file launches .R file 
* R file:
  + reads data
  + preprocesses data for Stan
  + calls **Stan** script to fit HBM 
  + does some post-processing and plotting etc. 
