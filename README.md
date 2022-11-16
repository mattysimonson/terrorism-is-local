# terrorism-is-local
Code for the All Terrorism is Local study

This repository contains the code for the All Terrorism is Local article by Simonson, Lazer, Minozzi, and Neblo. Please contact Matthew Simonson with quesitons. The data for this project cannot be made public because it contains identifying information about students that is protected under the IRB agreement and the agreement between the researchers and partner organization. Simulated data can be provide upon request.

The project is structured with a private root directory containing both a public subdirectory with the code and outputs (uploaded to this respository):

root_dir/public_data/terrorism-is-local contains the code and output files (figures and tables)

and a private subdirectory containing the data.

root_dir/restricted_files/survey_data_new.rdata contains the raw data
root_dir/restricted_files/use_this_data.rdata contains the cleaned data
root_dir/restricted_files/edges.rda contains the network data

The model objects generated by the Bayesian models are stored in the private subdirectory because it they include the data:

root_dir/restricted_files/model_objects/

To run the analysis, first run Run Bayesian Models.Rmd
then Terrorism Replication.Rmd

Both Rmarkdown files are well commented and should be self-explanatory. The former takes a long time to run, which is why it is kept seperate and its outputs are saved for future use.  The network analysis is contained in the file artistotelian_networks.R which is sourced from Terrorism Replication.Rmd

Figures for the main text are output to the figures/ subdirectory while figures and tables for the SI are output to the SI/ subdirectory (both are public).
