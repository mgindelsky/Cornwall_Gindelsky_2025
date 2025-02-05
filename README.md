# nowcasting_income_distribution_statistics
Replication code for "Nowcasting Distributional National Accounts for the United States: A Machine Learning Approach”
- Dr. Marina Gindelsky (corresponding author)
     - marina.gindelsky@bea.gov
- Dr. Gary Cornwall
     - gary.cornwall@bea.gov
- The views expressed in this paper are those of the authors and do not necessarily represent the U.S. Bureau of Economic Analysis or the U.S. Department of Commerce.
- All analysis completed in R version 4.3.2
- All analysis completed using NIPA Vintage October 2024
- All analysis completed using distributional accounts vintage December 2024

Replication package consists of several folders:
1. Base Data
   - main_analysis_dpi.Rda: targets and features for nowcasting disposable personal income (2020-2023)
   - main_analysis_pi.Rda: targets and features for nowcasting personal income (2020-2023)
   - main_analysis_w_macro_pi.Rda: targets and features (including additional macro economic indicators) for nowcasting personal income (2020-2023)
2. Helper Functions
   - plot_results_function: an R function that utilizes ggplot to create graphics similar to those found throughout the paper and online appendix.
3. Results Scripts
   - dpi_replication: a script that loads main_analysis_dpi.Rda and replicates the analysis found in the online appendix
   - pi_replication: a script that loads main_analysis_pi.Rda and replicates the analysis found in the paper
   - pi_replication_w_macros: a script that loads main_analysis_w_macro_pi.Rda and replicates the analysis found in the online appendix
