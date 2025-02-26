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

This replication package is based on inputs which have been rounded, consistent with BEA publication criteria. Accordingly, while the code will produce results consistent with the tables and figures in the published version, the results will not be exactly the same. However, researchers seeking to apply this method can use the code as a foundation from which to build.

In order to replicate paper results, researchers should proceed as follows:
1. Clone the repository to personal space
2. Run **results.R**. This will produce dataframes containing the main specification predictions.
3. Run **var_models.R** This will produce the autoregressive estimates that are directly compared to the main specification in Table 1.
4. Run **table.R** This will print Table 1.
5. Run **figures.R** This will generate six individual figures for the Gini and quintiles.

For those who wish to use another statistical language, the data is located in an excel **replication_data.xlsx** and can be loaded directly into an alternative program for further analysis.

