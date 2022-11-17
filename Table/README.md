# 

Environmental indicator data comes in many forms and may be spatially explicit or summarized into annual time series to track changes in the environment over time. Our goal was to take some of the ocean indicator data that we use every day as fishery scientists and share it via an easy to read table. We used `gt` to build a table that contains general metadata like a description of the environmental indicator, units and data source while also displaying annual time series trends and data distributions. Indicators with only annual observations had distributional summaries across all observed values (1980-present) and we used maps to display distributions of indicator observations in space.

The script used to generate the final version of our table for the RStudio 2022 table competition is available [here](https://github.com/Jamie-Behan/Indicator_Visualizations/blob/main/Table/RStudioTable2022.qmd) and all data and files used in the development of this table are described below.

| File/Folder | Description |
| ----------- | ----------- |
| Data | Folder containing environmental indicator data for the table and depth data for distribution maps |
| RJ_table.R | Early development file for processing bottom temperature data and test table developed with `kable` |
| [RStudioTable2022.qmd](https://github.com/Jamie-Behan/Indicator_Visualizations/blob/main/Table/RStudioTable2022.qmd) | Final script to generate table submitted to RStudio 2022 table competition |
| Table_development.Rmd | Main development file containing several early iterations of the final table built with `gt` package | 
| reactable_table_example.R | Early development file with table built using `reactable` package | 
