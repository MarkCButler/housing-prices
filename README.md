# housing-prices
Data analysis and machine-learning prediction of sale price for the
[Ames Housing Dataset](http://www.amstat.org/publications/jse/v19n3/decock.pdf).

The analysis and predictions are presented using R markdown in the file
*housing_prices.Rmd*.  The rendered html output is available at

https://markcbutler.github.io/housing-prices/housing_prices.html

Two models are developed:

  - A multilinear model that aims to give insight into the housing market
    represented by the dataset
  - A nonlinear model that aims for predictive accuracy

The script *preprocess.R* was used to repair some inconsistencies in the data,
and the resulting log file *data_repair.log* is included in the repo.
Functions for K-fold target encoding of the categorical variables are defined
in the script *encode.R*.
