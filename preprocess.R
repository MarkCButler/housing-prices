library(dplyr)

# Preprocess both the training and test data sets.

# When the data is read with na.strings = '', there are no missing values.
# All missing values in the csv file are represented by the unquoted string
# NA.
#
# From the data description, NA is a valid value for certain categorical
# columns.  The current script replaces NA by strings such as 'NB' for 'No
# Basement'.

# In replacing NA strings, it is convenient to use is.na to find the strings,
# so the csv file is imported with na.strings = 'NA'.
read_data <- function(file) {
    data <- read.csv(file, stringsAsFactors = F, na.strings = 'NA')
    return(data)
}
train_data <- read_data('data/train.csv')
test_data <- read_data('data/test.csv')

na_to_no_cols <- c(
    'FireplaceQu', 'GarageFinish', 'GarageQual', 'GarageCond', 'PoolQC',
    'Fence'
)
na_to_nb_cols <- c(
    'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2'
)
na_to_none_cols <- c('Alley', 'GarageType', 'MiscFeature')
replace_na <- function(char_vector, replacement) {
    bool_index <- is.na(char_vector)
    char_vector[bool_index] <- replacement
    return(char_vector)
}

# In addition to replacing NA in certain columns, the preprocess function
# changes 'C (all)' to 'C' in the MSZoning column.  The data description just
# uses 'C' for this category, and so it is distracting to see 'C (all)' in
# legends.
preprocess <- function(data) {
    data <- mutate(data,
                   across(all_of(na_to_no_cols),
                          ~ replace_na(., 'No'))) %>%
        mutate(across(all_of(na_to_nb_cols),
                      ~ replace_na(., 'NB'))) %>%
        mutate(across(all_of(na_to_none_cols),
                      ~ replace_na(., 'None')))
    data[['MSZoning']] <- sub('C \\(all\\)', 'C', data[['MSZoning']])
    return(data)
}

train_data <- preprocess(train_data)
test_data <- preprocess(test_data)

# Save modified data.
write.csv(train_data, 'data/train.csv', row.names = F)
write.csv(test_data, 'data/test.csv', row.names = F)
