library(dplyr)

# Preprocess both the training and test data sets.

train_data <- read.csv('data/train.csv', stringsAsFactors = F)
test_data <- read.csv('data/test.csv', stringsAsFactors = F)

# From the data description, NA is a valid value for certain categorical
# columns.  Replace NA by the string 'null' (not the R keyword NULL) in these
# columns in order to avoid treating valid NA values as missing values.
na_valid_cols <- c('Alley', 'BsmtQual', 'BsmtCond', 'BsmtExposure',
                   'BsmtFinType1', 'BsmtFinType2', 'FireplaceQu', 'GarageType',
                   'GarageFinish', 'GarageQual', 'GarageCond', 'PoolQC',
                   'Fence', 'MiscFeature')
rename_na <- function(char_vector) {
    na_indices <- is.na(char_vector)
    char_vector[na_indices] <- 'null'
    return(char_vector)
}
train_data <- mutate(train_data,
                     across(all_of(na_valid_cols), rename_na))
test_data <- mutate(test_data,
                    across(all_of(na_valid_cols), rename_na))

# Save modified data.
write.csv(train_data, 'data/train.csv', row.names = F)
write.csv(test_data, 'data/test.csv', row.names = F)
