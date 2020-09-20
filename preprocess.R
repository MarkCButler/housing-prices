library(dplyr)
library(purrr)
library(rlang)
library(stringr)

# Preprocess both the training and test data sets.

# When the data is read with na.strings = '', the check sum(is.na(data))
# returns 0 for both training and test data.  All missing values in the csv
# file are represented by the unquoted string NA.
read_data <- function(file) {
    data <- read.csv(file, stringsAsFactors = F, na.strings = '')
    return(data)
}
train_data <- read_data('data/train.csv')
test_data <- read_data('data/test.csv')

# From the data description, NA is a valid value for certain categorical
# columns.  The current script replaces NA by strings such as 'NB' for 'No
# Basement' in cases where it NA represents a valid category.

# The script also does consistency checks and data repair.
#
# Example:  Do all samples with category 'No Basement' for one predictor have
# the corresponding value for other basement-related predictors?  If not, the
# script attemps to repair the data.
#
# The approach used for consistency checks is first to define a collection of
# related variables, such as basement_predictors below.  For each variable, a
# vector of Id values is generated.  For the BsmQual variable, the Id values
# corresponding to NA are found, while for the TotalBsmtSF variable, the Id
# values corresponding to 0 are found.  If the data is self-consistent, the
# set of Id values found for each related variable should be the same.
#
# For most of the inconsistencies, it is clear that there is one variable in
# the inconsistent set for which NA indicates missing data rather than a valid
# category such as 'No Basement'.  These inconsistencies are repaired by the
# function repair_inconsistencies.
#
# For the cases that don't fit this pattern, it is clear from inspection how
# to correct the data but tricky to write the correction into a rule, so the
# a few corrections are done ad hoc as commands in the main body of the
# script.

basement_predictors <- list(
    no_amenity_na = c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1',
                      'BsmtFinType2'),
    no_amenity_zero = 'TotalBsmtSF'
)
garage_predictors <- list(
    no_amenity_na = c('GarageFinish', 'GarageQual', 'GarageCond', 'GarageType',
                      'GarageYrBlt'),
    no_amenity_zero = c('GarageCars', 'GarageArea')
)
fireplace_predictors <- list(
    no_amenity_na =  'FireplaceQu',
    no_amenity_zero = 'Fireplaces'
)
pool_predictors <- list(
    no_amenity_na = 'PoolQC',
    no_amenity_zero = 'PoolArea'
)

# Helper functions for doing consistency checks and repairing inconsistencies.

# Filter a single column of data using a variable name and a filter
# expression.  Return a vector of Id values that pass the filter.
get_filtered_id <- function(data, variable_name, filter_expr) {
    # The argument filter_expr should be a string that includes
    # the string literal 'variable_name'.  To convert this into an argument
    # for dplyr::filter, replace 'variable_name' by the name of the variable
    # and use rlang::parse_expr to generate an expression object.
    expression <- str_replace(filter_expr, 'variable_name', variable_name) %>%
        parse_expr()
    filtered_samples <- select(data, all_of(variable_name), Id) %>%
        filter(!!expression)
    return(filtered_samples$Id)
}

# Filter multiple columns of data using a vector of variables names and a
# filter expression.  Returns a list of vectors, each obtained by calling
# get_filtered_id for a single column of data.  The names of the list elements
# are variable names in order to facilitate making changes to the data if
# inconsistencies are found.
#
# The function is a wrapper to avoid repeatedly writing out detailed calls to
# purrr::map.
get_id_vectors <- function(data, variable_names, filter_expr) {
    id_vectors <- map(
        variable_names,
        ~ get_filtered_id(data, ., filter_expr)
    )
    names(id_vectors) <- variable_names
    return(id_vectors)
}

# Check whether all vectors in a list are identical after sorting.
are_equal <- function(list_of_vectors) {
    first_vector <- sort(list_of_vectors[[1]])
    match_first_vector <- map_lgl(
        list_of_vectors[-1],
        ~ identical(sort(.), first_vector)
    )
    return(all(match_first_vector))
}

# Report inconsistencies in a list of Id vectors and attempt to repair data.
repair_inconsistencies <- function(data, list_of_vectors) {
    variable_names <- names(list_of_vectors)
    common_ids <- reduce(list_of_vectors, intersect)
    inconsistent_ids <- map(list_of_vectors, ~ setdiff(., common_ids)) %>%
        reduce(c) %>%
        unique() %>%
        sort()

    # Print out the inconsistencies to allow checking of the automated repairs.
    cat('\nInconsistent values found:\n\n')
    rows <- select(data, Id, all_of(variable_names)) %>%
        filter(Id %in% inconsistent_ids)
    print(rows)

    # A robust algorithm to repair inconsistencies would be too complicated to
    # be worthwhile here.  For most of the inconsistencies, however, it is
    # clear that there is one variable in the inconsistent set for which NA
    # indicates missing data rather than a valid category such as 'No Pool'.
    # In the test data, for instance, there are three houses for which a pool
    # area is given but PoolQC is NA.  Assume that the NA in these three cases
    # indicates missing data for PoolQC.
    #
    # For the cases that don't fit this pattern, it is clear from inspection how
    # to correct the data but tricky to write the correction into a rule, so the
    # corrections are done ad hoc as commands in the main body of the script.
    for (id in inconsistent_ids) {
        bool_index <- map_lgl(list_of_vectors, ~ (id %in% .))
        variables_reporting_no_amenity <- variable_names[bool_index]
        if (length(variables_reporting_no_amenity) == 1) {
            row_selector <- data$Id == id
            data[row_selector, variables_reporting_no_amenity] <- NA
        }
    }

    cat('\nAfter repair attempt:\n\n')
    rows <- select(data, Id, all_of(variable_names)) %>%
        filter(Id %in% inconsistent_ids)
    print(rows)

    return(data)
}

# Check variables in a collection of related predictors such as
# basement_predictors.
check_related_predictors <- function(data, related_predictors) {
    id_vectors <- c(
        get_id_vectors(data, related_predictors$no_amenity_na,
                       'variable_name == "NA"'),
        get_id_vectors(data, related_predictors$no_amenity_zero,
                       'variable_name == 0')
    )
    if (!are_equal(id_vectors)) {
        data <- repair_inconsistencies(data, id_vectors)
    }
    return(data)
}

# The main function for checking and repairing data.
check_data <- function(data) {
    data <- check_related_predictors(data, basement_predictors) %>%
        check_related_predictors(garage_predictors) %>%
        check_related_predictors(fireplace_predictors) %>%
        check_related_predictors(pool_predictors)
    return(data)
}

cat('Checking for inconsistencies in the training data.')
train_data <- check_data(train_data)
cat('Checking for inconsistencies in the test data.')
test_data <- check_data(test_data)

# As noted in the comments for the function repair_inconsistencies, most of the
# inconsistencies can be repaired using a simple rule, but in a few rows in the
# test data, a general rule for the repair would be tool complicated to be worth
# the effort.  The ad hoc repairs below handle those cases.

print_related_variables <- function(data, id_values, related_predictors) {
    variable_names <- reduce(related_predictors, c)
    data <- select(data, Id, all_of(variable_names)) %>%
        filter(Id %in% id_values)
    print(data)
}
cat('\nExecuting ad hoc repairs.\n\n',
    'Before ad hoc repair of test data for basement:\n\n')
print_related_variables(test_data, 2121, basement_predictors)
test_data[test_data$Id == 2121, 'TotalBsmtSF'] <- 0
cat('\nAfter ad hoc repair of test data for basement:\n\n')
print_related_variables(test_data, 2121, basement_predictors)
cat('\nBefore ad hoc repair of test data for garage:\n\n')
print_related_variables(test_data, c(2127, 2577), garage_predictors)
test_data[test_data$Id == 2127, c('GarageFinish', 'GarageQual', 'GarageCond')] <- NA
test_data[test_data$Id == 2577, 'GarageType'] <- 'NA'
test_data[test_data$Id == 2577, c('GarageCars', 'GarageArea')] <- 0
cat('\nAfter ad hoc repair of test data for garage:\n\n')
print_related_variables(test_data, c(2127, 2577), garage_predictors)


# At this point, a number of values have been set to <NA> (missing data rather
# than string 'NA') in columns where the string 'NA' represents a valid
# category.  In these columns, replace string 'NA' with a valid value, such as
# 'NB' for 'No Basement'.  Note that a few different replacement values are used
# in order to increase the readability of the plots and data descriptions
# displayed in the markdown output.

na_to_no <- c(
    'FireplaceQu', 'GarageFinish', 'GarageQual', 'GarageCond', 'PoolQC',
    'Fence'
)
na_to_nb <- c(
    'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2'
)
na_to_none <- c('Alley', 'GarageType', 'MiscFeature')
# In cases where there is no garage, GarageYrBlt will be set to zero to
# distinguish these cases from missing data.
na_to_zero <- 'GarageYrBlt'

replace_na <- function(char_vector, replacement) {
    bool_index <- !is.na(char_vector) & (char_vector == 'NA')
    char_vector[bool_index] <- replacement
    return(char_vector)
}

# In addition to replacing NA in certain columns, the clean_data function
# changes 'C (all)' to 'C' in the MSZoning column.  The data description just
# uses 'C' for this category, and so it is distracting to see 'C (all)' in
# legends.
clean_data <- function(data) {
    data <- mutate(data,
                   across(all_of(na_to_no),
                          ~ replace_na(., 'No'))) %>%
        mutate(across(all_of(na_to_nb),
                      ~ replace_na(., 'NB'))) %>%
        mutate(across(all_of(na_to_none),
                      ~ replace_na(., 'None'))) %>%
        mutate(across(all_of(na_to_zero),
                      ~ replace_na(., '0')))
    data$MSZoning <- sub('C \\(all\\)', 'C', data$MSZoning)
    return(data)
}

train_data <- clean_data(train_data)
test_data <- clean_data(test_data)


# Save modified data.
write.csv(train_data, 'data/train.csv', row.names = F)
write.csv(test_data, 'data/test.csv', row.names = F)
