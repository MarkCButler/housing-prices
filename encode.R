library(caret)
library(dplyr)
library(tibble)
library(tidyr)

# Functions and variables for k-fold target encoding categorical variables.
#
# In cases where the number of samples used to find the mean for the fold is
# <10, a sigmoid function is used to take a weighted average of the
# out-of-fold mean and the mean of the target over the training data.
#
# To encode a categorical variable of the test data, each observation receives
# one of the k encodings generated for the same variable for training data,
# and the choice is random.  In order to facilitate this process, a list is
# used to store the k encodings for each variable that was encoded.  For
# instance, variable_encodings$Neighborhood is a data frame with row names
# equal to the unique categories for the Neightborhood variable and columns
# equal to the k different encodings.
variable_encodings <- list()

# Sigmoid function that varies from ~0 to ~1 in the range 0 to 2*center
sigmoid <- function(x, center) {
    y <- ifelse(
        x <= 0, 0,
        1 / (1 + exp(-5 * (x - center) / center))
    )
    return(y)
}

# Perform k-fold target encoding for the training data.
encode <- function(data, variable_name, k = 5, sigmoid_center = 5) {
    # To simplify dplyr commands, rename the column of categorical data to be
    # encoded.  Also replace NA by the string "missing" to simplify the
    # encoding process.
    to_encode <- select(data, SalePrice, all_of(variable_name)) %>%
        rename(category = .data[[variable_name]]) %>%
        mutate(
            category = replace(category, is.na(category), 'missing')
        )

    encoded <- as.numeric(rep(NA, nrow(to_encode)))

    # This data frame saves each of the k encodings.  For a given row of the
    # test data, a random choice of the k encodings in this data frame will be
    # used for encoding.
    k_fold_encodings <- data.frame(category = unique(to_encode$category),
                                   stringsAsFactors = FALSE)

    target_mean <- mean(to_encode$SalePrice)
    folds <- createFolds(to_encode$category, k = k)
    na_replacements <- list(count = 0, out_of_fold_mean = 0)
    for (index in seq_along(folds)) {
        fold_name <- names(folds)[index]
        fold <- folds[[index]]
        encoding <- group_by(to_encode[-fold, ], category) %>%
            summarise(out_of_fold_mean = mean(SalePrice), count = n(),
                      .groups = 'drop') %>%
            right_join(k_fold_encodings['category'], by = 'category') %>%
            replace_na(na_replacements) %>%
            mutate(
                out_of_fold_weight = sigmoid(count, sigmoid_center),
                value = out_of_fold_weight * out_of_fold_mean +
                       (1 - out_of_fold_weight) * target_mean
            ) %>%
            mutate(
                value = replace(value, category == 'missing', target_mean)
            ) %>%
            select(category, value)

        k_fold_encodings <- full_join(k_fold_encodings, encoding,
                                      by = 'category') %>%
            rename(!!fold_name := value)

        encoding <-  column_to_rownames(encoding, var = 'category')
        encoded[fold] <- map_dbl(to_encode$category[fold],
                                 ~ encoding[., 'value'])

    }

    k_fold_encodings <- column_to_rownames(k_fold_encodings, var = 'category')
    to_save <- list(k_fold_encodings)
    names(to_save) <- variable_name
    variable_encodings <<- c(variable_encodings, to_save)

    encoded_variable_name <- paste0(variable_name, 'Num')
    data[[encoded_variable_name]] <- encoded
    return(data)
}

# Assign encodings to a variable of the test data, choosing randomly for each
# observation between the k different encodings found for the same variable in
# the training data.
assign_encoding <- function(data, variable_name) {
    to_encode <- data[[variable_name]]
    encoding <- variable_encodings[[variable_name]]
    column_selection <- sample(ncol(encoding), size = nrow(data),
                               replace = TRUE)
    encoded_variable_name <- paste0(variable_name, 'num')
    data[[encoded_variable_name]] <- map_dbl(
        seq_along(to_encode),
        ~ encoding[to_encode[.], column_selection[.]]
    )
    return(data)
}
