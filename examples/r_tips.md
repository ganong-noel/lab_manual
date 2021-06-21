R Tips and Tricks
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.0.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

Using `purrr`
-------------

### General comments

The package `purrr` in the tidyverse offers relatively straightforward way to do iteration in R.

The most basic funciton in `purrr` is `purrr::map()`. It accepts 2 arguments: a list/vector, and a function. It's ouptut is a list which is the result of the element-wise application of the function to the input vector/list. Here are two examples:

``` r
#create vector to apply function to
example_vector <- c(2, 4, 6, 8)


#an example function that squares the input

square <- function(x) {
  return(x ^ 2)
}

# now use map to square each of the inputs

map(example_vector, square)
```

    ## [[1]]
    ## [1] 4
    ## 
    ## [[2]]
    ## [1] 16
    ## 
    ## [[3]]
    ## [1] 36
    ## 
    ## [[4]]
    ## [1] 64

Notice that the type of the output here is a list (which you can tell by the double bracket indexing) rather than a vector. If we wanted to return a vector we could `map_dbl` which returns a double vector. There are similar functions to return a variety of types.

``` r
map_dbl(example_vector, square)
```

    ## [1]  4 16 36 64

Map also has the capacity to use anonymous functions and formulas.

Anonymous functions are functions that are not named and defined as they are used. You define an anonymous function just as you would a regular function except you do not assign it to a name.

Formulas are a relatively frequent way to use map in gnlab (more so than anonymous funcitons). To use a formula, begin the second argument of `map` with a `~` symbol and then write out the transformation you want to apply to the input. You should refer to the input as `.x`.

Here as an example of an anonymous function we cube the inputs, or as an example of a formula we check to see if a string contains the letter "b"

``` r
#anonymous function

map(1:3, function(to_be_cubed) to_be_cubed ^ 3)
```

    ## [[1]]
    ## [1] 1
    ## 
    ## [[2]]
    ## [1] 8
    ## 
    ## [[3]]
    ## [1] 27

``` r
#formula

map(c("bannana", "tomato", "peach"), ~ str_detect(.x, "b"))
```

    ## [[1]]
    ## [1] TRUE
    ## 
    ## [[2]]
    ## [1] FALSE
    ## 
    ## [[3]]
    ## [1] FALSE

Finally note that if the function that you want to use map with take multiple arguments, but you want to keep an argument constant over all of the iteration, you can just supply it as an argument to `map`. For example we could rewrite the formula example above:

``` r
map(c("bannana", "tomato", "peach"), str_detect, "b")
```

    ## [[1]]
    ## [1] TRUE
    ## 
    ## [[2]]
    ## [1] FALSE
    ## 
    ## [[3]]
    ## [1] FALSE

We can also use map on lists. As an example, we want to find the sum and product of a number of sequences. It's often handy when using tidyverse to keep data together in a dataframe, so we'll do that.

``` r
#define the function that we want to apply to each element

get_product_sum <- function(sequence){
  product <- prod(sequence)
  sum <- sum(sequence)
  
  return(tibble(product = product, sum = sum))
}

#when using map, it's often helpful to check that the function
#you want to use is working as expected on a single element

get_product_sum(1:5)
```

    ## # A tibble: 1 x 2
    ##   product   sum
    ##     <dbl> <int>
    ## 1     120    15

Now lets make a list of the sequences we want to use we are making a named list, because this will be helpful when we turn it into a dataframe.

``` r
example_list <- list(sequence_1 = 1:5,
                     sequence_2 = 5:10, 
                     sequence_3 = -1:3)
```

we are going to use map\_dfr, which should be interpreted as "map dataframe by rows", which calculates dataframe for each element of the input and then returns a single dataframe that comes from stacking all of the dataframes for each element by row (like `rbind()` or `bind_rows()`)

``` r
#first here is the regular map

map(example_list, get_product_sum)
```

    ## $sequence_1
    ## # A tibble: 1 x 2
    ##   product   sum
    ##     <dbl> <int>
    ## 1     120    15
    ## 
    ## $sequence_2
    ## # A tibble: 1 x 2
    ##   product   sum
    ##     <dbl> <int>
    ## 1  151200    45
    ## 
    ## $sequence_3
    ## # A tibble: 1 x 2
    ##   product   sum
    ##     <dbl> <int>
    ## 1       0     5

``` r
#now map dfr
map_dfr(example_list, get_product_sum)
```

    ## # A tibble: 3 x 2
    ##   product   sum
    ##     <dbl> <int>
    ## 1     120    15
    ## 2  151200    45
    ## 3       0     5

This output would be helfpul except we've now lost track of which sequence corresponds to which ouput.We can use an ".id" argument (which is also available in `bind_rows`) to keep track of the rows. The ".id" argument will be the name of a new column in the dataframe, which is populated by the name of the element of the list that generated it. This is why we are using a named list rather than an unnamed list

``` r
map_dfr(example_list, get_product_sum, .id = "sequence_number")
```

    ## # A tibble: 3 x 3
    ##   sequence_number product   sum
    ##   <chr>             <dbl> <int>
    ## 1 sequence_1          120    15
    ## 2 sequence_2       151200    45
    ## 3 sequence_3            0     5

Pretty frequently, you will find that you will want to iterate with multiple arguments. For example, you might have a list of plots that you want to save, and a list of locations that you want to save them in. One way to deal with this is with `pmap()`.

`pmap` takes a list of lists: it will apply the function to the first elements of each of the lists, then the second elements and so on. As an example below, I do some multiplication. The order of the lists in the list of lists, will corespond to the order of the arguments in the function.

``` r
pmap(list(list(1, 2, 3), list(4, 5, 6)), 
     function(x, y) x * y)
```

    ## [[1]]
    ## [1] 4
    ## 
    ## [[2]]
    ## [1] 10
    ## 
    ## [[3]]
    ## [1] 18

You can see that the first element is `1 * 3`, the second `2 * 5`, and the third is `3 * 6`.

Sometimes you may want to iterate over all possible combinations of some inputs. For example, in a recent project we had a number of different choices when setting up a model, and we wanted to run the model for each of the possible combinations of different choices. As a toy example below, let's do this for a simple linear model for the inclusion of 3 different control variables:

``` r
set.seed(2021)


# Set up a dataframe with data for our model
#c ontrol variables 1 and 2 are correlated with variable x (through v)
# control variables 1 and 3 are in the true model

df <- tibble(u = rnorm(1000),
       v = rnorm(1000),
       x = rnorm(1000) + v,
       control_1 = rnorm(1000) + v,
       control_2 = rnorm(1000) + 0.5 * v,
       control_3 = rnorm(1000),
       y = 4 * x + 3 * control_1 + 0.5 * control_3 + u)

# Set up a list with our control variables 

options <- list(include_1 = c(TRUE, FALSE),
     include_2 = c(TRUE, FALSE),
     include_3 = c(TRUE, FALSE))

# Now we need to take a cartesan product of these variables 
#(i.e. we need all 8 combinations of true and false)

# we can use the cross family of functions to do this. Here I use cross_df(). 

options_df <- cross_df(options) 

print(options_df)
```

    ## # A tibble: 8 x 3
    ##   include_1 include_2 include_3
    ##   <lgl>     <lgl>     <lgl>    
    ## 1 TRUE      TRUE      TRUE     
    ## 2 FALSE     TRUE      TRUE     
    ## 3 TRUE      FALSE     TRUE     
    ## 4 FALSE     FALSE     TRUE     
    ## 5 TRUE      TRUE      FALSE    
    ## 6 FALSE     TRUE      FALSE    
    ## 7 TRUE      FALSE     FALSE    
    ## 8 FALSE     FALSE     FALSE

Now let's write a function that will take a vector of options and return the value of the coefficient on x for those given options

``` r
run_model <- function(options_vector){
  variables <- c("control_1", "control_2", "control_3")
  
  #here we always want y to be the outcome variable and we always want to include x
  # then we select from the variable by whether we want to include it 
  
  # The collapse = " + " is just going to concatenate all of the elements
  # of the vector into a single string
  
  formula_for_model <- str_c(c("y ~ x", variables[options_vector]), 
                             collapse = " + ")
  
  lm(formula_for_model, df)$coefficients["x"] %>%
    as.numeric() #get rid of name
}

#Now we want to apply this to each row of our options_df dataframe
# we can do this inside a mutate statement

options_df %>%
  mutate(coefficient = pmap_dbl(list(include_1,
                                     include_2,
                                     include_3),
                                function(x, y, z) run_model(c(x, y, z))))
```

    ## # A tibble: 8 x 4
    ##   include_1 include_2 include_3 coefficient
    ##   <lgl>     <lgl>     <lgl>           <dbl>
    ## 1 TRUE      TRUE      TRUE             4.02
    ## 2 FALSE     TRUE      TRUE             5.31
    ## 3 TRUE      FALSE     TRUE             4.02
    ## 4 FALSE     FALSE     TRUE             5.46
    ## 5 TRUE      TRUE      FALSE            4.02
    ## 6 FALSE     TRUE      FALSE            5.31
    ## 7 TRUE      FALSE     FALSE            4.02
    ## 8 FALSE     FALSE     FALSE            5.46

``` r
# In this case it might have been simpler to just use map.

# for example, as an alternative we could write

options %>%
  cross() %>% #which produces a list of list of each combination
  map(as_vector) %>% # transform to a list of vectors 
  map_dfr(~ list(.x, coefficient = run_model(.x)) %>% #produce a list  with the vector and coefficient
            flatten()) #flatten the list (prior to this it is a list of 2 lists).
```

    ## # A tibble: 8 x 4
    ##   include_1 include_2 include_3 coefficient
    ##   <lgl>     <lgl>     <lgl>           <dbl>
    ## 1 TRUE      TRUE      TRUE             4.02
    ## 2 FALSE     TRUE      TRUE             5.31
    ## 3 TRUE      FALSE     TRUE             4.02
    ## 4 FALSE     FALSE     TRUE             5.46
    ## 5 TRUE      TRUE      FALSE            4.02
    ## 6 FALSE     TRUE      FALSE            5.31
    ## 7 TRUE      FALSE     FALSE            4.02
    ## 8 FALSE     FALSE     FALSE            5.46

You can see from the output, that (as expected from theory), it seems like only the inclusion of control variable 1 matters. It should be included to recover the correct coefficient.

`dplyr` joins
-------------

Joins in `dplyr` are the way of matching observations between dataframes. The most basic of these operations is the `left_join`. The `left_join(x, y, by)` takes 3 arguments to run.

The function takes each row in `x` and looks at the value of `by`. It then finds all of the rows with a matching value of `by` in `y` ands the columns of `y` to `x` for these rows. If there are no matches for the value of `by` in `y`, then the row is retained but the row will have a value of `NA`. This is easier explained by examples below.

Basic example
-------------

Consider the toy data from below. For a set of customer `id`s we have a dataframe that has state data and another that has weekly income. We ultimately want to know things about incomes by state, so we need to match the data by customer.

``` r
data_state <- tribble(
  ~id, ~state,
  1, "FL", 
  2, "GA", 
  3, "TX", 
  4, "PA", 
  5, "CA", 
  6, "FL",
  7, "CA")

data_income <- tribble(
  ~id, ~weekly_income,
  1, 1000,
  2, 2100,
  3, 700,
  4, 600,
  5, 900,
  6, 1200)


data <- left_join(data_state, data_income, by = "id")

data
```

    ## # A tibble: 7 x 3
    ##      id state weekly_income
    ##   <dbl> <chr>         <dbl>
    ## 1     1 FL             1000
    ## 2     2 GA             2100
    ## 3     3 TX              700
    ## 4     4 PA              600
    ## 5     5 CA              900
    ## 6     6 FL             1200
    ## 7     7 CA               NA

`left_join` went through each row of `data_state` took the value of `id`. For each row, it goes through the `data_income` dataframe, looking for matching values of `id` in this dataframe. Where there is a match, the remaining columns of `data_income` get added to the columns of `data_state` in the coresponding row. Note that because the `id` 7 had no match in the `data_income` dataframe, it is here with an `NA`

Using joins to duplicate rows
-----------------------------

It's pretty common that you would want to duplicate rows of a dataframe before constructing sample statistics.

Suppose we want to know the average income in both a national sample and in a subset of states where we have, for example, Chase data by race. One straightforward way to do this is with an inner join to duplicate rows.

``` r
#Only wanted the NA for demonstration, lets get rid of it

data <- filter(data, !is.na(weekly_income))

chase_states <- c("FL", "GA", "LA")

sample_def_df <- tibble(in_chase = c(FALSE, TRUE, TRUE),
                        sample = c("national", "national", "chase"))

#Now display the data

data %>%
  mutate(in_chase = state %in% chase_states) %>%
  left_join(sample_def_df, by = "in_chase")
```

    ## # A tibble: 9 x 5
    ##      id state weekly_income in_chase sample  
    ##   <dbl> <chr>         <dbl> <lgl>    <chr>   
    ## 1     1 FL             1000 TRUE     national
    ## 2     1 FL             1000 TRUE     chase   
    ## 3     2 GA             2100 TRUE     national
    ## 4     2 GA             2100 TRUE     chase   
    ## 5     3 TX              700 FALSE    national
    ## 6     4 PA              600 FALSE    national
    ## 7     5 CA              900 FALSE    national
    ## 8     6 FL             1200 TRUE     national
    ## 9     6 FL             1200 TRUE     chase

Note that now the observations we wanted in both the Chase and national sample are duplicated, appearing once with sample as "national" and once with sample as "chase".

This is because the `in_chase` value of `TRUE` gets matched to 2 rows in the `sample_def_df`.

It's then straightforward to recover the averages in each sample:

``` r
data %>%
  mutate(in_chase = state %in% chase_states) %>%
  left_join(sample_def_df, by = "in_chase") %>%
  group_by(sample) %>%
  summarise(mean_inc = mean(weekly_income))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 2
    ##   sample   mean_inc
    ##   <chr>       <dbl>
    ## 1 chase       1433.
    ## 2 national    1083.

Note that because this method is relying on data duplication, it should be used when the memory requirements of your data are lower (i.e. if you were duplicating gigabytes of data, then it might make sense to take a different approach). But for working with small-medium data this approach is pretty handy (and you see it used both in the "Wealth, Race, and Consumption Smoothing" (rdfo) paper code (with the CPS) and the "Why Do Borrowers Default on Mortgages?" (strategic) paper (with the PSID)).

Using joins to subset rows
--------------------------

Sometimes you might keep a dataframe of observations that meet a particular sample screen: for exmaple, inside Chase, customers that meet a customer activity screen or elsewhere a random sample of IDs to reduce computational requirements (as in the CRISM code).

You can pretty easily use joins to subset observations in your data. One type of join that you might use to do this is the `inner_join`. Unlike the `left_join` which retains all rows of the first dataframe, the `inner_join` only retains those rows for which there is a match.

For example below:

``` r
inner_join(data_state, data_income)
```

    ## Joining, by = "id"

    ## # A tibble: 6 x 3
    ##      id state weekly_income
    ##   <dbl> <chr>         <dbl>
    ## 1     1 FL             1000
    ## 2     2 GA             2100
    ## 3     3 TX              700
    ## 4     4 PA              600
    ## 5     5 CA              900
    ## 6     6 FL             1200

See that because there was no match for the `id` of 7 it doesn't appear in the output.

Now see that this can be used to subset the data using a dataframe.

``` r
#Valid customers:
valid_cust_df <- tibble(id = c(1, 4, 6))

data 
```

    ## # A tibble: 6 x 3
    ##      id state weekly_income
    ##   <dbl> <chr>         <dbl>
    ## 1     1 FL             1000
    ## 2     2 GA             2100
    ## 3     3 TX              700
    ## 4     4 PA              600
    ## 5     5 CA              900
    ## 6     6 FL             1200

``` r
data %>%
  inner_join(valid_cust_df)
```

    ## Joining, by = "id"

    ## # A tibble: 3 x 3
    ##      id state weekly_income
    ##   <dbl> <chr>         <dbl>
    ## 1     1 FL             1000
    ## 2     4 PA              600
    ## 3     6 FL             1200

For contexts like these, you might prefer to use a `semi_join`. This will avoid duplication of your rows if there are duplicates in the dataframe you are joining on.

The `semi_join`, just checks to see if there is a match and keeps any rows in `x` for which there is a match in `y` but doesn't add columns.

``` r
valid_cust_df <- tibble(id = c(1, 1, 4, 6))

data %>%
  inner_join(valid_cust_df, by = "id")
```

    ## # A tibble: 4 x 3
    ##      id state weekly_income
    ##   <dbl> <chr>         <dbl>
    ## 1     1 FL             1000
    ## 2     1 FL             1000
    ## 3     4 PA              600
    ## 4     6 FL             1200

``` r
data %>%
  semi_join(valid_cust_df, by = "id")
```

    ## # A tibble: 3 x 3
    ##      id state weekly_income
    ##   <dbl> <chr>         <dbl>
    ## 1     1 FL             1000
    ## 2     4 PA              600
    ## 3     6 FL             1200

Using joins after transformations
---------------------------------

Sometimes it is helpful to use a `right_join` instead of a `left_join`. The `right_join(x, y)` is just the `left_join(y, x)`. It's helpful to use it sometimes though for cases where you want to join some tranformation or a summary of a dataframe back to itself because it works well with pipes.

For example consider the toy data below where we have house price indices for a number of zip codes. We want to normalise the prices by the price in the second period. We want our missing values to be explicit.

``` r
data <- tribble(~zip, ~date, ~price,
        1, 1, 100,
        1, 2, 120,
        1, 3, 300,
        2, 1, 200,
        2, 2, 250,
        2, 3, 200,
        3, 1, 200,
        3, 3, 400)

#notice that the second observation for the third zipcode is missing. Let's make this explicit with a row for the missing observation

data <- data %>%
  complete(zip, date)

data 
```

    ## # A tibble: 9 x 3
    ##     zip  date price
    ##   <dbl> <dbl> <dbl>
    ## 1     1     1   100
    ## 2     1     2   120
    ## 3     1     3   300
    ## 4     2     1   200
    ## 5     2     2   250
    ## 6     2     3   200
    ## 7     3     1   200
    ## 8     3     2    NA
    ## 9     3     3   400

``` r
#now we want to use a join to get the price in the second period and include this as a column

data %>%
  filter(date == 2) %>%
  rename(price_2nd = price) %>%
  select(-date) %>%
  right_join(data) %>%
  mutate(relative_price = price / price_2nd)
```

    ## Joining, by = "zip"

    ## # A tibble: 9 x 5
    ##     zip price_2nd  date price relative_price
    ##   <dbl>     <dbl> <dbl> <dbl>          <dbl>
    ## 1     1       120     1   100          0.833
    ## 2     1       120     2   120          1    
    ## 3     1       120     3   300          2.5  
    ## 4     2       250     1   200          0.8  
    ## 5     2       250     2   250          1    
    ## 6     2       250     3   200          0.8  
    ## 7     3        NA     1   200         NA    
    ## 8     3        NA     2    NA         NA    
    ## 9     3        NA     3   400         NA

``` r
#Note that in this simple case it would probably have been easier to do it with a
# mutate statement but sometimes it isn't so it's a helpful tool to keep in mind

data %>%
  group_by(zip) %>%
  mutate(price_2nd = max((date == 2) * price ),
         price / price_2nd) %>%
  ungroup()
```

    ## # A tibble: 9 x 5
    ##     zip  date price price_2nd `price/price_2nd`
    ##   <dbl> <dbl> <dbl>     <dbl>             <dbl>
    ## 1     1     1   100       120             0.833
    ## 2     1     2   120       120             1    
    ## 3     1     3   300       120             2.5  
    ## 4     2     1   200       250             0.8  
    ## 5     2     2   250       250             1    
    ## 6     2     3   200       250             0.8  
    ## 7     3     1   200        NA            NA    
    ## 8     3     2    NA        NA            NA    
    ## 9     3     3   400        NA            NA

`Easy mistakes`
---------------

### Booleans

Pretty frequently you need to either explicitly or implicitly make use of Boolean values (i.e. `TRUE` or `FALSE`) when working with data (most frequently when subsetting data according to conditions).

This easy mistake is that missing values might not behave has expected when working with Booleans.

Specifically note that `FALSE & NA` evaluates to `FALSE` (since regardless of what the NA value was, the statement would be false) and `TRUE | NA` evaluates to `TRUE` (since regardless of waht the `NA` value was the statement would be true).

This can cause hassles if you are using this in a filter statement and not thinking carefully.

### Packages issues

Pretty frequently loading a number of packages as required in gnlab will cause conflicts in R.This is where there is a function that uses the same name in two packages that are attached.

You can use the function `conflicts()` to check which conflicts are in your R session. Most frequently, there are conflicts for `summarise`, `matches` or `filter`.

To resolve a conflict you can just assign whatever function you want to the name. For example if I want to resolve in favour of the dplyr version of `matches`, I would write `matches <- dplyr::matches`.
