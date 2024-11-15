
# ATCodeR

<!-- badges: start -->
<!-- badges: end -->

The goal of ATCodeR is to convert free-text medication  data into structured and standardized ATC codes. The resulting output is a structured data  frame containing columns for antineoplastic substances, other medications, and  supplementary information. A Focus is on grouping antineoplastic medication.


## Installation

You can install the development version of ATCodeR like so:

``` r
# install the devtools package to enable installation from github
install.packages("devtools")

# install the ATCodeR package from github
devtools::install_github(repo = "https://github.com/IsiSchnorr/ATCodeR")
```

## Example

This is an example:

``` r
# Load ATCodeR package
library(ATCodeR)

# Load the example data
data("example_data")

# Apply the ATCtransfrom function to the example data
output_dataframe <- ATCtransform(input.df = example_data, column_name = "subs")

# View output
View(output_dataframe)

```

