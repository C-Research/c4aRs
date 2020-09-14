# c4aRs

This package is a convenient location for R resources for C4ADS analysts.

## Installation

1. Install `devtools` if not already installed with `install.packages('devtools')`.
2. Install `c4aRs` using `devtools` using the command `devtools::install_github('C-Research/c4aRs')`.
3. You may be prompted to install some updates to different packages. You may update, but make sure to not install the `stringi` update, as this will cause your installation to fail. 
4. Done!

## Century Gothic

The `ggplot2` resources used in this package rely on the use of the 'Century Gothic' font. To make Century Gothic available to R, you must first install Century Gothic on your computer, and then run `extrafont::font_import()` to import the new font.

## Usage 

#### Seizure Data

There are a few functions in this package that assist cleaning seizure data. Each function is documented, and you can access them using the command `??c4aRs::function_name`, replacing `function_name` with the name of the function you are curious about. 

One function is `selectByMultCols`. This function will filter a dataset by a number of columns, searching for a number of values. So, for instance, if you were looking for any seizure with an origin or destination of "Mexico" or "China", you would just call, `selectByMultCols(data, c('OriginCountry', 'DestinationCountry'), c('Mexico', 'China'))`

Another exampe is `traffickingInstanceCount`, which will count return a dataframe of trafficking instance counts based values in user-defined columns. For example, to find the number of TIs for each country in a dataset, you would run `traffickingInstanceCount(data, c('OriginCountry', TransitCountry, 'DestinationCountry'))`. Note, this will count the number of rows that each country appears in, so is inconsistent with the ROUTES method of TI calculation. 

#### ggplot2 theme 

When c4aRs is loaded, our theme is automatically loaded, so it will be applied to any ggplot that you create automatically. If 'Century Gothic' is not installed, the graphs will not render

#### Colors 

The C4ADS colors are also available in Hex code form in the c4aRs package. They are presented in the following objects.

* `c4_cols` - a named vector of all the C4 base colors.
* `c4_col_gradients` - a named list of all the c4 base colors, along with 3 lighter hues for each color.
* `c4_col_leafy` - a vector of all the colors in the leafy palette.
* `c4_col_spooky` - a vector of all the colors in the spooky palette.
* `c4_col_dusty` - a vector of all the colors in the dusty palette.
