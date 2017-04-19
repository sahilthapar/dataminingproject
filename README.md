## Approach

1. Problem definition
2. Gathering information - data
3. Preliminary - Exploratory Analysis
    - Always start by graphing the data.
    - Are there consistent patterns?
    - Is there a significant trend?
    - Is seasonality important?
    - Is there evidence of the presence of business cycles?
    - Are there any outliers in the data that need to be explained by those with expert knowledge?
    - How strong are the relationships among the variables available for analysis?
4. Fitting a model
    - regression models
    - exponential smoothing methods 
    - Box-Jenkins ARIMA models 
    - Dynamic regression models
    - Hierarchical forecasting
    - count time series, neural networks and vector autoregression
5. Comparing the models

## Time series graphics
[Chapter 2](https://www.otexts.org/fpp2/ch-graphics.html)

- xts - a time-series representation (instead of data frame) in R
- Frequency of the time series 
    + The “frequency” is the number of observations before the seasonal pattern repeats
    + Should decide the frequency of the observations
- Time plot
    + autoplot
    + Interpret these plots
- Trend
    + A trend exists when there is a long-term increase or decrease in the data. It does not have to be linear. 
- Seasonal
    + A seasonal pattern occurs when a time series is affected by seasonal factors such as the time of the year or the day of the week.
    + Seasonality is always of a fixed and known frequency.
- Cyclic
    + A cycle occurs when the data exhibit rises and falls that are not of a fixed frequency.
- Seasonal plots
    + Overlay a separate plot for each season - frequency
    + polar plots
    + `ggseasonplot`
- Seasonal sub series plots
    + An alternative plot that emphasises the seasonal patterns is where the data for each season are collected together in separate mini time plots.
- Lag plots
- Autocorrelation
    + Just as correlation measures the extent of a linear relationship between two variables, autocorrelation measures the linear relationship between lagged values of a time series.
    
## Forecaster's toolbox
[Chapter 3](https://www.otexts.org/fpp2/ch-toolbox.html)

### Simple Methods

  - Average
  - Naive
  - Drift (Naive + Trend)
  
### Transformations and Adjustments

### Cross Validation in Time-series
