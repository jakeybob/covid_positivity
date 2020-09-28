# covid_positivity

A look at using test positivity (i.e. percentage of tests taken that were positive) instead of crude case numbers.

## Animated Maps
Shows how the (daily) test positivity of the different council areas changed over time.

### First Wave
Firstly from the start of the data, up until June, i.e. the first wave...

![](pics/first.gif)

### Second Wave
And also from the start of August until the present date -- i.e. what looks to be the second wave.

![](pics/second.gif)


## Test Positivity
Comparing the test positivity from a rolling seven-day average, to that taken from a fitted binomial smooth model...

### Scotland Test Positivity
Here the test positivity represented by dots are the seven-day rolling values, where the point size represents the number of tests (note the increase in dot size when the schools returned in August for instance). The binomial smooth is represented by the solid line, with a 95% confidence interval also shown as a (thin) ribbon.

![](pics/plot_scot_smooth.png)

### Local Authority Test Positivity
As above, but for all the council areas, and without the rolling average data shown.

![](pics/plot_all_smooth.png)

## Heatmaps
Essentially same data as the animated maps above, but shown as linear strips so all the data is visible at the same time. When looking at the respective area's peaks, we can see that there is a spread of about two weeks over the country.

### Local Authority Heatmaps
Here, the council areas are shown alphabetically...

![](pics/plot_tile_alphabetical.png)

...and here they are ordered according to the date of their first-wave peak.
![](pics/plot_tile_fwpeakdate.png)


## Test Positivity Gradient
Looking at the gradient of the test positivity curve we can see which areas e.g. peaked *faster* or tailed off *slower* etc.

### Scotland Test Positivity Gradient
![](pics/plot_scot_gradient.png)

### Local Authority Test Positivity Gradient
![](pics/plot_all_gradient.png)

![](pics/plot_all_gradient_shifted.png)

