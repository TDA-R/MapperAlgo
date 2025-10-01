# Topological Data Analysis: Mapper Algorithm
<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/MapperAlgo)](https://cran.r-project.org/package=MapperAlgo)
<a href="https://CRAN.R-project.org/package=MapperAlgo" target="_blank" rel="noreferrer"> <img src="https://cranlogs.r-pkg.org/badges/grand-total/MapperAlgo" alt="mysql" width="100" height="20"/> </a> 
<!-- badges: end -->

## Playground & Document
For a more detailed explanation for this package, this [document](https://www.notion.so/MapperAlgo-21875012ce1a80b088dfc4a9ab263b02?source=copy_link) will keep update for better understanding the source code. You can also try the [playground](https://tf3q5u-0-0.shinyapps.io/mapperalgo/) I build to get familier with the algorithm<br/>
I've written some articles on Medium, which you can find [here](https://medium.com/@kennywang2003) to get familiar with topological data analysis. I'll be continuously updating my work, and I welcome any feedback!

> This package is based on the `TDAmapper` package by Paul Pearson. You can view the original package [here](https://github.com/paultpearson/TDAmapper). Since the original package hasn't been updated in over seven years, this version is focused on optimization. By incorporating vector computation into the Mapper algorithm, this package aims to significantly improve its performance.

## Get started quickly

![Mapper](man/figures/mapper.png) Step visualize from [Skaf et al.](https://doi.org/10.1016/j.jbi.2022.104082)

**Mapper is basically a three-step process:**

1\. **Cover**: This step splits the data into overlapping intervals and creates a cover for the data.

2\. **Cluster**: This step clusters the data points in each interval the cover creates.

3\. **Simplicial Complex**: This step combines the two steps above, which connects the data points in the cover to create a simplicial complex.

> you can know more about the basic here: Chazal, F., & Michel, B. (2021). An introduction to topological data analysis: fundamental and practical aspects for data scientists. Frontiers in artificial intelligence, 4, 667963.

**Besides to the steps above, you can find the following code in the package:**

1.  Mapper.R: Combining the three steps above
2.  ConvertLevelset.R: Converting a Flat Index to a Multi-index, or vice versa.
3.  EdgeVertices.R This is to find the nodes for plot, not for the Mapper algorithm.

### Example

``` r
Mapper <- MapperAlgo(
  filter_values = circle_data[,2:3],
  intervals = 4,
  percent_overlap = 30, 
  methods = "dbscan",
  method_params = list(eps = 0.3, minPts = 5),
  cover_type = 'extension',
  num_cores = 12
  )
MapperPlotter(Mapper, circle_data$circle, circle_data, type = "forceNetwork")
```

<table>
  <tr>
    <td><img src="man/figures/Circle.png" alt="Circle" width="500"/><br/>Figure 1</td>
    <td><img src="man/figures/CircleMapper.png" alt="CircleMapper" width="500"/><br/>Figure 2</td>
  </tr>
</table>