# Central Limit Theorem Viewer

The Central Limit Theorem Viewer is a web-based application that helps
users understand the idea behind the central limit theorem. The application is built with
R Shiny and deployed using shinylive. Check out the live application at
<https://shiny.thecoatlessprofessor.com/central-limit-theorem/>

With the shiny app, you can:

- Specify the population distribution
- Adjust the sample size and number of samples per draw
- Manually draw samples from the population
- Automatically draw samples from the population 
- Visualize the distributions of the current sample and the sample means

## Deployment

This application is deployed using shinylive, allowing it to run directly in
the browser without requiring an R server. Shinylive converts the R code to 
WebAssembly, making it possible to run R applications entirely client-side.

## Local Development Environment

1. Clone the repository:

```bash
git clone https://github.com/coatless-shiny/central-limit-theorem.git
```

2. Open the `central-limit-theorem.Rproj`

3. Install required R packages:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "bslib", "shinylive", "kableExtra", "DT", "moments", "scales"))
```

4. Run the application:

```r
shiny::runApp()
```

5. Check if the application can be converted to `{shinylive}`:

```r
shinylive::export(".", "_site")
```

## Acknowledgments

- Built using the R Shiny framework
- Uses the bslib package for Bootstrap 5 theming
- Deployed using shinylive for browser-based execution
