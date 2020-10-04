#' ---
#' title: "Practicals"
#' author: "Olivier Gimenez"
#' output:
#'   pdf_document: default
#'   html_document: default
#' ---
#' 
#' # Practical #1
#' 
#' 1. Use simulation to generate a data set of 100 covariate values from $X \sim \text{Normal}(20, \sigma = 2)$. Use a histogram to explore the distribution of $X$.
#' 
#' 2. Generate a Poisson rate from these values according to the relationship lambda=exp(-3+0.2*X). Have a look graphically to the relationship between $\lambda$ and $X$. Hint: You might want to order the values of $X$ using the R function `sort()`.
#' 
#' 3. Use simulation to generate a data set of 100 response values from a Poisson distribution according to this rate $Y \sim \text{Poisson}(\lambda(X))$. Explore the distribution of $Y$ using a bar plot (`?barplot`). 
#' 
#' 4. Construct a data frame containing the covariate (X) and response (Y) data. Have a look to the first and last rows.
#' 
#' 5. Use a GLM with the appropriate structure to try and retrieve the parameters -3 and 0.2 from question 2.
#' 
#' 6. Increase sample size (multiply by 1000) and comment on the parameter estimates.
#' 
#' 7. Overdispersion. We would like to do the same exercise with an overdispersed Poisson distribution. To do so, we need to generate data from a quasi-Poisson distribution. Interestingly, the negative-binomial (NB) distribution allows relaxing the Poisson assumption $E(Y)=V(Y)=\lambda$. There are several parameterizations for the NB distribution. Here, we will use $W \sim NB(\lambda,\phi)$ where $\lambda$ is the expected value as in the Poisson distribution and $\phi$ is the overdispersion parameter. From the NB properties, we have that the mean of $W$ is $\lambda$ while its variance is $\lambda + \lambda^2/\phi$. Therefore, a small value of $\phi$ means a large deviation from a Poisson distribution (the variance of $W$ is much larger than $\lambda$, which would also be the mean for a Poisson distribution), while as $\phi$ gets larger the NB looks more and more like a Poisson distribution (the term $\lambda^2/\phi$ tends to 0, and the mean and variance looks more and more alike). In R, we will specify:
#' 
## ----------------------------------------------------------------------------
lambda = 2
phi = 5
n = 1000

# simulate the response
w_nb <- rnbinom(n,size=phi,mu=lambda)

# recover the NB parameters from its mean and variance
mean(w_nb) # lambda
mean(w_nb)^2/(var(w_nb)-mean(w_nb)) # phi

#' 
#' Adopt the same approach as above to simulate data from a Poisson distribution with overdispersion parameter $\phi=0.1$. Inspect the residuals. Do they look ok to you? If not, fit a GLM with an appropriate structure. Have a look to the relationship between the response and the predictor for both models, without and with the overdispersion parameter (use the `visreg` from the package `visreg`). Is there any difference?
#' 
#' 
#' # Practical #2
#' 
#' 1. Relationship between tree diameter and height and LMM. You have data on 1000 trees from 10 plots, with 4 up to 392 trees per plot and several measurements taken on each tree. 
#' 
## ----------------------------------------------------------------------------
# dbh is diameter at breast height (diameter of the trunk)
trees <- read.table("trees.txt", header=TRUE)
str(trees)
head(trees)

#' 
#' Fit a linear model with height as the response variable and dbh as a predictor. Plot the data on a graph, and add the regression line (use `abline`).
#' 
#' Now we have only one intercept. What if allometry varies among plots? Fit a linear model with a different intercept for each plot, and inspect the results with `broom::tidy`. Try and represent the 20 regression lines on the same graph.
#' 
#' How many parameters are estimated in the model pooling all plots together vs. the model considering a different intercept for each plot?
#' 
#' Mixed models enable us to make a compromise between the two models by accounting for plot-to-plot variability.
#' 
#' Recall that:
#' $$
#'   \begin{aligned}  
#'   y_{ij} \sim N(\mu_{ij},\sigma^2) \\
#'   \mu{ij} = a_{j} + b x_{i} \\  
#'   a_{j} \sim N\left( \mu_{a},\tau^2 \right) \\  
#'   \end{aligned}  
#' $$
#' 
#' How does this GLMM formulation translate into in our example? Fit model with plot as a random effect. Comment on the outputs. Visualise the effect using `allEffects` and/or `visreg`.
#' 
#' 2. Longitudinal study on coral reef and GLMM. A survey of a coral reef uses 10 predefined linear transects covered by divers once every week. The response variable of interest is the abundance of a particular species of anemone as a function of water temperature. Counts of anemones are recorded at 20 regular line segments along the transect. The following piece of code will generate a data set with realistic properties according to the above design. Make sure you understand what it is doing. You might want to explain the script to the colleague next to you. Also, to try and make sense of the code of others, it is always good to plot and/or run small sections of the code. 
#' 
## ----------------------------------------------------------------------------
transects <- 10
data <- NULL
for (tr in 1:transects){
  ref <- rnorm(1,0,.5) # random effect (intercept)
  t <- runif(1, 18,22) + runif(1,-.2,0.2)*1:20 # water temperature gradient
  ans <- exp(ref -14 + 1.8 * t - 0.045 * t^2) # Anemone gradient (expected response)
  an <- rpois(20, ans) # Actual observations: counts on 20 segments of the current transect
  data <- rbind(data,cbind(rep(tr, 20), t, an))
}

#' 
#' Generate a data set using the anemone code and fit to it the following mixed effects models:
#' 
#' - m1: GLM with temperature
#' 
#' - m2: GLM with quadratic formula in temperature
#' 
#' - m3: GLMM with temperature and random intercept
#' 
#' - m4: GLMM with quadratic temperature and random intercept
#' 
#' Carry out model selection from this set of models. Inspect the temperature effect with `visreg`.
