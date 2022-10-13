# informationeffects

## Background

In politics like elsewhere, what we know matters for what we want. That’s why political scientists, rightly, are concerned with studying what voters know, and what difference it would make had they known more. Some of this work is experimental. However, for no other reason than resource constraint, a lot of political scientific work on political knowledge uses observational data, generally investigating so-called _information effects_, i.e., differences between actual (reported) preferences, and the preferences people likely would have reported, had they been more politically informed.

The information effects literature makes clear that knowledge can in some cases make a substantial difference, and even change the electoral outcome. Examples include: 

- [Ahlstrom-Vij (2020)](https://www.cambridge.org/core/journals/episteme/article/abs/case-for-modelled-democracy/B57E0E9B282C8E16FC28664F939E8C80) models an informed EU referendum in the UK, and sees the proportion of remain swing from a minority to a majority. 
- [Blais et al. (2009)](https://ejpr.onlinelibrary.wiley.com/doi/abs/10.1111/j.1475-6765.2008.00835.x) simulate the outcome of six past Canadian elections, involving three to four parties, with fully informed voters, and see a likely difference in outcome in one. 
- [Oscarsson (2007)](https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-9477.2007.00182.x) simulates six past Swedish elections, involving eight main parties, and sees a likely difference in outcome in two of them.

## How it works

Modeling of information effects involves a form of counterfactual or causal modeling ([Morgan and Winship 2015](https://www.cambridge.org/core/books/counterfactuals-and-causal-inference/5CC81E6DF63C5E5A8B88F79D45E1D1B7)): a model is fitted, not to make a straightforward prediction (as in predictive modeling), but to estimate how a respondent would have responded, had they been more informed, with reference to some relevant measure of political knowledge (e.g., [Delli Carpini and Keeter 1996](https://yalebooks.yale.edu/book/9780300072754/what-americans-know-about-politics-and-why-it-matters/)). Such an estimation is performed by fitting the model on the relevant data, and then using the model to make a "prediction," once the value on the political knowledge variable for each respondent has been set to whatever value designates being “informed,” thereby estimating what each respondent would have responded, had they been fully informed.

## A complete pipeline for calculating information effects

There is currently no well-established workflow for information effects research. The `informationeffects` package is looking to change that. It offers a complete pipeline for calculating information effects. It includes functions for the following:

- `info_scale()` calculates a knowledge scale using Item Response Theory (IRT) modeling by way of the `mirt` package on the basis of a set of binary knowledge items.
- `info_emmeans()` estimates marginal mean levels of knowledge using the `emmeans` package for different demographic variables in order to evaluate construct validity for the underlying knowledge scale.
- `info_prop_scores()` calculates propensity scores to be used as weights in subsequent, counterfactual modeling, in order to improve balance. 
- `info_effect()` calculates information effects on the basis of survey data with a binary knowledge variable, propensity scores, and survey weights, while controlling for a set of covariates. It can also generate bootstrapped confidence intervals.

## Installation

The package currently only exists in a development version, which can be installed as follows:

```
# install.packages("devtools")
devtools::install_github("ahlstromvij/informationeffects")
```

Please report any bugs or issues [here](https://github.com/ahlstromvij/informationeffects/issues).
