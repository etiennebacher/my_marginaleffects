

The `my_marginaleffects` package for `R` and `Python` offers a single point
of entry to easily interpret the results of [over 100 classes of
models,](https://my_marginaleffects.com/vignettes/supported_models.html)
using a simple and consistent user interface. Its benefits include:

-   *Powerful:* It can compute and plot predictions; comparisons
    (contrasts, risk ratios, etc.); slopes; and conduct hypothesis and
    equivalence tests for over 100 different classes of models in `R`.
-   *Simple:* All functions share a simple and unified interface.
-   *Documented*: Each function is thoroughly documented with abundant
    examples. The Marginal Effects Zoo website includes 20,000+ words of
    vignettes and case studies.
-   *Efficient:* [Some
    operations](https://my_marginaleffects.com/vignettes/performance.html)
    can be up to 1000 times faster and use 30 times less memory than
    with the `margins` package.  
-   *Valid:* When possible, [numerical results are
    checked](https://my_marginaleffects.com/vignettes/supported_models.html)
    against alternative software like `Stata` or other `R` packages.
-   *Thin:* The `R` package requires relatively few dependencies.
-   *Standards-compliant:* `my_marginaleffects` follows “tidy” principles
    and returns simple data frames that work with all standard `R`
    functions. The outputs are easy to program with and feed to other
    packages like
    [`ggplot2`](https://my_marginaleffects.com/vignettes/plot.html) or
    [`modelsummary`.](https://my_marginaleffects.com/vignettes/tables.html)
-   *Extensible:* Adding support for new models is very easy, often
    requiring less than 10 lines of new code. Please submit [feature
    requests on
    Github.](https://github.com/vincentarelbundock/my_marginaleffects/issues)
-   *Active development*: Bugs are fixed promptly.

To cite `my_marginaleffects` in publications please use:

Arel-Bundock V, Greifer N, Heiss A (Forthcoming). “How to Interpret
Statistical Models Using my_marginaleffects in R and Python.” *Journal of
Statistical Software*.

A BibTeX entry for LaTeX users is:

``` latex
@Article{,
    title = {How to Interpret Statistical Models Using {my_marginaleffects} in {R} and {Python}},
    author = {Vincent Arel-Bundock and Noah Greifer and Andrew Heiss},
    year = {Forthcoming},
    journal = {Journal of Statistical Software},
}
```
