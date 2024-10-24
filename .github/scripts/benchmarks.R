library(dplyr)
library(tidyr)
library(tinytable)

# Stored in the Github actions workflow
pr_number <- Sys.getenv("PR_NUMBER")

out <- cross::run(
  pkgs = c("vincentarelbundock/marginaleffects", "vincentarelbundock/marginaleffects#1246"),
  ~ {
    library(marginaleffects)
    library(data.table)

    bench::press(
      N = 75000,
      {
        dat <- data.frame(matrix(rnorm(N * 26), ncol = 26))
        mod <- lm(X1 ~ ., dat)

        set.seed(1234)
        dat2 <- data.frame(
          outcome = sample(0:1, N, TRUE),
          incentive = runif(N),
          agecat = sample(c("18-35", "35-60", "60+"), N, TRUE),
          distance = sample(0:10000, N, TRUE)
        )

        mod2 <- glm(outcome ~ incentive * (agecat + distance),
          data = dat2, family = binomial
        )
        grid <- data.frame(distance = 2, agecat = "18-35", incentive = 1)

        bench::mark(
          check = FALSE,
          iterations = 20,

          # Slopes =========================================
          slopes(mod, vcov = FALSE, newdata = "mean"),
          slopes(mod, newdata = "mean"),
          slopes(mod, vcov = FALSE, variables = "X3"),
          slopes(mod, variables = "X3"),
          slopes(mod, vcov = FALSE),
          slopes(mod),

          # Hypothesis =========================================
          hypotheses(mod, hypothesis = "b3 - b1 = 0"),
          hypotheses(mod, hypothesis = "b2^2 * exp(b1) = 0"),
          hypotheses(mod, hypothesis = ~reference),

          # Predictions =========================================
          predictions(mod),
          predictions(mod, newdata = "mean"),
          predictions(mod, newdata = datagrid(X2 = unique)),

          # Comparisons =========================================
          comparisons(mod2),
          comparisons(mod2, comparison = "dydxavg"),
          comparisons(mod2, comparison = "eydxavg"),
          comparisons(mod2, comparison = "ratioavg")
        )
      }
    )
  }
)

unnested <- out |>
  mutate(
    pkg = case_match(
      pkg,
      "vincentarelbundock/marginaleffects" ~ "main",
      "vincentarelbundock/marginaleffects#1246" ~ "PR"
    )
  ) |>
  unnest(result) |>
  mutate(
    expression = as.character(expression),
    # Get duration in seconds
    median = round(as.numeric(median), 3),
    # Get memory in MB
    mem_alloc = round(as.numeric(mem_alloc) / 1000000, 3)
  ) |>
  select(pkg, expression, median, mem_alloc)

final <- unnested |>
  pivot_wider(
    id_cols = expression,
    names_from = pkg,
    values_from = c(median, mem_alloc)
  ) |>
  mutate(
    median_diff_main_pr = round((median_PR - median_main) / median_main * 100, 2),
    median_PR = case_when(
      median_diff_main_pr >= 2 ~ paste0(
        ":collision: ", median_PR, " (", median_diff_main_pr, "%)"
      ),
      median_diff_main_pr < 2 & median_diff_main_pr > -2 ~ paste0(
        median_PR, " (", median_diff_main_pr, "%)"
      ),
      median_diff_main_pr <= -2 ~ paste0(
        ":zap: ", median_PR, " (", median_diff_main_pr, "%)"
      ),
      .default = NA
    ),
    mem_alloc_diff_main_pr = round((mem_alloc_PR - mem_alloc_main) / mem_alloc_main * 100, 2),
    mem_alloc_PR = paste0(mem_alloc_PR, " (", mem_alloc_diff_main_pr, "%)")
  ) |>
  select(
    Expression = expression,
    `Median time with PR (% change with main), seconds` = median_PR,
    `Memory used with PR (% change with main), MB` = mem_alloc_PR
  )

raw_table <- tt(final) |>
  save_tt("gfm")

paste0(
  "**Benchmark results**\n\n",
  ":collision: means that PR is more than 2% slower than main\n",
  ":zap: means that PR is more than 2% faster than main\n",
  "<details>\n<summary>Click to see benchmark results</summary>\n\n",
  raw_table,
  "\n\n</details>"
) |>
  writeLines("report.md")
