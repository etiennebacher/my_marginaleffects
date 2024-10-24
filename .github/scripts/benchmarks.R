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
      N = c(10),
      {
        dat <- data.frame(matrix(rnorm(N * 26), ncol = 26))
        mod <- lm(X1 ~ ., dat)
        bench::mark(
          check = FALSE,
          iterations = 5,

          # Slopes =========================================
          # marginal effects at the mean; no standard error
          slopes(mod, vcov = FALSE, newdata = "mean"),
          # marginal effects at the mean
          slopes(mod, newdata = "mean"),
          # 1 variable; no standard error
          slopes(mod, vcov = FALSE, variables = "X3"),
          # 1 variable
          slopes(mod, variables = "X3"),
          # 26 variables; no standard error
          slopes(mod, vcov = FALSE),
          # 26 variables
          slopes(mod),

          # Hypothesis =========================================
          hypotheses(mod, hypothesis = "b3 - b1 = 0"),
          hypotheses(mod, hypothesis = "b2^2 * exp(b1) = 0"),
          hypotheses(mod, hypothesis = ~reference)
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
    `Median time with PR (% change with main)` = median_PR,
    `Memory used with PR (% change with main)` = mem_alloc_PR
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
