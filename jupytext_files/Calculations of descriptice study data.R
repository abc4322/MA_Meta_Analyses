# -*- coding: utf-8 -*-
# ---
# jupyter:
#   jupytext:
#     formats: ipynb,jupytext_files//R:percent
#     text_representation:
#       extension: .R
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.18.1
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# %% [markdown]
# # # 5114 – Liu 2022

# %%
# install.packages("foreign")
library("foreign")
library("tidyverse")
df <- read.spss("# 5114 – Liu 2022.sav", to.data.frame = TRUE) |> select(c(mindfulness, T2mindfulness, group))
df.mind <- df |> filter(group == "mindfulnees")
df.cont <- df |> filter(group == "control")

# %%
nrow(df.mind); nrow(df.cont)

# %%
print("mindf")
mean(df.mind$mindfulness); sd(df.mind$mindfulness);
mean(df.mind$T2mindfulness); sd(df.mind$T2mindfulness);
print("cont");
mean(df.cont$mindfulness); sd(df.cont$mindfulness);
mean(df.cont$T2mindfulness); sd(df.cont$T2mindfulness)

# %% [markdown]
# # Tsai 2017 - #873

# %%
library("foreign")
library("tidyverse")

# %%
# prefer dplyr packages
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# %%
df <- read.spss("Tsai 2017 - #873.sav", to.data.frame = TRUE)
df

# %%
# generate dfs for control and intervention group of the outcome positive affect for t2 and t3 which are pre- and post-tests in our terms
df.mind <- df |> filter(IND_GRP == "Mindfulness") |> select(POS_AF_2, POS_AF_3)
df.cont <- df |> filter(IND_GRP == "Distraction") |> select(POS_AF_2, POS_AF_3)
df.mind
df.cont

# %%
desc.stat.t0.df <- data.frame(
  mean = c(
    mean(df.mind$POS_AF_2),
    mean(df.cont$POS_AF_2)
  ),
  sd = c(
    sd(df.mind$POS_AF_2),
    sd(df.cont$POS_AF_2)
  ),
  n = c(
    sum(!is.na(df.mind$POS_AF_2)),
    sum(!is.na(df.cont$POS_AF_2))
  ),
  row.names = list("Intervention 1", "Intervention 2")
)
desc.stat.t1.df <- data.frame(
  mean = c(
    mean(df.mind$POS_AF_3),
    mean(df.cont$POS_AF_3)
  ),
  sd = c(
    sd(df.mind$POS_AF_3),
    sd(df.cont$POS_AF_3)
  ),
  n = c(
    sum(!is.na(df.mind$POS_AF_3)),
    sum(!is.na(df.cont$POS_AF_3))
  ),
  row.names = list("Intervention 1", "Intervention 2")
)

print("pre-test")
desc.stat.t0.df
cat("\n")
print("post-test")
desc.stat.t1.df
