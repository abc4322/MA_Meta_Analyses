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

# %% vscode={"languageId": "r"}
# # Build R environment (use the renv_library.tar.gz file you can download from the URL in download_link_to_renv_library.tar.gz.txt)
# # --------------------------------------------------------------
# #  restore_target_renv.R
# #  Purpose: Recreate the exact R + package environment captured
# #           on the source machine using the renv lockfile.
# # --------------------------------------------------------------

# # ---- 1. Verify that you are using the same R version ----------------
# #    renv will warn you if the major/minor version differs.
# current_R_version <- as.character(getRversion())
# cat("Running R version:", current_R_version, "\n")
# if (current_R_version != "4.3.0") {
#   stop("⚠️ Warning: This R version does not match the source machine (R 4.3.0).\n")
# } else {
#   cat("✅ R version matches the source machine (R 4.3.0).\n")
# }

# # ---- 2. Install renv (needed only once per R installation) --------
# if (!requireNamespace("renv", quietly = TRUE)) {
#   install.packages("renv", repos = "https://cloud.r-project.org")
# }

# # ---- 3. Activate the renv project ----------------------------------
# #    This sets .libPaths() to point at the private library that will
# #    be populated by renv::restore().
# renv::activate()

# # ---- 4. OPTIONAL: If you received a pre‑built library archive -------
# #    Extract it so renv can reuse the binaries instead of downloading.
# #    Uncomment and adjust the path if you shipped `renv_library.tar.gz`.

# archive_path <- "renv_library.tar.gz"
# if (file.exists(archive_path)) {
#   utils::untar(archive_path, exdir = ".")
#   cat("\n🔧 Extracted pre‑built library archive.\n")
# }

# # # ---- 5. Restore the exact package snapshot -------------------------
# # #    renv reads renv.lock, resolves the exact versions, and installs
# # #    them (from CRAN/Bioconductor/GitHub caches or from the extracted
# # #    library if you provided one).
# # renv::restore(prompt = FALSE)   # prompt = FALSE skips the interactive yes/no

# # cat("\n✅ Environment restored! Your project now uses the same\n",
# #     "R version and package versions as the source machine.\n")

# %% [markdown] vscode={"languageId": "r"}
# ToDos:
# - [x] Assign meditation techniques to categories (e.g., focused attention, open monitoring, loving-kindness, body scan, etc.)
# - [x] Assign Scales to outcomes
# - [x] Implement additional outcomes and interventions from theresa
# - [x] fix update.meta bug
# - [] Define outliers and influential cases
# - [] Define studies causing inconsistency
# - [x] Check if addition of 6th intervention affects the med.vec.list variable

# %% [markdown]
# # Preprocess Data
#

# %% vscode={"languageId": "r"}
raw.df <- read.csv("2025_12_27_Data Extraction.csv")

# %% vscode={"languageId": "r"}
options(repr.matrix.max.rows=5, repr.matrix.max.cols=5)  # limit output display so notebook does not get overloaded

# %% vscode={"languageId": "r"}
# install.packages("sjmisc")
# install.packages("tidyverse")
# install.packages("abind")
# install.packages("qdapRegex")
library("qdapRegex")  # for removing and replacing substrings within specific boundaries
library(conflicted)  # solve tidyverse package conflicts with conflict_prefer()
library(tidyverse)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library("sjmisc")
library("abind")
# for data manipulation with dplyr see: https://www.youtube.com/watch?v=Gvhkp-Yw65U
# for splitting 2 values in 1 cell see: https://www.youtube.com/watch?v=DiY8EqZDwoI at 3:17 (e.g. if 2 scales for 1 outcome)
# for joining 2 data frames see:        https://www.youtube.com/watch?v=DiY8EqZDwoI at 11:57


# %% [markdown] heading_collapsed=true
# ## Drop unimportant columns by name

# %% [markdown] heading_collapsed=true hidden=true
# ### Remove rows

# %% hidden=true vscode={"languageId": "r"}
my.df <- raw.df  # %>%
#  select(-Reviewer.Name:-Further.Information.inserted.in.Extraction.Form.)  # "-" indicates deleting these columns

# %% hidden=true vscode={"languageId": "r"}
my.df[my.df$Covidence.. == 5658,"Study.ID"] <- "Pogrebtsova 2022"  # create study label for "Pogrebtsova 2022"
my.df <- my.df |> arrange(Study.ID, Reviewer.Name)
my.df

# %% hidden=true vscode={"languageId": "r"}
# delete all data from only one reviewer if consensus is present and keep Robins data if consensus is not present
for (study in unique(my.df$Covidence..)){
  studies.per.lab.df <- my.df |> filter(Covidence.. == study)
  if ("Consensus" %in% studies.per.lab.df$Reviewer.Name){
    my.df <- my.df |> filter(!(Covidence.. == study & Reviewer.Name != "Consensus"))
  } else if (all(!c("Consensus", "Robin") %in% studies.per.lab.df$Reviewer.Name)) {
    # Remove all but the first reviewer's data if no consensus or Robin Jacob data is present
    first_reviewer <- studies.per.lab.df$Reviewer.Name[1]
    my.df <- my.df |> filter(!(Covidence.. == study & Reviewer.Name != first_reviewer))
  } else {
    my.df <- my.df |> filter(!(Covidence.. == study & Reviewer.Name != "Robin Jacob"))
  }
}
my.df

# %% hidden=true vscode={"languageId": "r"}
# check if study labels are unique
unique(table(my.df$Study.ID))

# %% hidden=true vscode={"languageId": "r"}
# find out duplicate labes
table(my.df$Study.ID)[table(my.df$Study.ID) == 2]

# %% hidden=true vscode={"languageId": "r"}
my.df |> filter(Study.ID %in% c("Flett 2019", "Pilcher 2025"))

# %% hidden=true vscode={"languageId": "r"}
# adjust labels
my.df[my.df$Covidence.. == 3369, "Study.ID"] <- "Flett 2019b"
my.df[my.df$Covidence.. == 3120, "Study.ID"] <- "Flett 2019a"
my.df[my.df$Covidence.. == 10674, "Study.ID"] <- "Pilcher 2025b"
my.df[my.df$Covidence.. == 10673, "Study.ID"] <- "Pilcher 2025a"
my.df |> filter(Covidence.. %in% c(3369, 3120, 10674, 10673))

# %% vscode={"languageId": "r"}
# Check again if study labels are unique and what is the new number of unique study labels
unique(table(my.df$Study.ID))
my.df$Study.ID %>% n_distinct()

# %% [markdown] heading_collapsed=true hidden=true
# ### Remove single column names and repeating names with ascending numbers (table headlines)

# %% hidden=true vscode={"languageId": "r"}
cols.pop.char <- c()

for (time.point in 0:4){
  cols.pop.char <- append(cols.pop.char,
                          sprintf("Population.Characteristics..Time.Point.%d..",
                                  time.point
                                  )
                          )
}

cols.outc.t <- c()

for (outc in 1:7){
  for (t in 0:3){
    cols.outc.t <- append(cols.outc.t,
                          sprintf("Outcome.%d..Time.Point.%d..",
                                  outc, t
                                  )
                          )
  }
}

cols.quant.results <- c()

i <- 0
for (int.1 in 1:3){
  i <- i + 1
  if (i == 1){
    for (int.2 in 2:4){
      for (t in 1:3){
        cols.quant.results <- append(cols.quant.results,
                              sprintf("Results.for.Int%d.X.Int%d.of.Time.Point.%d.",
                                      int.1, int.2, t
                                      )
                              )
      }
    }
  } else if (i == 2){
    for (int.2 in 3:4){
      for (t in 1:3){
        cols.quant.results <- append(cols.quant.results,
                              sprintf("Results.for.Int%d.X.Int%d.of.Time.Point.%d.",
                                      int.1, int.2, t
                                      )
                              )
      }
    }
  } else if (i == 3){
    int.2 <- 4
    for (t in 1:3){
      cols.quant.results <- append(cols.quant.results,
                            sprintf("Results.for.Int%d.X.Int%d.of.Time.Point.%d.",
                                    int.1, int.2, t
                                    )
                            )
    }
  } else {
    print("i not = 1, 2, or 3")
  }
}

remove.cols <- c(cols.pop.char[],
                 "Dates.of.Measuring.Time.Points.",
                 "Between.Measuring.Time.Points..Duration.",
                 "Intervention.and.Comparisons.",
                 "Definition.of.Outcomes.",
                 "Measures.of.Outcomes.",
                 cols.outc.t[],
                 cols.quant.results[],
                 "Results.of.Key.Themes."
                 )

my.df <- my.df[, ! names(my.df) %in% remove.cols]


# %% vscode={"languageId": "r"}
my.df %>% head()

# %% [markdown] heading_collapsed=true hidden=true
# ### Remove specific values

# %% hidden=true vscode={"languageId": "r"}
# # remove commas of scale names that are in between parentheses
# ## get colnames of cells in scale name table
# scale.colnames <- my.df |>
#   select(Outcome.1.Scale.s.or.other.Measure.s.Name:Outcome.7.Scale.s.or.other.Measure.s.Name) |>
#   colnames()

# ## remove
# for (col in scale.colnames){
#   for (row in 1:nrow(my.df)){
#     # get the substring between first ( and first ) if present, if not NA
#     chars.between.par <- ex_between(my.df[row, col], "(", ")")[[1]][1]
#     if (grepl(",", chars.between.par)){
#       # remove commas between parentheses
#       chars.between.par.nc <- gsub(',', '', chars.between.par)
#       my.df[row, col] <- rm_between(my.df[row, col], "(", ")", replacement = chars.between.par.nc)
#     }
#   }
# }

# %% hidden=true vscode={"languageId": "r"}
# remove more unwanted specific commas
my.df[44, "Outcome.2.Scale.s.or.other.Measure.s.Name"] <-
  "Depression Anxiety and Stress Scale – 21 (DASS-21) subscale depression"
my.df[44, "Outcome.3.Scale.s.or.other.Measure.s.Name"] <-
  "Depression Anxiety and Stress Scale – 21 (DASS-21) subscale anxiety"
my.df[44, "Outcome.4.Scale.s.or.other.Measure.s.Name"] <-
  "Depression Anxiety and Stress Scale – 21 (DASS-21) subscale stress"
my.df[4, "Outcome.3.Scale.s.or.other.Measure.s.Name"] <-
"Depression Anxiety and Stress Scale (DASS), Perceived Stress Scale (PSS)"

# %% vscode={"languageId": "r"}
# Adjust my.df for systematic fixes
my.df <- my.df %>%
  mutate(
    # Remove "Other:" outcomes
    across(
      starts_with("Name.of.Outcome."),
      ~ case_when(
        grepl("Other:", .x) ~ NA_character_,
        TRUE ~ .x
      )
    ),
    # Adjust duration of intervention sessions
    Duration.of.single.intervention.sessions.in.minutes.Intervention.1 = case_when(
      Duration.of.single.intervention.sessions.in.minutes.Intervention.1 == "600 (five two hour sessions)" ~ "120",
      TRUE ~ Duration.of.single.intervention.sessions.in.minutes.Intervention.1
    ),
    # Remove commas from scale names
    across(
      ends_with("Scale.s.or.other.Measure.s.Name"),
      ~ case_when(
        .x == "DASS - Depression, Anxiety, and Stress Scale" ~ "DASS - Depression Anxiety and Stress Scale",
        .x == "10- item Perceive Stress scale (PSS, Klein et al. 2016)" ~ "The Perceived Stress Scale (PSS)",
        TRUE ~ .x
      )
    )
  )

# %% [markdown] heading_collapsed=true
# ## Create arrays, data frame lists, and data frames from Covidence tables

# %% [markdown] heading_collapsed=true hidden=true
# ### Set basic parameters 

# %% hidden=true vscode={"languageId": "r"}
nm.placeholder <- -999  # placeholder for values marked as nm (not mentioned)

study.no <- length(my.df[,"Study.ID"])
study.no

# %% [markdown] heading_collapsed=true hidden=true
# ### Functions

# %% [markdown] heading_collapsed=true hidden=true
# #### Checking for digits and characters

# %% hidden=true vscode={"languageId": "r"}
# function returning TRUE if string contains no characters but digits instead
no.char.but.digit.inside <- function(value){
  !(str_contains(value, letters, logic = "OR") |
  str_contains(value, LETTERS, logic = "OR")) &
  grepl("\\d", value)
}

# function returning TRUE if string contains characters
char.inside <- function(value){
  str_contains(value, letters, logic = "OR") |
  str_contains(value, LETTERS, logic = "OR")
}


# %% [markdown] heading_collapsed=true hidden=true
# #### For mean values

# %% hidden=true vscode={"languageId": "r"}
# extracts mean r and mean s values as double
get.all.means <- function(value, study.name = ""){
  if (substr(value, nchar(value) - nchar("mean r") + 1, nchar(value)) == "mean r"){
      # value contains "mean r" at the end
    extracted.value <- as.double(sub(" mean r.*", "", value))
        # extracts anything before " mean r" as double                 
  } else if (substr(value, nchar(value) - nchar("mean s") + 1, nchar(value)) == "mean s"){
    extracted.value <- as.double(sub(" mean s.*", "", value))
  } else if (substr(value, nchar(value) - nchar("mean") + 1, nchar(value)) == "mean"){
    extracted.value <- NA
    print(study.name)
    cat("value ", value, " not added because mean only")
    cat("\n")
  } else {
    extracted.value <- value
  }
  extracted.value
}

# extracts mean s only as double
get.mean.s.only <- function(value){
    if (grepl("mean s", value)){
    extracted.value <- as.double(sub(" mean s.*", "", value))
  } else if (grepl("mean r", value)){
    extracted.value <- NA
    cat("value ", value, " not added because mean r")
    cat("\n")
  } else if (grepl("mean", value)){
    extracted.value <- NA
    cat("value ", value, " not added because mean only")
    cat("\n")
  } else {
    extracted.value <- value
  }
  extracted.value
}


# %% [markdown] heading_collapsed=true hidden=true
# #### Convert value from nm, NA, digit, or char

# %% hidden=true vscode={"languageId": "r"}
convert.value <- function(
  value, missing.value.placeholder = nm.placeholder, only.double = TRUE, exclude.greater.than = FALSE, study.name = "insert study name",
  flag.x.s.r = 2
    # x.s.r = exact value (0), mean s (1), or mean r (2)
    # flag.x.s.r = 2 --> include all
    # flag.x.s.r = 1 --> only include mean s and exact values
    # flag.x.s.r = 0 --> include only exact vaules
){
  
  na.value <- FALSE
  if (is.na(value)){
    na.value <- TRUE
  }
  if (na.value == TRUE){
    extracted.value <- NA
  } else {
    value <- as.character(value)

# special cases
    if (value == "nm"){
      extracted.value <- missing.value.placeholder
    } else if (value == ""){
      extracted.value <- NA
    } else if (value %in% c("^", "^, ^", "v, ^", "^, v")){
      extracted.value <- value 
    } else if (grepl("<", value)){
      extracted.value <- as.double(sub(".*<", "", value))
        # extracts value after "<" as double 
    } else if (grepl(">", value)){
      if (exclude.greater.than == TRUE){
        extracted.value <- NA
      } else if (exclude.greater.than == FALSE){
        extracted.value <- as.double(sub(".*>", "", value))
        # extracts value after ">" as double
      } else {
        print("set 'exclude.greater.than' to 'FALSE' or 'TRUE'")
      }

# no.char.but.digit
    } else if (no.char.but.digit.inside(value)){
      if (grepl(",", value)){
        cat("decimal separator replaced with point in ", value)
        extracted.value <- as.double(sub(",", ".", value))
          # sub(a, b, value) replaces a with b in value
      } else if (grepl("-", value)){
        # like "0 - 40"
        extracted.value <- value
      } else {
        extracted.value <- as.double(value) 
      }

# char
    } else if (char.inside(value)){
      if (flag.x.s.r == 2){
        extracted.value <- get.all.means(value, study.name)
      } else if (flag.x.s.r == 1){
        extracted.value <- get.mean.s.only(value)
      } else if ((flag.x.s.r == 0) & !(grepl("mean r", value) | grepl("mean s", value))){
        extracted.value <- NA
        if (grepl("mean", value)){
          cat("value not added because mean only: ", value)
          cat("\n\n")
        }
      } else {
        if (only.double == TRUE){
          extracted.value <- NA
          cat("unknown case with char inside: ", value)
          cat("\n\n")
        } else {
          extracted.value <- value
        }
      }
    } else {
      cat("unknown case: ", value)
      cat("\n")
    }
  }
  extracted.value
}


# %% [markdown] heading_collapsed=true hidden=true
# #### Check if multiple commas are present in string

# %% hidden=true vscode={"languageId": "r"}
multiple.commas.present <- function(input.string){
  multiple.commas.present <- FALSE
  comma.count <- 0

  iterable.string <- tryCatch(
    {
      strsplit(input.string, "")[[1]]
        # strsplit(input.string, "")[[1]] makes string iterable
        # "try" part
    },
    error=function(cond) {
      message(paste("Error of multiple.commas.present() for input:", input.string, "| Type:", class(input.string)))
      message(cond)
      return("")
      # "except" part for errors
    },
    warning=function(cond) {
      message(paste("Warning of multiple.commas.present() for input:", input.string, "| Type:", class(input.string)))
      message(cond)
      return("")
      # "except" part for warnings
    }
  )
    # see following link for exception handling: https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r

  for (i in iterable.string){
    if (i == ","){
      comma.count <- comma.count + 1
    }
  }
  if (comma.count > 1){
    multiple.commas.present <- TRUE
  }
  multiple.commas.present
}


# %% [markdown] heading_collapsed=true hidden=true
# #### For arrays

# %% hidden=true vscode={"languageId": "r"}
clean.and.shape.data.to.array <- function(
  my.df, start, end, dims, dimname.list, nm.placeholder, study.no, exclude.greater.than = FALSE
){
  my.array <- array(rep(NA),
                    dim = dims,
                    dimnames = dimname.list
                    )
  my.data <- my.df %>%
    select(start:end)
  
  if (length(dims) == 6){
    for (study in 1:study.no){
      my.data.col.no = 1
      for (dim.4.elmnt in 1:dims[4]){
        for (mtrx in 1:dims[3]){
          for (row in 1:dims[1]){
            for (col in 1:dims[2]){
              value <- my.data[study, my.data.col.no]
              my.data.col.no <- my.data.col.no + 1
              if (grepl(",", value)){
                if (!multiple.commas.present(value)){
                  value.before.comma <- sub(",.*", "", value)
                    # extracts anything before "," as double
                  my.array[row, col, mtrx, dim.4.elmnt, 1, study] <- convert.value(
                    value.before.comma, nm.placeholder, exclude.greater.than = exclude.greater.than
                    )
                  value.after.comma <- sub(".*,", "", value)
                    # extracts anything after "," as double
                  my.array[row, col, mtrx, dim.4.elmnt, 2, study] <- convert.value(
                    value.after.comma, nm.placeholder, exclude.greater.than = exclude.greater.than
                    )
                } else {
                    cat(value, "has multiple commas, find solution")
                  }
              } else {
                my.array[row, col, mtrx, dim.4.elmnt, 1, study] <- convert.value(
                  value, nm.placeholder, exclude.greater.than = exclude.greater.than
                  )
              }
            }
          }
        }
      }
    }
  } else if (length(dims) <= 4){
    for (study in 1:study.no){
      my.data.col.no = 1
      for (mtrx in 1:dims[3]){
        for (row in 1:dims[1]){
          for (col in 1:dims[2]){
            value <- my.data[study, my.data.col.no]
            my.data.col.no <- my.data.col.no + 1
            my.array[row, col, mtrx, study] <- convert.value(
              value, nm.placeholder, exclude.greater.than = exclude.greater.than
              )
          }
        }
      }
    }
  } else {
    print("unknwon dimesion")
  }
  my.array
}


# %% [markdown] heading_collapsed=true hidden=true
# #### For data frame lists

# %% hidden=true vscode={"languageId": "r"}
# create data frames out of 2D Tables (with rows and cols swapped)
clean.data.to.df.list.swap <- function(
  my.df, start, end, dims, study.names, dimname.list, check.multiple.commas = FALSE, flag.x.s.r = 2
  ){
  study.df <- data.frame(matrix(
    rep(NA),
    nrow = dims[1],
    ncol = dims[2]
  ))
  row.names(study.df) <- dimname.list[[1]]
  colnames(study.df) <- dimname.list[[2]]
  
  # create list of NA data frames of preferred shape
  study.df.list <- list()
  for (i in 1:length(study.names)){
    study.df.list <- append(study.df.list, list(study.df), 0)
  }
  names(study.df.list) <- study.names
  
  my.data <- my.df %>%
    select(start:end)
  
  # vector of my.data.col.no to swapped rows and cols of Covidence output
  my.data.col.no.swapped <- c()
  i <- 0
  j <- 1
  for (k in 1:dims[1]){
    i <- 0
    for (l in 1:dims[2]){
      my.data.col.no.swapped <- append(my.data.col.no.swapped, i * dims[1] + j)
      i <- i + 1
    }
    j <- j + 1 
  }
  # dims[1] = rows new, cols old
  # dims[2] = cols new, rows old
  
  # e.g., filling pattern for old 4 x 7 and new 7 x 4 data frame 
  ## 1, dims[1]+1, 2*dims[1]+1, 3*dims[1]+1, 4*dims[1]+1, 5*dims[1]+1, 6*dims[1]+1,
  ## 2, dims[1]+2, 2*dims[1]+2, 3*dims[1]+2, 4*dims[1]+2, 5*dims[1]+2, 6*dims[1]+2,
  ## x, x, x, x, x, x, x,
  ## x, x, x, x, x, x, x
  
  for (study in 1:study.no){
    m <- 1
    for (row in 1:dims[1]){
      for (col in 1:dims[2]){
        value <- my.data[study, my.data.col.no.swapped[m]]
        m <- m + 1

        if (check.multiple.commas & is.character(value)){
          if (multiple.commas.present(value)){
            cat("multiple commas in value, find solution:\n", value, "\n")
          }
        }
        
        study.df.list[[study]][row, col] <- convert.value(
          value, nm.placeholder, only.double = FALSE, study.name = study.names[study], flag.x.s.r
        )
      }
    }
  }
  study.df.list
}


# %% hidden=true vscode={"languageId": "r"}
# create data frames out of 2D Tables
clean.data.to.df.list <- function(
  my.df, start, end, dims, study.names, dimname.list, check.multiple.commas = FALSE, flag.x.s.r = 2
){
  study.df <- data.frame(matrix(
    rep(NA),
    nrow = dims[1],
    ncol = dims[2]
  ))
  row.names(study.df) <- dimname.list[[1]]
  colnames(study.df) <- dimname.list[[2]]
  
  # create list of NA data frames of preferred shape
  study.df.list <- list()
  for (i in 1:length(study.names)){
    study.df.list <- append(study.df.list, list(study.df), 0)
  }
  names(study.df.list) <- study.names
  
  my.data <- my.df %>%
    select(start:end)
  
  for (study in 1:study.no){
    my.data.col.no <- 1
    for (row in 1:dims[1]){
      for (col in 1:dims[2]){
        value <- my.data[study, my.data.col.no]
        my.data.col.no <- my.data.col.no + 1
        
        if (check.multiple.commas & is.character(value)){
          if (multiple.commas.present(value)){
            cat("multiple commas in value, find solution:\n", value, "\n\n")
          }
        }
        tryCatch(
          {
            study.df.list[[study]][row, col] <- convert.value(
              value, nm.placeholder, only.double = FALSE, flag.x.s.r
              )
          },
          error=function(e){
            cat("\n")
            print(study.names[study])
            print(value)
            print(e)
            cat("\n")
          },
          warning=function(w){
            cat("\n")
            print(study.names[study])
            print(value)
            print(w)
            cat("\n")
          }
        )
      }
    }
  }
  study.df.list
}


# %% [markdown] hidden=true
#
# #### Print array without NA values
#

# %% hidden=true vscode={"languageId": "r"}
print.array.not.na <- function(input.array){
  if (length(dim(input.array)) == 6){
    for (dim.6.element in 1:dim(input.array)[6]){
      cat("\n###### ", dimnames(input.array)[[6]][dim.6.element], " ######")
      for (dim.5.element in 1:dim(input.array)[5]){
        cat("\n### ", dimnames(input.array)[[5]][dim.5.element], " ###")
        for (dim.4.element in 1:dim(input.array)[4]){
          cat("\n#", dimnames(input.array)[[4]][dim.4.element], "\n")
          for (mtrx in 1:dim(input.array)[3]){
            mtrx.is.na <- TRUE
            for (col in 1:dim(input.array)[2]){
              for (row in 1:dim(input.array)[1]){
                if (!is.na(input.array[
                  row, col, mtrx, dim.4.element, dim.5.element, dim.6.element
                                      ]
                          )
                   ){
                     mtrx.is.na <- FALSE
                }
              }
            }
            if (mtrx.is.na == FALSE){
              print(dimnames(input.array)[[3]][mtrx])
              print(input.array[,,mtrx, dim.4.element, dim.5.element, dim.6.element])
              cat("\n")
            }
          } 
        }
      }
    }
  } else if (length(dim(input.array)) == 5){
    for (dim.5.element in 1:dim(input.array)[5]){
      cat("\n### ", dimnames(input.array)[[5]][dim.5.element], " ###")
      for (dim.4.element in 1:dim(input.array)[4]){
        cat("\n#", dimnames(input.array)[[4]][dim.4.element], "\n")
        for (mtrx in 1:dim(input.array)[3]){
          mtrx.is.na <- TRUE
          for (col in 1:dim(input.array)[2]){
            for (row in 1:dim(input.array)[1]){
              if (!is.na(input.array[
                row, col, mtrx, dim.4.element, dim.5.element
                                    ]
                        )
                  ){
                    mtrx.is.na <- FALSE
              }
            }
          }
          if (mtrx.is.na == FALSE){
            print(dimnames(input.array)[[3]][mtrx])
            print(input.array[,,mtrx, dim.4.element, dim.5.element])
            cat("\n")
          }
        } 
      }
    } 
  } else if (length(dim(input.array)) == 4){
    for (dim.4.element in 1:dim(input.array)[4]){
      cat("\n#", dimnames(input.array)[[4]][dim.4.element], "\n")
      for (mtrx in 1:dim(input.array)[3]){
        mtrx.is.na <- TRUE
        for (col in 1:dim(input.array)[2]){
          for (row in 1:dim(input.array)[1]){
            if (!is.na(input.array[
              row, col, mtrx, dim.4.element
                                  ]
                      )
                ){
                  mtrx.is.na <- FALSE
            }
          }
        }
        if (mtrx.is.na == FALSE){
          print(dimnames(input.array)[[3]][mtrx])
          print(input.array[,,mtrx, dim.4.element])
          cat("\n")
        }
      } 
    }
  } else if (length(dim(input.array)) == 3){
    for (mtrx in 1:dim(input.array)[3]){
      mtrx.is.na <- TRUE
      for (col in 1:dim(input.array)[2]){
        for (row in 1:dim(input.array)[1]){
          if (!is.na(input.array[
            row, col, mtrx
                                ]
                    )
              ){
                mtrx.is.na <- FALSE
          }
        }
      }
      if (mtrx.is.na == FALSE){
        print(dimnames(input.array)[[3]][mtrx])
        print(input.array[,,mtrx])
        cat("\n")
      }
    }
  } else {
    print("unknown case")
  }
}


# %% [markdown] heading_collapsed=true hidden=true
# #### Print df list without NA values

# %% hidden=true vscode={"languageId": "r"}
# # does not work yet
# print.df.list.not.na <- function(df.list, nm.placeholder){
#   i = 1
#   for (df in df.list){
#     all.na.flag = TRUE
#     for (row in 1:nrow(df)){
#       for (col in 1:ncol(df)){
#         if (
#           !(
#             is.na(df[row, col]) |
#             df[row, col] %in% c("NA", nm.placeholder, as.character(nm.placeholder))
#           )
#         ){
#           all.na.flag = FALSE
#         }
#       }
#     }
#     if (all.na.flag){
#       print(names(df.list)[i])
#       print(df)
#       cat("\n")
#     }
#     i = i + 1
#   }
# }

# %% [markdown] heading_collapsed=true hidden=true
# #### Search in df list

# %% hidden=true vscode={"languageId": "r"}
search.df.list <- function(df.list, search.string){
  found <- FALSE
  for (i in 1:length(df.list)){
    for (row in 1:nrow(df.list[[i]])){
      for (col in 1:ncol(df.list[[i]])){
        cell.string <- as.character(df.list[[i]][row,col])
        if (!is.character(search.string)){
          print("search term has to be character")
        }
        if (grepl(search.string, cell.string)){
          cat(
            "search term found in:\ndf:", names(df.list)[i], "\nrow:",
            rownames(df.list[[i]])[row], "\ncol:", colnames(df.list[[i]])[col],
            "\nvalue:", df.list[[i]][row, col], "\n\n"
            )
          found <- TRUE
        }
      }
    }
  }
  if (!found){
    print("serach string not found")
  }
}


# %% hidden=true vscode={"languageId": "r"}
# function checking if value is NA or nm.placeholder
is.na.or.nm <- function(val){
  is.na(val) | val %in% c("NA", nm.placeholder, as.character(nm.placeholder))
}

# %% [markdown] heading_collapsed=true hidden=true
# #### replace value in data frame col

# %% hidden=true vscode={"languageId": "r"}
replace_val_in_df <- function(df, col, search, replace){
  df[
    which(grepl(search, df[,col])),
    col
  ] <- replace
  df
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Array

# %% [markdown] heading_collapsed=true hidden=true
# #### Population Characteristics

# %% hidden=true vscode={"languageId": "r"}
dims <- c(4, 5, 4, study.no)

dimname.list <- list(c("No.Participants", "Mean.Age", "No.Females", "No.Males"),
                     c("Intervention.1", "Intervention.2", "Intervention.3", "Intervention.4", "Over.All"),
                     c("T0", "T1", "T2", "T3"),
                     my.df[,"Study.ID"]
                     )

start <- "Number.of.Participants.Intervention.1.T0"
end <- "Number.of.Males.Over.All.T3"

population.characteristics.array <- clean.and.shape.data.to.array(
  my.df, start, end, dims, dimname.list, nm.placeholder, study.no
)

# add 5th and 6th interventions to array that were not expected to be present but were in some studies
## create empty array with two more levels at the second dimension (interventions)
temp.array <- array(NA, dim=c(4, 7, 4, study.no))

## insert the previous array
temp.array[,1:5,,] <- population.characteristics.array
population.characteristics.array <- temp.array

## set interventions in correct positions (5th intervention at position 5, Over.All at position 7)
population.characteristics.array[,7,,] <- temp.array[,5,,]  # Move Over.All to position 7
population.characteristics.array[,5,,] <- NA  # Clear position 5 for Intervention.5
population.characteristics.array[,6,,] <- NA  # Clear position 6 for Intervention.6

## add dimnames to new array
dimname.list <- list(
  c("No.Participants", "Mean.Age", "No.Females", "No.Males"),
  c("Intervention.1", "Intervention.2", "Intervention.3", "Intervention.4", "Intervention.5", "Intervention.6", "Over.All"),
  c("T0", "T1", "T2", "T3"),
  my.df[,"Study.ID"]
)
dimnames(population.characteristics.array) <- dimname.list

my.df <- my.df %>%
  select(-Number.of.Participants.Intervention.1.T0:-Number.of.Males.Over.All.T3)

print.array.not.na(population.characteristics.array)


# %% vscode={"languageId": "r"}
# Show names of dimensions for population.characteristics.array to understand indexing below
dimnames(population.characteristics.array)

# %% hidden=true vscode={"languageId": "r"}
# add number of participants at pre-test of 5th intervention for "Klibert 2022"
population.characteristics.array[1,5,1,"Klibert 2022"] <- 30
print.array.not.na(population.characteristics.array[,,,"Klibert 2022"])

# add number of participants at pre-test of 5th and 6th intervention for "Nath 2023"
population.characteristics.array[1,5:6,1,"Nath 2023"] <- c(13, 13)
print.array.not.na(population.characteristics.array[,,,"Nath 2023"])

# %% [markdown] heading_collapsed=true hidden=true
# #### Results Descriptive

# %% hidden=true vscode={"languageId": "r"}
dims <- c(4, 3, 4, 7, 2, study.no)

dimname.list <- list(c("Intervention.1", "Intervention.2", "Intervention.3", "Control"),
                     c("Mean", "SD", "n"),
                     c("T0", "T1", "T2", "T3"),
                     c("Outcome.1", "Outcome.2", "Outcome.3", "Outcome.4", "Outcome.5", "Outcome.6", "Outcome.7"),
                     c("Scale.1", "Scale.2"),
                     my.df[,"Study.ID"]
                     )

start <- "Intervention.1.Mean.O1T0"
end <- "Control.or.Intervention.4.n.in.case.of.period.O7T3"

results.descriptive.array <- clean.and.shape.data.to.array(
    my.df, start, end, dims, dimname.list, nm.placeholder, study.no
)

# add 5th and 6th intervention to array that was not expected to be present but was in one study
## create empty array with with two more levels at the first dimension (interventions)
temp.array <- array(NA, dim=c(6, 3, 4, 7, 2, study.no))

## insert the previous array
temp.array[1:4,,,,,] <- results.descriptive.array
results.descriptive.array <- temp.array

## add dimnames to new array
dimname.list <- list(
  c("Intervention.1", "Intervention.2", "Intervention.3", "Control", "Intervention.5", "Intervention.6"),
  c("Mean", "SD", "n"),
  c("T0", "T1", "T2", "T3"),
  c("Outcome.1", "Outcome.2", "Outcome.3", "Outcome.4", "Outcome.5", "Outcome.6", "Outcome.7"),
  c("Scale.1", "Scale.2"),
  my.df[,"Study.ID"]
)
dimnames(results.descriptive.array) <- dimname.list

print.array.not.na(results.descriptive.array)

my.df <- my.df %>%
  select(-Intervention.1.Mean.O1T0:-Control.or.Intervention.4.n.in.case.of.period.O7T3)


# %% vscode={"languageId": "r"}
# Show names of dimensions for results.descriptive.array to understand indexing below
dimnames(results.descriptive.array)

# %% hidden=true vscode={"languageId": "r"}
# add descriptive results of 5th intervention for "Klibert 2022"
results.descriptive.array[5,1:2,1,1,1,"Klibert 2022"] <- c(510.967, 230.34)  # mean and sd at pre-test
results.descriptive.array[5,1:2,2,1,1,"Klibert 2022"] <- c(633.167, 214.915)  # mean and sd at post-test
print.array.not.na(results.descriptive.array[,,,,,"Klibert 2022"])

# %% [markdown] hidden=true
#
# #### Results Quantitative
#

# %% hidden=true vscode={"languageId": "r"}
dims <- c(7, 3, 3, 6, 2, study.no) # 5th intervention was not added here as it was not usefull

dimname.list <- list(c("Outcome.1", "Outcome.2", "Outcome.3", "Outcome.4", "Outcome.5", "Outcome.6", "Outcome.7"),
                     c("P.Value", "Effect.Size", "Effect.Size.CI"),
                     c("T1", "T2", "T3"),
                     c("Int1.X.Int2", "Int1.X.Int3", "Int1.X.Int4", "Int2.X.Int3", "Int2.X.Int4", "Int3.X.Int4"),
                     c("Scale.1", "Scale.2"),
                     my.df[,"Study.ID"]
                     )

start <- "Outcome.1.P.Value.1x2T1"
end <- "Outcome.7.Confidence.Interval.of.Effect.Size.3x4T3"

results.quantitative.array <- clean.and.shape.data.to.array(
  my.df, start, end, dims, dimname.list, nm.placeholder, study.no,
  exclude.greater.than = TRUE  # <-- sets p.values > 0.05 to NA
)

print.array.not.na(results.quantitative.array)

my.df <- my.df %>%
  select(-Outcome.1.P.Value.1x2T1:-Outcome.7.Confidence.Interval.of.Effect.Size.3x4T3)


# %% [markdown] heading_collapsed=true hidden=true
# ### Data frame lists
# #### Intervention Comparison
#

# %% code_folding=[] hidden=true vscode={"languageId": "r"}
# correct input mistakes
study.names <- my.df[,"Study.ID"]

if ("0.99 mean" == my.df[which(study.names =="Sloan 2016"), "Frequency.of.intervention.sessions.in.times.per.week.Intervention.1"]){
  my.df[which(study.names =="Sloan 2016"), "Frequency.of.intervention.sessions.in.times.per.week.Intervention.1"] <- "0.99 mean s"
}
  # comment: Frequency: "Among the group, 20 participants attended all eight sessions and one attended seven sessions" --> 20   ×   (8   ×   1   +   1   ×   7   ×   1) / 21 / 8 = 0.99

if ("3.79 during intervention, 2.04 during follow-up mean" == my.df[which(study.names =="Huberty 2019"), "Frequency.of.intervention.sessions.in.times.per.week.Intervention.1"]){
  my.df[which(study.names =="Huberty 2019"), "Frequency.of.intervention.sessions.in.times.per.week.Intervention.1"] <- "3.79 mean s"
}
  # comment: - frequency: "Participants in the intervention group engaged in an average of 37.9 (SD 30.5) minutes of meditation per week over the course of the 8-week study." "Approximately one-third (14/41, 34%) of intervention participants continued to meditate during the follow-up period (12 weeks from baseline) and spent an average of 20.4 (SD 23.9) minutes meditating." 37.9/10, 20.04/10--> 3.79 and 2.04 times per week
  # --> mistake in calculation only 37.9 min per week during intervention is interesting for meta-regression

if ("4.19 mean" == my.df[which(study.names =="Nolan 2020"), "Frequency.of.intervention.sessions.in.times.per.week.Intervention.1"]){
  my.df[which(study.names =="Nolan 2020"), "Frequency.of.intervention.sessions.in.times.per.week.Intervention.1"] <- "4.19 mean s"
}
  # comment: Frequency (compare tab. 10 & 12): 10   ×   10   +   9   +   2   ×   8   +   3   ×   7   +   6   ×   6   +   2   ×   5   +   5   ×   4   +   3   ×   2   +   2   ×   2 = 222 sessions completed from tab. 10 222   +   10   ×   2   +   2   +   3   ×   1 ( tab. 12) = 247 Mean practiced sessions = 247 / 33 paricipants Mean practiced sessions / total duration (12.5) * 7 week days = 4.19 sessions per week 

if ("12.5 mean" == my.df[which(study.names =="Nolan 2020"), "Total.Duration.of.Intervention.in.Days.Intervention.1"]){
  my.df[which(study.names =="Nolan 2020"), "Total.Duration.of.Intervention.in.Days.Intervention.1"] <- NA
}
  # comment: "10-15-day" --> 12.5 days duration
  # --> higher range than 30% of the higher value (15) --> NA

if ("2.5 mean" == my.df[which(study.names =="Toole 2016"), "Frequency.of.intervention.sessions.in.times.per.week.Intervention.1"]){
  my.df[which(study.names =="Toole 2016"), "Frequency.of.intervention.sessions.in.times.per.week.Intervention.1"] <- "2.5 mean s"
}
  # comment: "The mean number of days participants in the intervention group listened to the podcasts (excluding Visit 1) was 1.5" --> Frequncy = 1 + 1.5 = 2.5

if ("11.19 mean" == my.df[which(study.names =="Schulte-Frankenfeld 2021"), "Duration.of.single.intervention.sessions.in.minutes.Intervention.1"]){
  my.df[which(study.names =="Schulte-Frankenfeld 2021"), "Duration.of.single.intervention.sessions.in.minutes.Intervention.1"] <- "11.19 mean s"
}
  # comment: mean duration of single session: total minutes of all programs (638) / total number of all sessions (57) = 11.19 see: https://iaap-journals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Faphw.12328&file=aphw12328-sup-0001-App-Based+Mindfulness+-+Supplements.docx

if ("2.82 mean" == my.df[which(study.names =="Schulte-Frankenfeld 2021"), "Frequency.of.intervention.sessions.in.times.per.week.Intervention.1"]){
  my.df[which(study.names =="Schulte-Frankenfeld 2021"), "Frequency.of.intervention.sessions.in.times.per.week.Intervention.1"] <- "2.82 mean s"
}
  # comment: Mean Frequency: mean of "sessions" column at https://osf.io/wujsg/ (22.53) / 8 weeks = 2.82

# %% hidden=true vscode={"languageId": "r"}
dims <- c(4, 7)

study.names <- my.df[,"Study.ID"]

dimname.list <- list(c("Intervention.1", "Intervention.2", "Intervention.3", "Control"),
                     c("Name", "Short.Description", "Delivery.Mode", "Meditation.App", "Sessions.Duration.in.minutes",
                       "Frequency.in.times.per.week", "Total.Duration.in.Days")
                     )
start <- "Name.Intervention.1"
end <- "Total.Duration.of.Intervention.in.Days.Control.or.Intervention.4"

intervention.comparisons.df.list <- clean.data.to.df.list.swap(
 my.df, start, end, dims, study.names, dimname.list
)

intervention.comparisons.df.list.w.o.mean.r <- clean.data.to.df.list.swap(
 my.df, start, end, dims, study.names, dimname.list, flag.x.s.r = 1
)

# add 5th and 6th intervention to df list that was not expected to be present but was in one study
## create NA df on dimension 2, 7 (2 rows for interventions 5 and 6)
int.5.6.df <- data.frame(matrix(
  NA, nrow = 2, ncol = 7,
  dimnames = list(
    c("Intervention.5", "Intervention.6"),
    dimname.list[[2]]
  )
))

## append this df as 5th and 6th intervention
for (df.i in 1:study.no){
  intervention.comparisons.df.list[[df.i]] <-
    rbind(intervention.comparisons.df.list[[df.i]], int.5.6.df)
  
  intervention.comparisons.df.list.w.o.mean.r[[df.i]] <-
    rbind(intervention.comparisons.df.list.w.o.mean.r[[df.i]], int.5.6.df)
}

my.df <- my.df %>%
  select(-Name.Intervention.1:-Total.Duration.of.Intervention.in.Days.Control.or.Intervention.4)

intervention.comparisons.df.list


# %% vscode={"languageId": "r"}
intervention.comparisons.df.list[["Nath 2023"]] %>% names

# %% hidden=true vscode={"languageId": "r"}
# add 5th intervention's descriptions of Klibert 2022
intervention.comparisons.df.list[["Klibert 2022"]]["Intervention.5", c(1:4)] <- c(
  "Savoring the Moment",
  "savoring the moment:\nself-review of emotions that contribute to a good day and carefully reading of text passages including themes of joy and optimism",
  "Synchronous guiding in presence (e.g. seminar)",
  ""
)

intervention.comparisons.df.list[["Klibert 2022"]]["Intervention.5", c(5:7)] <- c(
  15,
  1,
  1
)

intervention.comparisons.df.list[["Klibert 2022"]]

intervention.comparisons.df.list.w.o.mean.r[["Klibert 2022"]]["Intervention.5", c(1:4)] <- c(
  "Savoring the Moment",
  "savoring the moment:\nself-review of emotions that contribute to a good day and carefully reading of text passages including themes of joy and optimism",
  "Synchronous guiding in presence (e.g. seminar)",
  ""
)

intervention.comparisons.df.list.w.o.mean.r[["Klibert 2022"]]["Intervention.5", c(5:7)] <- c(
  15,
  1,
  1
)
intervention.comparisons.df.list.w.o.mean.r[["Klibert 2022"]]

# add 5th and 6th intervention's descriptions of Nath 2023
## 5th intervention
### 'Name', 'Short.Description', 'Delivery.Mode', 'Meditation.App'
intervention.comparisons.df.list[["Nath 2023"]]["Intervention.5", c(1:4)] <- c(
  "CA essential oil and mindfulness",
  "The students in the aromatherapy and mindfulness meditation groups applied 1 drop of oil to their masks and put on their masks for 30 min. while practicing the mindfulness meditation based on the audio recording.",
  "at home with recording",
  "none"
)

### 'Sessions.Duration.in.minutes', 'Frequency.in.times.per.week', 'Total.Duration.in.Days'
intervention.comparisons.df.list[["Nath 2023"]]["Intervention.5", c(5:7)] <- c(
  30,
  7,
  15
)

intervention.comparisons.df.list.w.o.mean.r[["Nath 2023"]]["Intervention.5", c(1:4)] <- c(
  "CA essential oil and mindfulness",
  "The students in the aromatherapy and mindfulness meditation groups applied 1 drop of oil to their masks and put on their masks for 30 min. while practicing the mindfulness meditation based on the audio recording.",
  "at home with recording",
  "none"
)

intervention.comparisons.df.list.w.o.mean.r[["Nath 2023"]]["Intervention.5", c(5:7)] <- c(
  30,
  7,
  15
)

## 6th intervention
### 'Name', 'Short.Description', 'Delivery.Mode', 'Meditation.App'
intervention.comparisons.df.list[["Nath 2023"]]["Intervention.6", c(1:4)] <- c(
  "mindfulness",
  "The students in the meditation group practiced mindfulness meditation by listening to pre-recorded audio for 30 min.",
    "at home with recording",
    "none"
)

### 'Sessions.Duration.in.minutes', 'Frequency.in.times.per.week', 'Total.Duration.in.Days'
intervention.comparisons.df.list[["Nath 2023"]]["Intervention.6", c(5:7)] <- c(
  30,
  7,
  15
)

intervention.comparisons.df.list[["Nath 2023"]]

intervention.comparisons.df.list.w.o.mean.r[["Nath 2023"]]["Intervention.6", c(1:4)] <- c(
  "mindfulness",
  "The students in the meditation group practiced mindfulness meditation by listening to pre-recorded audio for 30 min.",
    "at home with recording",
    "none"
)

intervention.comparisons.df.list.w.o.mean.r[["Nath 2023"]]["Intervention.6", c(5:7)] <- c(
  30,
  7,
  15
)
intervention.comparisons.df.list.w.o.mean.r[["Nath 2023"]]

# %% [markdown] hidden=true
#
# #### Measure of Outcomes
#

# %% hidden=true vscode={"languageId": "r"}
dims <- c(7, 3)

study.names <- my.df[,"Study.ID"]

dimname.list <- list(c("Outcome.1", "Outcome.2", "Outcome.3", "Outcome.4", "Outcome.5", "Outcome.6", "Outcome.7"),
                     c("Measures.Name", "Start.and.End.Point", "High.or.low.means.resilient")
                     )
start <- "Outcome.1.Scale.s.or.other.Measure.s.Name"
end <- "Outcome.7.High.or.low.means.resilient"

outcome.measures.df.list <- clean.data.to.df.list(
 my.df, start, end, dims, study.names, dimname.list, check.multiple.commas = TRUE
)

my.df <- my.df %>%
  select(-Outcome.1.Scale.s.or.other.Measure.s.Name:-Outcome.7.High.or.low.means.resilient)

outcome.measures.df.list


# %% hidden=true vscode={"languageId": "r"}
# correct vaulues
outcome.measures.df.list[["Warnecke 2011"]]["Outcome.1","Start.and.End.Point"] <- "0 - 40, 0 - 42"

# %% hidden=true vscode={"languageId": "r"}
outcome.measures.df.list[["Warnecke 2011"]]

# %% hidden=true vscode={"languageId": "r"}
outcome.measures.df.list[["Flett 2019a"]]["Outcome.4","Measures.Name"] <- "Cognitive Affective Mindfulness Scale–Revised (Feldman et al. 2007)"
outcome.measures.df.list[["Flett 2019a"]]["Outcome.5","Measures.Name"] <- "Brief Resilience Scale (Smith et al. 2008)"

# %% [markdown] hidden=true
#
# #### Qualitative Results of Data Analyses
#

# %% hidden=true vscode={"languageId": "r"}
dims <- c(7, 3)

study.names <- my.df[,"Study.ID"]

dimname.list <- list(c("Key.Theme.1", "Key.Theme.2", "Key.Theme.3", "Key.Theme.4", "Key.Theme.5", "Key.Theme.6", "Key.Theme.7"),
                     c("Name.of.Key.Theme", "Description.of.Key.Theme", "Results")
                     )
start <- "Key.Theme.1.Name.of.Key.Theme"
end <- "Key.Theme.7.Results"

results.qualitative.df.list <- clean.data.to.df.list(
 my.df, start, end, dims, study.names, dimname.list
)

my.df <- my.df %>%
  select(-Key.Theme.1.Name.of.Key.Theme:-Key.Theme.7.Results)

results.qualitative.df.list


# %% [markdown] heading_collapsed=true hidden=true
# ### Data frames
# #### Dates of Measuring Time Points
#

# %% hidden=true vscode={"languageId": "r"}
dates.measuring.time.points.df <- my.df[
  , which(colnames(my.df)=="Time.Point.0.Date"):which(colnames(my.df)=="Time.Point.3.Date")
]

for (row in 1:nrow(dates.measuring.time.points.df)){
  for (col in 1:ncol(dates.measuring.time.points.df)){
    dates.measuring.time.points.df[row, col] <- convert.value(dates.measuring.time.points.df[row, col])
  }
}

rownames(dates.measuring.time.points.df) <- study.names

my.df <- my.df %>%
  select(-Time.Point.0.Date:-Time.Point.3.Date)


dates.measuring.time.points.df


# %% [markdown] hidden=true
#
# #### Between-Measuring Time Points Duration
#

# %% hidden=true vscode={"languageId": "r"}
# correct input mistakes
if ("7 mean" == my.df[which(study.names == "Toole 2016"), "Time.Point.0...1.Duration.in.Days"]){
  my.df[which(study.names == "Toole 2016"), "Time.Point.0...1.Duration.in.Days"] <- "7 mean s"
}
  # quote: "Participants attended two lab visits approximately 1 week apart"

# %% hidden=true vscode={"languageId": "r"}
between.T.duration.df <- my.df[
  , which(colnames(my.df)=="Time.Point.0...1.Duration.in.Days"):which(colnames(my.df)=="Time.Point.0...3.Duration.in.Days")
]

for (row in 1:nrow(between.T.duration.df)){
  for (col in 1:ncol(between.T.duration.df)){
    between.T.duration.df[row, col] <- convert.value(between.T.duration.df[row, col], study.name = study.names[row])
  }
}

rownames(between.T.duration.df) <- study.names

my.df <- my.df %>%
  select(-Time.Point.0...1.Duration.in.Days:-Time.Point.0...3.Duration.in.Days)

between.T.duration.df


# %% [markdown] hidden=true
#
# #### Definition of Outcomes
#

# %% hidden=true vscode={"languageId": "r"}
outcome.definitions.df <- my.df[
  , which(colnames(my.df)=="Outcome.1.Definition"):which(colnames(my.df)=="Outcome.7.Definition")
]

for (row in 1:nrow(outcome.definitions.df)){
  for (col in 1:ncol(outcome.definitions.df)){
    outcome.definitions.df[row, col] <- convert.value(outcome.definitions.df[row, col])
  }
}

rownames(outcome.definitions.df) <- study.names

my.df <- my.df %>%
  select(-Outcome.1.Definition:-Outcome.7.Definition)

outcome.definitions.df


# %% [markdown] hidden=true
#
# #### Outcome Names
#

# %% hidden=true vscode={"languageId": "r"}
outcome.names.df <- my.df[, which(colnames(my.df)=="Name.of.Outcome.1"):which(colnames(my.df)=="Name.of.Outcome.7")]

for (row in 1:nrow(outcome.names.df)){
  for (col in 1:ncol(outcome.names.df)){
    outcome.names.df[row, col] <- convert.value(outcome.names.df[row, col])
  }
}

my.df <- my.df %>%
  select(-Name.of.Outcome.1:-Name.of.Outcome.7)

rownames(outcome.names.df) <- study.names

outcome.names.df[
  "Siembor 2018",
  which(outcome.names.df["Siembor 2018",] == "Coping flexibility")
] <- "Active coping"  # correct input mistake

outcome.names.df


# %% hidden=true vscode={"languageId": "r"}
# unify trait and state as one outcome
outcome.names.df[outcome.names.df == "Mindfulness (trait)" | outcome.names.df == "Mindfulness (state)"] <- "Mindfulness"
outcome.names.df[outcome.names.df == "Anxiety (trait)" | outcome.names.df == "Anxiety (state)"] <- "Anxiety"
outcome.names.df[outcome.names.df == "Depression (trait)" | outcome.names.df == "Depression (state)"] <- "Depression"
outcome.names.df

# %% hidden=true vscode={"languageId": "r"}
outcome.names.df[outcome.names.df == "Optimism or positive attributional style"] <- "Optimism"
outcome.names.df[outcome.names.df == "Well-being or quality of life"] <- "Well-being"
outcome.names.df[outcome.names.df == "Religiosity or spirituality or religious coping"] <- "Religious coping"

# %% [markdown] hidden=true
#
# #### Meditation Techniques
#

# %% hidden=true vscode={"languageId": "r"}
meditation.techniques.df <- my.df[
  , which(colnames(my.df)=="Practiced.Techniques.in.Intervention.1"):which(colnames(my.df)=="Practiced.Techniques.in.Control.or.Intervention.4")
]

for (row in 1:nrow(meditation.techniques.df)){
  for (col in 1:ncol(meditation.techniques.df)){
    meditation.techniques.df[row, col] <- convert.value(meditation.techniques.df[row, col])
  }
}

rownames(meditation.techniques.df) <- study.names

my.df <- my.df %>%
  select(-Practiced.Techniques.in.Intervention.1:-Practiced.Techniques.in.Control.or.Intervention.4)

# add 5th and 6th interventions
int.5.6.df <- data.frame(matrix(
  NA, nrow = study.no, ncol = 2,
  dimnames = list(
    study.names,
    c("Practiced.Techniques.in.Intervention.5", "Practiced.Techniques.in.Intervention.6")
  )
))

meditation.techniques.df <- cbind(meditation.techniques.df, int.5.6.df)

meditation.techniques.df


# %% hidden=true vscode={"languageId": "r"}
# add meditation tech of Klibert 2022
meditation.techniques.df["Klibert 2022", "Practiced.Techniques.in.Intervention.5"] <- "Other: self-review of emotions, reading of text of joy and optimism"
meditation.techniques.df["Klibert 2022",]

# add meditation tech of Nath 2023
meditation.techniques.df["Nath 2023", "Practiced.Techniques.in.Intervention.5"] <- "Other: Aromatherapy + mindfulness meditation"
meditation.techniques.df["Nath 2023", "Practiced.Techniques.in.Intervention.6"] <- "mindfulness meditation"
meditation.techniques.df["Nath 2023",]

# %% [markdown] heading_collapsed=true hidden=true
# #### RoB Data

# %% hidden=true vscode={"languageId": "r"}
raw.rob.df <- my.df
rownames(raw.rob.df) <- raw.rob.df[,"Study.ID"]
rob.df <- raw.rob.df[,
  c(
    2,which(names(raw.rob.df) == "Domain1..Risk.of.bias.arising.from.the.randomization.process"):
    which(names(raw.rob.df) == "Overall.risk.of.bias.supporting.text")
  )
]
rob.df <- rob.df[,-c(3:5, 7, 9, 11, 13, 15)]  # cut out Domain S for crossover designs and the additional comments
rob.df <- rob.df[order(rob.df$Study.ID), ]  # sort alphabetically 
rob.df[,1] <- 1:nrow(rob.df)

my.df <- my.df %>%
  select(-Domain1..Risk.of.bias.arising.from.the.randomization.process:-Overall.risk.of.bias.supporting.text)

rob.df

# %% hidden=true vscode={"languageId": "r"}
studies.high.rob <- rownames(rob.df[rob.df$Overall.risk.of.bias == "High risk of bias",])
studies.high.rob

# %% [markdown] hidden=true
#
# #### Left 1 D Data
#

# %% hidden=true vscode={"languageId": "r"}
my.df[my.df == 'None' | my.df == '' | is.na(my.df)] <- "NA"

one.D.info.df <- my.df

rownames(one.D.info.df) <- study.names

one.D.info.df


# %% [markdown] heading_collapsed=true hidden=true
# #### Correct input mistakes in Doraris 2021 (post-test was inserted as 1st follow-up)

# %% hidden=true vscode={"languageId": "r"}
results.descriptive.array[,,"T1",,"Scale.1","Dorais 2021"] <- results.descriptive.array[,,"T2",,"Scale.1","Dorais 2021"]
results.descriptive.array[,,c("T2", "T3"),,"Scale.1","Dorais 2021"] <- NA
print.array.not.na(results.descriptive.array[,,,,"Scale.1","Dorais 2021"])

# %% hidden=true vscode={"languageId": "r"}
one.D.info.df["Dorais 2021", "Numer.of.Measuring.Time.Points"] <- 2
one.D.info.df["Dorais 2021", "Numer.of.Measuring.Time.Points"]

# %% hidden=true vscode={"languageId": "r"}
# correcting input mistakes
between.T.duration.df["Dorais 2021", 1] <- between.T.duration.df["Dorais 2021", 2]
between.T.duration.df["Dorais 2021", 2] <- NA
between.T.duration.df["Dorais 2021", ]

# %% [markdown] heading_collapsed=true hidden=true
# ### Follow-up periods

# %% [markdown] heading_collapsed=true hidden=true
# #### Calculate follow-up periods

# %% hidden=true vscode={"languageId": "r"}
# correct input mistake
one.D.info.df["Sloan 2016","Numer.of.Measuring.Time.Points"] <- 2

# %% hidden=true vscode={"languageId": "r"}
# get studies with more than 2 measuring time points
studies.t3..4 <- c()
i <- 1
for (t.no in one.D.info.df[,"Numer.of.Measuring.Time.Points"]){
  if (
    is.na(t.no) |
    t.no %in% c("NA", nm.placeholder, as.character(nm.placeholder))
  ){
    t.no <- 0
  }
  if (as.double(t.no) > 2){
    studies.t3..4 <- append(studies.t3..4, study.names[i])
  }
  i <- i + 1
}
studies.t3..4

# %% hidden=true vscode={"languageId": "r"}
# data relevant for calculation of follow up period:
# "dates.measuring.time.points.df", <-- no important information
# "between.T.duration.df",
# one.D.info.df[,"Dates.of.Measuring.Time.Points.mentioned."],
# one.D.info.df[,"Between.Measuring.Time.Points..Duration.mentioned."]

follow.up.period.df <- data.frame(
  period.t1.t2 <- c(rep(NA, study.no)),
  period.t1.t3 <- c(rep(NA, study.no))
)
colnames(follow.up.period.df) <- c("period.t1.t2", "period.t1.t3")
rownames(follow.up.period.df) <- study.names

for (study.name in study.names){
  # period.t1.t2
  if (
    !(
      is.na(between.T.duration.df[study.name, 'Time.Point.0...1.Duration.in.Days']) |
      is.na(between.T.duration.df[study.name, 'Time.Point.0...1.Duration.in.Days']) %in%
      c("NA", nm.placeholder, as.character(nm.placeholder))
    ) &
    !(
      is.na(between.T.duration.df[study.name, 'Time.Point.0...2.Duration.in.Days']) |
      is.na(between.T.duration.df[study.name, 'Time.Point.0...2.Duration.in.Days']) %in%
      c("NA", nm.placeholder, as.character(nm.placeholder))
    )
  ){
    follow.up.period.df[study.name, "period.t1.t2"] <-
    as.double(between.T.duration.df[study.name, 'Time.Point.0...2.Duration.in.Days']) -
    as.double(between.T.duration.df[study.name, 'Time.Point.0...1.Duration.in.Days'])
  }
  
  # period.t1.t3
  if (
    !(
      is.na(between.T.duration.df[study.name, 'Time.Point.0...2.Duration.in.Days']) |
      is.na(between.T.duration.df[study.name, 'Time.Point.0...2.Duration.in.Days']) %in%
      c("NA", nm.placeholder, as.character(nm.placeholder))
    ) &
    !(
      is.na(between.T.duration.df[study.name, 'Time.Point.0...3.Duration.in.Days']) |
      is.na(between.T.duration.df[study.name, 'Time.Point.0...3.Duration.in.Days']) %in%
      c("NA", nm.placeholder, as.character(nm.placeholder))
    )
  ){
    follow.up.period.df[study.name, "period.t1.t3"] <-
    as.double(between.T.duration.df[study.name, 'Time.Point.0...3.Duration.in.Days']) -
    as.double(between.T.duration.df[study.name, 'Time.Point.0...1.Duration.in.Days'])
  }
}

# correct zeros to 1
follow.up.period.df[follow.up.period.df == 0] <- 1

follow.up.period.df
# keep in mind that t1 is post-test and t2 is first follow-up

# %% [markdown] heading_collapsed=true hidden=true
# #### Append follow-up periods to intervention.comparison.df.list

# %% hidden=true vscode={"languageId": "r"}
# it is easier to have this information in this df list for later meta-regression

# %% hidden=true vscode={"languageId": "r"}
# for intervention.comparisons.df.list
for (study in 1:study.no){
  intervention.comparisons.df.list[[study]][
    !(
      is.na(intervention.comparisons.df.list[[study]][,"Name"]) |
      intervention.comparisons.df.list[[study]][,"Name"] %in% c("NA", nm.placeholder, as.character(nm.placeholder))
    ),
    "period.t1.t2"
  ] <- follow.up.period.df[study,"period.t1.t2"]
  
  intervention.comparisons.df.list[[study]][
    !(
      is.na(intervention.comparisons.df.list[[study]][,"Name"]) |
      intervention.comparisons.df.list[[study]][,"Name"] %in% c("NA", nm.placeholder, as.character(nm.placeholder))
    ),
    "period.t1.t3"
  ] <- follow.up.period.df[study,"period.t1.t3"]
}
intervention.comparisons.df.list

# %% hidden=true vscode={"languageId": "r"}
# for intervention.comparisons.df.list.w.o.mean.r
for (study in 1:study.no){
  intervention.comparisons.df.list.w.o.mean.r[[study]][
    !(
      is.na(intervention.comparisons.df.list.w.o.mean.r[[study]][,"Name"]) |
      intervention.comparisons.df.list.w.o.mean.r[[study]][,"Name"] %in% c("NA", nm.placeholder, as.character(nm.placeholder))
    ),
    "period.t1.t2"
  ] <- follow.up.period.df[study,"period.t1.t2"]
  
  intervention.comparisons.df.list.w.o.mean.r[[study]][
    !(
      is.na(intervention.comparisons.df.list.w.o.mean.r[[study]][,"Name"]) |
      intervention.comparisons.df.list.w.o.mean.r[[study]][,"Name"] %in% c("NA", nm.placeholder, as.character(nm.placeholder))
    ),
    "period.t1.t3"
  ] <- follow.up.period.df[study,"period.t1.t3"]
}
intervention.comparisons.df.list.w.o.mean.r

# %% [markdown]
# ## Set unique names or variables for scale, delivery modes, and meditation type 

# %% [markdown]
# ### Meditation types and control groups

# %% vscode={"languageId": "r"}
# get all unique meditation types
meditation.types <- c()

for (intervention in 1:ncol(meditation.techniques.df)) {
  for (study in 1:nrow(meditation.techniques.df)){
    if (
      !(
        meditation.techniques.df[study, intervention] == "NA" |
        is.na(meditation.techniques.df[study, intervention]) |
        meditation.techniques.df[study, intervention] == nm.placeholder |
        meditation.techniques.df[study, intervention] == "None" |
        meditation.techniques.df[study, intervention] == "No Intervention" |
        meditation.techniques.df[study, intervention] == "Wait-List Control"
      )
    ){
      meditation.types <- append(meditation.types, meditation.techniques.df[study, intervention])
    }
  }
}

unique.meditation.types <- unique(meditation.types)
for (meditation.type in unique.meditation.types){
  print(meditation.type)
}


# %% vscode={"languageId": "r"}
# categorize control groups
cont.passive <- c("Wait-List Control", "No Intervention")

cont.active.rest <- c(
  "Still Sitting or Lying",
  "Other: looking at a plant",
  "Other: mind-wandering induction",
  "Other: somatic inactivity and environmental planning"
)

cont.active.cognitive <- c(
  "Other: listening to poems",
  "Other: Nature sounds",
  "Other: Evernote (not further described)",
  "Other: note-taking",
  "Other: Listening to interview with Jon Kabat-Zinn",
  "Other: video of background music, crashing waves, and drone\nfootage of tropical beaches",
  "Other: Listening to recordings of nursing news",
  "Other: Listening to nursing news audio",
  "Other: Poetry Analysis",
  "Other: Listen to a neutral news",
  "Other: Other activities (i.e., reading, chatting, napping) in another room",
  "Other: Browse the internet or sleep",
  "Other: Distraction task: Attending to 45 statements that were externally oriented and unrelated to the self (e.g.  “Think about the parking lot at a\ncinema” or “Think about and picture the Sydney Harbor Bridge\")",
  "Other: Sham meditation: Deep breathing without mindfulness aspect",
  "Other: Control: Sitting in a room with others (conversations were allowed, doing homework or sleep was not)",
  "Other: Link-link game: Finding two same symbols in a random-ordered square matrix",
  "Other: active control (listening to music)",
  "Other: active control audio recordings",
  "Other: active control"
)

cont.act.med.incl.move <- c(
  "Body Scan; Breathing Exercise (no further Info); Other: mindful eating",
  "Other: Loving-kindness coloring",
  "Other: Yoga",
  "Other: Hatha yoga",
  "Other: Mindfulness meditation (includes movement)",
  "Still Sitting or Lying; Other: writing down mind-wandering thoughts every 2 minutes of still sitting",
  "Still Sitting or Lying; Other: Typing thoughts after each 2 min interval"
)

cont.active.pmr <- c(
  "Progressive Muscle Relaxation (PMR)"
)

cont.active.at <- c(
  "Autogenic Training (AT)"
)

cont.active.mbsr <- c(
  "Other: Mindfulness Based Stress Reduction (MBSR)"
)

cont.active.bio.feedback <- c(
  "Other: EEG-alpha neurofeedback",
  "Other: Biofeedback"
)

cont.active.b.fb.sham <- c(
  "Other: sham neurofeedback"
)

cont.active.stress.man <- c(
  "Other: Stress management",
  "Other: self-reflection of stressful induction task which tool place before the pre-intervention test",
  "Other: re-imagining a past memory of joy",
  "Other: Standard treatment for student stress and anxiety (one-to-one session with a student advisor)",
  "Other: Study skills training",
  "Other: Stress mindset condition: Participants \"focused on reappraising stress as potentially enhancing performance and developing a stress-is-enhancing mindset.\"",
  "Other: Positive emotion regulation",
  "Other: self-review of emotions, reading of text of joy and optimism",
  "Other: memory-building exercise (re-imagining a past memory of joy)"
)

cont.active.dog <- c(
  "Other: Dog therapy"
)

cont.active.walk <- c(
  "Other: Walking"
)

cont.active.comb <- c(
  "Breathing Exercise (no further Info); Other: Recent memory recall, distant memory recall",
  "Other: Aromatherapy + mindfulness meditation",
  "Imagination; Progressive Muscle Relaxation (PMR)"
)

cont.active.aroma <- c(
  "Other: Aromatherapy "
)

control.all <- c(
  cont.passive,
  cont.active.rest,
  cont.active.cognitive,
  cont.act.med.incl.move,
  cont.active.pmr,
  cont.active.at,
  cont.active.mbsr,
  cont.active.bio.feedback,
  cont.active.b.fb.sham,
  cont.active.stress.man,
  cont.active.dog,
  cont.active.walk,
  cont.active.comb
)

control.all.active <- c(
  cont.active.rest,
  cont.active.cognitive,
  cont.act.med.incl.move,
  cont.active.pmr,
  cont.active.at,
  cont.active.mbsr,
  cont.active.bio.feedback,
  cont.active.b.fb.sham,
  cont.active.stress.man,
  cont.active.dog,
  cont.active.walk,
  cont.active.comb
)

control.all.list <- list(
  cont.passive = cont.passive,
  cont.active.rest = cont.active.rest,
  cont.active.cognitive = cont.active.cognitive,
  cont.act.med.incl.move = cont.act.med.incl.move,
  cont.active.pmr = cont.active.pmr,
  cont.active.at = cont.active.at,
  cont.active.mbsr = cont.active.mbsr,
  cont.active.bio.feedback = cont.active.bio.feedback,
  cont.active.b.fb.sham = cont.active.b.fb.sham,
  cont.active.stress.man = cont.active.stress.man,
  cont.active.dog = cont.active.dog,
  cont.active.walk = cont.active.walk,
  cont.active.comb = cont.active.comb
)


# %% vscode={"languageId": "r"}
# categorize meditation types
meditation.type.attentional <- c(
  "Breathing Exercise (no further Info)",
  "Transcendental Meditation (TM)",
  "Body Scan; Breathing Exercise (no further Info)",
  "Breathing Exercise (no further Info); Only \"Mindfulness Meditation\" named",
  "Only \"Mindfulness Meditation\" named",
  "Mantra Meditation",
  "Body Scan",
  "Breathing Exercise (no further Info); Other: mindfulness toward sensations and experiences of the body, mindfulness toward the contents of the mind, skill of mental noting to label their mental contents, cultivating awareness and understanding of emotions as mental contents and the nonjudgmental stance toward emotions with a goal of optimizing the response to one’s emotions",
  "Other: silent meditations with bells, personalized meditations with or without guided\nintro and bells",
  "Breathing Exercise (no further Info); Other: \"awareness to any sensations in their body or in their immediate environment\", focus on \"thoughts and feelings\",  \"non-judgmental observation\"",
  "Shavasana",
  "Zen Meditation",
  "Mantra Meditation; Only \"Focused Attention Meditation\" named; Other: Readings reflecting meditative perspective",
  "Breathing Exercise (no further Info); Open Monitoring Meditation",
  "Body Scan; Other: \"become aware of their sensations, including sights, sounds and somatic sensations\", relaxing body areas, observe their physical sensations thoughts and emotions without reaction or judgment,",
  'Body Scan; Only "Mindfulness Meditation" named',
  "Other: quietly observing and reflecting on internal and external stimuli such as breathing, thought, feeling, physical sensation, and sound, without reactions, judgments, or evaluations",
  'Breathing Exercise (no further Info); Other: "focus aware-\nness on their breath, sensations on their skin, other physical sensations and then\nas many different sensations as possible. Participants were instructed to maintain\nawareness during regular silent periods',
  "Imagination",
  'Breathing Exercise (no further Info); Other: \"focus aware-\nness on their breath, sensations on their skin, other physical sensations and then\nas many different sensations as possible. Participants were instructed to maintain\nawareness during regular silent periods\"',
  "Breathing Exercise (no further Info); Other: relaxation suggestion",
  "Body Scan; Breathing Exercise (no further Info); Other: open monitoring",
  "Breathing Exercise (slow)",
  "Breathing Exercise (no further Info); Other: mindful awareness, focused attention at the present moment",
  "Other: Didgeridoo sound meditation",
  "Autogenic Training (AT); Breathing Exercise (slow)",
  "Other: Attention focus element, Open-ended mindfulness activity",
  "Body Scan; Other: metaphorical observe thoughts as soap bubbles",
  "Body Scan; Other: metaphorical observe thoughts as objects",
  "Body Scan; Other: non-metaphorical attention to thoughts",
  "Other: Focused on mindfulness and values",
  "Other: Om meditation",
  "Pranayama",
  "Other: Opening-up meditation",
  "Other: Mindfulness: Training for relaxation, Training to stabilize the attention, Training for clarity, Teachings from MBSR (Tonglen teachings, Metta teachings)",
  "Only \"Focused Attention Meditation\" named",
  "Vipassana",
  "Other: focused awareness using breath awareness techniques, further courses without further information",
  "Other: attention monitoring, focused breathing",
  "mindfulness meditation"
)

meditation.type.constructive <- c(
  "Loving-Kindness Meditation",
  "Other: Gratitude practice (recalling and generating feelings of thankfulness)"
)

meditation.type.deconstructive <- c(
  "Other: devotional readings to ponder, meditations taken from a widely used devotional book designed for Christian readers titled"
  # no forgivingness compared to other intervention in Vasiliauskas 2013
)

meditation.type.attentional.and.constructive <- c(
  "Body Scan; Breathing Exercise (no further Info); Loving-Kindness Meditation",
  "Breathing Exercise (no further Info); Loving-Kindness Meditation",
  "Breathing Exercise (no further Info); Other: development of positive emotions, development of acceptance",
  "Body Scan; Breathing Exercise (no further Info); Other:  visualization exercise targeting test anxiety (including imagination of emotions)",
  "Other: Meditation recordings regarding stress, anxiety, self-compassion, and gratitude",
  'Breathing Exercise (no further Info); Other: Mindfulness meditation regarding body awareness, focused breathing, stress relief, self-\ncompassion, and gratitude',
  "Body Scan; Breathing Exercise (no further Info); Other: Observation of thoughts without judgment, Emotional identification, Internalization and concentration on positive\nand friendly attitudes to help oneself and others",
  "Other: Mindfulness, cognitive reappraisal, and savoring exercises",
  "Breathing Exercise (no further Info); Imagination; Mantra Meditation; Loving-Kindness Meditation"
)

meditation.type.attentional.and.deconstructive <- c(
  "Prayer; Other: devotional readings to ponder, meditations taken from a widely used devotional book designed for Christian readers titled, forgiveness meditations",
    # due to "forgiveness" --> strengthens cognitive and affective patterns (constructive), "reading to ponder" --> deconstructive
  "Breathing Exercise (no further Info); Other: mindfulness toward sensations and experiences of the body, mindfulness toward the contents of the mind, skill of mental noting to label their mental contents, cultivating awareness and understanding of emotions as mental contents and the nonjudgmental stance toward emotions with a goal of optimizing the response to oneâ€™s emotions"
)

meditation.type.attentional.constructive.and.deconstructive <- c(
  "Other: mindfulness-based exercises through audio video or text files, grounding visualization, gratitude, imagining the life you want, finding meaning",
  "Breathing Exercise (no further Info); Other: focusing on meta-awareness, \"Mindfulness-based exercises [...] about happiness in life as the result of a benevolent attitude towards life and a positive relationship to oneself and others\", \"Methods of introspection, reflection and self-care\"",
  'Other: VR-based focus, self-love, and purpose (meditation categories of the software webpage of \"Guided Meditation VR\")',
  "Other: Non-VR-based meditation (same as in other intervention just without VR)"
)

meditation.type.all <- c(
  meditation.type.attentional,
  meditation.type.constructive,
  meditation.type.attentional.and.constructive,
  meditation.type.attentional.and.deconstructive,
  meditation.type.attentional.constructive.and.deconstructive,
  meditation.type.deconstructive
)

meditation.type.all.list <- list(
  meditation.type.attentional = meditation.type.attentional,
  meditation.type.constructive = meditation.type.constructive,
  meditation.type.attentional.and.constructive = meditation.type.attentional.and.constructive,
  meditation.type.attentional.and.deconstructive = meditation.type.attentional.and.deconstructive,
  meditation.type.attentional.constructive.and.deconstructive = meditation.type.attentional.constructive.and.deconstructive,
  meditation.type.deconstructive = meditation.type.deconstructive
)

# %% code_folding=[] vscode={"languageId": "r"}
# print all techniques that are not classified yet
med.techs.conts.categorized <- c(
  meditation.type.attentional,
  meditation.type.constructive,
  meditation.type.attentional.and.constructive,
  meditation.type.attentional.and.deconstructive,
  meditation.type.attentional.constructive.and.deconstructive,
  meditation.type.deconstructive,
  cont.passive,
  cont.active.rest,
  cont.active.cognitive,
  cont.active.pmr,
  cont.active.bio.feedback,
  cont.active.b.fb.sham,
  cont.act.med.incl.move,
  cont.active.at,
  cont.active.mbsr,
  cont.active.stress.man,
  cont.active.dog,
  cont.active.comb,
  cont.active.walk
)

# check if meditation techniques are not categorized
med.techs.present <- as.vector(t(meditation.techniques.df))
for (tech in med.techs.present){
  if (
    !tech %in% c(med.techs.conts.categorized, "None", "Other: None") &
    !is.na.or.nm(tech)
  ){
    print(tech)
  }
}
print("done")

# %% [markdown]
# #### Insert Meditation Techniques in df

# %% vscode={"languageId": "r"}
meditation.techniques.df

# %% vscode={"languageId": "r"}
# Clustering (all techniques) for intervention.comparisons.df.list
for (study in 1:study.no){
  for (intervention in 1:ncol(meditation.techniques.df)){
    value <- meditation.techniques.df[study, intervention]
    if (!(
      is.na(value) |
      value %in% c(
        "NA", nm.placeholder, as.character(nm.placeholder),
        "Wait-List Control", "None", "No Intervention"
      )
    )){
      if (value %in% meditation.type.attentional){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "attentional family"
      } else if (value %in% meditation.type.constructive){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "constructive family"
      } else if (value %in% meditation.type.deconstructive){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "deconstructive family"
      } else if (value %in% cont.passive){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "passive control"
      } else if (value %in% cont.active.rest){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "physical rest"
      } else if (value %in% cont.active.cognitive){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "congnitive control"
      } else if (value %in% cont.act.med.incl.move){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "meditation including movement"
      } else if (value %in% cont.active.pmr){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "Progressive Muscle Relaxation"
      } else if (value %in% cont.active.at){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "Autogenic Training"
      } else if (value %in% cont.active.mbsr){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "MBSR"
      } else if (value %in% cont.active.bio.feedback){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "biofeedback"
      } else if (value %in% cont.active.b.fb.sham){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "sham neurofeedback"
      } else if (value %in% cont.active.stress.man){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "stress management"
      } else if (value %in% cont.active.dog){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "dog therapy"
      } else if (value %in% cont.active.walk){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "walking"
      } else if (value %in% cont.active.comb){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "meditation in combination with other treatments"
      } else if (value == "Other: None"){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "None"
      } else if (
        value %in% c(
          meditation.type.attentional.and.constructive,
          meditation.type.attentional.and.deconstructive,
          meditation.type.attentional.constructive.and.deconstructive
        )
      ){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "hybrid form"
      } else {
        cat(
          "unknown meditation technique:", "\n",
          value, "\n\n"
        )
      }
    }
  }
}

# %% vscode={"languageId": "r"}
control.groups <- c(
  "passive control",
  "physical rest",
  "congnitive control",
  "meditation including movement",
  "Progressive Muscle Relaxation",
  "Autogenic Training",
  "MBSR",
  "biofeedback",
  "sham neurofeedback",
  "stress management",
  "dog therapy",
  "walking",
  "meditation in combination with other treatments"
)

# %% vscode={"languageId": "r"}
# Clustering (all techniques) for intervention.comparisons.df.list.w.o.mean.r
for (study in 1:study.no){
  for (intervention in 1:ncol(meditation.techniques.df)){
    value <- meditation.techniques.df[study, intervention]
    if (!(
      is.na(value) |
      value %in% c(
        "NA", nm.placeholder, as.character(nm.placeholder),
        "Wait-List Control", "None", "No Intervention"
      )
    )){
      if (value %in% meditation.type.attentional){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "attentional Family"
      } else if (value %in% meditation.type.constructive){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "constructive Family"
      } else if (value %in% meditation.type.deconstructive){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "deconstructive Family"
      } else if (value %in% cont.passive){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "passive control"
      } else if (value %in% cont.active.rest){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "physical rest"
      } else if (value %in% cont.active.cognitive){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "congnitive control"
      } else if (value %in% cont.act.med.incl.move){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "meditation including movement"
      } else if (value %in% cont.active.pmr){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "Progressive Muscle Relaxation"
      } else if (value %in% cont.active.at){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "Autogenic Training"
      } else if (value %in% cont.active.mbsr){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "MBSR"
      } else if (value %in% cont.active.bio.feedback){
        intervention.comparisons.df.list[[study]][intervention, "Meditation.Type"] <- "biofeedback"
      } else if (value %in% cont.active.b.fb.sham){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "sham neurofeedback"
      } else if (value %in% cont.active.stress.man){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "stress management"
      } else if (value %in% cont.active.dog){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "dog therapy"
      } else if (value %in% cont.active.walk){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "walking"
      } else if (value %in% cont.active.comb){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "meditation in combination with other treatments"
      } else if (value == "Other: None"){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "None"
      } else if (
        value %in% c(
          meditation.type.attentional.and.constructive,
          meditation.type.attentional.and.deconstructive,
          meditation.type.attentional.constructive.and.deconstructive
        )
      ){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Meditation.Type"] <- "hybrid form"
      } else {
        cat(
          "unknown meditation technique:", "\n",
          value, "\n\n"
        )
      }
    }
  }
}

# %% [markdown]
# #### Get studies per interventions/controls

# %% vscode={"languageId": "r"}
for (cont.i in 1:length(control.all.list)){
  # print control
  cat("\n#############", names(control.all.list)[cont.i], "#########################################################\n")

  # empty vectors for control/intervention description and study names 
  int.descs <- c()
  studies <- c()

  # iterate over studies that have the group of interest present
  for (study in study.names){
    if (T %in%(control.all.list[[cont.i]] %in% meditation.techniques.df[study,])){

      # collect study names
      studies <- paste(studies, study, sep = ", ")

      # collect intervention/control description
      group.of.interest <- as.vector(unlist(meditation.techniques.df[study,]) %in% control.all.list[[cont.i]])
      int.desc <- intervention.comparisons.df.list[[study]]$Short.Description[group.of.interest]
      int.descs <- append(int.descs, int.desc)
    }
  }
  
  int.descs <- unique(int.descs[!is.na.or.nm(int.descs)])
  int.descs <- paste(int.descs, collapse = "\n")
  
  cat(studies,"\n\n")
  cat(int.descs,"\n")
}

# %% [markdown] editable=true slideshow={"slide_type": ""}
# ### Scales

# %% [markdown]
# ToDo: Add the following scales:
# - Short Warwick-Edinburgh Mental Well-being Scale (SWEMWBS) 
# - Sussex-Oxford Compassion for the Self Scale (SOCS-S)

# %% vscode={"languageId": "r"}
# find out all unique scale cell entries
# measures <- c()
# 
# for (outcome.measures.df in outcome.measures.df.list) {
#   for (outcome.measure in outcome.measures.df[,"Measures.Name"]){
#     if (!(is.na(outcome.measure) | outcome.measure == "NA" | outcome.measure == nm.placeholder)){
#       # if (multiple.commas.present(outcome.measure)){
#       #   cat("multiple commas in: ", outcome.measure, "\n")
#       # }
#       measures <- append(measures, outcome.measure)
#     }
#   }
# }
# 
# unique.measures <- unique(measures)
# 
# i <- 1
# for (j in unique.measures){
#   print(i)
#   print(j)
#   cat("\n")
#   i <- i + 1
# }


# %% [markdown]
#
# #### Resilience
#

# %% vscode={"languageId": "r"}
remove.commas.b.p <- function(value){
  # get the substring between first ( and first ) if present, if not NA
  chars.between.par <- ex_between(value, "(", ")")[[1]]
  chars.between.par.nc <- c(rep(NA, length(chars.between.par)))
  for (par.case in 1:length(chars.between.par)){
    if (grepl(",", chars.between.par[par.case])){
      # remove commas between parentheses
      chars.between.par.nc[par.case] <- gsub(',', '', chars.between.par[par.case])
    }
  }
  value <- rm_between(value, "(", ")", replacement = chars.between.par.nc)
  value
}

# %% vscode={"languageId": "r"}
scale.CD.RISC.10.synonyms <- c(
  "Connor-Davidson Resilience Scale (CD-RISC-10) (Connor & Davidson, 2003)"
)

scale.BResS.synonyms <- c(
  "6-item Brief\nResilience Scale (Smith et al. 2008)", "Brief Resilience Scale (Smith et al. 2008",
  "Brief Resilience Scale (Smith et al. 2008)"
)


# %% [markdown] heading_collapsed=true
# #### Mental health-related
# ##### Including Subscales
#

# %% hidden=true vscode={"languageId": "r"}
scale.DASS.synonyms <- c(
  "Depression, Anxiety\nand Stress Scale (DASS) 26",
  "Depression Anxiety and Stress Scale (DASS) - Depression Subcale",
  "Depression Anxiety and Stress Scale (DASS) - Anxiety Subcale",
  "Depression Anxiety and Stress Scale [32] - depression subcale",
  "Depression Anxiety and Stress Scale [32] - anxiety subcale",
  "Depression Anxiety and Stress Scale [32] - stress subcale",
  "Depression, Anxiety, and Stress Scale – 21 (DASS-21)",
  "Depression Anxiety and Stress Scale (DASS)",
  "Depression, Anxiety, and Stress Scale – 21 (DASS-21), subscale depression",
  "Depression, Anxiety, and Stress Scale â€“ 21 (DASS-21), subscale depression",
  "Depression, Anxiety, and Stress Scale – 21 (DASS-21), subscale anxiety",
  "Depression, Anxiety, and Stress Scale â€“ 21 (DASS-21), subscale anxiety",
  "Depression, Anxiety, and Stress Scale â€“ 21 (DASS-21),subscale stress",
  "Depression Anxiety Stress Scales [DASS]; Lovibond and Lovibond 1995",
  "Depression Anxiety Scale 21 (DASS21; Henry and Crawford 2005)", "Depression Anxiety Scale 21 (DASS21; Henry and Crawford 2005)", "Depression Anxiety Scale 21 (DASS21; Henry and Crawford 2005)", "Depression Anxiety and Stress Scale‐21 (DASS‐21; Antony Bieling Cox Enns & Swinson 1998)", "Depression Anxiety and Stress Scale‐21 (DASS‐21; Antony Bieling Cox Enns & Swinson 1998)",
  "Depression Anxiety and Stress Scale‐21 (DASS‐21; Antony Bieling Cox Enns & Swinson 1998)", "Depression Anxiety Stress Scales (DASS-42)", "Depression Anxiety Stress Scales (DASS-42)", "Depression Anxiety Stress Scales (DASS-42)", "Depression Anxiety and Stress Scale – 21 (DASS-21) subscale depression", "Depression Anxiety and Stress Scale – 21 (DASS-21) subscale anxiety", "Depression Anxiety and Stress Scale – 21 (DASS-21) subscale stress",
  " DASS - Depression, Anxiety, and Stress Scale"
)

scale.POMS.synonyms <- c(
  "Profile of Mood States total mood disturbance main scale 23",
  "Profile of Mood States anxiety subscale",
  "Profile of Mood States depression subscale",
  "Profile of Mood States (POMS)",
  "Profile of Mood States-Short Form (POMS; McNair et al. 1971; Curran et al. 1995)"
)

scale.DIPAS.ND.synonyms <- c(
  "Self-administered questionnaire to measure anxiety depression anger and sense of wellbeing developed and validated by he Defence Institute of Physiology and Allied Sciences New Delhi [13 see notes for source]"
)

scale.HADS.synonyms <- c(
  "Hospital Anxiety and\nDepression Scale–Anxiety Subscale (HADS-A; Zigmond and\nSnaith 1983)", "Hospital Anxiety and Depression Scale–Anxiety Subscale (HADS-A; Zigmond and Snaith 1983)"
)

scale.SCL.90.synonyms <- c(
  "Self Checklists-90 (SCL-90)"
)

# %% [markdown] hidden=true
#
# ##### 2 Scales per Cell
#

# %% hidden=true vscode={"languageId": "r"}
scales.DASS.PSS.synonyms <- c(
  "Depression, Anxiety\nand Stress Scale (DASS) 26, Perceived Stress Scale\n(PSS)27,28",
  "Depression, Anxiety, and Stress Scale – 21 (DASS-21),\nPerceived Stress Scale (PSS)",
  "Depression, Anxiety, and Stress Scale – 21 (DASS-21),subscale stress\nPerceived Stress Scale (PSS)",
  "Depression, Anxiety, and Stress Scale â€“ 21 (DASS-21),subscale stress\nPerceived Stress Scale (PSS)",
  "Depression Anxiety and Stress Scale (DASS), Perceived Stress Scale (PSS)"
)

scales.PSS.DASS.synonyms <- c(
  "Perceived Stress Scale (PSS), Depression Anxiety and Stress Scale (DASS) - Stress Subcale"
)

scales.SOM.PSS.synonyms <- c(
  "Stress-O-Meter (SOM), Perceived Stress Scale (PSS)"
)

scales.IPANAT.PANAS.synonyms <- c(
  "Implicit positive and negative affect task. IPANAT [29], Positive and negative affect schedule. The PANAS [30] "
)

scales.SCS.VAS.synonyms <- c(
  "Self-Compassion Scale (SCS; Neff 2003b), visual analog scales (VAS)"
)


# %% [markdown] hidden=true
#
# ##### Anxiety
#

# %% hidden=true vscode={"languageId": "r"}
scale.SAS.synonyms <- c(
  "Self-Rating Anxiety Scale (SAS) (Zung, 1971)"
)

scale.STAI.synonyms <- c(
  "State Anxiety Scale from the State–Trait Anxiety Inventory (Spielberger, Gorsuch, Lushene, Vagg, & Jacobs, 1983)",
  "State Anxiety Scale from the Stateâ€“Trait Anxiety Inventory (Spielberger, Gorsuch, Lushene, Vagg, & Jacobs, 1983)",
  "State-Trait Anxiety Inventory (STAI)",
  "State-Trait Anxiety Inventory (S-TAI) - state (SAI)",
  "state-trait anxiety inventory (STAI) -  state anxiety",
  "State-Trait Anxiety Inventory (STAI) - state subscale",
  "State-Trait Anxiety Inventory (STAI) - trait subscale",
  "State-Trait Anxiety Inventory (Spielberger, Gorsuch, & Lushene, 1970)", "State-Trait Anxiety Inventory for Adults (STAI; Spielberger 1983)", "State-Trait Anxiety Inventory for Adults (STAI; Spielberger 1983)", "State-Trait Anxiety Inventory (STAI form Y; Spielberger 1975)", "State-Trait Anxiety Scale (Marteau & Bekker, 1992)", "State-Trait Anxiety Scale (Marteau & Bekker, 1992)", "State-Trait Anxiety Inventory (STAI; Spielberger et al. 1983)", "State-Trait Anxiety Inventory (STAI; Spielberger et al. 1983)", "State-Trait Anxiety Inventory", "State Anxiety Scale from the State–Trait Anxiety Inventory (Spielberger Gorsuch Lushene Vagg & Jacobs 1983)", "State-Trait Anxiety Inventory (STAI-S; Spielberger et al. 1970)", "State/Trait Anxiety Inventory (STAI)",
  "State Anxiety Scale of the STAI (Spielberger 1983)", "State-trait anxiety inventory short form (STAI-Y-6; Marteau and Bekker 1992)",
  "State Anxiety Inventory (Marteau & Bekker 1992)", "State anxiety inventory scale (Marteau & Bekker 1992)"
)

scale.STICSA.synonyms <- c(
  " State-Trait Inventory of Cognitive and Somatic Anxiety (STICSA)"
)

scale.GAD.7.synonyms <- c(
  "Generalized Anxiety Disorder (GAD-7) subscale",
  "Generalized Anxiety Disorder 7-item scale (GAD-7; Spitzer et al. 2006)",
  "Generalized Anxiety Disorder Scale-7 (GAD-7)",
  "Genarilzed Anxiety Disorder scale (GAD7; Spitzer et al. 2006)"
)

scale.CAS.synonyms <- c(
  "College Adjustment Scales"
)

# %% [markdown] hidden=true
#
# ##### Depression
#

# %% hidden=true vscode={"languageId": "r"}
scale.BDI.synonyms <- c(
  "Beck Depression Inventory (BDI [72])",
  "Beck Depression Inventory (BDI)",
  "Beck Depression Inventory-II; BDI-II (Beck et al. 1996)",
  'BDI; Depressive symptomatology; "Beck Depression Inventory (BDI)')

scale.PHQ.9.synonyms <- c(
  "Patient Health Questionnaire-9 (PHQ-9; Kroenke & Spitzer, 2002)",
  "Patient Health Questionnaire-9 (PHQ-9)",
  " Patient Health Questionnaire-9 (PHQ-9)",
  "Patient Health Questionnaire- 8 scale (Kroenke et al. 2009)"
)

scale.QIDS.SR.synonyms <- c(
  "Quick Inventory of Depressive Symptomatology Self-Report (QIDS-SR)"
)

scale.SDS.synonyms <- c(
  "Self-Rating Depression Scale (SDS) (Zung et al., 1965)"
)

scale.CES.D.synonyms <- c(
  "20-item Center for Epidemiological Studies\nDepression Scale (CES-D; Radloff 1977)",
  "Center for Epidemiological Studies Depression Scale (CES-D; Radloff 1977)",
  "Center for Epidemiologic Studies Depression scale (CES-D)"
)


# %% [markdown] hidden=true
#
# ##### Stress
#

# %% hidden=true vscode={"languageId": "r"}
scale.PSS.synonyms <- c(
  "Perceived Stress Scale (PSS; Cohen, Kamarck, & Mermelstein, 1983)",
  "The Perceived Stress Scale (PSS)",
  "Perceived Stress Scale (PSS).16",
  "Perceived Stress Scale (PSS-10; Cohen, Kamarck, & Mermelstein, 1983)",
  "Perceived Stress Scale (PSS)",
  "Perceived Stress Scale (PSS) (Cohen et al., 1983)",
  "Perceived Stress Scale (PSS) [58,59]",
  "The Perceived Stress Scale (PSS; Cohen, Kamark, & Mermelstein, 1983)",
  "Perceived Stress Scale (PSS; Cohen et al., 1994)",
  "Perceived Stress Scale (PSS-10) (Cohen, 2017)",
  "erceived Stress Scale (PSS [74])",
  "Perceived Stress Scale (PSS; Cohen and Williamson 1988)", "Perceived Stress Scale (PSS; Cohen and Williamson 1988)", "Perceived Stress Scale (PSS; Cohen, Kamarck & Mermelstein 1983)", "Perceived Stress Scale (PSS; S. Cohen et al. 1983)", "Perceived Stress Scale (PSS; Cohen et al. 1983) ",
  "Perceived Stress Scale",
  "10- item Perceive Stress scale (PSS Klein et al. 2016)",
  "Perceived Stress Scale (PSS) (Cohen et al. 1983)",
  "Perceived Stress Scale (14 Items)"
)

scale.BRS.synonyms <- c(
  " Behavioral Relaxation Scale"
)

scale.DSSQ.S.synonyms <- c(
  "Dundee Stress State Questionnaire (DSSQ-S; Matthews 2016)"
)

scale.1.i.stress.synonyms <- c(
  '1-item question “How stressed do you feel right now (at this present moment)?”'
)

scale.SIG.synonyms <- c(
  'Stress in General Scale (SIG; Stanton et al. 2001)'
)

# %% [markdown] hidden=true
#
# ##### Well-Being
#

# %% hidden=true vscode={"languageId": "r"}
scale.CPIwb.synonyms <- c(
  "California Psychological Inventroy (CPI), subscale sense of well-being"
)

scale.LSS.synonyms <- c(
  "Questionnaire for the Assessment of Happiness ( ger.: Lebensglückskala, LSS) (Ciccarello & Reinhard, 2014)",
  "Questionnaire for the Assessment of Happiness ( ger.: LebensglÃ¼ckskala, LSS) (Ciccarello & Reinhard, 2014)"
)

scale.RAND.synonyms <- c(
  "RAND 36-Item Health Survey, concept emotional wellbeing"
)

scale.SWLS.synonyms <- c(
  "Satisfaction with Life Scale (SWLS [71])",
  "Satisfaction with Life Scale (SWLS)",
  "satisfaction with life scale (SWLS; Diener et al., 1985)",
  "Satisfaction with Life Scale"
)

scale.WHO.QOL.BREF.synonyms <- c(
  "World Health Organization Quality of Life-Brief (WHO-QOL-BREF)"
)

scale.SA.HDS.synonyms <- c(
  "Subjective Authentic-Durable Happiness Scale (SA-HDS; Dambrun et al., 2012)"
)

scale.WEMWBS.synonyms <- c(
  "Warwick-Edinburgh Mental Wellbeing Scale (WEMWBS; Tennant et al.\n2007)"
)

scale.FS.synonyms <- c(
  "Flourishing Scale (Diener et al. 2010)"
)


# %% [markdown]
# #### Resilience Factors
# ##### Active Coping
#

# %% vscode={"languageId": "r"}
scale.COPE.synonyms <- c(
  "Coping Style (Brief COPE) (Brief COPE; Carver, 1997)"
)


# %% [markdown]
#
# ##### Cognitive Control
#

# %% vscode={"languageId": "r"}
scale.Stroop.synonyms <- c(
  "Stroop Task"
)


# %% [markdown]
#
# ##### Coping, positive
#

# %% vscode={"languageId": "r"}
scale.not.common.positive.coping.synonyms <- c(
  "scales adapted from a\nwell-known set of brief measures developed by the Fetzer Institute (1999, pp. 86–87) - positve coping subscale",
  "scales adapted from a\nwell-known set of brief measures developed by the Fetzer Institute (1999, pp. 86â€“87) - positve coping subscale"
)


# %% [markdown]
#
# ##### Empathy
#

# %% vscode={"languageId": "r"}
scale.BEA.synonyms <- c(
  "Batson Empathy Adjectives (BEA; Batson, 1986; Coke, Batson, & McDavis, 1978)"
)

scale.JSE.synonyms <- c(
  "Jefferson Scale of Empathy (JSE; Hojat 2016a; Hojat et al. 2018; Thomas Jefferson University 2021a) "
)

# %% [markdown]
#
# ##### Mindfulness
#

# %% vscode={"languageId": "r"}
scale.CAMS.R.synonyms <- c(
  "Cognitive and Affective Mindfulness Scale-Revised (CAMS-R)",
  "Cognitive and Affective Mindfulness Scale-Revised (CAMS-R) - acceptace subscale",
  "Cognitive and Affective Mindfulness Scale–Revised (CAMS-R; Feldman, Hayes, Kumar, Greeson, & Laurenceau, 2007)",
  "Cognitive and Affective Mindfulness Scaleâ€“Revised (CAMS-R; Feldman, Hayes, Kumar, Greeson, & Laurenceau, 2007)",
  "Cognitive and Affective Mindfulness Scale – Revised (CAMS-R)",
  "Cognitive and Affective Mindfulness Scale â€“ Revised (CAMS-R)",
  "Cognitive and Affective Mindfulness\nScale-Revised (CAMS-R; Feldman et al., 2007)",
  "12-item\nCognitive Affective Mindfulness Scale–Revised (Feldman\net al. 2007)",
  "Cognitive Affective Mindfulness Scale–Revised (Feldman et al. 2007)",
  " Five-Facet Mindfulness"
)

scale.FFMQ.synonyms <- c(
  "Five Factor Mindfulness Questionnaire (FFMQ) [60].",  # mistake in paper --> Factor = Facet
  "Five Facet Mindfulness Questionnaire (FFMQ; Baer, Smith, Hopkins, Krietemeyer, & Toney, 2006)",
  "Five Facet Mindfulness Questionnaire [FFMQ]; Baer et al. 2006",
  "Five Facet Mindfulness Questionnaire (FFMQ)", "Mindfulness Questionnaire (FFMQ; Baer et al. 2006 2008)",
  "Five Facets Mindfulness Questionnaire (FFMQ; Baer et al. 2006)",
  " Five-Facet Mindfulness \nQuestionnaire (Michalak et al. 2016)"
)

scale.FMI.14.synonyms <- c(
  "Freiburg Mindfulness Inventory (FMI-14) (Buchheld & Walach, 2002)",
  "Freiburg Mindfulness Inventory (FMI)"
)

scale.KIMS.synonyms <- c(
  "Kentucky Inventory of Mindfulness Skills (KIMS; Baer et al., 2004)",
  "Kentucky Inventory of Mindfulness Skills (KIMS; Baer et al. 2004)"
)

scale.MAAS.synonyms <- c(
  "Mindfulness Attention Awareness Scale (MAAS) (Brown and Ryan (2003)",
  "Mindfulness Attention Awareness Scale (MAAS) (Brown & Ryan, 2003)",
  "mindful attention awareness scale (MAAS; Brown and Ryan, 2003)",
  "Mindful Attention Awareness Scale (MAAS)",
  "Mindfulness Attention Awareness Scale (MAAS; Brown & Ryan 2003)", "Mindfulness Attention Awareness Scale (MAAS)",
  "Mindfulness Attention Awareness Scale (Brown & Ryan 2003)"
)

scale.TMS.synonyms <- c(
  "The Toronto Mindfulness Scale (TMS)",
  "Toronto Mindfulness Scale (TMS; Lau et al. 2006)",
  "Toronto Mindfulness Scale", "Toronto Mindfulness Scale (TMS; Lau et al. 2006) "
)

scale.SMS.synonyms <- c(
  "State Mindfulness Scale (SMS)",
  "State Mindfulness Scale (Tanay & Bernstein 2013)", "State mindfulness scale (Tanay & Bernstein 2013)"
)


# %% [markdown]
#
# ##### Positve Affect
#

# %% vscode={"languageId": "r"}
scale.PANAS.synonyms <- c(
  "Positive Affect Negative Affect Schedule (PANAS; Watson et al. 1988)",
  "Positive and Negative Affect Scale – Positive Affect (PANAS-PA)",
  "Positive Affect Negative Affect Scale (PANAS)", "Positive and Negative Affect Scale (PANAS; Watson et al. 1988)",
  "Positive and Negative Affect Scale (PANAS)",
  "Positive and Negative Affect Schedule (PANAS; Watson Clark and Tellegen 1988)", "Positive and Negative Affect Schedule (PANAS)", "Positive and Negative Affect Schedule (PANAS)", "Positive and Negative Affect Schedule (PANAS)",
  "Positive and Negative Affect Schedule (PANAS; Watson, Clark, and Tellegen 1988)"
)

scale.SPANE.synonyms <- c(
  "12-item Scale of Positive and Negative Experiences (SPANE; Diener et al. 2010)",
  "Positive and Negative Experience (SPANE)"
)

scale.m.DES.synonyms <- c(
  "Modified Differential Emotions Scale (Fredrickson et al. 2003)"
)


# %% [markdown]
#
# ##### Positve Emotion
#

# %% vscode={"languageId": "r"}
scale.SHS.synonyms <- c(
  "Subjective Happiness Scale (SHS)",
  "Subjective Happiness Scale (SHS; Lyubomirsky & Lepper 1999)"
)


# %% [markdown]
#
# ##### Psychological Capital
#

# %% vscode={"languageId": "r"}
scale.PCQ.synonyms <- c(
  "Psychological Capital\nQuestionnaire (PCQ) 19"
)


# %% [markdown]
#
# ##### Self-acceptance
#

# %% vscode={"languageId": "r"}
scale.CPIsa.synonyms <- c(
  "California Psychological Inventory French version (CPI) (Gough 1957) - Subscale Self-acceptance (Sa)",
  "California Psychological Inventory French version (CPI)"
)


# %% [markdown]
#
# ##### Self-compassion
#

# %% vscode={"languageId": "r"}
scale.SCS.synonyms <- c(
  "Self-Compassion Scale (SCS; Neff, 2003a)"
)

scale.SCS.SF.synonyms <- c(
  "Self-Compassion Survey Short-Form (SCS-SF)",
  "Self-Compassion Scale-Short Form (SCS-SF)"
)

scale.SSCS.synonyms <- c(
  "State self-compassion scale (Breines & Chen, 2013)"
)


# %% [markdown]
# ##### Self-efficacy

# %% vscode={"languageId": "r"}
scale.m.GSES.synonyms <- c(
  "modified version of the General Self-Efficacy Scale (GSES; Schwarzer & Jerusalem 1995)"
)


# %% [markdown]
#
# ##### Self-esteem
#

# %% vscode={"languageId": "r"}
scale.RSES.synonyms <- c(
  "Rosenberg Self-Esteem Scale (RSES; Rosenberg, 1965)"
)


# %% [markdown]
#
# #### Set unique scale names
#

# %% code_folding=[] vscode={"languageId": "r"}
# loop for renaming could be a better approach
# scales with comment # NEW # were added after search update

i <- 1
uncategorized.scales <- c()
for (study in 1:study.no) {
  for (outcome in 1:7){
    outcome.measure <- outcome.measures.df.list[[study]][outcome,"Measures.Name"]
    if (
      !(is.na(outcome.measure) |
        outcome.measure == "NA" |
        outcome.measure == nm.placeholder |
        outcome.measure == as.character(nm.placeholder)
      )

    
    ){
# Resilience
      if (outcome.measure %in% scale.CD.RISC.10.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "CD-RISC-10"
      } else if (outcome.measure %in% scale.BResS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "BResS"  ############################## NEW #############
# Mental health-related
  ## Including Subscales
      } else if (outcome.measure %in% scale.DASS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "DASS"
      } else if (outcome.measure %in% scale.POMS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "POMS"
      } else if (outcome.measure %in% scale.DIPAS.ND.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "DIPAS-ND"  ############################## NEW #############
      } else if (outcome.measure %in% scale.HADS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "HADS"  ############################## NEW #############
      } else if (outcome.measure %in% scale.SCL.90.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SCL-90"  ############################## NEW #############
  ## 2 Scales per cell
      } else if (outcome.measure %in% scales.DASS.PSS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "DASS, PSS"
      } else if (outcome.measure %in% scales.PSS.DASS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "PSS, DASS"
      } else if (outcome.measure %in% scales.SOM.PSS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SOM, PSS"  ############################## NEW #############
      } else if (outcome.measure %in% scales.IPANAT.PANAS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "IPANAT, PANAS"  ############################## NEW #############
      } else if (outcome.measure %in% scales.SCS.VAS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SCS, VAS"  ############################## NEW #############
  ## Anxiety
      } else if (outcome.measure %in% scale.SAS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SAS"
      } else if (outcome.measure %in% scale.STAI.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "STAI"
      } else if (outcome.measure %in% scale.STICSA.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "STICSA" ############################## NEW #############
      } else if (outcome.measure %in% scale.SPANE.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SPANE" ############################## NEW #############
      } else if (outcome.measure %in% scale.GAD.7.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "GAD-7" ############################## NEW #############
      } else if (outcome.measure %in% scale.CAS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "CAS" ############################## NEW #############
  ## Depression
      } else if (outcome.measure %in% scale.BDI.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "BDI"
      } else if (outcome.measure %in% scale.PHQ.9.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "PHQ-9"
      } else if (outcome.measure %in% scale.QIDS.SR.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "QIDS-SR"
      } else if (outcome.measure %in% scale.SDS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SDS"
      } else if (outcome.measure %in% scale.CES.D.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "CES-D" ############################## NEW #############
  ## Stress
      } else if (outcome.measure %in% scale.PSS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "PSS"
      } else if (outcome.measure %in% scale.BRS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "BRS"
      } else if (outcome.measure %in% scale.DSSQ.S.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "DSSQ-S"  ############################## NEW #############
      } else if (outcome.measure %in% scale.1.i.stress.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "1-item Stress"  ############################## NEW #############
      } else if (outcome.measure %in% scale.SIG.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SIG"  ############################## NEW #############
  ## Quality of Life
      } else if (outcome.measure %in% scale.CPIwb.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "CPI"
      } else if (outcome.measure %in% scale.LSS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "LSS"
      } else if (outcome.measure %in% scale.RAND.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "RAND 36-Item Health Survey"
      } else if (outcome.measure %in% scale.SWLS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SWLS"
      } else if (outcome.measure %in% scale.WHO.QOL.BREF.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "WHO-QOL-BREF"
      } else if (outcome.measure %in% scale.SA.HDS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SA-HDS"  ############################## NEW #############
      } else if (outcome.measure %in% scale.WEMWBS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "WEMWBS"  ############################## NEW #############
      } else if (outcome.measure %in% scale.FS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "FS"  ############################## NEW #############
# Resilience factors
  ## Cognitive Control
      } else if (outcome.measure %in% scale.Stroop.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Stroop Task"  ############################## NEW #############
  ## Coping, active (active coping)
      } else if (outcome.measure %in% scale.COPE.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Brief COPE"
  ## Coping, religious (religious coping)
      } else if (outcome.measure %in% scale.not.common.positive.coping.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Positve coping subscale (adapted from scales of the Fetzer Institute)"
  ## Empathy
      } else if (outcome.measure %in% scale.BEA.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "BEA"
      } else if (outcome.measure %in% scale.JSE.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "JSE"  ############################## NEW #############
  ## Mindfulness
      } else if (outcome.measure %in% scale.CAMS.R.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "CAMS-R"
      } else if (outcome.measure %in% scale.FFMQ.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "FFMQ"
      } else if (outcome.measure %in% scale.FMI.14.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "FMI"
      } else if (outcome.measure %in% scale.KIMS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "KIMS"
      } else if (outcome.measure %in% scale.MAAS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "MAAS"
      } else if (outcome.measure %in% scale.TMS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "TMS"
      } else if (outcome.measure %in% scale.SMS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SMS"
  ## Positive Affect
      } else if (outcome.measure %in% scale.PANAS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "PANAS"  ############################## NEW #############
      } else if (outcome.measure %in% scale.m.DES.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "m.DES"  ############################## NEW #############
  ## Positive Emotion
      } else if (outcome.measure %in% scale.SHS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SHS"
  ## Psychological Capital
      } else if (outcome.measure %in% scale.PCQ.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "PCQ"
  ## Self-acceptance
      } else if (outcome.measure %in% scale.CPIsa.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "CPI"
  ## Self-compassion
      } else if (outcome.measure %in% scale.SCS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SCS"
      } else if (outcome.measure %in% scale.SCS.SF.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SCS-SF"
      } else if (outcome.measure %in% scale.SSCS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "SSCS"
  ## Self-esteem
      } else if (outcome.measure %in% scale.RSES.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "RSES"
  ## Self-efficay
      } else if (outcome.measure %in% scale.m.GSES.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "m.GSES"  ############################## NEW #############
        
      } else {
        uncategorized.scales[i] <- outcome.measure
        i <- i + 1
#         cat(
#           "unknown case:", outcome.measure, "\n",
#           "study: ", study.names[study], "\n\n"
#           )
      }
    }
  }
}
uncategorized.scales


# %% [markdown]
# #### Get foot note for explaining scale abbreviations

# %% vscode={"languageId": "r"}
# scales after search update are not listed here (see scales marked as ### New ### in the last cell)
# furthermore uncategorized.scales were not abbreviated as they only occure one time
scale.abbreviations <- sort(c(
  "CD-RISC-10", "BResS", "DASS", "DIPAS-ND", "HADS", "SCL-90", "POMS", "SAS", "STAI", "BDI", "PHQ-9", "QIDS-SR", "SDS", "PSS", "BRS", "CPI", "LSS", "RAND 36-Item Health Survey", "SWLS", "WHO-QOL-BREF", "Brief COPE",
   "BEA","CAMS-R", "FFMQ", "FMI-14", "KIMS", "MAAS", "TMS", "SHS", "PCQ", "CPI", "SCS", "SCS-SF", "RSES"
))
scale.abbreviations

# %% vscode={"languageId": "r"}
scale.synonyms <- list(
  scale.CD.RISC.10.synonyms,
  scale.BResS.synonyms,
  scale.DASS.synonyms, scale.POMS.synonyms,
  scale.DIPAS.ND.synonyms,
  scale.HADS.synonyms,
  scale.SCL.90.synonyms,
  scale.SAS.synonyms, scale.STAI.synonyms,
  scale.BDI.synonyms, scale.PHQ.9.synonyms, scale.QIDS.SR.synonyms, scale.SDS.synonyms,
  scale.PSS.synonyms, scale.BRS.synonyms,
  scale.CPIwb.synonyms, scale.LSS.synonyms, scale.RAND.synonyms, scale.SWLS.synonyms, scale.WHO.QOL.BREF.synonyms,
  scale.COPE.synonyms,
  scale.BEA.synonyms,
  scale.CAMS.R.synonyms, scale.FFMQ.synonyms, scale.FMI.14.synonyms, scale.KIMS.synonyms, scale.MAAS.synonyms, scale.TMS.synonyms,
  scale.SHS.synonyms,
  scale.PCQ.synonyms,
  scale.CPIsa.synonyms,
  scale.SCS.synonyms,
  scale.SCS.SF.synonyms,
  scale.RSES.synonyms
)
scale.synonyms

# %% vscode={"languageId": "r"}
# generate data frame with scale abbreviations and scale names
scale.abbreviations.df <- data.frame(
  scale.abbreviation = scale.abbreviations,
  scale.names = c("NA")
)

for (row in 1:nrow(scale.abbreviations.df)){
  scale.ab <- scale.abbreviations.df[row, "scale.abbreviation"]
  for (scale.syn in scale.synonyms){
    if(TRUE %in% grepl(scale.ab, scale.syn)){
      scale.n.space <- gsub("\\(..*", "", scale.syn[[1]])  # gsub function to cut out "("
      scale.n <- substr(scale.n.space, 1, nchar(scale.n.space) - 1)
      scale.abbreviations.df[row, "scale.names"] <- scale.n
    }
  }
}

scale.abbreviations.df

# %% vscode={"languageId": "r"}
# # generate string to paste in legend
# legend.string <- ""
# for (row in 1:nrow(scale.abbreviations.df)){
#   legend.string <- paste(legend.string, scale.abbreviations.df[row, 1], " = ", scale.abbreviations.df[row, 2], ", ", sep = "")
# }
# legend.string

# %% [markdown] heading_collapsed=true
# ### Delivery Mode

# %% code_folding=[] hidden=true vscode={"languageId": "r"}
# get all unique delivery mode entries
delivery.modes <- c()
for (study in 1:study.no){
  for (intervention in 1:nrow(intervention.comparisons.df.list[[1]])){
    delivery.mode <- intervention.comparisons.df.list[[study]][intervention,"Delivery.Mode"]
    
    if (
      !is.na(delivery.mode) &
      !(delivery.mode %in% c("NA", nm.placeholder, "nm.placeholder"))
    )
    delivery.modes <- append(
      delivery.modes,
      intervention.comparisons.df.list[[study]][intervention,"Delivery.Mode"]
    )
  }
}

unique.delivery.modes <- unique(delivery.modes)
unique.delivery.modes


# %% code_folding=[0] hidden=true vscode={"languageId": "r"}
# # cluster delivery modes (synch. vs asynch. vs. analog vs. digital vs. self)
# 
# delivery.digital.aynch <- c(
#   "Asynchronous guiding by a recorded voice (e.g. App), audio recording",
#   "Asynchronous guiding by a recorded voice (e.g. App), recorded mindfulness practice on CD",
#   "Asynchronous guiding by a recorded voice (e.g. App), audio CD",
#   "Youtube Video, Asynchronous guiding by a recorded voice (e.g. App)",
#   "Asynchronous guiding by a recorded voice (e.g. App)",
#   "Asynchronous guiding by a recorded voice (e.g. App",
#   "Asynchronous guiding by a recorded voice (e.g. App), audio clips"
# )
# 
# delivery.analogue.synch <- c(
#   "Synchronous guiding in presence (e.g. seminar)",
#   "nm but probably Synchronous guiding in presence (e.g. seminar)"
# )
# 
# delivery.analogue.asynch <- c(
#   "readings"
#   
# )
# 
# delivery.digital.synch..self <- c(
#   "Self-conducted meditation, nm but probably Synchronous online guiding on the internet (e.g. webinar)"
# )
# 
# delivery.digital.asynch..self <- c(
#   "Asynchronous guiding by a recorded voice (e.g. App),  Self-conducted meditation",
#   "Asynchronous guiding by a recorded voice, video,  written instructions on centering prayer"
# )
# 
# delivery.analogue.synch..self <- c(
#   "Synchronous guiding in presence (e.g. seminar), Self-conducted meditation",
#   "nm but probalby Synchronous guiding in presence (e.g. seminar), Self-conducted meditation",
#   "nm probably Synchronous guiding in presence (e.g. seminar), Self-conducted meditation"
# )
# 
# delivery.all <- c(
#   delivery.digital.aynch,
#   delivery.analogue.synch,
#   delivery.analogue.asynch,
#   delivery.digital.synch..self,
#   delivery.digital.asynch..self,
#   delivery.analogue.synch..self
# )
# # --> to fine clustering?


# %% code_folding=[0] hidden=true vscode={"languageId": "r"}
# # Clustering (synchronous vs. asynchronous guiding vs. self-conducted)

# delivery.asynch <- c(
#   "Asynchronous guiding by a recorded voice (e.g. App), audio recording",
#   "Asynchronous guiding by a recorded voice (e.g. App), recorded mindfulness practice on CD",
#   "Asynchronous guiding by a recorded voice (e.g. App), audio CD",
#   "Youtube Video, Asynchronous guiding by a recorded voice (e.g. App)",
#   "Asynchronous guiding by a recorded voice (e.g. App)",
#   "Asynchronous guiding by a recorded voice (e.g. App",
#   "Asynchronous guiding by a recorded voice (e.g. App), audio clips",
#   "readings"
# )

# delivery.synch <- c(
#   "Synchronous guiding in presence (e.g. seminar)",
#   "nm but probably Synchronous guiding in presence (e.g. seminar)"
# )

# delivery.synch..self <- c(
#   "Self-conducted meditation, nm but probably Synchronous online guiding on the internet (e.g. webinar)",
#   "Synchronous guiding in presence (e.g. seminar), Self-conducted meditation",
#   "nm but probalby Synchronous guiding in presence (e.g. seminar), Self-conducted meditation",
#   "nm probably Synchronous guiding in presence (e.g. seminar), Self-conducted meditation"
# )

# delivery.asynch..self <- c(
#   "Asynchronous guiding by a recorded voice (e.g. App),  Self-conducted meditation",
#   "Asynchronous guiding by a recorded voice, video,  written instructions on centering prayer"
# )

# delivery.all <- c(
#   delivery.asynch,
#   delivery.synch,
#   delivery.synch..self,
#   delivery.asynch..self
# )

# delivery.all.names <- c(
#   "delivery.asynch",
#   "delivery.synch",
#   "delivery.synch..self",
#   "delivery.asynch..self"
# )

# %% code_folding=[0] hidden=true vscode={"languageId": "r"}
# # clustering (synchronous vs. asynchronous & digital vs. analog)

# delivery.asynch <- c(
#   "Asynchronous guiding by a recorded voice (e.g. App), audio recording",
#   "Asynchronous guiding by a recorded voice (e.g. App), recorded mindfulness practice on CD",
#   "Asynchronous guiding by a recorded voice (e.g. App), audio CD",
#   "Youtube Video, Asynchronous guiding by a recorded voice (e.g. App)",
#   "Asynchronous guiding by a recorded voice (e.g. App)",
#   "Asynchronous guiding by a recorded voice (e.g. App",
#   "Asynchronous guiding by a recorded voice (e.g. App), audio clips",
#   "readings",
#   "Asynchronous guiding by a recorded voice (e.g. App),  Self-conducted meditation",
#   "Asynchronous guiding by a recorded voice, video,  written instructions on centering prayer"
# )

# delivery.synch <- c(
#   "Synchronous guiding in presence (e.g. seminar)",
#   "nm but probably Synchronous guiding in presence (e.g. seminar)",
#   "Self-conducted meditation, nm but probably Synchronous online guiding on the internet (e.g. webinar)",
#   "Synchronous guiding in presence (e.g. seminar), Self-conducted meditation",
#   "nm but probalby Synchronous guiding in presence (e.g. seminar), Self-conducted meditation",
#   "nm probably Synchronous guiding in presence (e.g. seminar), Self-conducted meditation"
# )

# delivery.analog <- c(
#   "readings",
#   "Synchronous guiding in presence (e.g. seminar)",
#   "nm but probably Synchronous guiding in presence (e.g. seminar)",
#   "Synchronous guiding in presence (e.g. seminar), Self-conducted meditation",
#   "nm but probalby Synchronous guiding in presence (e.g. seminar), Self-conducted meditation",
#   "nm probably Synchronous guiding in presence (e.g. seminar), Self-conducted meditation"
# )

# delivery.analog..digital <- c(
#   "Self-conducted meditation, nm but probably Synchronous online guiding on the internet (e.g. webinar)"
# )

# delivery.digital <- c(
#   "Asynchronous guiding by a recorded voice (e.g. App), audio recording",
#   "Asynchronous guiding by a recorded voice (e.g. App), recorded mindfulness practice on CD",
#   "Asynchronous guiding by a recorded voice (e.g. App), audio CD",
#   "Youtube Video, Asynchronous guiding by a recorded voice (e.g. App)",
#   "Asynchronous guiding by a recorded voice (e.g. App)",
#   "Asynchronous guiding by a recorded voice (e.g. App",
#   "Asynchronous guiding by a recorded voice (e.g. App), audio clips",
#   "Asynchronous guiding by a recorded voice (e.g. App),  Self-conducted meditation",
#   "Asynchronous guiding by a recorded voice, video,  written instructions on centering prayer"
# )

# delivery.all <- c(
#   delivery.asynch,
#   delivery.synch,
#   delivery.analog,
#   delivery.analog..digital,
#   delivery.digital
# )

# delivery.all.names <- c(
#   "delivery.asynch",
#   "delivery.synch",
#   "delivery.analog",
#   "delivery.analog..digital",
#   "delivery.digital"
# )

# %% hidden=true vscode={"languageId": "r"}
# clustering (synchronous vs. asynchronous)

delivery.asynch <- c(
  "Asynchronous guiding by a recorded voice (e.g. App), audio recording",
  "Asynchronous guiding by a recorded voice (e.g. App), recorded mindfulness practice on CD",
  "Asynchronous guiding by a recorded voice (e.g. App), audio CD",
  "Youtube Video, Asynchronous guiding by a recorded voice (e.g. App)",
  "Asynchronous guiding by a recorded voice (e.g. App)",
  "Asynchronous guiding by a recorded voice (e.g. App",
  "Asynchronous guiding by a recorded voice (e.g. App), audio clips",
  "readings",
  "Asynchronous guiding by a recorded voice (e.g. App),  Self-conducted meditation",
  "Asynchronous guiding by a recorded voice, video,  written instructions on centering prayer",
  "Self-conducted meditation",
  "Asynchronous guiding by a recorded voice (e.g. App), Self-conducted meditation",
  "nm but Asynchronous guiding by a recorded voice (e.g. App),  Self-conducted meditation"
)

delivery.synch <- c(
  "Synchronous guiding in presence (e.g. seminar)",
  "nm but probably Synchronous guiding in presence (e.g. seminar)",
  "Self-conducted meditation, nm but probably Synchronous online guiding on the internet (e.g. webinar)",
  "Synchronous guiding in presence (e.g. seminar), Self-conducted meditation",
  "nm but probalby Synchronous guiding in presence (e.g. seminar), Self-conducted meditation",
  "Synchronous online guiding on the internet (e.g. webinar), Self-conducted meditation",
  "Self-conducted (no meditation)",
  "Synchronous online guiding on the internet (e.g. webinar)"
)

delivery.all <- c(
  delivery.asynch,
  delivery.synch
)

delivery.all.names <- c(
  "delivery.asynch",
  "delivery.synch"
)

# %% [markdown] hidden=true
#
#
#

# %% hidden=true vscode={"languageId": "r"}
unique.delivery.modes

# %% hidden=true vscode={"languageId": "r"}
# check if all entries are mentioned
length(unique.delivery.modes); length(delivery.all)
unique.delivery.modes[!unique.delivery.modes %in% delivery.all]
delivery.all[!delivery.all %in% unique.delivery.modes]

# %% [markdown] heading_collapsed=true hidden=true
# #### Insert unique names in intervention.comparisons.df.list

# %% code_folding=[0] hidden=true vscode={"languageId": "r"}
# # Clustering (synchronous vs. asynchronous guiding vs. self-conducted)
# for (study in 1:study.no){
#   for (intervention in 1:nrow(intervention.comparisons.df.list[[1]])){
#     value <- intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"]
#     if (!(
#       is.na(value) |
#       value %in% c("NA", nm.placeholder, as.character(nm.placeholder))
#     )){
#       if (value %in% delivery.asynch){
#         intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- "Asynchronous guiding only"
#       } else if (value %in% delivery.synch){
#         intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- "Synchronous guiding only"
#       } else if (value %in% delivery.synch..self){
#         intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- "Synchronous guiding and self-conducted"
#       } else if (value %in% delivery.asynch..self){
#         intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- "Asynchronous guiding and self-conducted"
#       } else {
#         cat(
#           "unknown delivery mode:", "\n",
#           value, "\n\n"
#         )
#       }
#     }
#   }
# }

# %% code_folding=[0] hidden=true vscode={"languageId": "r"}
# # Clustering (synchronous vs. asynchronous guiding & digital vs. analog)
# for (study in 1:study.no){
#   for (intervention in 1:nrow(intervention.comparisons.df.list[[1]])){
#     value <- intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"]
#     if (!(
#       is.na(value) |
#       value %in% c("NA", nm.placeholder, as.character(nm.placeholder))
#     )){
      
#       if (value %in% delivery.asynch){
#         intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- "Asynchronous guiding"
#       } else if (value %in% delivery.synch){
#         intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- "Synchronous guiding"
#       } else {
#         cat(
#           "unknown delivery mode:", "\n",
#           value, "\n\n"
#         )
#       }
      
#       value.new <- intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"]
      
#       if (value %in% delivery.analog){
#         if (value.new %in% c("Asynchronous guiding", "Synchronous guiding")){
#           intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- paste(value.new, "Analog guiding", sep = ", ")
#         } else {
#           intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- "Analog guiding"
#         }
        
#       } else if (value %in% delivery.analog..digital){
#         if (value.new %in% c("Asynchronous guiding", "Synchronous guiding")){
#           intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- paste(value.new, "Analog and digital guiding", sep = ", ")
#         } else {
#           intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- "Analog and digital guiding"
#         }
        
#       } else if (value %in% delivery.digital){
#         if (value.new %in% c("Asynchronous guiding", "Synchronous guiding")){
#           intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- paste(value.new, "Digital guiding", sep = ", ")
#         } else {
#           intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- "Digital guiding"
#         }
        
#       } else {
#         cat(
#           "unknown meditation type:", "\n",
#           value, "\n\n"
#         )
#       }
#     }
#   }
# }

# %% hidden=true vscode={"languageId": "r"}
# Clustering (synchronous vs. asynchronous guiding) for intervention.comparisons.df.list
for (study in 1:study.no){
  for (intervention in 1:nrow(intervention.comparisons.df.list[[1]])){
    value <- intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"]
    if (!(
      is.na(value) |
      value %in% c("NA", nm.placeholder, as.character(nm.placeholder))
    )){
      if (value %in% delivery.asynch){
        intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- "asynchronous guiding"
      } else if (value %in% delivery.synch){
        intervention.comparisons.df.list[[study]][intervention, "Delivery.Mode"] <- "synchronous guiding"
      } else {
        cat(
          "unknown delivery mode:", "\n",
          value, "\n\n"
        )
      }
    }
  }
}

# %% hidden=true vscode={"languageId": "r"}
# Clustering (synchronous vs. asynchronous guiding) for intervention.comparisons.df.list.w.o.mean.r
for (study in 1:study.no){
  for (intervention in 1:nrow(intervention.comparisons.df.list.w.o.mean.r[[1]])){
    value <- intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Delivery.Mode"]
    if (!(
      is.na(value) |
      value %in% c("NA", nm.placeholder, as.character(nm.placeholder))
    )){
      if (value %in% delivery.asynch){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Delivery.Mode"] <- "asynchronous guiding"
      } else if (value %in% delivery.synch){
        intervention.comparisons.df.list.w.o.mean.r[[study]][intervention, "Delivery.Mode"] <- "synchronous guiding"
      } else {
        cat(
          "unknown delivery mode:", "\n",
          value, "\n\n"
        )
      }
    }
  }
}

# %% hidden=true vscode={"languageId": "r"}
# correct imput mistakes
intervention.comparisons.df.list[['deGrace 1976']][1, "Delivery.Mode"] <- "synchronous guiding"
intervention.comparisons.df.list.w.o.mean.r[['deGrace 1976']][1, "Delivery.Mode"] <- "synchronous guiding"

# %% [markdown] heading_collapsed=true
# ## Fill empty n, mean age, and sex values

# %% [markdown] heading_collapsed=true hidden=true
# ### n

# %% hidden=true vscode={"languageId": "r"}
# set correct n from Population Characteristics to descriptive results
methods.excluding.subjects <- c(
  "Per-protocol analysis",
  "Listwise or case deletion",
  "Pairwise deletion",
  "NA",
  NA,
  nm.placeholder,
  as.character(nm.placeholder),
  "Not mentioned",
  "not mentioned",
  "Other: "
)

methods.keeping.n.of.t0 <- c(
  "Intention-to-treat (ITT)",
  "Baseline observation carried forward (BOCF)",
  "Best Obeservation Carried Forward",
  "Expectation-Maximization",
  "Last observation carried forward (LOCF)",
  "Maximum likelihood",
  "Mean substitution",
  "Multiple imputation",
  "Regression imputation",
  "Sensitivity analysis"  # in this case values of ITT are inserted if present
)


for (study in 1:study.no){
  for (scale in 1:2){
    for (outcome in 1:7){
      for (t in 1:4){
        for (intervention in 1:6){
          int.name <- intervention.comparisons.df.list[[study]][intervention, "Name"]
          no.incl.outcomes <- one.D.info.df[study, "Number.of.included.Outcomes"]
          if (grepl(",", outcome.measures.df.list[[study]][outcome, "Measures.Name"])){
            # the value of "Measures.Name" is devided in 2 scale names by a comma
            no.used.scales.per.outcome <- 2
          } else {
            no.used.scales.per.outcome <- 1
          }
          if (
            !(is.na(int.name) | int.name %in% c("NA", nm.placeholder, as.character(nm.placeholder))) &
            (outcome <= no.incl.outcomes) &
            (scale <= no.used.scales.per.outcome)
          ){
            value <- results.descriptive.array[intervention, "n", t, outcome, scale, study]
            imputation <- one.D.info.df[study,"Imputation.of.missing.Data"]
            ITT.PP.analysis <- one.D.info.df[study,"ITT.or.Per.Protocol.Analysis.present."]
            if (is.character(value)){
              cat("value is character but should be double instead:", value)
            } else if (is.na(value) | value == nm.placeholder){

              if (
                imputation %in% methods.excluding.subjects &
                ITT.PP.analysis %in% methods.excluding.subjects
              ){
                n <- population.characteristics.array["No.Participants", intervention, t, study]
                if ((is.na(n)) & t == 4){
                  n <- population.characteristics.array["No.Participants", intervention, t - 1, study]
                  if (is.na(n)){
                    n <- population.characteristics.array["No.Participants", intervention, t - 2, study]
                    if (is.na(n)){
                      n <- population.characteristics.array["No.Participants", intervention, t - 3, study]
                      if (is.na(n)){
                        cat(
                          "ERROR#1: no n of present intervention of study:", study.names[study], "\n",
                          "intervention:", intervention, "\n",
                          "n:", n, "\n\n"
                        )
                      }
                    }
                  }
                } else if ((is.na(n)) & t == 3){
                  n <- population.characteristics.array["No.Participants", intervention, t - 1, study]
                  if (is.na(n)){
                    n <- population.characteristics.array["No.Participants", intervention, t - 2, study]
                    if (is.na(n)){
                      cat(
                        "ERROR#2: no n of present intervention of study:", study.names[study], "\n",
                        "intervention:", intervention, "\n",
                        "n:", n, "\n\n"
                      )
                    }
                  }
                } else if ((is.na(n)) & t == 2){
                  n <- population.characteristics.array["No.Participants", intervention, t - 1, study]
                  if (is.na(n)){
                    cat(
                      "ERROR#3: no n of present intervention of study:", study.names[study], "\n",
                      "intervention:", intervention, "\n",
                      "n:", n, "\n\n"
                    )
                  }
                }

              } else if (
                imputation %in% methods.keeping.n.of.t0 |
                ITT.PP.analysis %in% methods.keeping.n.of.t0
              ){
                n <- population.characteristics.array["No.Participants", intervention, 1, study]
                # for ITT: "once randomized, always analyzed" --> n = n of T0
                if (is.na(n)){
                  cat("ERROR#5: no n of present intervention of study:", study.names[study], "\n")
                }

              } else {
                cat(
                  "Unknown case #1, find solution\n",
                  "Study: ", study.names[study], "\n",
                  "Imputation of missing Data:", one.D.info.df[study,"Imputation.of.missing.Data"], "\n",
                  "ITT or Per-Protocol-Analysis:", one.D.info.df[study,"ITT.or.Per.Protocol.Analysis.present."], "\n",
                  "n:", n, "\n\n"
                   )
              }
              results.descriptive.array[intervention, "n", t, outcome, scale, study] <- n  # 
            }
          }
        }
      }
    }
  }
}


# %% [markdown] heading_collapsed=true hidden=true
# ### Age

# %% hidden=true vscode={"languageId": "r"}
# get overall mean age of T1 (post-test)
age.overall.t1 <- NA
for (study in 1:study.no){
  
  # get over all mean age T1
  if (
    !is.na(population.characteristics.array["Mean.Age", "Over.All", "T1", study]) &
    population.characteristics.array["Mean.Age", "Over.All", "T1", study] != nm.placeholder
  ){
    age.overall.t1 <- population.characteristics.array["Mean.Age", "Over.All", "T1", study]
  
  # if over all mean age T1 is NA, over all mean age T0 is over all mean age T1
  } else if (
    !is.na(population.characteristics.array["Mean.Age", "Over.All", "T0", study]) &
    population.characteristics.array["Mean.Age", "Over.All", "T0", study] != nm.placeholder
  ){
    age.overall.t1 <- population.characteristics.array["Mean.Age", "Over.All", "T0", study]
   
  # if over all mean age at T1 and T0 is NA, calculate over all mean age from mean of groups
  } else if (
    (
      is.na(population.characteristics.array["Mean.Age", "Over.All", "T1", study]) |
      population.characteristics.array["Mean.Age", "Over.All", "T1", study] == nm.placeholder
    ) &
    (
      is.na(population.characteristics.array["Mean.Age", "Over.All", "T0", study]) |
      population.characteristics.array["Mean.Age", "Over.All", "T0", study] == nm.placeholder
    )
  ){
    
    no.participants.vec <- c()
    mean.age.vec <- c()
    t.range <- c(1,0)
    for (intervention in 1:6){
      for (t in t.range){
        
        try(
          if (!(
                is.na(population.characteristics.array["Mean.Age", intervention, t, study]) |
                (population.characteristics.array["Mean.Age", intervention, t, study] %in% nm.placeholder) |
                is.na(population.characteristics.array["No.Participants", intervention, t, study]) |
                (population.characteristics.array["No.Participants", intervention, t, study] %in% nm.placeholder)
          )){

            no.participants.vec <- append(
              no.participants.vec,
              population.characteristics.array["No.Participants", intervention, t, study]
            )

            mean.age.vec <- append(
              mean.age.vec,
              population.characteristics.array["Mean.Age", intervention, t, study]
            )
            break  # do not add values of T0 if values of T1 are present
          },
          silent=TRUE
        )
      }
    }
    
    mean.age.x.n.vec <- c()
    
    if (length(no.participants.vec) != length(mean.age.vec)){
      cat(
        "\n\nERROR#1: length of no.participants.vec and mean.age.vec are not equal in:",
        "\n", study.names[study], "| Intervention", intervention, "| T", t, "\n\n"
      )
    }
    
    if (length(no.participants.vec) > 0){
      mean.age.x.n.vec <- mean.age.vec * no.participants.vec
      age.overall.t1 <- sum(mean.age.x.n.vec) / sum(no.participants.vec)
    }
  } else {
      print("ERROR#2: unknown case")
  }
  population.characteristics.array["Mean.Age", "Over.All", "T1", study] <- age.overall.t1
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Sex

# %% hidden=true vscode={"languageId": "r"}
# add rows of sex percentages to population.characteristics.array
population.characteristics.array <- abind(
  population.characteristics.array,
  array(NA, replace(dim(population.characteristics.array), 1, 3)),
  along = 1
)

dimnames(population.characteristics.array) <- list(
  c("No.Participants", "Mean.Age", "No.Females", "No.Males", "Females.Percent", "Males.Percent", "Diverse.Percent"),
  c("Intervention.1", "Intervention.2", "Intervention.3", "Intervention.4", "Intervention.5", "Intervention.6", "Over.All"),
  c("T0", "T1", "T2", "T3"),
  my.df[,"Study.ID"]
)

# %% hidden=true vscode={"languageId": "r"}
for (study in 1:study.no){
  if (
    !TRUE %in%
    c(
      population.characteristics.array[
        c("No.Females", "No.Males", "No.Participants"),
        "Over.All", "T1", study
      ] %in%
      c(NA, nm.placeholder)
    )
  ){
    population.characteristics.array[c("Females.Percent", "Males.Percent"), "Over.All", "T1", study] <- round(
      population.characteristics.array[c("No.Females", "No.Males"), "Over.All", "T1", study] /
      rep(population.characteristics.array["No.Participants", "Over.All", "T1", study], 2) * 100,
      digits = 2
    )
    
    population.characteristics.array["Diverse.Percent", "Over.All", "T1", study] <- round(
      100 -
      sum(population.characteristics.array[c("No.Females", "No.Males"), "Over.All", "T1", study]) /
      population.characteristics.array["No.Participants", "Over.All", "T1", study] * 100,
      digits = 2
    )
    
  } else if (
    !TRUE %in%
    c(
      population.characteristics.array[
        c("No.Females", "No.Males", "No.Participants"),
        "Over.All", "T0", study
      ] %in%
      c(NA, nm.placeholder)
    )
  ){
    population.characteristics.array[c("Females.Percent", "Males.Percent"), "Over.All", "T1", study] <- round(
      population.characteristics.array[c("No.Females", "No.Males"), "Over.All", "T0", study] /
      rep(population.characteristics.array["No.Participants", "Over.All", "T0", study], 2) * 100,
      digits = 2
    )
    
    population.characteristics.array["Diverse.Percent", "Over.All", "T1", study] <- round(
      100 -
      sum(population.characteristics.array[c("No.Females", "No.Males"), "Over.All", "T0", study]) /
      population.characteristics.array["No.Participants", "Over.All", "T0", study] * 100,
      digits = 2
    )
    
    population.characteristics.array["Diverse.Percent", "Over.All", "T1", study][population.characteristics.array["Diverse.Percent", "Over.All", "T1", study] < 0] <- 0
    
  } else (
    cat("\nnot enough sex data in study:", study.names[study])
  )
}

# %% hidden=true vscode={"languageId": "r"}
# investigate relevant female percentage
female.perc.used <- c(rep(NA, study.no))
male.perc.used <- c(rep(NA, study.no))
diverse.perc.used <- c(rep(NA, study.no))

names(female.perc.used) <- study.names
names(male.perc.used) <- study.names
names(diverse.perc.used) <- study.names

# add relevent female percentage per study to vector female.perc.used in regard to what imputation or ITT/PPA is used
for (study in 1:study.no){
  imputation <- one.D.info.df[study,"Imputation.of.missing.Data"]
  ITT.PP.analysis <- one.D.info.df[study,"ITT.or.Per.Protocol.Analysis.present."]
  
  female.perc.1 <- population.characteristics.array["Females.Percent", "Over.All", 1, study]
  female.perc.2 <- population.characteristics.array["Females.Percent", "Over.All", 2, study]
  
  male.perc.1 <- population.characteristics.array["Males.Percent", "Over.All", 1, study]
  male.perc.2 <- population.characteristics.array["Males.Percent", "Over.All", 2, study]
  
  diverse.perc.1 <- population.characteristics.array["Diverse.Percent", "Over.All", 1, study]
  diverse.perc.2 <- population.characteristics.array["Diverse.Percent", "Over.All", 2, study]
  
  if (
    imputation %in% methods.excluding.subjects &
    ITT.PP.analysis %in% methods.excluding.subjects
  ){
    if (is.na(female.perc.2) | female.perc.2 == nm.placeholder){
      female.perc.used[study] <- female.perc.1  # if value of pre-test equals the one of post-test, no value was inserted for post-test
      male.perc.used[study] <- male.perc.1
      diverse.perc.used[study] <- diverse.perc.1
    } else if (!(is.na(female.perc.2) | female.perc.2 == nm.placeholder)){
      female.perc.used[study] <- female.perc.2
      male.perc.used[study] <- male.perc.2
      diverse.perc.used[study] <- diverse.perc.2
    } else {
      print("ERROR#1 Unknown case")
    }
  } else if (
    imputation %in% methods.keeping.n.of.t0 |
    ITT.PP.analysis %in% methods.keeping.n.of.t0
  ){
    if (is.na(female.perc.1) | female.perc.1 == nm.placeholder){
      female.perc.used[study] <- female.perc.2  # of no value is present for pre-test, post-test value should be taken for approximation
      male.perc.used[study] <- male.perc.2
      diverse.perc.used[study] <- diverse.perc.2
    } else if (!(is.na(female.perc.1) | female.perc.1 == nm.placeholder)){
      female.perc.used[study] <- female.perc.1
      male.perc.used[study] <- male.perc.1
      diverse.perc.used[study] <- diverse.perc.1
    } else {
      print("ERROR#2 Uknown case")
    }
    # for ITT: "once randomized, always analyzed" --> n = n of T0
  } else {
    cat(
      "ERROR#3 Unknown case, find solution\n",
      "Study: ", study.names[study], "\n",
      "Imputation of missing Data:", one.D.info.df[study,"Imputation.of.missing.Data"], "\n",
      "ITT or Per-Protocol-Analysis:", one.D.info.df[study,"ITT.or.Per.Protocol.Analysis.present."], "\n",
      "female.perc.1 and 2:", female.perc.1, female.perc.2, "\n\n"
       )
  }
}
diverse.perc.used[which(diverse.perc.used < 0)] <- 0
data.frame(female.perc.used, male.perc.used, diverse.perc.used)

for (study in study.names){
  intervention.comparisons.df.list[[study]]$female.percent <- female.perc.used[study]
  intervention.comparisons.df.list[[study]]$male.percent <- male.perc.used[study]
  intervention.comparisons.df.list[[study]]$diverse.percent <- diverse.perc.used[study]
  
  intervention.comparisons.df.list.w.o.mean.r[[study]]$female.percent <- female.perc.used[study]
  intervention.comparisons.df.list.w.o.mean.r[[study]]$male.percent <- male.perc.used[study]
  intervention.comparisons.df.list.w.o.mean.r[[study]]$diverse.percent <- diverse.perc.used[study]
}
intervention.comparisons.df.list; intervention.comparisons.df.list.w.o.mean.r

# %% [markdown] heading_collapsed=true
# ## Put total data into a list

# %% hidden=true vscode={"languageId": "r"}
m.data.list <- list(
  one.D.info.df,
  population.characteristics.array,
  intervention.comparisons.df.list,
  intervention.comparisons.df.list.w.o.mean.r,
  outcome.measures.df.list,
  results.descriptive.array,
  results.quantitative.array,
  results.qualitative.df.list,
  dates.measuring.time.points.df,
  between.T.duration.df,
  outcome.definitions.df,
  outcome.names.df,
  rob.df,
  meditation.techniques.df
)

names(m.data.list) <- c(
  "one.D.info.df",
  "population.characteristics.array",
  "intervention.comparisons.df.list",
  "intervention.comparisons.df.list.w.o.mean.r",
  "outcome.measures.df.list",
  "results.descriptive.array",
  "results.quantitative.array",
  "results.qualitative.df.list",
  "dates.measuring.time.points.df",
  "between.T.duration.df",
  "outcome.definitions.df",
  "outcome.names.df",
  "rob.df",
  "meditation.techniques.df"
)


# %% [markdown] cell_style="center" heading_collapsed=true
# ## Get all present outcomes names

# %% cell_style="center" code_folding=[] hidden=true vscode={"languageId": "r"}
#
present.outcomes <- c()

for (row in 1:nrow(outcome.names.df)){
  for (col in 1:ncol(outcome.names.df)){
    if (!(
      is.na(outcome.names.df[row, col]) |
      outcome.names.df[row, col] == "NA"
    )){
      present.outcomes <- append(present.outcomes, outcome.names.df[row, col])
    }
  }
}

# present.outcomes <- present.outcomes[-c(which(present.outcomes == "Other: "))]  # delete "Other: " already done above
outcomes.no.df <- data.frame(table(present.outcomes))

present.outcomes <- unique(present.outcomes)
outcomes.no.df


# %% code_folding=[] hidden=true vscode={"languageId": "r"}
outcomes.no.10.plus <- as.vector(outcomes.no.df[
  outcomes.no.df$Freq >= 10, "present.outcomes"
])
outcomes.no.10.plus



# %% [markdown] heading_collapsed=true
# ## Check if direction of scale is the same for each outcome

# %% hidden=true vscode={"languageId": "r"}
scales.per.outcome.df.list <- rep(
  list(data.frame(
      Scale = c(NA),
      High.or.low.means.resilient = c(NA)
  )),
  length(present.outcomes)
)
names(scales.per.outcome.df.list) <- present.outcomes

for (study in 1:study.no){
  for (outcome in 1:7){
    outcome.name <- outcome.names.df[study, outcome]
    scale.name <- outcome.measures.df.list[[study]][outcome, "Measures.Name"]
    h.l.resilient <- outcome.measures.df.list[[study]][outcome, "High.or.low.means.resilient"]
    if (!(
      (TRUE %in% is.na(c(outcome.name, scale.name))) |
      (TRUE %in% (
        c(outcome.name, scale.name) %in%
        c("NA", nm.placeholder, as.character(nm.placeholder), "Other: ")
      ))
    )){
      nrow.df <- nrow(scales.per.outcome.df.list[[outcome.name]])
      if (nrow.df == 1 & is.na(scales.per.outcome.df.list[[outcome.name]][1,1])){
        nrow.df <- 0
      }
      scales.per.outcome.df.list[[outcome.name]][nrow.df + 1,"Scale"] <- scale.name
      scales.per.outcome.df.list[[outcome.name]][nrow.df + 1,"High.or.low.means.resilient"] <-
        h.l.resilient
    }
  }
}
scales.per.outcome.df.list

# %% hidden=true vscode={"languageId": "r"}
# keep only unique rows
for (n in 1:length(scales.per.outcome.df.list)){
  scales.per.outcome.df.list[[n]] <- scales.per.outcome.df.list[[n]] %>%
    distinct(Scale, High.or.low.means.resilient, .keep_all=TRUE)
}
scales.per.outcome.df.list

# %% hidden=true vscode={"languageId": "r"}
# find unique scale directions
## create empty df.list
scale.direction.per.outcome.df.list <- rep(
  list(data.frame(
      Scale = c(NA),
      High.or.low.means.resilient = c(NA)
  )),
  length(present.outcomes)
)
names(scale.direction.per.outcome.df.list) <- present.outcomes


## fill empty df.list
for (outcome in 1:length(present.outcomes)){
  for (scale in 1:nrow(scales.per.outcome.df.list[[outcome]])){
    
    scale.name <- scales.per.outcome.df.list[[outcome]][scale, "Scale"]
    filtered.scale.names <- scale.direction.per.outcome.df.list[[outcome]][, "Scale"]
    h.l.resilient.new <- scales.per.outcome.df.list[[outcome]][scale, "High.or.low.means.resilient"]
    h.l.resilient.old <- scale.direction.per.outcome.df.list[[outcome]][
        scale.direction.per.outcome.df.list[[outcome]]$Scale == scale.name,
        "High.or.low.means.resilient"
    ]
    
    nrow.df <- nrow(scale.direction.per.outcome.df.list[[outcome]])
    if (nrow.df == 1 & is.na(scale.direction.per.outcome.df.list[[outcome]][1,1])){
      nrow.df <- 0
    }
    
    if (!scale.name %in% filtered.scale.names){
      scale.direction.per.outcome.df.list[[outcome]][nrow.df + 1,"Scale"] <- scale.name
      scale.direction.per.outcome.df.list[[outcome]][nrow.df + 1,"High.or.low.means.resilient"] <- h.l.resilient.new
      
    } else if (!h.l.resilient.new %in% h.l.resilient.old){
      if (
        is.na(h.l.resilient.old) |
        (h.l.resilient.old %in% c("NA", nm.placeholder, as.character(nm.placeholder)))
      ){
        scale.direction.per.outcome.df.list[[outcome]][
          scale.direction.per.outcome.df.list[[outcome]]$Scale == scale.name,
          "High.or.low.means.resilient"
        ] <- h.l.resilient.new  
        
      } else if (is.na(h.l.resilient.new) | h.l.resilient.new %in% c("NA", nm.placeholder, as.character(nm.placeholder))){
        next
      } else {
        cat(
          "\n\n", "h.l.resilient is different in outcome", present.outcomes[outcome], "and scale name", scale.name,
          h.l.resilient.new, h.l.resilient.old, "\n\n"
        )
        scale.direction.per.outcome.df.list[[outcome]][
          scale.direction.per.outcome.df.list[[outcome]]$Scale == scale.name,
          "High.or.low.means.resilient"
        ] <- NA
      }
    }
  }
}
scale.direction.per.outcome.df.list

# %% hidden=true vscode={"languageId": "r"}
# correct direction mistake in SWLS
scale.direction.per.outcome.df.list[["Well-being"]][1, "High.or.low.means.resilient"] <- "^"
scale.direction.per.outcome.df.list
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7967519/

# %% hidden=true vscode={"languageId": "r"}
# find unclear scale directions
scale.direction.unclear.df.list <- scale.direction.per.outcome.df.list
for (outcome in 1:length(present.outcomes)){
  scale.direction.unclear.df.list[[outcome]] <- scale.direction.unclear.df.list[[outcome]] %>%
    filter(
      is.na(High.or.low.means.resilient) |
      High.or.low.means.resilient %in% c("NA", nm.placeholder, as.character(nm.placeholder))
    )
}
scale.direction.unclear.df.list

# %% vscode={"languageId": "r"}
scale.direction.per.outcome.df.list

# %% hidden=true vscode={"languageId": "r"}
# add missing scale direction (inclomplet for search update)
for (outcome in 1:length(scale.direction.per.outcome.df.list)){
  for (scale in 1:nrow(scale.direction.per.outcome.df.list[[outcome]])){
    scale.name <- scale.direction.per.outcome.df.list[[outcome]][scale,"Scale"]
    if (scale.name == "POMS"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "v"  # http://dx.doi.org/10.3389/fpsyg.2021.631668
    } else if (scale.name == "BRS"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "v"
        # https://doi.org/10.1016/0005-7916(83)90027-7
        # "Scores on the BRS could range from 50 to zero, with lower scores indicating greater relaxation"
    } else if (scale.name == "SAS"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "v"
        # https://doi.org/10.1186/s12888-019-2427-6
        # '“I feel my heartbeating fast.”) in nature. Responses are given on a 4-point scale which range from 1 (none, or a little of thetime) to 4 (most, or all of the time)''
    } else if (scale.name == "STAI"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "v"
        # https://www.mdpi.com/1660-4601/19/12/7053
        # "A result was considered positive when a score equal to or higher than 30 points was obtained in each subscale"
    } else if (scale.name == "SDS"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "v"
        # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5591521/
        # An SDS Index score of 50 (raw score = 40) suggests clinically significant symptoms with the following three levels of severity ratings: Index scores 25–49 (raw scores 20–40) Normal; 50–59 (raw scores 41–47) Mild to Moderate; 60–69 (raw scores 48–55) Moderate to Severe; and 70 and over (raw scores 56 and over) Severe [23]. ""
    } else if (scale.name == "BDI"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "v"
        # https://www.cambridge.org/core/services/aop-cambridge-core/content/view/832EDF02CBC32FF1297CE7A4F67CF7B1/S2045796019000088a.pdf/div-class-title-translating-the-bdi-and-bdi-ii-into-the-hamd-and-vice-versa-with-equipercentile-linking-div.pdf
    } else if (scale.name == "QIDS-SR"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "v"
        # https://doi.org/10.3389/fpsyt.2020.598609
        # "The C-QIDS-SR total score ranges from 0 to 27, with higher scores indicating more severe depressive symptomatology."
    } else if (scale.name == "PCQ"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "^"
        # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7967519/
        # ''“I always look on the bright side of things” (Optimism scale); and “I usually manage difficulties one way or another” (Resilience scale). In this study, the response pattern followed a 6-point Likert scale ranging from 6 (totally agree) to 1 (totally disagree).''
    } else if (scale.name == "MAAS"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "^"
        # https://www.actaspsiquiatria.es/repositorio//14/75/ENG/14-75-ENG-19-26-466350.pdf
        # "The five items of the MAAS-5 have six Likert-type response options (6 = almost never to 1 = almost always)" "Examples of the items are “I perform my activities quickly, without being very attentive to what I am doing”, “I do work automatically, without noticing what I am doing” "
    } else if (scale.name == "WHO-QOL-BREF"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "^"
        # https://www.who.int/publications/i/item/WHOQOL-BREF
    } else if (scale.name == "RAND 36-Item Health Survey"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "^"
        # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8045101/
    } else if (scale.name == "CPI"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "^"
    } else if (scale.name == "CAMS-R"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "^"
    } else if (scale.name == "CPI"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "^"
    } else if (scale.name == "BEA"){
      scale.direction.per.outcome.df.list[[outcome]][scale,"High.or.low.means.resilient"] <- "^"
    }
  }
}
scale.direction.per.outcome.df.list

# %% hidden=true vscode={"languageId": "r"}
# find left unclear scale dircetions
outcome.direction.df <- data.frame(
  Outcome = present.outcomes,
  High.or.low.means.resilient = rep(NA, length(present.outcomes))
)

for (outcome in 1:length(present.outcomes)){
  scale.direction <- scale.direction.per.outcome.df.list[[outcome]][1, "High.or.low.means.resilient"]
  
  other.scale.direction.found <- FALSE
  for (scale in 1:nrow(scale.direction.per.outcome.df.list[[outcome]])){
    if (
      is.na.or.nm(scale.direction.per.outcome.df.list[[outcome]][scale, "High.or.low.means.resilient"]) |
      is.na.or.nm(scale.direction)
    ){
      next
    }

    if (!grepl(scale.direction, scale.direction.per.outcome.df.list[[outcome]][scale, "High.or.low.means.resilient"])){
      other.scale.direction.found <- TRUE
        # does not consider if first and second scale direction (separated by comma) differ from each other
        # does not work if first scale direction is a comma separated value
    }
  }
       
  if (!other.scale.direction.found){
    outcome.direction.df[outcome, "High.or.low.means.resilient"] <- scale.direction
  } else {
    cat("unclear scale direction for outcome:", present.outcomes[outcome], "\n\n")
  }
}
outcome.direction.df

# %% vscode={"languageId": "r"}
# correct input mistakes
outcome.direction.df <- outcome.direction.df |>
  mutate(
    High.or.low.means.resilient = replace(
      High.or.low.means.resilient,
      Outcome %in% c("Depression", "Stress"),
      "v"
    )
  )
outcome.direction.df

# %% hidden=true vscode={"languageId": "r"}
# correct left scale directions
outcome.direction.df <- outcome.direction.df |>
  mutate(
    High.or.low.means.resilient = replace(
      High.or.low.means.resilient,
      Outcome %in% c("Cognitive control", "Self-compassion", "Positive emotion", "Positive affect"),
      "^"
    ),
    High.or.low.means.resilient = replace(
      High.or.low.means.resilient,
      Outcome %in% c("Anxiety"),
      "v"
    ),
  )
outcome.direction.df

# %% [markdown] heading_collapsed=true
# ## Check which data is present for outcomes with 2 scales per study

# %% hidden=true vscode={"languageId": "r"}
# Outcomes in which occure 2 scales per outcome per study at once (manually detected)
outcome.measures.mult.df <- data.frame(
  Outcome = c("Stress", "Positive affect", "Self-compassion"),
  Scales = c("DASS, PSS, SOM", "IPANAT, PANAS", "SCS, VAS")
)
outcome.measures.mult.df

# %% [markdown] heading_collapsed=true hidden=true
# ### Stress | SOM

# %% hidden=true vscode={"languageId": "r"}
search.df.list(outcome.measures.df.list, "SOM")

# %% hidden=true vscode={"languageId": "r"}
print.array.not.na(results.descriptive.array[,,"T1","Outcome.1",,"Dawson 2014"])
# no sufficient data for SOM sacle --> not relevant for analyses

# %% [markdown] heading_collapsed=true hidden=true
# ### Positive affect | IPANAT vs. PANAS

# %% hidden=true vscode={"languageId": "r"}
search.df.list(outcome.measures.df.list, "IPANAT")

# %% hidden=true vscode={"languageId": "r"}
print.array.not.na(results.descriptive.array[,,"T1","Outcome.1",,"Hirshberg 2018"])

# %% hidden=true vscode={"languageId": "r"}
search.df.list(outcome.measures.df.list, "IPANAT")
outcome.measures.df.list[["Hirshberg 2018"]][
  which(outcome.names.df["Hirshberg 2018",] == "Positive affect")
]
meditation.techniques.df["Hirshberg 2018",]
meditation.techniques.df["Hirshberg 2018",] %in% control.all.active
meditation.techniques.df["Hirshberg 2018",] %in% meditation.type.all
# only inverentions clustered as meditation in Hirshberg 2018 --> not relevant for data collection in meta.analyze() and in turn for all analyses
# (could be used for a network-meta analysis within meditation types which is not investigated)

# %% hidden=true vscode={"languageId": "r"}
search.df.list(outcome.measures.df.list, "PANAS")

# %% [markdown] heading_collapsed=true hidden=true
# ### Self-compassion | SCS vs. VAS

# %% hidden=true vscode={"languageId": "r"}
search.df.list(outcome.measures.df.list, "SCS, VAS")
search.df.list(outcome.measures.df.list, "VAS, SCS")
print.array.not.na(results.descriptive.array[,,"T1","Outcome.1",,"Forsyth 2017"])
meditation.techniques.df["Forsyth 2017",]
meditation.techniques.df["Forsyth 2017",] %in% control.all.active
meditation.techniques.df["Forsyth 2017",] %in% meditation.type.all
# only inverentions clustered as meditation in Hirshberg 2018 --> not relevant for data collection in meta.analyze() and in turn for all analyses
# (could be used for a network-meta analysis within meditation types which is not investigated)

# %% [markdown] hidden=true
# --> no sufficient data for SOM and multiple scales for positive affect and self-compassion not relevant

# %% [markdown] heading_collapsed=true
# # Meta-Analysis Functions

# %% hidden=true vscode={"languageId": "r"}
# set plot size
# options(repr.plot.width = 12, repr.plot.height = 6, repr.plot.res = 400)

# universal functions
# forest etc.
# print.meta.results(
#   "Stress", preferred.scale = "DASS",
#   regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
#   return.data = "results.meta" # ,
#   # filter.forest..funnel.vec = - outlier.list[["Stress"]]
# )

# regression etc.
# print.meta.results(
#   "Stress", preferred.scale = "DASS",
#   basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
#   regression.label = T, return.data = "results.metafor"  # ,
#   # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][["Stress"]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][["Stress"]])}
# )

# network meta-analysis
# net.meta.analyze(
#   c("Stress"), preferred.scale = "DASS", net.df = F, net.res = F, comparisons.skip.list = F,
#   plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = T,
#   reference.group = "passive control", random = T, return.data = F
# )

# %% hidden=true vscode={"languageId": "r"}
# install.packages("meta")
# if (!require("remotes")) {
#   install.packages("remotes")
# }
# remotes::install_github("MathiasHarrer/dmetar")
# install.packages(c("netmeta", "metafor", "esc", "gridExtra", "fpc", "mclust"))

library(esc)
library(metafor)
library(meta)
library(netmeta)
library(gridExtra)
library(grid)
library(dmetar)
library(fpc)
library(mclust)

# %% [markdown] hidden=true
# ### Set paramters

# %% [markdown] heading_collapsed=true hidden=true
# #### Units

# %% code_folding=[] hidden=true vscode={"languageId": "r"}
# set units
regression.labels.df <- data.frame(
  sessions.duration = "Session duration [minutes]",
  sessions.frequency = "Session frequency [1/week]",
  programs.duration = "Program duration [days]",
  follow.up.period = "Follow-up period [days]",
  delivery.mode = "0 = asynchronous; 1 = asynchronous guiding",
  meditation.type = "meditation type",
  female.percent = "Female percentage [%]"
)
regression.labels.df

# %% [markdown] heading_collapsed=true hidden=true
# #### Outliers and Influential cases

# %% code_folding=[] hidden=true vscode={"languageId": "r"}
# set outliers
# for regression and subgroup analyses cases with k < 10 were not taken into account
outlier.list <- list(
  `Resilience Scale` = c(),
  `Anxiety` = c(12),
  `Depression` = c(10),
  Stress = c(17),
  `Well-being or quality of life` = c(),
  Acceptance = c(),
  `Active coping` = c(),
  `Cognitive control` = c(),
  Empathy = c(),
  Hope = c(),
  `Mindfulness` = c(10, 11),
  `Optimism or positive attributional style` = c(),
  `Positive emotion` = c(),
  `Religiosity or spirituality or religious coping` = c(),
  `Self-acceptance` = c(),
  `Self-compassion` = c(),
  `Self-efficacy` = c(),
  `Self-esteem` = c(),

# Program's Duration
  programs.duration.lin = list(
    `Resilience Scale` = c(),
    `Anxiety` = c(11),
    `Depression` = c(),  # k < 10
    Stress = c(1, 8, 16),
    `Well-being or quality of life` = c(),
    Acceptance = c(),
    `Active coping` = c(),
    `Cognitive control` = c(),
    Empathy = c(),
    Hope = c(),
    `Mindfulness` = c(),  # k < 10
    `Optimism or positive attributional style` = c(),
    `Positive emotion` = c(),
    `Religiosity or spirituality or religious coping` = c(),
    `Self-acceptance` = c(),
    `Self-compassion` = c(),
    `Self-efficacy` = c(),
    `Self-esteem` = c()
  ),
  
  programs.duration.sq = list(
    `Resilience Scale` = c(),
    `Anxiety` = c(1, 11),
    `Depression` = c(),  # k < 10
    Stress = c(1, 8, 16),
    `Well-being or quality of life` = c(),
    Acceptance = c(),
    `Active coping` = c(),
    `Cognitive control` = c(),
    Empathy = c(),
    Hope = c(),
    `Mindfulness` = c(),  # k < 10
    `Optimism or positive attributional style` = c(),
    `Positive emotion` = c(),
    `Religiosity or spirituality or religious coping` = c(),
    `Self-acceptance` = c(),
    `Self-compassion` = c(),
    `Self-efficacy` = c(),
    `Self-esteem` = c()
  ),
  
# Sessions' Duration
  sessions.duration.lin = list(
    `Resilience Scale` = c(),
    `Anxiety` = c(1, 10),
    `Depression` = c(),  # k < 10
    Stress = c(6, 12),
    `Well-being or quality of life` = c(),
    Acceptance = c(),
    `Active coping` = c(),
    `Cognitive control` = c(),
    Empathy = c(),
    Hope = c(),
    `Mindfulness` = c(),  # k < 10
    `Optimism or positive attributional style` = c(),
    `Positive emotion` = c(),
    `Religiosity or spirituality or religious coping` = c(),
    `Self-acceptance` = c(),
    `Self-compassion` = c(),
    `Self-efficacy` = c(),
    `Self-esteem` = c()
  ),
  
  sessions.duration.sq = list(
    `Resilience Scale` = c(),
    `Anxiety` = c(1, 10),
    `Depression` = c(),  # k < 10
    Stress = c(6, 12),
    `Well-being or quality of life` = c(),
    Acceptance = c(),
    `Active coping` = c(),
    `Cognitive control` = c(),
    Empathy = c(),
    Hope = c(),
    `Mindfulness` = c(),  # k < 10
    `Optimism or positive attributional style` = c(),
    `Positive emotion` = c(),
    `Religiosity or spirituality or religious coping` = c(),
    `Self-acceptance` = c(),
    `Self-compassion` = c(),
    `Self-efficacy` = c(),
    `Self-esteem` = c()
  ),
  
# Sessions' Frequency
  sessions.frequency.lin = list(
    `Resilience Scale` = c(),
    `Anxiety` = c(2, 10),
    `Depression` = c(),  # k < 10
    Stress = c(10, 11, 12),
    `Well-being or quality of life` = c(),
    Acceptance = c(),
    `Active coping` = c(),
    `Cognitive control` = c(),
    Empathy = c(),
    Hope = c(),
    `Mindfulness` = c(),  # k < 10
    `Optimism or positive attributional style` = c(),
    `Positive emotion` = c(),
    `Religiosity or spirituality or religious coping` = c(),
    `Self-acceptance` = c(),
    `Self-compassion` = c(),
    `Self-efficacy` = c(),
    `Self-esteem` = c()
  ),
  
  sessions.frequency.sq = list(
    `Resilience Scale` = c(),
    `Anxiety` = c(1, 2, 10),
    `Depression` = c(),  # k < 10
    Stress = c(10, 11, 12),
    `Well-being or quality of life` = c(),
    Acceptance = c(),
    `Active coping` = c(),
    `Cognitive control` = c(),
    Empathy = c(),
    Hope = c(),
    `Mindfulness` = c(),  # k < 10
    `Optimism or positive attributional style` = c(),
    `Positive emotion` = c(),
    `Religiosity or spirituality or religious coping` = c(),
    `Self-acceptance` = c(),
    `Self-compassion` = c(),
    `Self-efficacy` = c(),
    `Self-esteem` = c()
  ),
  
# Follow-up Period
  follow.up.period.lin = list(
    `Resilience Scale` = c(),
    `Anxiety` = c(),
    `Depression` = c(),
    Stress = c(16, 20),
    `Well-being or quality of life` = c(),
    Acceptance = c(),
    `Active coping` = c(),
    `Cognitive control` = c(),
    Empathy = c(),
    Hope = c(),
    `Mindfulness` = c(10, 11, 14),
    `Optimism or positive attributional style` = c(),
    `Positive emotion` = c(),
    `Religiosity or spirituality or religious coping` = c(),
    `Self-acceptance` = c(),
    `Self-compassion` = c(),
    `Self-efficacy` = c(),
    `Self-esteem` = c()
  ),
  
  follow.up.period.sq = list(
    `Resilience Scale` = c(),
    `Anxiety` = c(),
    `Depression` = c(),
    Stress = c(16, 19, 20),
    `Well-being or quality of life` = c(),
    Acceptance = c(),
    `Active coping` = c(),
    `Cognitive control` = c(),
    Empathy = c(),
    Hope = c(),
    `Mindfulness` = c(10, 11, 12, 14),
    `Optimism or positive attributional style` = c(),
    `Positive emotion` = c(),
    `Religiosity or spirituality or religious coping` = c(),
    `Self-acceptance` = c(),
    `Self-compassion` = c(),
    `Self-efficacy` = c(),
    `Self-esteem` = c()
  ),
  
  meditation.type = list(
    `Resilience Scale` = c(),
    `Anxiety` = c(12),
    `Depression` = c(2),
    Stress = c(16, 17),  # "DASS" was used as preferred scale
    `Well-being or quality of life` = c(),
    Acceptance = c(),
    `Active coping` = c(),
    `Cognitive control` = c(),
    Empathy = c(),
    Hope = c(),
    `Mindfulness` = c(10, 11),
    `Optimism or positive attributional style` = c(),
    `Positive emotion` = c(),
    `Religiosity or spirituality or religious coping` = c(),
    `Self-acceptance` = c(),
    `Self-compassion` = c(),
    `Self-efficacy` = c(),
    `Self-esteem` = c()
  ),
  
  delivery.mode = list(
    `Resilience Scale` = c(),
    `Anxiety` = c(12),
    `Depression` = c(1, 2, 10),
    Stress = c(1, 16, 17),
    `Well-being or quality of life` = c(),
    Acceptance = c(),
    `Active coping` = c(),
    `Cognitive control` = c(),
    Empathy = c(),
    Hope = c(),
    `Mindfulness` = c(10, 11),
    `Optimism or positive attributional style` = c(),
    `Positive emotion` = c(),
    `Religiosity or spirituality or religious coping` = c(),
    `Self-acceptance` = c(),
    `Self-compassion` = c(),
    `Self-efficacy` = c(),
    `Self-esteem` = c()
  ),
  
  overall = c(1, 4, 5, 7, 65, 67, 68, 69),  # single entries in multivariate 3-level meta-analysis
  
  net.overall = c(
    "Spruin 2021",  # causing inconsistancy in comparisons with dog therapy
    "Ratanasiripong 2015", # causing heterogeneity in comparisons with biofeedback
    'Bultas 2021', 'Devillers-Réolon 2022', 'Huberty 2019', 'Messer 2016', 'Bonamo 2015',  # causing heterogeneity or have high influence in meditation (exclusive) vs. passive control
    "Silvestre-López 2021", "Ramsburg 2014",  # causing heterogeneity or have high influence  in meditation (exclusive) vs. passive control
    'Klibert 2022', 'Spruin 2021'  # causing heterogeneity in meditation (exclusive) vs. stress management
  )
)

# %% [markdown] heading_collapsed=true hidden=true
# #### Outlier analysis of categorical moderators

# %% hidden=true vscode={"languageId": "r"}
# # outlier analysis for categorical moderators "meditation.type" and "delivery.mode" was not implemented into the dashboard but did here instead
# outcome <- "Stress"
# preferred.scale <- "DASS"
# # moderator.vec <- c("meditation.type")
# moderator.vec <- c("delivery.mode")

# # influence plots
# plot.influnece(
#   print.meta.results(
#     outcome, preferred.scale = preferred.scale,
#     basic = F, moderator.vec = moderator.vec, print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
#     regression.label = T, return.data = "regression.results.linear"  # ,
#     # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][["Stress"]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][["Stress"]])}
#   )
# )

# # baujat plot
# print.meta.results(
#   outcome, preferred.scale = preferred.scale,
#   basic = F, moderator.vec = moderator.vec, print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
#   regression.label = T, return.data = F  # ,
#   # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][["Stress"]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][["Stress"]])}
# )

# # forest plot
# print.meta.results(
#   outcome, preferred.scale = preferred.scale,
#   regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = moderator.vec[1], print.meta.results = F,
#   return.data = F # ,
#   # filter.forest..funnel.vec = - outlier.list[["Stress"]]
# )



# %% hidden=true vscode={"languageId": "r"}
# # Get influence plot for overall results
# res.overall <- get.overall.res.metafor()
# options(repr.plot.width = 20, repr.plot.height = 7, repr.plot.res = 150)
# plot.influnece(res.overall)

# %% hidden=true vscode={"languageId": "r"}
# # use trim and fill metheod to insert potential missing studies due to publication bias
# options(repr.plot.width = 15, repr.plot.height = 7, repr.plot.res = 150)
# study.labels <- res.overall$data$id
# study.labels[-outlier.list$overall] <- ""
# funnel(  # with paramter slab adjusted
#   res.overall, legend = T,  yaxis="seinv", label = T, slab = study.labels,
#   level=c(90, 95, 99), shade=c("white", "gray55", "gray75"),
#   refline=0, lty = 0, refline2 = res.overall$b[1,1], lty2 = 3
#     # results.meta$TE.random = overall effect size of the random effects model
# )

# %% hidden=true vscode={"languageId": "r"}
# analyses for outliers/influential cases is done in the section of sensitivity analyses (sub-section Cutting out Studies due to results of netheat and netsplit)

# %% [markdown] heading_collapsed=true hidden=true
# #### Get study labes of outlaying/influential moderators

# %% hidden=true vscode={"languageId": "r"}
# outl.stud.labs.df <- data.frame(matrix(nrow = 0, ncol = 4))
# colnames(outl.stud.labs.df) <- c("Outcome", "Moderator", "Degree", "Outliers or influential cases")
                                
# for (outcome in c("Anxiety", "Depression", "Mindfulness", "Stress")){
#   for (moderator in c("sessions.duration", "sessions.frequency", "programs.duration", "follow.up.period", "delivery.mode", "meditation.type")){
#     for (degree in c(".lin", ".sq")){
#       if (
#         (moderator %in% c("delivery.mode", "meditation.type") & degree == ".sq") |
#         (outcome == "Anxiety" & moderator == "follow.up.period") |
#         (outcome %in% c("Depression", "Mindfulness") & !moderator %in% c("delivery.mode", "meditation.type"))
#       ){
#         next
#       }
      
#       if (moderator %in% c("sessions.duration", "sessions.frequency", "programs.duration", "follow.up.period")){
#         outl.stud.labs <- unique(print.meta.results(
#           outcome, preferred.scale = get.1st.preferred.scale(outcome),
#           basic = F, moderator.vec = c(moderator), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = ifelse(degree == ".lin", T, F), regression.degree.2 = ifelse(degree == ".sq", T, F),
#           regression.label = T, return.data = ifelse(degree == ".lin", "regression.results.linear", "regression.results.poly")
#         )$data$study.id[outlier.list[[paste(moderator, degree, sep = "")]][[outcome]]])
#       } else {
#         outl.stud.labs <- unique(print.meta.results(
#           outcome, preferred.scale = get.1st.preferred.scale(outcome),
#           basic = F, moderator.vec = c(moderator), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = ifelse(degree == ".lin", T, F), regression.degree.2 = ifelse(degree == ".sq", T, F),
#           regression.label = T, return.data = ifelse(degree == ".lin", "regression.results.linear", "regression.results.poly")
#         )$data$study.id[outlier.list[[moderator]][[outcome]]])
#       }
      
#       if (length(outl.stud.labs) == 0){
#         outl.stud.labs <- "None"
#       }
      
#       outl.stud.labs.df.temp <- data.frame(
#         Outcome = str_to_lower(outcome),
#         Moderator = moderator,
#         Degree = ifelse(degree == ".lin", "linear", "quadratic"),
#         `Outliers or influential cases` = paste(outl.stud.labs, collapse = ", ")
#       )
#       colnames(outl.stud.labs.df.temp) <- colnames(outl.stud.labs.df)
      
#       outl.stud.labs.df <- rbind(outl.stud.labs.df, outl.stud.labs.df.temp)
      
#     }
#   }
# }
# outl.stud.labs.df

# %% [markdown] heading_collapsed=true hidden=true
# ### Function | Return 1st preferred scale for outcomes that have multiple scales per outcome present

# %% hidden=true vscode={"languageId": "r"}
get.1st.preferred.scale <- function(outcome){
  if (outcome == "Stress"){
    return("DASS")
  } else if (outcome == "Positive affect"){
    return("IPANAT")
  } else if (outcome == "Self-compassion"){
    return("SCS")
  } else {
    return(F)
  }
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Get regression (moderator) data

# %% hidden=true vscode={"languageId": "r"}
get.regression.data <- function(
    m.data.list, moderator, study, intervention.no, nm.placeholder, without.mean.r
){
  moderators <- c(
    "Delivery.Mode", "Sessions.Duration.in.minutes", "Frequency.in.times.per.week",
    "Total.Duration.in.Days", "period.t1.t2", "period.t1.t3", "Meditation.Type",
    "female.percent", "male.percent", "diverse.percent"
  )
  
  if (without.mean.r){
    intervention.comparisons.df.list.str <- "intervention.comparisons.df.list.w.o.mean.r"
  } else {
    intervention.comparisons.df.list.str <- "intervention.comparisons.df.list"
  }
  
  if (!moderator %in% moderators){
    cat("use one of the following moderators in get.regression.data():\n", moderators)
  } else {
    moderator.value <- m.data.list[[intervention.comparisons.df.list.str]][[study]][intervention.no, moderator]
    if (!(
      is.na(moderator.value) |
      moderator.value %in% c("NA", nm.placeholder, as.character(nm.placeholder))
    )){
      regression.data <- moderator.value
    } else {
      regression.data <- NA
    }
    return(regression.data)
  }
}


# %% [markdown] heading_collapsed=true hidden=true
# ### Get Funnel and Regression Result

# %% code_folding=[] hidden=true vscode={"languageId": "r"}
get.results.metafor <- function(
  meta.df, moderator = FALSE, degree = 1, interaction = F, results.metafor.fixed = F  # , categorical.mod.trans = FALSE
){
  results.escalc <- escalc(
    data = meta.df, measure = "SMD",
    m1i = meta.df[,"mean.int"], sd1i = meta.df[,"sd.int"], n1i = meta.df[,"n.int"],
    m2i = meta.df[,"mean.control"], sd2i = meta.df[,"sd.control"], n2i = meta.df[,"n.control"]
  )
  
  if (if(is.logical(moderator)){moderator == F}else{F}){
    results.metafor <- rma.uni(
      yi, vi, measure="SMD", data = results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")  # "REML" is default method for random-effects model; "FE" fixed-effects model
    )
    
  } else {
    
    if (length(moderator) == 1){
      moderator.string <- moderator
    } else {
      if (interaction){
        moderator.string <- paste(moderator, collapse="*")
      } else {
        moderator.string <- paste(moderator, collapse="+")
      }
    }
    
    if (degree == 1){
      results.metafor <- rma.uni(
        yi, vi, mods = formula(paste("~", moderator.string)), data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
      )
    } else if (degree == 2) {
      if (length(moderator) == 1){
        results.metafor <- rma.uni(
          yi, vi, mods = ~ poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE), data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")  # formula(paste("~", moderator[1], "+I(", moderator[1], "^2)"))
        )
      } else if (length(moderator) == 2){
        
        if (interaction){
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE), 
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        } else {
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE), 
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        }
      } else if (length(moderator) == 3){
        if (interaction){
        results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[3]], degree=degree, raw=TRUE), 
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        } else {
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[3]], degree=degree, raw=TRUE), 
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        }
      } else if (length(moderator) == 4){
        if (interaction){
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[3]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[4]], degree=degree, raw=TRUE), 
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        } else {
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[3]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[4]], degree=degree, raw=TRUE), 
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        }
      } else if (length(moderator) == 5){
        if (interaction){
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[3]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[4]], degree=degree, raw=TRUE) *
            poly(results.escalc[, moderator[5]], degree=degree, raw=TRUE),  
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        } else {
          results.metafor <- rma.uni(
            yi, vi, mods = ~
            poly(results.escalc[, moderator[1]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[2]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[3]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[4]], degree=degree, raw=TRUE) +
            poly(results.escalc[, moderator[5]], degree=degree, raw=TRUE),  
            data=results.escalc, method = ifelse(results.metafor.fixed, "FE", "REML")
          )
        }
      } else {
          print("get.results.metafor() currently only works for polynomial multiple regressions of 1 to 5 moderator")
      }
    } else {
      print("get.results.metafor() currently only works for degree 1 or 2")
    }
  }
  return(results.metafor)
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Fill meta.df for meta.analyze()

# %% cell_style="center" code_folding=[] hidden=true vscode={"languageId": "r"}
fill.meta.df <- function(
  scale, time.point, intervention.no, control.no, outcome.no, study,
  m.data.list, nm.placeholder, descr..reg.data.list, without.mean.r
){
  
  t.with.pre <- time.point + 1
  
  # Study ID
  descr..reg.data.list[[time.point]][["study.id"]] <- append(
    descr..reg.data.list[[time.point]][["study.id"]],
    m.data.list[["one.D.info.df"]][study,"Study.ID"]
  )
  
  # Descriptive Results
  ## ...of control
  descr..reg.data.list[[time.point]][["n.control"]] <- append(
    descr..reg.data.list[[time.point]][["n.control"]],
    results.descriptive.array[control.no, "n", t.with.pre, outcome.no, scale, study]
  )
  descr..reg.data.list[[time.point]][["mean.control"]] <- append(
    descr..reg.data.list[[time.point]][["mean.control"]],
    results.descriptive.array[control.no, "Mean", t.with.pre, outcome.no, scale, study]
  )
  descr..reg.data.list[[time.point]][["sd.control"]] <- append(
    descr..reg.data.list[[time.point]][["sd.control"]],
    results.descriptive.array[control.no, "SD", t.with.pre, outcome.no, scale, study]
  )
  
  ## ...of intervention
  descr..reg.data.list[[time.point]][["n.int"]] <- append(
    descr..reg.data.list[[time.point]][["n.int"]],
    results.descriptive.array[intervention.no, "n", t.with.pre, outcome.no, scale, study]
  )
  descr..reg.data.list[[time.point]][["mean.int"]] <- append(
    descr..reg.data.list[[time.point]][["mean.int"]],
    results.descriptive.array[intervention.no, "Mean", t.with.pre, outcome.no, scale, study]
  )
  descr..reg.data.list[[time.point]][["sd.int"]] <- append(
    descr..reg.data.list[[time.point]][["sd.int"]],
    results.descriptive.array[intervention.no, "SD", t.with.pre, outcome.no, scale, study]
  )

# Results for Meta-Regression and Subgroup Analysis (this data will be drawn from the intervention group only)
  descr..reg.data.list[[time.point]][["sessions.duration"]] <- append(
    descr..reg.data.list[[time.point]][["sessions.duration"]],
    get.regression.data(m.data.list, "Sessions.Duration.in.minutes", study, intervention.no, nm.placeholder, without.mean.r)
  )

  descr..reg.data.list[[time.point]][["sessions.frequency"]] <- append(
    descr..reg.data.list[[time.point]][["sessions.frequency"]],
    get.regression.data(m.data.list, "Frequency.in.times.per.week", study, intervention.no, nm.placeholder, without.mean.r)
  )

  descr..reg.data.list[[time.point]][["programs.duration"]] <- append(
    descr..reg.data.list[[time.point]][["programs.duration"]],
    get.regression.data(m.data.list, "Total.Duration.in.Days", study, intervention.no, nm.placeholder, without.mean.r)
  )

  descr..reg.data.list[[time.point]][["delivery.mode"]] <- append(
    descr..reg.data.list[[time.point]][["delivery.mode"]],
    get.regression.data(m.data.list, "Delivery.Mode", study, intervention.no, nm.placeholder, without.mean.r)
  )

  descr..reg.data.list[[time.point]][["meditation.type"]] <- append(
    descr..reg.data.list[[time.point]][["meditation.type"]],
    get.regression.data(m.data.list, "Meditation.Type", study, intervention.no, nm.placeholder, without.mean.r)
  )

  descr..reg.data.list[[time.point]][["female.percent"]] <- append(
    descr..reg.data.list[[time.point]][["female.percent"]],
    get.regression.data(m.data.list, "female.percent", study, intervention.no, nm.placeholder, without.mean.r)
  )

  if (time.point == 1){
    descr..reg.data.list[[time.point]][["follow.up.period"]] <- append(
      descr..reg.data.list[[time.point]][["follow.up.period"]],
      0
    )
  } else if (time.point == 2){
    descr..reg.data.list[[time.point]][["follow.up.period"]] <- append(
      descr..reg.data.list[[time.point]][["follow.up.period"]],
      get.regression.data(m.data.list, "period.t1.t2", study, intervention.no, nm.placeholder, without.mean.r)
    )
    # note that t1 is post-test and t2 is 1st follow-up
  } else if (time.point == 3){
    descr..reg.data.list[[time.point]][["follow.up.period"]] <- append(
      descr..reg.data.list[[time.point]][["follow.up.period"]],
      get.regression.data(m.data.list, "period.t1.t3", study, intervention.no, nm.placeholder, without.mean.r)
    )
  } else {
    print("unknown time point in fill.meta.df()")
  }
  
  descr..reg.data.list
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Generate influence table

# %% hidden=true vscode={"languageId": "r"}
get.influence.df <- function(results.metafor){
  
  # get influence metrics
  results.metafor.inf <- influence(results.metafor)
  
  # define thresholds 
  rstudent.thresh <- qnorm(1 - (0.05 / (2 * results.metafor.inf$k))) # suggested by reporter function {metafor}
  dffits.thresh <- 3 * sqrt(results.metafor.inf$p / (results.metafor.inf$k - results.metafor.inf$p)) 
  hat.thresh <- 3 * (results.metafor.inf$p / results.metafor.inf$k)
  dfbetas.thresh <- 1 
  cook.d.thresh <- qchisq(0.5, df = results.metafor.inf$m)
  cook.d.threshs <- pchisq(results.metafor.inf$inf$cook.d, df=results.metafor.inf$m)
  cov.r.thresh <- 1
  
  # create threshold df
  threshs.df <- data.frame(matrix(
    data = c(rstudent.thresh, dffits.thresh, dfbetas.thresh, cook.d.thresh, 0.5, cov.r.thresh, NA, NA, hat.thresh, NA, NA),
    ncol = 11
  ))
  rownames(threshs.df) <- "thresh"
  colnames(threshs.df) <- c("rstudent", "dffits", "dfbetas", "cook.d", "cook.d.chi2.percent", "cov.r", "tau2.del", "QE.del", "hat", "weight", "inf")
  
  # generate data frame of all influece metrics
  inf.vals.df <- cbind(data.frame(results.metafor.inf$inf), data.frame(results.metafor.inf$dfbs)) %>%
    relocate(intrcpt, .after = dffits) %>%
    rename(dfbetas = intrcpt) %>%
    mutate(cook.d.chi2.percent = cook.d.threshs) %>%
  relocate(cook.d.chi2.percent, .after = cook.d)
  
  # add thresholds to data frame and round values
  inf.vals.df <- rbind(inf.vals.df, threshs.df) %>%
    mutate(across(where(is.numeric), ~ round(., digits = 2))) %>%
    rename(`is.influential {metafor}` = inf) %>%
    mutate(`is.influential {metafor}` = c(results.metafor.inf$is.infl, NA)) %>%
    mutate(study = rownames(.), .before = rstudent)
  return(as.data.frame(inf.vals.df))
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Plot influence plots

# %% code_folding=[31, 110] hidden=true vscode={"languageId": "r"}
# set plot size
# options(repr.plot.width = 11, repr.plot.height = 7, repr.plot.res = 200)

# this function is built on basis of the influence.analysis function of the dmetar package by Mathias Harrer et al.
# see: https://github.com/MathiasHarrer/dmetar/blob/master/R/influence.analysis.R

plot.influnece <- function(results.metafor, study.ids = "name", cluster = "id"){
  
  if ("rma.mv" %in% class(results.metafor)){
    class.mv <- T
  } else if ("rma.uni" %in% class(results.metafor)){
    class.mv <- F
  } else {
    cat("input has to be of class rma.uni or rma.mv got (from {metafor}) ", class(results.metafor), "instead.\n")
  }
  
  # get influence metrics
  results.metafor.inf <- if(class.mv){NA}else{influence(results.metafor)}
  
  # define thresholds 
  rstudent.thresh <- qnorm(1 - (0.05 / (2 * results.metafor$k))) # suggested by reporter function {metafor}
  dffits.thresh <- 3 * sqrt(results.metafor$p / (results.metafor$k - results.metafor$p)) 
  hat.thresh <- 3 * (results.metafor$p / results.metafor$k)
  dfbetas.thresh <- 1 
  cook.d.thresh <- qchisq(0.5, df = results.metafor$m)
  cov.r.thresh <- 1
  tau2.original <- results.metafor$tau2
  QE.original <- results.metafor$QE
  
  
  if (study.ids == "name"){
    if (class.mv) {
      # get study ids
      stud.id <- substr(results.metafor$data$study.id, 1, 4)

      # get study ids that are present more than once
      stud.id.tab <- table(stud.id)
      stud.id.multi <- names(stud.id.tab[stud.id.tab > 1])

      # add number to study ids that are present more than once
      if (length(stud.id.multi) >= 1){
        for (id.multi in stud.id.multi){
          id.m.i <- 1
          id.i <- 1
          for (id in stud.id){
            if (id %in% id.multi){
              stud.id[id.i] <- paste(stud.id[id.i], id.m.i)
              id.m.i <- id.m.i + 1
            }
          id.i <- id.i + 1
          }
        }
      } 
      # add outcome substring to id
      id.i <- 1
      for (id in stud.id){
        outc.string <- results.metafor$data[id.i, "outcome"]

        # get first 3 letters of outcome strings (if " " or "-" is present get first 3 letters ob splitted substrings)
        if (!(grepl(" ", outc.string) | grepl("-", outc.string))) {
          outc.string <- substr(results.metafor$data[id.i, "outcome"], 1, 4)
          outc.string <- paste(outc.string, id.i)
        } else if (grepl("-", outc.string[1])){
          outc.string <- substr(str_split_1(outc.string, "-")[1:2], 1, 3)
          outc.string <- paste(outc.string, sep = "-", collapse = "-")
          outc.string <- paste(outc.string, id.i)
        } else if (grepl(" ", outc.string)){
          outc.string <- substr(str_split_1(outc.string, " ")[1:2], 1, 3)
          outc.string <- paste(outc.string, sep = " ", collapse = " ")
          outc.string <- paste(outc.string, id.i)
        }

        stud.id[id.i] <- paste(stud.id[id.i], outc.string)

        id.i <- id.i + 1
      }
    } else {
      # get study ids
      stud.id <- substr(results.metafor$data$study.id, 1, 4)

      # get study ids that are present more than once
      stud.id.tab <- table(stud.id)
      stud.id.multi <- names(stud.id.tab[stud.id.tab > 1])

      # add number to study ids that are present more than once
      if (length(stud.id.multi) >= 1){
        for (id.multi in stud.id.multi){
          id.m.i <- 1
          id.i <- 1
          for (id in stud.id){
            if (id %in% id.multi){
              stud.id[id.i] <- paste(stud.id[id.i], id.m.i)
              id.m.i <- id.m.i + 1
            }
          id.i <- id.i + 1
          }
        }
      } 
    }
  } else if (study.ids == "num"){
    stud.id <- rownames(results.metafor$data)
  } else {
    print("error in plot.influnece: set parameter 'study.ids' to 'name' or 'num'")
  }
  
  # Generate plots
  y.ax.title.size <- 12
  y.ax.text.size <- 10
  
  if (!class.mv){
    
    # convert to desired data frame
    if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 1){
      cheungviechtdata = cbind(study.num = rownames(as.data.frame(results.metafor.inf$inf)), study.id = stud.id, as.data.frame(results.metafor.inf$inf), is.infl = results.metafor.inf$is.infl) %>%
        add_column(as.data.frame(results.metafor.inf$dfbs), .after = "dffits") %>%
        rename(dfbetas = intrcpt)
    } else {
      cheungviechtdata = cbind(study.num = rownames(as.data.frame(results.metafor.inf$inf)), study.id = stud.id, as.data.frame(results.metafor.inf$inf), is.infl = results.metafor.inf$is.infl) %>%
        add_column(as.data.frame(results.metafor.inf$dfbs), .after = "dffits")
    }
    
    # rstudent.thresh = qnorm(1 - (0.05 / (2 * results.metafor.inf$k)))
    rstudent.plot = ggplot(cheungviechtdata, aes(y = rstudent, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab(expression(r["stud-del"])) +
        geom_hline(yintercept = rstudent.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = -rstudent.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    # dffits.thresh = 3 * sqrt(results.metafor.inf$p/(results.metafor.inf$k - results.metafor.inf$p))
    dffits.plot = ggplot(cheungviechtdata, aes(y = dffits, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFFITS") +
        geom_hline(yintercept = dffits.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = - dffits.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')
    # cov.r.thresh = 1
    cov.r.plot = ggplot(cheungviechtdata, aes(y = cov.r, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("COVRATIO") +
        geom_hline(yintercept = cov.r.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    tau2.del.plot = ggplot(cheungviechtdata, aes(y = tau2.del, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab(~ paste(tau ^ 2, " (L-0-0)")) +
        geom_hline(yintercept = 0, linetype='dashed', color='grey') +
        geom_hline(yintercept = tau2.original, linetype='dashed', color='grey')

    QE.del.plot = ggplot(cheungviechtdata, aes(y = QE.del, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("Q (L-0-0)") +
        geom_hline(yintercept = 0, linetype='dashed', color='grey') +
        geom_hline(yintercept = QE.original, linetype='dashed', color='grey')
    weight.plot = ggplot(cheungviechtdata, aes(y = weight, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("study weight") +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')
    
    ###
    
    # cook.d.thresh = qchisq(0.5, df=results.metafor.inf$m)
    cook.d.plot = ggplot(cheungviechtdata, aes(y = cook.d, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab(expression(D["i"])) +
        geom_hline(yintercept = cook.d.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')



    # hat.thresh = 3 * (results.metafor.inf$p / results.metafor.inf$k)
    hat.plot = ggplot(cheungviechtdata, aes(y = hat, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) + geom_line(color = "black") +
        geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) + theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1),
        legend.position = "none",  axis.title.y = element_text(size = y.ax.title.size),
        axis.text.y = element_text(size = y.ax.text.size)) + ylab("HAT") + 
        geom_hline(yintercept = hat.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')



    # dfbetas.thresh = 1
    if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 1){
      dfbetas.plot = ggplot(cheungviechtdata, aes(y = dfbetas, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    } else if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 2){  # for meta-regression (linear)
      dfbetas.intrcpt.plot = ggplot(cheungviechtdata, aes(y = intrcpt, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(inter)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

      dfbetas.mod.plot = ggplot(cheungviechtdata, aes(y = !!sym(colnames(as.data.frame(results.metafor.inf$dfbs)[2])), x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(moderator)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    } else if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 3){  # for meta-regression (squared)
      dfbetas.intrcpt.plot = ggplot(cheungviechtdata, aes(y = intrcpt, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(inter)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

      dfbetas.mod1.plot = ggplot(cheungviechtdata, aes(y = !!sym(colnames(as.data.frame(results.metafor.inf$dfbs)[2])), x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(b1)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

      dfbetas.mod2.plot = ggplot(cheungviechtdata, aes(y = !!sym(colnames(as.data.frame(results.metafor.inf$dfbs)[3])), x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(b2)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    } else if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 4){  # for meta-regression (e.g., subgroup analysis with 4 levels)
      dfbetas.intrcpt.plot = ggplot(cheungviechtdata, aes(y = intrcpt, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(inter)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

      dfbetas.mod1.plot = ggplot(cheungviechtdata, aes(y = !!sym(colnames(as.data.frame(results.metafor.inf$dfbs)[2])), x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(b1)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

      dfbetas.mod2.plot = ggplot(cheungviechtdata, aes(y = !!sym(colnames(as.data.frame(results.metafor.inf$dfbs)[3])), x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(b2)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

      dfbetas.mod3.plot = ggplot(cheungviechtdata, aes(y = !!sym(colnames(as.data.frame(results.metafor.inf$dfbs)[4])), x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
          geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS(b3)") +
          geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    } else {
      print("error in plot.influnece(): function only works up to 4 model coefficients")
    }
    if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 1){
      rma.influence.plot = arrangeGrob(rstudent.plot, dffits.plot, cook.d.plot, cov.r.plot, tau2.del.plot, QE.del.plot,
          hat.plot, weight.plot, dfbetas.plot, ncol = 2)
    } else if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 2){
      rma.influence.plot = arrangeGrob(rstudent.plot, dffits.plot, cook.d.plot, cov.r.plot, tau2.del.plot, QE.del.plot,
          hat.plot, weight.plot, dfbetas.mod.plot, dfbetas.intrcpt.plot, ncol = 2)
    } else if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 3){
      rma.influence.plot = arrangeGrob(rstudent.plot, dffits.plot, cook.d.plot, cov.r.plot, tau2.del.plot, QE.del.plot,
          hat.plot, weight.plot, dfbetas.mod1.plot, dfbetas.mod2.plot, dfbetas.intrcpt.plot, ncol = 2)
    } else if (ncol(as.data.frame(results.metafor.inf$dfbs)) == 4){
      rma.influence.plot = arrangeGrob(rstudent.plot, dffits.plot, cook.d.plot, cov.r.plot, tau2.del.plot, QE.del.plot,
          hat.plot, weight.plot, dfbetas.mod1.plot, dfbetas.mod2.plot, dfbetas.mod3.plot, dfbetas.intrcpt.plot, ncol = 2)
    }
    
  } else {
    if (cluster == "id"){  # no clustering
     cheungviechtdata <- data.frame(
        study.id = stud.id,
        study.num = results.metafor$id,
        cook.d = c(cooks.distance(results.metafor, cluster = eval(parse(text = cluster)))),
        hat = c(hatvalues(results.metafor, cluster = eval(parse(text = cluster)))),
        dfbetas = c(dfbetas(results.metafor, cluster = eval(parse(text = cluster)))$intrcpt),
        is.infl = c(F)
      )
    } else if (cluster == "study.id"){  # clustering per study
     cheungviechtdata <- data.frame(
        study.id = unique(results.metafor$data$study.id),
        study.num = 1:length(unique(results.metafor$data$study.id)),
        cook.d = c(cooks.distance(results.metafor, cluster = eval(parse(text = cluster)))),
        # hat = c(hatvalues(results.metafor, cluster = eval(parse(text = cluster)))),  # cluster parameter of hat values does not work
        dfbetas = c(dfbetas(results.metafor, cluster = eval(parse(text = cluster)))$intrcpt),
        is.infl = c(F)
      )
    } else if (cluster == "outcome"){  # clustering per study
     cheungviechtdata <- data.frame(
        study.id = unique(results.metafor$data$outcome),
        study.num = 1:length(unique(results.metafor$data$outcome)),
        cook.d = c(cooks.distance(results.metafor, cluster = eval(parse(text = cluster)))),
        # hat = c(hatvalues(results.metafor, cluster = eval(parse(text = cluster)))),  # cluster parameter of hat values does not work
        dfbetas = c(dfbetas(results.metafor, cluster = eval(parse(text = cluster)))$intrcpt),
        is.infl = c(F)
      )
    } else {
      print("error in plot.influence(): set parameter 'cluster' to 'id', 'study.id', or 'outcome'")
    }
 
    # cook.d.thresh = qchisq(0.5, df=results.metafor.inf$m)
    cook.d.plot = ggplot(cheungviechtdata, aes(y = cook.d, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab(expression(D["i"])) +
        geom_hline(yintercept = cook.d.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = 0, linetype='dashed', color='grey')

    if (!cluster %in% c("study.id", "outcome")){
    # hat.thresh = 3 * (results.metafor.inf$p / results.metafor.inf$k)
      hat.plot = ggplot(cheungviechtdata, aes(y = hat, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) + geom_line(color = "black") +
          geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) + theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1),
          legend.position = "none",  axis.title.y = element_text(size = y.ax.title.size),
          axis.text.y = element_text(size = y.ax.text.size)) + ylab("HAT") + 
          geom_hline(yintercept = hat.thresh, linetype='dashed', color='black') +
          geom_hline(yintercept = 0, linetype='dashed', color='grey')
    }

    # dfbetas.thresh = 1
    dfbetas.plot = ggplot(cheungviechtdata, aes(y = dfbetas, x = reorder(study.id, as.double(study.num)), color = is.infl, group = 1)) +
        geom_line(color = "black") + geom_point(size = 2) + scale_color_manual(values = c("blue", "red")) +
        theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
        axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + ylab("DFBETAS") +
        geom_hline(yintercept = dfbetas.thresh, linetype='dashed', color='black') +
        geom_hline(yintercept = - dfbetas.thresh, linetype='dashed', color='black') +
      geom_hline(yintercept = 0, linetype='dashed', color='grey') 
    
    if (cluster == "id"){
      rma.influence.plot = arrangeGrob(cook.d.plot, hat.plot, dfbetas.plot, ncol = 2)
    } else {
      rma.influence.plot = arrangeGrob(cook.d.plot, dfbetas.plot, ncol = 2)
    }
  }
  
  grid.draw(rma.influence.plot)
}

# %% hidden=true vscode={"languageId": "r"}
# install.packages("reshape2")
library(reshape2)

# %% hidden=true vscode={"languageId": "r"}
# set plotsize to options(repr.plot.width = 30, repr.plot.height = 33, repr.plot.res = 200)

# function to split lines in plot labels
addline_format <- function(x,...){
    gsub('\\s','\n',x)
}

# load melt function it it appeared to be not accessable
melt <- reshape2::melt

# this function is built on basis of the influence.analysis function of the dmetar package by Mathias Harrer et al.
# see: https://github.com/MathiasHarrer/dmetar/blob/master/R/influence.analysis.R
# ... and the measplot function of the NMAoutlier package by Maria Petropoulou
# see: https://github.com/petropouloumaria/NMAoutlier/blob/master/R/measplot.R

plot.net.influnece <- function(NMA.o.res, study.names.suff.data, use.labels.of.data.set = F){
  
  # define thresholds
  k <- length(NMA.o.res$estand.deleted)
  rstudent.thresh <- qnorm(1 - (0.05 / (2 * k)))  # suggested by reporter function of the metafor package (bonferoni correction)
  cov.r.thresh <- dfbetas.thresh <- cook.d.thresh <- 1
  
  # plot parameters
  y.ax.title.size <- 15
  y.ax.text.size <- 13
  xlabel <- "study deleted"
  
  # get data frame of all data
  stat.names <- names(NMA.o.res)[-c(1, 2, 16)]
  stats.df <- data.frame(matrix(NA, ncol = 0, nrow = k))
  for (stat in stat.names){
    if (!stat %in% c("Restimates", "DFbetas")){
      stat.data <- data.frame(NMA.o.res[[stat]])
      colnames(stat.data) <- stat
      stats.df <- cbind(stats.df, stat.data)
    }
  }
  
  for (stat in c("Restimates", "DFbetas")){
    stat.data <- t(NMA.o.res[[stat]])
    rownames(stat.data) <- NULL
    colnames(stat.data) <- paste(stat, colnames(stat.data))
    stats.df <- cbind(stats.df, stat.data)
  }
  stats.df <- t(stats.df)
  stats <- nrow(stats.df)
  
  if (use.labels.of.data.set){
    stlab <- unique(NMA.o.res$dat[ ,3])
    xlabels <- factor(as.character(stlab), levels = as.character(stlab))
  } else {
    stlab.nchars <- nchar(study.names.suff.data)
    
    # generate study labels with 1st 3 and last 2 letters of original label
    stlab <- paste(substr(study.names.suff.data, 1, 3), substr(study.names.suff.data, stlab.nchars - 1, stlab.nchars), sep = "")
    xlabels <- factor(as.character(stlab), levels = as.character(stlab)) # as factor to prevent ggplot from reordering the x labels in alphabetical order (comment by Petropoulou, M. in measplot())
  }

  graphs <- vector("list", stats - 1)

  upper <- max(stats.df)
  lower <- min(stats.df)

  limu <- round(max(upper), 2)
  liml <- round(min(lower), 2)
  
  # collect plots
  for (j in 1:(stats - 1)) {
    
    ## Localizing variables - workaround for ggplot problem with
    ## handling variables in multiple plots (variable environment
    ## scope problem; comment by Petropoulou, M. in measplot())
    local({
      
      ylabel <- rownames(stats.df)[j]
      
      if (ylabel == "estand.deleted") {

        ylabel <- expression(r["stan-del"])
        tresh.u <- rstudent.thresh
        tresh.l <- -rstudent.thresh

      } else if (ylabel == "estud.deleted") {

        ylabel <- expression(r["stud-del"])
        tresh.u <- rstudent.thresh
        tresh.l <- -rstudent.thresh

      } else if (ylabel == "Cooks.distance") {

        ylabel <- expression(D["i"])
        tresh.u <- cook.d.thresh
        tresh.l <- NULL

      } else if (ylabel == "Covratio") {

        ylabel <- "COVRATIO"
        tresh.u <- cov.r.thresh
        tresh.l <- NULL

      } else if (ylabel == "w.leaveoneout") {

        ylabel <- "study weight"
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (ylabel == "H.leaveoneout") {

        ylabel <- "leverage"
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (ylabel == "heterog.leaveoneout") {

        ylabel <- ~ tau ^ 2
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (ylabel == "Rheterogeneity") {

        ylabel <- "R for het."
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (ylabel == "RQtotal") {

        ylabel = "R for Qtotal"
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (ylabel == "RQhet") {

        ylabel <- "R for Qhet."
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (ylabel == "RQinc") {

        ylabel <- "R for incons."
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (grepl("Restimates", ylabel)) {
        
        ylabel <- addline_format(gsub("Restimates", "R_for Qestimates", ylabel))
        tresh.u <- NULL
        tresh.l <- NULL

      } else if (grepl("DFbetas", ylabel)) {
        
        ylabel <- addline_format(ylabel)
        tresh.u <- dfbetas.thresh
        tresh.l <- -dfbetas.thresh

      }
      
      melt_data <- melt(stats.df[j, ], id.vars = 0)
      y_values <- melt_data$value
      j <- j
      g <- eval(substitute(
        ggplot(data = melt_data, aes(y = y_values, x = xlabels, group = 1)) + # eval(substitute) is another workaround for the aforementioned problem (comment by Petropoulou, M. in measplot())
          geom_line(color = "black") + geom_point(size = 2, col = "blue") + 
          theme_minimal() + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1), legend.position = "none", 
          axis.title.y = element_text(size = y.ax.title.size), axis.text.y = element_text(size = y.ax.text.size)) + xlab(xlabel) + ylab(ylabel) +
          geom_hline(yintercept = tresh.u, linetype='dashed', color='black') +
          geom_hline(yintercept = tresh.l, linetype='dashed', color='black') +
          geom_hline(yintercept = 0, linetype='dashed', color='grey')
      ))
      graphs[[j]] <<- g
    })
  }
  net.influence.plot <- arrangeGrob(grobs = graphs, ncol = 1)
    
  
  grid.draw(net.influence.plot)
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Get pooled standard deviation from studies

# %% hidden=true vscode={"languageId": "r"}
get.pooled.sd <- function(n.e, sd.e, n.c, sd.c){
  # check if input data has same length
  len.vec <- c(length(n.e), length(sd.e), length(n.c), length(sd.c))
  vec.s.l <- T
  if (length(unique(len.vec)) > 1){
    cat("\nError in get.pooled.var(): Length of input has to be same length. Got n.e = ", length(n.e), ", sd.e = ", length(sd.e), ", n.c = ", length(n.c), ", sd.c = ", length(sd.c), "instead.\n", sep ="")
    vec.s.l <- F
  }
  
  if (vec.s.l){
    # valculate numerator for pooled standard deviation
    N <- n.e + n.c
    sd.pooled <- sqrt(((n.e - 1) * sd.e^2 + (n.c - 1) * sd.c^2) / (N - 2))
    return(sd.pooled)
  } else {
    return(NA)
  }
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Plot Forest & Funnel Plots, Return regression data frame

# %% code_folding=[] hidden=true vscode={"languageId": "r"}
meta.analyze <- function(
# includes...
  # ...collecting data from m.data.list for desired comparison
  # ...printing forest and funnel plots for desired comparison
  # ...returning...
    # ...filtered single regression data frame (meta-regression was outsourced to print.meta.results function)
    # ...single plots
    # ...other data
  
  # basic parameters
  outcome, meditation.types, m.data.list, preferred.scale = F, meta.df.list = F, comparison.list = list(meditation.type.all, cont.passive),
  
  # print plots or data
  print.forest = F, forest.add.fix.eff.mod = T, forest.add.power = F, forest.layout = "RevMan5", print.funnel = F, funnel.label = T, funnel.label.out.only = T, print.meta.results = F,
  print.descriptive = F, print.influence = F, influence.ids = "name", print.baujat = F, report = F,
  
  # regression and subgroup analysis 
  moderator = F, split.subgroups = T, subgroups = c("delivery.mode", "meditation.type"), print.forest.sub.single = F, subgroup.method = "random",
  without.mean.r = F,
  
  # filter and sorting
  filter.forest..funnel.vec = F, filter.regression.vec = F, sort.by = "hedges.g",
  exclude.high.rob.vec = F, comparisons.skip.list = F,
  
  # return
  return.data = F, results.metafor.fixed = F, silent = F
    # meta.df.list is used to increase performace if data already was extracted, otherwise --> F
    # meditation.types = vector of meditation types (not used anymore, but not removed in case of causing any bug)
    # preferred.scale = preferred scale in case of 2 scales for one outcome (Stress)
    # moderator returns regression df instead of printing because latter does not work for some reason
    # filter.forest..funnel.vec (filters basic data frame) and filter.regression.vec (filters regression results) should not be combined
      # filter.forest..funnel.vec works with numbers and study labels (filter out study numbers with e.g. -c(1, 2, 3) and study labels with c("stud1", "stud2", "stud3"))
    # meta.df.list can be inserted from earlier calculations with the same outcome to skip the data collection process
    # print.forest.sub.single = insert categorical moderator to get single results of it (works only for {meta} and with split.subgroups = T)
    # comparisons.skip.list = a list of lists of intervention comparison vectors (e.g. list(list(int.vec.1, int.vec.2), list(int.vec.3, int.vec.3)))
      # ... if just one intervention should be cutted set this intervention to both elements of list (e.g. list(list(int.vec.1, int.vec.1)))
    # set forest.layout = gs("layout") to use custom layout
){
  
  if (length(comparison.list) != 2){
    cat("\nerror in meta.analyze(): parameter comparison.list should have length 2 but has length", length(comparison.list), "instead.\n")
  }
  
  if (if(is.logical(meta.df.list)){meta.df.list == F}else{F}){
    study.id <- c()

  # Set Variables
  ## Descriptive Results
    n.int <- c()
    n.control <- c()
    mean.int <- c()
    mean.control <- c()
    sd.int <- c()
    sd.control <- c()

  ## Meta-Regression Factors
    # for active control groups with regression data, create vectors also for control group
    # and insert them at the function's control group part
    # for this script there are only passive controls that do not have regression data
    sessions.duration <- c()
    sessions.frequency <- c()
    programs.duration <- c()
    delivery.mode <- c()
    follow.up.period <- c()
    meditation.type <- c()
    female.percent <- c()

  ## create list of all meta data vectors to pass it to fill.meta.df()
    # arrays or data frames are not used to be able to append values to vectors separately 
    descr..reg.data.list.t1 <- list(
      study.id, n.int, n.control, mean.int, mean.control, sd.int, sd.control,
      sessions.duration, sessions.frequency, programs.duration, delivery.mode,
      follow.up.period, meditation.type, female.percent
    )

    names(descr..reg.data.list.t1) <- c(
      "study.id", "n.int", "n.control", "mean.int", "mean.control", "sd.int", "sd.control",
      "sessions.duration", "sessions.frequency", "programs.duration", "delivery.mode",
      "follow.up.period", "meditation.type", "female.percent"
    )

    descr..reg.data.list.t2 <- descr..reg.data.list.t1

    descr..reg.data.list.t3 <- descr..reg.data.list.t1

    descr..reg.data.list <- list(descr..reg.data.list.t1, descr..reg.data.list.t2, descr..reg.data.list.t3)

    names(descr..reg.data.list) <- c("t1", "t2", "t3")

    results.descriptive.array <- m.data.list[["results.descriptive.array"]]
    
  #___For other than Stress, Self-compassion, and Positive affect (Outcomes with one scale per outcome per study)___#
    if (preferred.scale == F){
      for (study in 1:study.no){
        # skip study if none of both of the comparison groups are present
        interventions <- meditation.techniques.df[study,]
        if (!(
            T %in% (interventions %in% comparison.list[[1]]) &
            T %in% (interventions %in% comparison.list[[2]])
        )){next}
        
        # skip study if comparison of comparisons.skip.list is present
        if(!is.logical(comparisons.skip.list)){
          comp.skip.present <- F
          for (comparison in 1:length(comparisons.skip.list)){
            if (length(comparisons.skip.list[[comparison]]) != 2){
              cat(
                "\n\n error in meta.analyze(): set parameter comparisons.skip.list to a list of lists of 2 intervention comparison vectors (e.g. list(list(int.vec.1, int.vec.2), list(int.vec.3, int.vec.3))).\n",
                "got ", length(comparisons.skip.list[[comparison]]), " elements in entry ", comparison, " of comparisons.skip.list.", sep = ""
              )
            }
            int.1 <- comparisons.skip.list[[comparison]][[1]]
            int.2 <- comparisons.skip.list[[comparison]][[2]]
            if (
              T %in% (int.1 %in% interventions) &
              T %in% (int.2 %in% interventions)
            ){comp.skip.present <- T}
          }
          if (comp.skip.present) next
        }

        #  find out which are the respective intervention numbers
        intervention.no.vec <- which(interventions %in% comparison.list[[1]])
        control.no.vec <- which(interventions %in% comparison.list[[2]])
        
        for (time.point in 2:4){  # in this case 2 is post-test
          for (outcome.no in 1:7){            
            # iterate overall intervention vs. control combinations
            for (intervention.no in intervention.no.vec){
              for (control.no in control.no.vec){
                # Define boolean variables for checking if 1) wanted outcome is present, 2) intervention name is present, and 3) data is complete
                outcome_name <- m.data.list[["outcome.names.df"]][study, sprintf("Name.of.Outcome.%d", outcome.no)]
                outcome_name_present <- if (is.na(outcome_name)) {F} else {outcome_name == outcome}
                intervention_name_present <- !(
                  is.na(m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"]) |
                  m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == "NA" |
                  m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] ==
                    nm.placeholder |
                  m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] ==
                    as.character(nm.placeholder)
                )
                data_complete <- !(
                  NA %in% results.descriptive.array[intervention.no,,time.point, outcome.no, "Scale.1", study] |
                  nm.placeholder %in% results.descriptive.array[intervention.no,,time.point, outcome.no, "Scale.1", study] |
                  NA %in% results.descriptive.array[control.no,,time.point, outcome.no, "Scale.1", study] |
                  nm.placeholder %in% results.descriptive.array[control.no,,time.point, outcome.no, "Scale.1", study]
                )
                # Fill descr..reg.data.list if all conditions are met 
                if (
                  outcome_name_present &
                  intervention_name_present &
                  data_complete
                ){
                  descr..reg.data.list <- fill.meta.df(
                    "Scale.1", time.point - 1, intervention.no, control.no, outcome.no, study,
                    m.data.list, nm.placeholder, descr..reg.data.list, without.mean.r
                    # time.point - 1 because descr..reg.data.list starts with pos-test
                  )
                }
              }
            }
          }
        }
      }

  #___For Stress, Self-compassion, and Positive affect (Sometimes 2 scales per study per outcome)___#
    } else if (preferred.scale != F){
      for (study in 1:study.no){
        
        # skip study if none of both of the comparison groups are present
        interventions <- meditation.techniques.df[study,]
        if (!(
            T %in% (interventions %in% comparison.list[[1]]) &
            T %in% (interventions %in% comparison.list[[2]])
        )){next}
        
        # skip study if comparison of comparisons.skip.list is present
        if(!is.logical(comparisons.skip.list)){
          comp.skip.present <- F
          for (comparison in 1:length(comparisons.skip.list)){
            if (length(comparisons.skip.list[[comparison]]) != 2){
              cat(
                "\n\n error in meta.analyze(): set parameter comparisons.skip.list to a list of lists of 2 intervention comparison vectors (e.g. list(list(int.vec.1, int.vec.2), list(int.vec.3, int.vec.3))).\n",
                "got ", length(comparisons.skip.list[[comparison]]), " elements in entry ", comparison, " of comparisons.skip.list.", sep = ""
              )
            }
            int.1 <- comparisons.skip.list[[comparison]][[1]]
            int.2 <- comparisons.skip.list[[comparison]][[2]]
            if (
              T %in% (int.1 %in% interventions) &
              T %in% (int.2 %in% interventions)
            ){comp.skip.present <- T}
          }
          if (comp.skip.present) next
        }

        #  find out which are the respective intervention numbers
        intervention.no.vec <- which(interventions %in% comparison.list[[1]])
        control.no.vec <- which(interventions %in% comparison.list[[2]])
        
        for (time.point in 2:4){  # in this case 2 is post-test
          for (scale in 1:2){
            for (outcome.no in 1:7){
              
  # skip iteration if 2 scales per outcome are present and current scale is not the preferred one
              scale.name <- m.data.list[["outcome.measures.df.list"]][[study]][
                sprintf("Outcome.%d", outcome.no), "Measures.Name"
              ]
              if (!is.na.or.nm(scale.name)){
                if (multiple.commas.present(scale.name)){
                  cat("multiple commas in scale name, find solution:", scale.name, "\n\n")
                  next
                } else if (grepl(",", scale.name)){  # comma = 2 scales per outcome

                  if (scale == 1){
                    scale.name.substring <- sub(",.*", "", scale.name)
                      # extracts substring before comma
                    if (!grepl(gsub("([()])","\\\\\\1", preferred.scale), scale.name.substring)){
                      next
                    }
                  } else if (scale == 2){
                    scale.name.substring <- sub(".*,", "", scale.name)
                      # extracts substring after comma
                    if (!grepl(gsub("([()])","\\\\\\1", preferred.scale), scale.name.substring)){
                      next
                    }
                  }
                }
              }

              # iterate overall intervention vs. control combinations
              for (intervention.no in intervention.no.vec){
                for (control.no in control.no.vec){
                  # Define boolean variables for checking if 1) wanted outcome is present, 2) intervention name is present, and 3) data is complete
                  outcome_name <- m.data.list[["outcome.names.df"]][study, sprintf("Name.of.Outcome.%d", outcome.no)]
                  outcome_name_present <- if (is.na(outcome_name)) {F} else {outcome_name == outcome}
                  intervention_name_present <- !(
                    is.na(m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"]) |
                    m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == "NA" |
                    m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] ==
                      nm.placeholder |
                    m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] ==
                      as.character(nm.placeholder)
                  )
                  data_complete <- !(
                    NA %in% results.descriptive.array[intervention.no,,time.point, outcome.no, scale, study] |
                    nm.placeholder %in% results.descriptive.array[intervention.no,,time.point, outcome.no, scale, study] |
                    NA %in% results.descriptive.array[control.no,,time.point, outcome.no, scale, study] |
                    nm.placeholder %in% results.descriptive.array[control.no,,time.point, outcome.no, scale, study]
                  )
                  # Fill descr..reg.data.list if all conditions are met
                  if (
                    outcome_name_present &
                    intervention_name_present &
                    data_complete
                  ){
                    descr..reg.data.list <- fill.meta.df(
                      scale, time.point - 1, intervention.no, control.no, outcome.no, study,
                      m.data.list, nm.placeholder, descr..reg.data.list, without.mean.r
                      # time.point - 1 because descr..reg.data.list starts with pos-test
                    )
                  }
                }
              }
            }
          }
        }
      }
    }

  # Create data frame list from descr..reg.data.list
    meta.df.list.unfiltered <- list()

    for (time.point in 1:3){  # here 1 is post-test

      n.control <- descr..reg.data.list[[time.point]][["n.control"]]
      mean.control <- descr..reg.data.list[[time.point]][["mean.control"]]
      sd.control <- descr..reg.data.list[[time.point]][["sd.control"]]
      n.int <- descr..reg.data.list[[time.point]][["n.int"]]
      mean.int <- descr..reg.data.list[[time.point]][["mean.int"]]
      sd.int <- descr..reg.data.list[[time.point]][["sd.int"]]

      sessions.duration <- descr..reg.data.list[[time.point]][["sessions.duration"]]
      sessions.frequency <- descr..reg.data.list[[time.point]][["sessions.frequency"]]
      programs.duration <- descr..reg.data.list[[time.point]][["programs.duration"]]

      delivery.mode <- descr..reg.data.list[[time.point]][["delivery.mode"]]

      follow.up.period <- descr..reg.data.list[[time.point]][["follow.up.period"]]

      meditation.type <- descr..reg.data.list[[time.point]][["meditation.type"]]
      
      female.percent <- descr..reg.data.list[[time.point]][["female.percent"]]

      pooled.sd <- get.pooled.sd(n.e = n.int, sd.e = sd.int, n.c = n.control, sd.c = sd.control)
      mean.diff <- mean.int - mean.control
      hedges.g <- mean.diff / pooled.sd 
      weights <- 1/sqrt(pooled.sd)  # this weight and hedges.g will not be used for caluclations
        # ... weights and SMD calculated by {metafor} and {meta} will be used instead 
      meta.df.list.unfiltered <- append(
        meta.df.list.unfiltered,
        list(data.frame(  # data frame as to be in a list to append it to a list
          study.id = descr..reg.data.list[[time.point]][["study.id"]],
          n.control = n.control,
          mean.control = mean.control,
          sd.control = sd.control,
          n.int = n.int,
          mean.int = mean.int,
          sd.int = sd.int,
          pooled.sd = pooled.sd,
          mean.diff = mean.diff,
          hedges.g = hedges.g,
          weights = weights,
          sessions.duration = sessions.duration,
          sessions.frequency = sessions.frequency,
          programs.duration = programs.duration,
          delivery.mode = delivery.mode,
          follow.up.period = follow.up.period,
          meditation.type = meditation.type,
          meditation.total = if (
            c(sessions.duration, sessions.frequency, programs.duration) %>%
              sapply(is.numeric) %>%
              all()
          ) {
            sessions.duration * sessions.frequency * (programs.duration / 7)
          } else {
            NA_real_
          },
          female.percent = female.percent
        ))
      )

      # sort studies
      if (sort.by == "hedges.g"){
        meta.df.list.unfiltered[[time.point]] <- arrange(meta.df.list.unfiltered[[time.point]], hedges.g)
      } else if (sort.by == "name"){
        meta.df.list.unfiltered[[time.point]] <- arrange(meta.df.list.unfiltered[[time.point]], study.id)
      } else if (sort.by != F){
        cat('\n\nsort by "hedges.g" or "name"\n\n')
      }
    }
    names(meta.df.list.unfiltered) <- c("t1", "t2", "t3")
  } else {
    meta.df.list.unfiltered <- meta.df.list
  }

  # filter study results from posttest
  if (!F %in% exclude.high.rob.vec){
    studies.high.rob.included <- which(meta.df.list.unfiltered[[1]]$study.id %in% studies.high.rob)
    if (length(studies.high.rob.included) > 0){
      meta.df.list[[1]] <- meta.df.list.unfiltered[[1]][
        -studies.high.rob.included,
      ]
    }
  } else {
    meta.df.list <- meta.df.list.unfiltered
  }

  if (!F %in% filter.forest..funnel.vec){
    if (!F %in% exclude.high.rob.vec){
      if (is.character(filter.forest..funnel.vec)){
        meta.df.list[[1]] <- meta.df.list[[1]] |> filter(!study.id %in% filter.forest..funnel.vec)
      } else {
        meta.df.list[[1]] <- meta.df.list[[1]][filter.forest..funnel.vec,]
      }
    } else {
      if (is.character(filter.forest..funnel.vec)){
        meta.df.list[[1]] <- meta.df.list.unfiltered[[1]] |> filter(!study.id %in% filter.forest..funnel.vec)
      } else {
        meta.df.list[[1]] <- meta.df.list.unfiltered[[1]][filter.forest..funnel.vec,]
      }
    }
  } else {
    meta.df.list <- meta.df.list.unfiltered
  }
  
  if (nrow(meta.df.list[[1]]) >= 1) {
    
# get results for forest plot {meta}
    results.meta <- metacont(
      n.e = n.int, mean.e = mean.int, sd.e = sd.int,
      n.c = n.control, mean.c = mean.control, sd.c = sd.control,
      common = forest.add.fix.eff.mod, random = T, studlab = study.id,
      data = meta.df.list[[1]], sm = "SMD"
    )
    
# calculate power
## a post-hoc power analysis seems to be a not usefull as it is a function of the p-value (HOENIG, J. M. & HEISEY, D. M. (2001). The Abuse of Power)
## a way to use it however could be to calculate the post-hoc power and its convidence interval (CI) by using the of the effect size and its CI (Wang, L. L. (2010). Retrospective Statistical Power)
## the latter approach was not implemented so the post-hoc power was not reported within the results
    if (forest.add.power){
      results.meta.cohens.d <- metacont(
      n.e = n.int, mean.e = mean.int, sd.e = sd.int,
      n.c = n.control, mean.c = mean.control, sd.c = sd.control,
        common = T, random = T, studlab = study.id,
        data = meta.df.list[[1]], sm = "SMD", method.smd = "Cohen"
      )

      if (results.meta.cohens.d$TE.random > 0){
        te.random <- results.meta.cohens.d$TE.random
      } else {
        te.random <- -results.meta.cohens.d$TE.random
      }
      I2 <- results.meta.cohens.d$I2
      I2 <- ifelse(is.na(I2), 1, I2)

      power.obj <- power.analysis(
        d = te.random,  # over all effect size (must be positive)
        k = results.meta.cohens.d$k,  # number of included studies
        n1 = mean(results.meta.cohens.d$n.c), n2 = mean(results.meta.cohens.d$n.e),  # mean numbers of control and experimental goup
        heterogeneity = if(I2 <= 0.3){"low"}else if(I2 <= 0.6){"moderate"}else{"high"}
      )

      results.meta$power <- round(100 * power.obj$Power, digits = 2)
    }
    
# Print Forest plot or descriptive or meta-results
    if (print.descriptive){
      print(meta.df.list[[1]])
    }

    if (print.meta.results){
      print(results.meta)
    }

    # set right and left label for scale of SMD
    if (
      outcome.direction.df[
        which(outcome.direction.df[,"Outcome"] == outcome),
        "High.or.low.means.resilient"
      ] == "v"
    ){
      label.left <- "may improve resilience   "
      label.right <- "   may reduce resilience"  
    } else {
      label.left <- "may reduce resilience   "
      label.right <- "   may improve resilience"
    }
    
    if (print.forest){
            
      # create label to show outcome in case of multiple plots in wrong order
      label.add <- ""
      if (preferred.scale != F){
        if (grepl("PSS", preferred.scale)){
          label.add <- " (PSS) "
        } else if (grepl("DASS", preferred.scale)){
          label.add <- " (DASS) "
        } 
      }
      
      if (forest.layout != "RevMan5"){
        forest(
          results.meta,  # leftcols = c('studlab'), <-- would hide descriptive statistics in plot
          layout = forest.layout,
          prediction = T, print.Q = T,
          label.left = label.left, label.right = label.right,
          plotwidth = "5cm",
          smlab = "Standardized Mean\nDifference",  # using Standardized... instad of Standardised.. as heading above plot of SMDs
          # smlab = paste("SMD for\n", outcome, label.add),  # label to show outcome
          hetlab = ifelse(
            forest.add.power,
            paste("Power = ", as.character(results.meta$power), "%, ", sep = ""),
            gs("hetlab")  # default value of hetlab
          ),
          addrows.below.overall = 4,  # rows below SMD scale
          digits.pval.Q = 4,
          digits.I2 = 2,
          digits.sd = 2  # ,  
          # colgap.forest.left = "1cm"  # left space from graph 
        )
      } else {
        forest(
          results.meta,
          common = F,
          layout = forest.layout,
          prediction = T,
          label.left = label.left, label.right = label.right,
        )
      }
    }
    
    # Subgroup Analysis
    if (split.subgroups){
      if (results.meta$k >= 4){
        i <- 1
        for (subgroup in subgroups){
          if (print.forest.sub.single == F){
            cat("\n")
            cat("# Subgroup Analysis:", subgroup, "\n")
          }
          
          # get results for test of subgroup differences with desired method 
          results.meta.sub <- metacont(
            n.e = n.int, mean.e = mean.int, sd.e = sd.int,
            n.c = n.control, mean.c = mean.control, sd.c = sd.control,
            common = T, random = T, studlab = study.id,
            data = meta.df.list[[1]] %>%
              filter(!(
                is.na(!!sym(subgroup)) |
                !!sym(subgroup) %in% c( "NA", nm.placeholder)
              )),
            sm = "SMD"
          )
          if (subgroup.method == "fixed"){  # corresponds to Schwarzer et al. (2015). Meta-Analysis with R. doi: 10.1007/978-3-319-21416-0, pp. 41 - 45 & 89 - 91
            results.meta.sub <- update.meta(
              results.meta.sub, subgroup = eval(parse(text = subgroup)), random = F
                # eval(parse()) makes it possible to pass string as variable
            )
          } else if (subgroup.method == "random"){  # this delivers same results as "random.separate.tau2" but without confidence interval of tau2 (within-group)
            results.meta.sub <- update.meta(
              results.meta.sub, subgroup = eval(parse(text = subgroup)), common = F
            )
          } else if (subgroup.method == "rand.fix"){
            results.meta.sub <- update.meta(
              results.meta.sub, subgroup = eval(parse(text = subgroup))
            )
          } else if (subgroup.method == "random.separate.tau2"){  # corresponds to Schwarzer et al. (2015) pp. 91 - 94
            # get separete results for all levels of subgroups (random effect model)
            sub.lvls <- unique(results.meta$data[, subgroup])
            results.meta.subs <- list()
            TE.del <- c()
            seTE.del <- c()
            i <- 1
            for (sub.lvl in sub.lvls){
              # generate results object for subset ob subgroup level
              sub.res <- metacont(
                n.e = n.int, mean.e = mean.int, sd.e = sd.int,
                n.c = n.control, mean.c = mean.control, sd.c = sd.control,
                common = F, random = T, studlab = study.id,
                data = meta.df.list[[1]], sm = "SMD",
                subset = eval(parse(text = subgroup)) == sub.lvl
              )
              
              results.meta.subs[[i]] <- sub.res
              
              # subgroup treatment effects 
              TE.del[i] <- sub.res$TE.random

              # corresponding standard error
              seTE.del[i] <- sub.res$seTE.random
              
              i <- i + 1
            }

            # meta-analysis of subgroup estimates (fixed effect model)
            results.meta.sub <- metagen(
              TE.del, seTE.del, sm="SMD",
              studlab = sub.lvls,
              random = F, common = T
            )
            
            # add results of subgroups to results object
            results.meta.sub$TE.random.w <- TE.del
            results.meta.sub$seTE.random.w <- seTE.del
            
            results.meta.sub$tau2.w <- c()
            results.meta.sub$lower.tau2.w <- c()
            results.meta.sub$upper.tau2.w <- c()
            results.meta.sub$I2.w <- c()
            results.meta.sub$lower.I2.w <- c()
            results.meta.sub$upper.I2.w <- c()
            results.meta.sub$Q.w <- c()
            results.meta.sub$pval.Q.w <- c()

            results.meta.sub$lower.random.w <- c()
            results.meta.sub$upper.random.w <- c()
            results.meta.sub$zval.random.w <-  c()

            results.meta.sub$k <- 0
            results.meta.sub$k.w <- c()
            results.meta.sub$n.c <- 0
            results.meta.sub$n.e <- 0
            
            for (i in 1:length(sub.lvls)){
              results.meta.sub$tau2.w[i] <- results.meta.subs[[i]]$tau2
              results.meta.sub$lower.tau2.w[i] <- results.meta.subs[[i]]$lower.tau2
              results.meta.sub$upper.tau2.w[i] <- results.meta.subs[[i]]$upper.tau2
              results.meta.sub$I2.w[i] <- results.meta.subs[[i]]$I2
              results.meta.sub$lower.I2.w[i] <- results.meta.subs[[i]]$lower.I2
              results.meta.sub$upper.I2.w[i] <-results.meta.subs[[i]]$upper.I2
              results.meta.sub$Q.w[i] <- results.meta.subs[[i]]$Q
              results.meta.sub$pval.Q.w[i] <- results.meta.subs[[i]]$pval.Q
              
              results.meta.sub$lower.random.w[i] <- results.meta.subs[[i]]$lower.random
              results.meta.sub$upper.random.w[i] <- results.meta.subs[[i]]$upper.random
              results.meta.sub$zval.random.w[i] <-  results.meta.subs[[i]]$zval.random
              
              results.meta.sub$k <- results.meta.sub$k + results.meta.subs[[i]]$k
              results.meta.sub$k.w[i] <- results.meta.subs[[i]]$k
              results.meta.sub$n.c <- results.meta.sub$n.c + sum(results.meta.subs[[i]]$n.c)
              results.meta.sub$n.e <- results.meta.sub$n.e + sum(results.meta.subs[[i]]$n.e)
            }
            
          } else if (subgroup.method == "random.common.tau2"){  # corresponds to Schwarzer et al. (2015) pp. 94 - 97
            results.meta.sub <- update.meta(results.meta, subgroup = eval(parse(text = subgroup)), tau.common = T, common = F, random = T)
          } else {
            print("error in meta.analyze(): set parameter 'subgroup.method' to 'fixed', 'random.separate.tau2' or 'random.common.tau2'")
          }
          
          if (print.forest.sub.single == F){
            print(results.meta.sub)
          }
          
          if (if(print.forest.sub.single != F){subgroup == print.forest.sub.single}else{T}){
            
            # calculate power for delivery.mode (level 1 vs. level 2)
            if (subgroup == "delivery.mode" & (forest.add.power | return.data == "power.subgroup")){
              TE1 <- results.meta.sub$TE.random.w[[1]]
              seTE1 <- results.meta.sub$seTE.random.w[[1]]
              TE2 <- results.meta.sub$TE.random.w[[2]]
              seTE2 <- results.meta.sub$seTE.random.w[[2]]

              power.sub <- 100 * round(
                power.analysis.subgroup(
                  TE1 = TE1, TE2 = TE2, seTE1 = seTE1, seTE2 = seTE2
                )$Power,
                digits = 4
              )
              power.sub.str <- paste("= ", as.character(power.sub), "%", sep = "")
              
              if (return.data == "results.meta"){
                return(results.meta.sub)
              }
              
            # calculate all possible powers of meditation.type (all combinations of level 1:4)
            } else if (subgroup == "meditation.type" & (forest.add.power | return.data == "power.subgroup")){
              # get total effects and standard errors
              TE.vec <- c()
              seTE.vec <- c()
              for (i in 1:4){
                tryCatch(
                  {
                    TE.vec[length(TE.vec) + 1] <- results.meta.sub$TE.random.w[[i]]
                    seTE.vec[length(seTE.vec) + 1] <- results.meta.sub$seTE.random.w[[i]]
                  },
                  error=NULL,
                  warning=NULL
                )
              }
              power.sub <- c()
              
              # calculate power of all combinations
              for (j in 1:4){
                for (k in 1:4){
                  if (k <= j){
                    next
                  }
                  tryCatch(
                    {
                      power.sub[length(power.sub) + 1] <- 100 * round(
                        power.analysis.subgroup(
                          TE1 = TE.vec[j], TE2 = TE.vec[k], seTE1 = seTE.vec[j], seTE2 = seTE.vec[k]
                        )$Power,
                        digits = 4
                      )
                    },
                    error=NULL,
                    warning=NULL
                  )
                }
                
                # show only highest power in plot
                power.sub.str <- paste(
                  ifelse(length(power.sub) > 1, "<= ", "= "),
                  as.character(max(power.sub)), "%", sep = ""
                )
              }
            }
            
            if (return.data == "power.subgroup"){
              return(power.sub)
            } else if (return.data == "results.meta"){
              return(results.meta.sub)
            }
            
            if (forest.layout != "RevMan5"){
              forest(
                results.meta.sub,  # leftcols = c('studlab'),
                layout = forest.layout,
                prediction = T, print.Q = T,
                label.left = label.left, label.right = label.right,
                hetlab = ifelse(
                  forest.add.power,
                  paste(
                    "Power (total, subgroup) = ", as.character(results.meta$power), "%, ", power.sub.str, ", ",
                    sep = ""
                  ),
                  gs("hetlab")  # default value of hetlab
                ),
                smlab = "Standardized Mean\nDifference",
                plotwidth = "6cm", addrows.below.overall = 4,
                # colgap.forest.left = ifelse(forest.add.power, "9cm", "4cm"),
                # squaresize = 1.05,
                digits.pval.Q = 4,
                digits.I2 = 2,
                digits.sd = 2
              )
            } else {
              forest(
                results.meta.sub,
                common = F,
                layout = forest.layout,
                prediction = T,
                label.left = label.left, label.right = label.right,
              )
            }
          }
          i <- i + 1
        }
        
        if (print.forest.sub.single != F & !print.forest.sub.single %in% c("delivery.mode", "meditation.type")){
          print("error in meta.analyze(): set parameter 'print.forest.sub.single' to 'delivery.mode' or 'meditation.type'")
        }
        
      } else {
        cat("\nk < 4 => no Subgroup Analysis\n\n")
      }
    }
    
    # Outlier Analysis (influential outliers present?)
    if(results.meta$k >= 3){
      results.influence.metafor <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
      
      if (print.influence){
        # par(mar = c(5, 6, 4, 2))  # to set plot parameters?
        # plot(influence(results.influence.metafor), cex = 0.8, las = 1)  # using metafor
        plot.influnece(results.influence.metafor, influence.ids)  # self-gernerated (based on {dmetar})
      }
      
      if (print.baujat){
        # results.influence.meta <- InfluenceAnalysis(results.meta, random = T)
        # plot(results.influence.meta, "baujat")  # plot by dmetar
        
        results.influence.metafor$slab <- results.influence.metafor$data$study.id  # set author name and year as study label (instead of numbers)
        baujat(results.influence.metafor, symbol = "slab")  # plot by metafor
      }
      
    } else if ((print.influence | print.baujat) & results.meta$k < 3){
      print("number of included studies is below 3 --> now influence or baujat plot")
    }
    
    if(report){
      results.report <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
      reporter(results.report)
    }
  } else if (!silent) {
    cat("no studies included for outcome:", outcome, "\n\n")
  }

  if (print.funnel & nrow(meta.df.list[[1]]) >= 3){
    
# Funnel plot
    # get results for funnel and meta-regression plot
    results.funnel <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
    
      # method argument is set default to:
      # method="REML" = restricted maximum likelihood estimator (Viechtbauer, 2005; Raudenbush, 2009)
    
    # # with {meta}
    # funnel(results.meta)
    
    # with {metafor}
    # test for relationship between effect sizes and standard error (which implies funnel plot asynnetry) with the Egger's regression test (mixed-effects meta-regression version)
    if(return.data %in% c("funnel.asym.p.egger", "funnel.asym.p.rank")){
      print(regtest(results.funnel))
      print(ranktest(results.funnel))
    }
    
    # use trim and fill metheod to insert potential missing studies due to publication bias
    trim.and.fill <- trimfill(results.funnel)
    
    # create custom labels
    ## set study labels to numbers
    if (funnel.label.out.only){
      if (!is.null(outlier.list[[outcome]]) & T %in% (filter.forest..funnel.vec == F)){
        # set study labels to author and year and only show labels of outlaying studies
        trim.and.fill$slab[1:length(results.funnel$data$study.id)] <- results.funnel$data$study.id
        trim.and.fill$slab[-outlier.list[[outcome]]] <- ""
        funnel(  # with paramter slab adjusted
          trim.and.fill, legend = T,  yaxis="seinv", label = ifelse(funnel.label, T, F),
          level=c(90, 95, 99), shade=c("white", "gray55", "gray75"),
          refline=0, lty = 0, refline2 = results.meta$TE.random, lty2 = 3
            # results.meta$TE.random = overall effect size of the random effects model
        )
      } else {
        # no outliers --> no labels
        funnel(  # without paramter slab adjusted, label set to F
          trim.and.fill, legend = T,  yaxis="seinv", label = F,
          level=c(90, 95, 99), shade=c("white", "gray55", "gray75"),
          refline=0, lty = 0, refline2 = results.meta$TE.random, lty2 = 3
        )
      }
    } else {
      funnel(  # without paramter slab adjusted
        trim.and.fill, legend = T,  yaxis="seinv", label = ifelse(funnel.label, T, F),  # slab = as.character(1:trim.and.fill$k),
          # slab cant be set as for some reason trim.and.fill$k cant be found (only of trimfill fills in studies?)
        level=c(90, 95, 99), shade=c("white", "gray55", "gray75"),
        refline=0, lty = 0, refline2 = results.meta$TE.random, lty2 = 3
      )
    }
    
  } else if (print.funnel & nrow(meta.df.list[[1]]) < 3){
    print(sprintf("Number of included studies (%d) for the funnel plot is below 3 for outcome: %s", nrow(meta.df.list[[1]]), outcome))
    cat("\n")
  }

# Meta-Regression {metafor}
  # only preparing data frames for regression analysis in print.meta.results().
  # (an unknown error occured while doing this in the current function)
  if (moderator %in% c(
    "sessions.duration", "sessions.frequency", "programs.duration", "meditation.total", "delivery.mode",
    "follow.up.period", "meditation.type", "female.percent"
    )
  ){
  # follow.up.period
    if (moderator == "follow.up.period"){
      regression.df.list <- list()
      for (t in 1:3){
        if (nrow(meta.df.list[[t]]) < 1){
          regression.df <- meta.df.list[[t]]
        } else {
          regression.df <- meta.df.list[[t]] %>%
            filter(!(
              is.na(!!sym(moderator)) |
              !!sym(moderator) %in% c("NA", nm.placeholder, as.character(nm.placeholder))
            ))
            # filters out rows in which the moderator's column is NA
        }
        regression.df.list <- append(
          regression.df.list,
          list(regression.df)
        )
      }
      regression.df <- rbind(regression.df.list[[1]], regression.df.list[[2]], regression.df.list[[3]])
        # rbind() appends rows of data frames together
      
  # other moderators    
    } else {
      regression.df <- meta.df.list[[1]] %>%
        filter(!(
          is.na(!!sym(moderator)) |
          !!sym(moderator) %in% c("NA", nm.placeholder, as.character(nm.placeholder))
        ))
    }
    
  # filter
    if (!F %in% filter.regression.vec){
      regression.df <- regression.df[filter.regression.vec,]
    }
    
    if(moderator != F){
      return(regression.df)
    }

    
  } else if (moderator != F) {
    print('set moderator to "follow.up.period", "sessions.duration", "sessions.frequency", "programs.duration", "meditation.total", "delivery.mode", or "female.percent"')
  }

# Return Data
  if (moderator == F & return.data != F){
    if (return.data == "meta.df.list"){
      return(meta.df.list)
    } else if (return.data == "funnel.asym.p.egger"){
      if (nrow(meta.df.list[[1]]) >= 3){
        results.funnel <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
        return(regtest(results.funnel)$pval)
      }
    } else if (return.data == "funnel.asym.p.rank"){
      if (nrow(meta.df.list[[1]]) >= 3){
        results.funnel <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
        return(ranktest(results.funnel)$pval)
      }
    } else if (return.data == "descriptive"){
      return(meta.df.list)
    } else if (return.data == "regression.all"){
      return(rbind(meta.df.list[[1]], meta.df.list[[2]], meta.df.list[[3]]))
    } else if (return.data == "results.meta" & print.forest.sub.single == F){
      if (nrow(meta.df.list[[1]]) >= 1){
        results.meta <- metacont(
          n.e = n.int, mean.e = mean.int, sd.e = sd.int,
          n.c = n.control, mean.c = mean.control, sd.c = sd.control,
          common = T, random = T, studlab = study.id,
          data = meta.df.list[[1]], sm = "SMD"
        )

        if (results.meta$TE.random > 0){
          te.random <- results.meta$TE.random
        } else {
          te.random <- -results.meta$TE.random
        }
        power.obj <- power.analysis(
          d = te.random,  # over all effect size (must be positive)
          k = results.meta$k,  # number of included studies
          n1 = mean(results.meta$n.c), n2 = mean(results.meta$n.e)  # mean numbers of control and experimental goup
        )
        results.meta$power <- round(100 * power.obj$Power, digits = 2)

        return(results.meta)
      } else {
        return(
          data.frame(k = c(0))
        )
      }
    } else if (return.data == "results.meta" & print.forest.sub.single != F){
      # results.meta for print.forest.sub.single != T are returned above
    } else if (return.data == "results.metafor"){
      if (nrow(meta.df.list[[1]]) >= 1){
        results.metafor <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
      } else {
        results.metafor <- NULL
      }
      return(results.metafor)
    } else if (return.data == "hedges.g"){
      return(meta.df.list[[1]][, "hedges.g"])
    } else if (return.data == "influence.df"){
      if (nrow(meta.df.list[[1]]) >= 3){
        results.influence.metafor <- get.results.metafor(meta.df.list[[1]], results.metafor.fixed = results.metafor.fixed)
        return(get.influence.df(results.influence.metafor))
      }
    } else if (return.data == "power.subgroup"){
      # data returned above
    } else if (return.data %in% c("regression.results.linear", "regression.results.poly")){
      # data return will happen in print.meta.results()
    } else {
      print('error in meta.analyze(): set return.data to "meta.df.list", "funnel.asym.p.egger", "funnel.asym.p.rank", "descriptive", "regression.all", "results.meta", "results.metafor", "hedges.g", "influence.df", "regression.results.linear", "regression.results.poly", or "power.subgroup"')
    }
  } else if (moderator != F & return.data != F){
    print("Argument moderator and return.data are != F. Only regression data was returned.")
  }
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Multiple Regressions

# %% hidden=true vscode={"languageId": "r"}
do.multiple.regressions <- function(  # fixed effects model not implemented
  outcome, preferred.scale = F, meta.df.list = F, moderator.multiple.list,
  filter.multiple.regression.linear.list = F, filter.multiple.regression.poly.list = F,
  print.regplot = T, print.baujat.regression = T, print.gosh.regression = F,
  print.regression.results = T, print.regression.df = F, regression.label = F,
  degree.1 = T, degree.2 = F, non.interaction = T, interaction = F, without.mean.r = F
  # moderator.multiple.list --> list of string vectors specifying what moderators has to be combined
  # filter.regression.linear/poly.list (filters only linear regression) --> list with filtering vectors in order of moderator.vec ("" means no filtering)
){
  
  meditation.type.var <- meditation.type.all
  
  mod.comb.no <- 1
  
  for (moderator.combination in moderator.multiple.list){
    
    regression.df <- meta.analyze(
      outcome, meditation.type.var, m.data.list, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
      split.subgroups = F, return.data = "regression.all", without.mean.r = without.mean.r
    )
    
    # cut out data after post-test (that have follow.up.period > 0) if follow.up.period is not in moderator.combination
    if (!"follow.up.period" %in% moderator.combination){
      regression.df <- regression.df[
        which(regression.df[,"follow.up.period"] == 0),
      ]
    }
    
    # eliminate NAs
    if (length(moderator.combination) == 2){
      regression.df <- regression.df %>%
        filter(!(
          is.na(!!sym(moderator.combination[1])) |
          !!sym(moderator.combination[1]) %in% c("NA", nm.placeholder, as.character(nm.placeholder)) |
          is.na(!!sym(moderator.combination[2])) |
          !!sym(moderator.combination[2]) %in% c("NA", nm.placeholder, as.character(nm.placeholder))
        ))
    } else if (length(moderator.combination) == 3){
      regression.df <- regression.df %>%
        filter(!(
          is.na(!!sym(moderator.combination[1])) |
          !!sym(moderator.combination[1]) %in% c("NA", nm.placeholder, as.character(nm.placeholder)) |
          is.na(!!sym(moderator.combination[2])) |
          !!sym(moderator.combination[2]) %in% c("NA", nm.placeholder, as.character(nm.placeholder)) |
          is.na(!!sym(moderator.combination[3])) |
          !!sym(moderator.combination[3]) %in% c("NA", nm.placeholder, as.character(nm.placeholder))
        ))
    } else {
      print("filtering multiple regression data frame from NAs is only implemted for length(moderator.combination) == 2 or 3")
        # could be implemented with for loop
    }
    
# Linear
    if (degree.1){
      
      # filter for linear regression
      if (
        is.list(filter.multiple.regression.linear.list) &
        if(is.list(filter.multiple.regression.linear.list)){filter.multiple.regression.linear.list[[mod.comb.no]][1] != ""}else{F}
      ){
        regression.df.filtered <- regression.df[filter.multiple.regression.linear.list[[mod.comb.no]],]
      } else {
        regression.df.filtered <- regression.df
      }
      
      if (print.regression.df){
        cat("regression data frame for multiple linear regression of", moderator.combination, "\n")
        print(regression.df.filtered)
      }
      
      if (non.interaction){
        results.regression.linear <- get.results.metafor(
          regression.df.filtered, moderator = moderator.combination
        )
      }
      
      if (interaction){
        results.regression.linear.interact <- get.results.metafor(
          regression.df.filtered, moderator = moderator.combination, interaction = T
        )
      }
      
      # bubble/regression plot      
      if (print.regplot & length(moderator.combination) == 2){
        
        # get model results for 500 values between -1 and maximum of moderator 1 + 5 for 3 fixed values of moderatore 2
        if (non.interaction){
          model.res.list <- list(min = "", mean = "", max = "", data = "")
        }
        
        if (interaction){
          model.res.list.interact <- list(min = "", mean = "", max = "", data = "")
        }

        for (i in 1:3){
          if (i == 1){
            mod.fix.vals <- rep(min(regression.df.filtered[, moderator.combination[2]], na.rm = T), 500)
          } else if (i == 2){
            mod.fix.vals <- rep(
              (max(regression.df.filtered[, moderator.combination[2]], na.rm = T) + min(regression.df.filtered[, moderator.combination[2]], na.rm = T)) / 2,
              500
            )
          } else if (i == 3){
            mod.fix.vals <- rep(max(regression.df.filtered[, moderator.combination[2]], na.rm = T), 500)
          }   
          
          mod.pred.xvals <- seq(-1, max(regression.df.filtered[, moderator.combination[1]], na.rm = T) + 5, length=500)
          
          if (non.interaction){
            res.obj <- predict(results.regression.linear, newmods = unname(cbind(mod.pred.xvals, mod.fix.vals)), addx=T)
                
            res.df <- data.frame(
              pred = res.obj$pred,
              ci.lb = res.obj$ci.lb,
              ci.ub = res.obj$ci.ub,
              X.mod.1 = res.obj$X[,moderator.combination[1]]
            )
            model.res.list[[i]] <- res.df
          }

          if (interaction){
            res.obj.interact <- predict(results.regression.linear.interact, newmods = unname(cbind(mod.pred.xvals, mod.fix.vals, mod.pred.xvals * mod.fix.vals)), addx=T)
            
            res.df.interact <- data.frame(
              pred = res.obj.interact$pred,
              ci.lb = res.obj.interact$ci.lb,
              ci.ub = res.obj.interact$ci.ub,
              X.mod.1 = res.obj.interact$X[,moderator.combination[1]]
            )
            model.res.list.interact[[i]] <- res.df.interact
          }
        }
        
        # combine data from escalc() and regression.df.filtered to data frame and filter out NA for the multiple regression / bubble plot
        res.escalc <- escalc(
          data = regression.df.filtered, measure = "SMD",
          m1i = regression.df.filtered[,"mean.int"], sd1i = regression.df.filtered[,"sd.int"], n1i = regression.df.filtered[,"n.int"],
          m2i = regression.df.filtered[,"mean.control"], sd2i = regression.df.filtered[,"sd.control"], n2i = regression.df.filtered[,"n.control"]
        )
        
        res.escalc.df <- data.frame(res.escalc[
          which(!(
            is.na(res.escalc[,moderator.combination[1]]) |
            is.na(res.escalc[,moderator.combination[2]]) 
          ))
          ,
        ])
        
        dat <- regression.df.filtered[
          which(!(
            is.na(regression.df.filtered[,moderator.combination[1]]) |
            is.na(regression.df.filtered[,moderator.combination[2]]) 
          ))
          ,
        ]
        
        if (non.interaction){
          model.res.list[[4]] <- dat[,c("study.id", "hedges.g", "weights", moderator.combination[1], moderator.combination[2])]

          # set correct SMD and weights that are used by metafor
          model.res.list[[4]]$hedges.g <- res.escalc.df$yi
          model.res.list[[4]]$weights <- 1 / res.escalc.df$vi
        }
        
        if (interaction){
          model.res.list.interact[[4]] <- dat[,c("study.id", "hedges.g", "weights", moderator.combination[1], moderator.combination[2])]

          # set correct SMD and weights that are used by metafor
          model.res.list.interact[[4]]$hedges.g <- res.escalc.df$yi
          model.res.list.interact[[4]]$weights <- 1 / res.escalc.df$vi
        }
        
        # get fixed values of moderator 2 that was used for prediction as string to label graphs
        mod.fix.val.min <- as.character(round(min(regression.df.filtered[, moderator.combination[2]], na.rm = T), digits = 2))
        mod.fix.val.mean <- as.character(round(
          (max(regression.df.filtered[, moderator.combination[2]], na.rm = T) + min(regression.df.filtered[, moderator.combination[2]], na.rm = T)) / 2,
          digits = 2
        ))
        mod.fix.val.max <- as.character(round(max(regression.df.filtered[, moderator.combination[2]], na.rm = T), digits = 2)) 
        
        # plot
        scale.color.values <- c("red", "blue", "chartreuse3")
        names(scale.color.values) <- c(mod.fix.val.min, mod.fix.val.mean, mod.fix.val.max)
        
        if (non.interaction){
          print(
            ggplot(model.res.list[["min"]], aes(x = X.mod.1, y = pred, group = 1)) + 
              # 3 regression lines (for low, medium, and high moderator 2 values)
              geom_line(aes(col = mod.fix.val.min), size = 1, data = model.res.list[["min"]],) + 
              geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "red", data = model.res.list[["min"]]) +
              geom_line(aes(col = mod.fix.val.mean), size = 1, data = model.res.list[["mean"]]) + 
              geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "blue", model.res.list[["mean"]]) +
              geom_line(aes(col = mod.fix.val.max), size = 1, data = model.res.list[["max"]]) + 
              geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "chartreuse3", model.res.list[["max"]]) +

              # bubbles (SMDs x Moderator 1 x weights)
              geom_point(aes(x=!!sym(moderator.combination[1]), y = hedges.g, size = weights), color="darkslategray", alpha = 0.5, data = model.res.list[["data"]]) +

              # reference line
              geom_hline(yintercept = 0, linetype='dotted') +

              # labels
              ylab("Standard Mean Difference") +
              xlab(moderator.combination[1]) +
              labs(title = paste("Multiple Linear Regression Plot of", moderator.combination[1], "and", moderator.combination[2])) +
              scale_color_manual(
                name = moderator.combination[2], values = scale.color.values,
                breaks = c(mod.fix.val.min, mod.fix.val.mean, mod.fix.val.max)
              ) +
              scale_size_continuous(guide = "none")  # +
              # coord_cartesian(ylim = c(NA, 0))  # to change display range
          )
        }
        
        if (interaction){
          print(
            ggplot(model.res.list.interact[["min"]], aes(x = X.mod.1, y = pred, group = 1)) + 
              # 3 regression lines (for low, medium, and high moderator 2 values)
              geom_line(aes(col = mod.fix.val.min), size = 1, data = model.res.list.interact[["min"]],) + 
              geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "red", data = model.res.list.interact[["min"]]) +
              geom_line(aes(col = mod.fix.val.mean), size = 1, data = model.res.list.interact[["mean"]]) + 
              geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "blue", model.res.list.interact[["mean"]]) +
              geom_line(aes(col = mod.fix.val.max), size = 1, data = model.res.list.interact[["max"]]) + 
              geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "chartreuse3", model.res.list.interact[["max"]]) +

              # bubbles (SMDs x Moderator 1 x weights)
              geom_point(aes(x=!!sym(moderator.combination[1]), y = hedges.g, size = weights), color="darkslategray", alpha = 0.5, data = model.res.list.interact[["data"]]) +

              # reference line
              geom_hline(yintercept = 0, linetype='dotted') +

              # labels
              ylab("Standard Mean Difference") +
              xlab(moderator.combination[1]) +
              labs(title = paste(
                "Multiple Linear Regression Plot of", moderator.combination[1], "and", moderator.combination[2],
                "(with interaction)"
              )) +
              scale_color_manual(
                name = moderator.combination[2], values = scale.color.values,
                breaks = c(mod.fix.val.min, mod.fix.val.mean, mod.fix.val.max)
              ) +
              scale_size_continuous(guide = "none")  # +
              # coord_cartesian(ylim = c(NA, 0))  # to change display range
          )
        }
        
        
#         ############# TEST to plot multiple regression plot for factors ###############
#         results.regression.linear.cat <- get.results.metafor(
#           regression.df.filtered, moderator = moderator.combination, categorical.mod.trans = T
#         )
        
#         y.lim.min <- min(results.regression.linear.cat$data$hedges.g)
#         y.lim.max <- max(results.regression.linear.cat$data$hedges.g)
        
#         plot(
#           coef(results.regression.linear.cat)[1:3], type="o", pch=19,
#           xlim=c(0.8,3.2), ylim=c(y.lim.min,y.lim.max),
#           xlab=moderator.combination[1],
#           ylab="Standardized Mean Difference", xaxt="n", bty="l"
#         )
        
#         axis(side=1, at=1:3, labels=c("min","mean","max"))
        
#         lines(
#           coef(results.regression.linear.cat)[1:3] +
#           coef(results.regression.linear.cat)[4], type="o", pch=15, lty="dotted"
#         )
        
#         legend(
#           "topright", legend=c(paste("min", moderator.combination[2]), paste("max", moderator.combination[2])),
#           lty=c("solid", "dotted"), pch=c(19,15), inset=0.01
#         )
        
#         title("Estimated Average Effects based on the Additive Model")

#       } else if (print.regplot){
#         print("linear multiple regression plot currently works only with 2 moderators")
        #################################
      } else if (print.regplot) {
        print("printing the bubble plot for multiple linear regressions is only implemented for 2 moderators at once")
      }
                                            
      # baujat plot
      if (print.baujat.regression){
        # par(mar = c(5, 6, 4, 2))  # to set plot parameters?
        # plot(influence(results.regression.linear), cex = 0.8, las = 1)
        
        if (non.interaction){
          baujat(results.regression.linear, xlab = paste("Squared Pearson Residual (for ", paste(moderator.combination, collapse = ", "), "; linear)", sep = ""))
        }
        
        if (interaction){
          if (length(moderator.combination) < 3){
            baujat(results.regression.linear.interact, xlab = paste("Squared Pearson Residual (for ", paste(moderator.combination, collapse = ", "), "; linear; with interaction)", sep = ""))
          } else {
            # for length(moderator.combination) == 3 an error occured with baujat plot, so the hat value and the weights of the influnce() will be used to search for outliers instead
            cat("\ninfluence plots of:", moderator.combination)
            par(mar = c(5, 6, 4, 2))
            plot(influence(results.regression.linear.interact), cex = 0.8, las = 1)
          }
        }
      }

      # gosh plot
      if (print.gosh.regression){
        if (
          is.list(filter.multiple.regression.linear.list) &
          if(is.list(filter.multiple.regression.linear.list)){filter.multiple.regression.linear.list[[mod.comb.no]][1] != ""}else{F}
        ){
          
          if (non.interaction){
            for (outlier in -filter.multiple.regression.linear.list[[mod.comb.no]]){

              results.regression.linear.unfiltered <- get.results.metafor(
                regression.df, moderator = moderator.combination
              )

              cat("\n Gosh plot for", paste(moderator.combination, sep = ", "), "(outlier number:", outlier, "linear)")
              sav <- gosh(results.regression.linear.unfiltered, subsets = 15000)
              plot(sav, out = outlier)
            }
          }
          
          if (interaction){
            for (outlier in -filter.multiple.regression.linear.list[[mod.comb.no]]){

              results.regression.linear.interact.unfiltered <- get.results.metafor(
                regression.df, moderator = moderator.combination, interaction = T
              )

              cat("\n Gosh plot for", paste(moderator.combination, sep = ", "), "(outlier number:", outlier, "linear)")
              sav <- gosh(results.regression.linear.interact.unfiltered, subsets = 15000)
              plot(sav, out = outlier)
            }
          }
          
        } else {
          cat("\nno Gosh plot printed for", moderator.combination, "(linear)\n")
        }
      }
    }
    
# Polynomial
    if (degree.2){
      if (interaction){
        print("using multiple regression models with interaction ist not implemented yet")
      }
      
      degree <- 2
  # filter for polynomial regression
      if (
        is.list(filter.multiple.regression.poly.list) &
        if(is.list(filter.multiple.regression.poly.list)){filter.multiple.regression.poly.list[[mod.comb.no]][1] != ""}else{F}
      ){
        regression.df.filtered <- regression.df[filter.multiple.regression.poly.list[[mod.comb.no]],]
      } else {
        regression.df.filtered <- regression.df
      }
      
      # eliminate NAs
      regression.df.filtered <- regression.df.filtered %>%
        filter(!(
          is.na(!!sym(moderator.combination[1])) |
          !!sym(moderator.combination[1]) %in% c("NA", nm.placeholder, as.character(nm.placeholder)) |
          is.na(!!sym(moderator.combination[2])) |
          !!sym(moderator.combination[2]) %in% c("NA", nm.placeholder, as.character(nm.placeholder))
        ))
      
      if (print.regression.df){
        cat("regression data frame for multiple polynomial regression of", moderator.combination, "\n")
        print(regression.df.filtered)
      }
      
      results.regression.poly <- get.results.metafor(
        regression.df.filtered, moderator = moderator.combination, degree = degree
      )
      
      # bubble plot
      if (print.regplot){
        print("bubble plot for multiple polynomial regressions is not implemented yet")
        
#         ############### TEST for multiple polynomial regression plots ##############
#         for (mod.no in 1:length(moderator.combination)){
          
#           xvals <- seq(
#             -1,
#             max(regression.df.filtered[
#               which(!is.na(regression.df.filtered[,moderator.combination[mod.no]])),
#               moderator.combination[mod.no]
#             ]) + 5,
#             length=500
#           )
          
#           sav <- predict(
#             results.regression.poly,
#             newmods = unname(poly(xvals, degree=degree, raw=T))
#           )
          
#           regplot(
#             results.regression.poly,
#             mod = (mod.no - 1) * degree + 2,
#             refline=0, pred=sav, xvals=xvals, las=1, digits=1, bty="l",
#             xlab = paste(moderator.combination[mod.no], " with follwing fixed: ", moderator.combination[-mod.no], "; polynomial", sep = ""),
#             label=regression.label
#             # xlim = c(0,60), predlim = c(0,60)  # adjusting display area of graph
#           )
#         }
#        ##################################
        
       }

      # baujat plot
      if (print.baujat.regression){
        # par(mar = c(5, 6, 4, 2))  # to set plot parameters?
        # plot(influence(results.regression.poly), cex = 0.8, las = 1)

        baujat(results.regression.poly, xlab = paste("Squared Pearson Residual (for ", paste(moderator.combination, collapse = ", "), "; Degree: ", degree, ")", sep = ""))
      }

      # gosh plot
      if (print.gosh.regression){
        if (
          is.list(filter.multiple.regression.poly.list) &
          if(is.list(filter.multiple.regression.poly.list)){filter.multiple.regression.poly.list[[mod.comb.no]][1] != ""}else{F}
        ){
          for (outlier in -filter.multiple.regression.poly.list[[mod.comb.no]]){
            results.regression.poly.unfiltered <- get.results.metafor(
              regression.df, moderator = moderator.combination
            )

            cat("\n Gosh plot for", paste(moderator.combination, sep = ", "), "(outlier number:", outlier, "; Degree:",degree, ")")
            sav <- gosh(results.regression.poly.unfiltered, subsets = 15000)
            plot(sav, out = outlier)
          }
        } else {
          cat("\nno Gosh plot printed for", moderator.combination, "(linear)\n")
        }
      }
    }
    
    if (print.regression.results & degree.1){
      cat("\nMultiple Regression Results for", moderator.combination, "\n")
      if (non.interaction){
        print("Multiple Linear (no interaction)")
        print(results.regression.linear)
      }
      if (interaction){
        print("Multiple Linear (with interaction)")
        print(results.regression.linear.interact)
      }

      if (degree.2){
        cat("\n")
        print("Multiple Polynomial")
        print(results.regression.poly)
      }
    }
    
    mod.comb.no <- 1 + 1
  }
}

# implement also interaction of moderators?

# %% [markdown] heading_collapsed=true hidden=true
# ### Plot Regression plots with ggplot

# %% hidden=true vscode={"languageId": "r"}
# only for linear regression
regplot.ggplot <- function(reg.df, moderator, regression.label = T){    
  res.reg.lin.metafor <- get.results.metafor(
    reg.df, moderator = moderator
  )

  res.escalc <- escalc(
    data = reg.df, measure = "SMD",
    m1i = reg.df[,"mean.int"], sd1i = reg.df[,"sd.int"], n1i = reg.df[,"n.int"],
    m2i = reg.df[,"mean.control"], sd2i = reg.df[,"sd.control"], n2i = reg.df[,"n.control"]
  ) %>% mutate(labels = 1:length(rownames(.))) %>% relocate(labels)

  pred <- as.data.frame(predict(res.reg.lin.metafor, addx=TRUE))

  w <- weights(res.reg.lin.metafor)
  
  font.size <- 21
  
  if (regression.label){
    p <- ggplot(pred, aes(x = !!sym(paste("X.", moderator, sep = "")), y = pred, group = 1)) +
      geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "blue", col = "blue", linetype=3, data = pred) +
      geom_ribbon(aes(ymin = pi.lb, ymax = pi.ub), alpha = 0.05, fill = "red", col = "red", linetype=3, data = pred) +
      geom_line(col = "blue", data = pred,) + 
      geom_point(aes(x=!!sym(moderator), y = yi, size = w), color="darkslategray", alpha = 0.4, data = res.escalc) +
      geom_text(data = res.escalc, aes(x=!!sym(moderator),y=yi, label = labels), vjust = 1.8) +

      # reference line
      geom_hline(yintercept = 0, linetype='dotted') +

      # labels
      ylab("Standard Mean Difference") +
      xlab(regression.labels.df[,moderator]) +
      labs(title = paste(regression.labels.df[,moderator])) +
      scale_size_continuous(guide = "none") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      theme(text=element_text(size=font.size))
  } else {
    p <- ggplot(pred, aes(x = !!sym(paste("X.", moderator, sep = "")), y = pred, group = 1)) +
      geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.05, fill = "blue", col = "blue", linetype=3, data = pred) +
      geom_ribbon(aes(ymin = pi.lb, ymax = pi.ub), alpha = 0.05, fill = "red", col = "red", linetype=3, data = pred) +
      geom_line(col = "blue", data = pred,) + 
      geom_point(aes(x=!!sym(moderator), y = yi, size = w), color="darkslategray", alpha = 0.4, data = res.escalc) +

      # reference line
      geom_hline(yintercept = 0, linetype='dotted') +

      # labels
      ylab("Standard Mean Difference") +
      xlab(regression.labels.df[,moderator]) +
      labs(title = paste(regression.labels.df[,moderator])) +
      scale_size_continuous(guide = "none") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      theme(text=element_text(size=font.size))
  }
  plot(p)
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Get correlation matrix

# %% hidden=true vscode={"languageId": "r"}
get.cor.matrix <- function(
  regression.df,  # regression.df should be df with all regression data (all time points)
  moderator.cor.vec = c("sessions.duration", "sessions.frequency", "programs.duration", "follow.up.period", "female.percent")
){
  
  # generate correlation matrix
  mod.len <- length(moderator.cor.vec)
  cor.df <- data.frame(matrix(rep(NA, mod.len**2), nrow = mod.len, ncol = mod.len))
  colnames(cor.df) <- moderator.cor.vec
  rownames(cor.df) <- moderator.cor.vec

  i <- 0
  for(moderator.cor.1 in moderator.cor.vec){
    i <- i + 1
    j <- 0
    for (moderator.cor.2 in moderator.cor.vec){
      j <- j + 1
      if (j >= i){
        cor.df[i, j] <- NA
      } else {
        mod.vec.1 <- regression.df[, moderator.cor.1]
        mod.vec.2 <- regression.df[, moderator.cor.2]
        mod.df <- data.frame(
          mod.1 = mod.vec.1,
          mod.2 = mod.vec.2
        )
        r.test <- cor.test(~ mod.1 + mod.2, data = mod.df)
        r.est <- round(r.test$estimate, digits = 2)  # pearson's rho
        r.p <- r.test$p.value  # p-value
        print(r.p)
        r.p.symbol <- if(T %in% (r.p <= 0.001)){"***"}else if(T %in% (r.p <= 0.01)){"**"}else if(T %in% (r.p <= 0.05)){"*"}else{""}
        cor.df[i, j] <- paste(r.est, r.p.symbol, sep = "")
      }
    }
  }
  
  # add column with moderator names (for display in dashboard)
  cor.df <- cor.df %>%
    mutate(moderators = moderator.cor.vec) %>%
    relocate(moderators)
  
  as.data.frame(cor.df)
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Print all Results

# %% code_folding=[] hidden=true vscode={"languageId": "r"}
# calculating regression results was outsourced from meta.analyze() to this function as it occured an unknown error
# does not support network-meta-analysis or choice of the comparison groups
print.meta.results <- function(
  outcome, preferred.scale = F, print.meta.results = T, meta.df.list = F, return.data = F, results.metafor.fixed = F,
  
  # arguments for forest and funnel plot
  basic = T, 
  print.forest = T, forest.add.fix.eff.mod = T, forest.add.power = F, forest.layout = "RevMan5", print.funnel = T, funnel.label = T, funnel.label.out.only = T,
  print.influence = F, influence.ids = "name", print.baujat = T,
  split.subgroups = T, print.forest.sub.single = F, subgroup.method = "random",
  filter.forest..funnel.vec = F, exclude.high.rob.vec = F,
    # basic is to decide if forest and funnel plot shell be printed
    # set forest.layout = gs("layout") to use custom layout
  
  # arguments for regression
  ## (singlie regression)
  regression = T, 
  moderator.vec = c("sessions.duration", "sessions.frequency", "programs.duration", "meditation.total", "follow.up.period", "delivery.mode", "meditation.type", "female.percent"),
  regression.degree.1 = T, regression.degree.2 = T, filter.regression.vec = F, print.qq.norm = F, print.regression.df = F,
  filter.regression.linear.list = F, filter.regression.poly.list = F,
    # regression = F will disable only single not multiple regression
    # filter.regression.vec filters regression.df that comes from meta.analyze
    # filter.regression.linear/poly.list (filters only linear regression) --> list with filtering vectors in order of moderator.vec ("" means no filtering)
  
  ## (additive arguments for multiple regression)
  regression.multiple = F,
  moderator.multiple.list = F, filter.multiple.regression.linear.list = F, filter.multiple.regression.poly.list = F,
  regression.multiple.degree.2 = T, non.interaction = T, interaction = F,
  
  ## for both single and multiple regression
  print.regression.results = T,  print.regplot = T, print.baujat.regression = T, print.gosh.regression = F, regression.label = F,
  without.mean.r = F
){
  
  meditation.type.var <- meditation.type.all
  
  cat("\n#", outcome, "----------------------------------\n")
  
  return.data.var <- NULL
  
# Forest and Funnel Plot
  if (basic){
    return.data.var <- meta.analyze(
      outcome, meditation.types = meditation.type.var, m.data.list = m.data.list, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
      print.forest = print.forest, forest.add.fix.eff.mod = forest.add.fix.eff.mod, forest.add.power = forest.add.power, forest.layout = forest.layout, print.baujat = print.baujat, print.funnel = print.funnel,
      funnel.label = funnel.label, funnel.label.out.only = funnel.label.out.only, print.meta.results = print.meta.results, print.influence = print.influence, influence.ids = influence.ids,
      return.data = return.data, results.metafor.fixed = results.metafor.fixed,
      filter.forest..funnel.vec = filter.forest..funnel.vec, split.subgroups = split.subgroups, print.forest.sub.single = print.forest.sub.single, subgroup.method = subgroup.method,
      exclude.high.rob.vec = exclude.high.rob.vec, sort.by = "hedges.g", without.mean.r = without.mean.r  # print.descriptive = T
    )
  }
  
# Single Regression (one moderator)
  if (regression){
    mod.no <- 1
    
    # create regression df
    for (moderator in moderator.vec){
      if (length(moderator.vec) > 1){
        print(moderator)
      }
      regression.df <- meta.analyze(
        outcome, meditation.type.var, m.data.list, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        moderator = moderator, filter.regression.vec = filter.regression.vec,
        split.subgroups = F, print.influence = F, without.mean.r = without.mean.r
      )
      
      if (nrow(regression.df) >= 4){
        if (F %in% (regression.df[1,moderator] == regression.df[,moderator])){
          # ^ checks if there is more than one equal value present in regression.df[,moderator]
          
  ## Linear Regression
          if (regression.degree.1){
            # filter for linear regression
            if (
              is.list(filter.regression.linear.list) &
              if(is.list(filter.regression.linear.list)){filter.regression.linear.list[[mod.no]][1] != ""}else{F}
            ){
              regression.df.filtered <- regression.df[filter.regression.linear.list[[mod.no]],]
            } else {
              regression.df.filtered <- regression.df
            }

            # get results for linear regression
            results.regression.linear <- get.results.metafor(
              regression.df.filtered, moderator = moderator, results.metafor.fixed = results.metafor.fixed
            )
            
            if (return.data == "regression.results.linear"){
              return.data.var <- results.regression.linear
            }
            
            for (modertor.label in colnames(regression.labels.df)){
              if (moderator == modertor.label){
                x.label <- regression.labels.df[1,moderator]
              }
            }
            
            # regression/bubble plot
            if (moderator == "delivery.mode" & print.regplot){
              # with metafor | categorical moderators are not implemented in regplot.ggplot()
              regplot(
                results.regression.linear,
                refline=0,
                pi = T,
                grid = T,
                label = regression.label,
                legend = F,
                xlab = x.label
                # xlim = c(0,60), predlim = c(0,60)  # adjusting display area of graph
              )
            } else if (moderator == "meditation.type" & print.regplot){
              print("error in print.meta.results(): ploting regression plots is not implemented for categorical moderators with more than 2 levels like 'meditation.type'")
            } else if (print.regplot){
              # with ggplot
              regplot.ggplot(regression.df.filtered, moderator, regression.label)
            }
            
            if (print.qq.norm){
              qqnorm(results.regression.linear, label = regression.label)
            }

            # baujat plot
            if (print.baujat.regression){
              # outlier analysis
              # par(mar = c(5, 6, 4, 2))  # to set plot parameters?
              # plot(influence(results.regression.linear), cex = 0.8, las = 1)
              if (nrow(regression.df.filtered) > 4){
                # get study ids
                stud.id <- results.regression.linear$data$study.id

                ## get study ids that are present more than once
                stud.id.tab <- table(stud.id)
                stud.id.multi <- names(stud.id.tab[stud.id.tab > 1])

                ## add number to study ids that are present more than once
                if (length(stud.id.multi) >= 1){
                  for (id.multi in stud.id.multi){
                    id.m.i <- 1
                    id.i <- 1
                    for (id in stud.id){
                      if (id %in% id.multi){
                        stud.id[id.i] <- paste(stud.id[id.i], ", ", id.m.i, sep = "")
                        id.m.i <- id.m.i + 1
                      }
                    id.i <- id.i + 1
                    }
                  }
                }
                results.regression.linear$slab <- stud.id
                
                # print baujat plot
                baujat(results.regression.linear, xlab = paste("Squared Pearson Residual (for ", regression.labels.df[1, moderator], "; linear)", sep = ""), symbol = "slab")
              } else {
                print("no baujat plot due to less than 5 stuides")
              }
            } 

            # gosh plot
            if (print.gosh.regression){
              if (
                is.list(filter.regression.linear.list) &
                if(is.list(filter.regression.linear.list)){filter.regression.linear.list[[mod.no]][1] != ""}else{F}
              ){
                for (outlier in -filter.regression.linear.list[[mod.no]]){

                  results.regression.linear.unfiltered <- get.results.metafor(
                    regression.df, moderator = moderator, results.metafor.fixed = results.metafor.fixed
                  )

                  cat("\n Gosh plot for", moderator, "(outlier number:", outlier, "linear)")
                  sav <- gosh(results.regression.linear.unfiltered, subsets = 15000)
                  plot(sav, out = outlier)
                }
              } else {
                cat("\nno Gosh plot printed for", moderator, "(linear)\n")
              }
            }
            
            if (print.influence){
              plot.influnece(results.regression.linear, influence.ids)
            }
          }
          
  ## Polynomial Regression
          if (regression.degree.2 & moderator %in% moderator.vec[1:4]){
              # only for continuous moderators
            
            # filter polynomial regression data
            if (
              is.list(filter.regression.poly.list) &
              if(is.list(filter.regression.poly.list)){filter.regression.poly.list[[mod.no]][1] != ""}else{F}
            ){
              regression.df.filtered <- regression.df[filter.regression.poly.list[[mod.no]],]
            } else {
              regression.df.filtered <- regression.df
            }
            
            for (modertor.label in colnames(regression.labels.df)){
              if (moderator == modertor.label){
                x.label <- regression.labels.df[1,moderator]
              }
            }
            
            degree <- 2
            
            # get polynomial regression results
            results.regression.polynomial <- get.results.metafor(
              regression.df.filtered, moderator = moderator, degree = degree, results.metafor.fixed = results.metafor.fixed
            )
            
            if (return.data == "regression.results.poly"){
              return.data.var <- results.regression.polynomial
            }
            
            # bubble/regression plot
            if (!moderator %in% c("delivery.mode", "meditation.type") & print.regplot){
              if (nrow(regression.df.filtered) > 4){
                # not figured out how to plot regplot with ggplot yet
                
                # with metafor
                xvals <- seq(-1, max(regression.df.filtered[,moderator]) + 5, length=500)
                sav <- predict(results.regression.polynomial, newmods= unname(poly(xvals, degree=degree, raw=T)))

                regplot(
                  results.regression.polynomial, mod=2, pred=sav, xvals=xvals, refline=0, las=1, digits=1, bty="l",
                  label=regression.label, xlab=x.label,
                  pi = T, grid = T, legend = F
                )
              } else {
                print("polynomial bubble plot was not printed because number of incluided studies is <= 4")
              }
            }
            
            # baujat plot
            if (print.baujat.regression){
              # outlier analysis
              # par(mar = c(5, 6, 4, 2))  # to set plot parameters?
              # plot(influence(results.regression.polynomial), cex = 0.8, las = 1)
              if (nrow(regression.df.filtered) > 4){
                # get study ids
                stud.id <- results.regression.polynomial$data$study.id

                ## get study ids that are present more than once
                stud.id.tab <- table(stud.id)
                stud.id.multi <- names(stud.id.tab[stud.id.tab > 1])

                ## add number to study ids that are present more than once
                if (length(stud.id.multi) >= 1){
                  for (id.multi in stud.id.multi){
                    id.m.i <- 1
                    id.i <- 1
                    for (id in stud.id){
                      if (id %in% id.multi){
                        stud.id[id.i] <- paste(stud.id[id.i], ", ", id.m.i, sep = "")
                        id.m.i <- id.m.i + 1
                      }
                    id.i <- id.i + 1
                    }
                  }
                }
                results.regression.polynomial$slab <- stud.id
                
                # plot baujat plot
                baujat(results.regression.polynomial, xlab = paste("Squared Pearson Residual (for ", regression.labels.df[1, moderator], "; squared)", sep = ""), symbol = "slab")
              } else{
                print("no baujat plot due to less than 5 stuides")
              }
            }
            
            # gosh plot
            if (print.gosh.regression){
              if (
                is.list(filter.regression.poly.list) &
                if(is.list(filter.regression.poly.list)){filter.regression.poly.list[[mod.no]][1] != ""}else{F}
              ){
                for (outlier in -filter.regression.poly.list[[mod.no]]){

                  results.regression.polynomial.unfiltered <- get.results.metafor(
                    regression.df, moderator = moderator, results.metafor.fixed = results.metafor.fixed
                  )

                  cat("\n Gosh plot for", moderator, "(outlier number:", outlier, "polynomial)")
                  sav <- gosh(results.regression.polynomial.unfiltered, subsets = 15000)
                  plot(sav, out = outlier)
                }
              } else {
                cat("\nno Gosh plot printed for", moderator, "(polynomial)\n")
              }
            }
            
            if (print.influence){
              plot.influnece(results.regression.polynomial, influence.ids)
            }
          }
          
          # print regression results
          if (print.regression.results){
            print(moderator)
            print("Linear")
            print(results.regression.linear)
            if (regression.degree.2){
              cat("\n")
              print("Polynomial")
              print(results.regression.polynomial)
            }
          }
        } else {
          print(sprintf('Only equal moderator values present for regression of outcome:"%s" and moderator: "%s"', outcome, moderator))
          cat("\n")
        }
      } else {
        print(sprintf('Number of included studies (%d) in regression is below 4 for outcome "%s" and moderator "%s"', nrow(regression.df), outcome, moderator))
        cat("\n")
      }
      
      # print regression data frame
      if(print.regression.df){
        print(moderator)
        print(regression.df)
      }
      
      mod.no <- mod.no + 1
    }    
  }
  
  if (regression.multiple){
    do.multiple.regressions(  # fixed effects model not implemented
      outcome = outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
      moderator.multiple.list = moderator.multiple.list,
      filter.multiple.regression.linear.list = filter.multiple.regression.linear.list,
      filter.multiple.regression.poly.list = filter.multiple.regression.poly.list,
      print.regplot = print.regplot, print.baujat.regression = print.baujat.regression,
      print.gosh.regression = print.gosh.regression, print.regression.df = print.regression.df,
      regression.label = regression.label, degree.2 = regression.multiple.degree.2,
      non.interaction = non.interaction, interaction = interaction
    )
  }
  
  if (return.data == "correlation.matrix"){
    regression.all.df <- meta.analyze(
      outcome, meditation.type.all, m.data.list, preferred.scale = preferred.scale,  # meta.df.list = meta.df.list,
      split.subgroups = FALSE, return.data = "regression.all", results.metafor.fixed = results.metafor.fixed  #, without.mean.r = without.mean.r
    )
    return.data.var <- get.cor.matrix(regression.all.df)
  }
  
  if (
    !return.data %in% c("meta.df.list", "funnel.asym.p.egger", "funnel.asym.p.rank", "descriptive", "regression.all", "results.meta", "results.metafor", "hedges.g", "influence.df", "regression.results.linear", "regression.results.poly", "correlation.matrix", "power.subgroup") &
    return.data != F
  ){
    print('error in print.meta.results(): set return.data to "meta.df.list", "funnel.asym.p.egger", "funnel.asym.p.rank", "descriptive", "regression.all", "results.meta", "results.metafor", "hedges.g", "influence.df", "regression.results.linear", "regression.results.poly", "correlation.matrix", or "power.subgroup"')
  }
  
  if (return.data != F){
    return(return.data.var)
  }
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Get global data frame of all important data to calculate overall SMD from all outcomes

# %% hidden=true vscode={"languageId": "r"}
get.global.df <- function(exclude.outcome.vec = c(), comparison.list = list(meditation.type.all, cont.passive), filter.forest..funnel.vec = F){
  
  # gernerate empty df with columns of meta.df.list[[1]] + an outcome column
  global.df <- data.frame(matrix(ncol = 20, nrow = 0))
  colnames(global.df) <- c(
    "study.id", "n.control", "mean.control", "sd.control", "n.int", "mean.int", "sd.int", "pooled.sd", "mean.diff", "hedges.g",
    "weights", "sessions.duration", "sessions.frequency", "programs.duration", "delivery.mode", "follow.up.period", "meditation.type",
    "meditation.total", "female.percent", "outcome"
  )
  
  for (outcome in present.outcomes.sorted){
    if (outcome %in% exclude.outcome.vec){
      next
    }
    # get data of post-test per outcome
    outcome.df <- meta.analyze(
      outcome, meditation.type.all, m.data.list, preferred.scale = get.1st.preferred.scale(outcome),
      comparison.list = comparison.list, filter.forest..funnel.vec = filter.forest..funnel.vec,
      split.subgroups = F, return.data = "meta.df.list" # ,
      # filter.forest..funnel.vec = - outlier.list[["Stress"]]
    )[[1]]
    
    if (nrow(outcome.df) > 0){
      # add column of outcome
      outcome.df$outcome <- outcome

      # append to global.df
      global.df <- rbind(global.df, outcome.df)
    }
  }
  global.df
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Get overall results

# %% hidden=true vscode={"languageId": "r"}
get.overall.res.metafor <- function(
  subset = NULL, exclude.outcome.vec = c(), return.data = "results.metafor", mods = F, comparison.list = list(meditation.type.all, cont.passive),
  filter.forest..funnel.vec = F
){
  # get df of all studies' data
  global.df <- get.global.df(exclude.outcome.vec, comparison.list = comparison.list, filter.forest..funnel.vec = filter.forest..funnel.vec)

  # append SMDs and sampling variances
  if (nrow(global.df) > 0){

    res.escalc <- escalc(
      data = global.df, measure = "SMD",
      m1i = mean.int, sd1i = sd.int, n1i = n.int,
      m2i = mean.control, sd2i = sd.control, n2i = n.control
    )

    # add id
    res.escalc <- res.escalc |> mutate(id = 1:nrow(res.escalc))

    # adjust polarity
    outcomes.neg.pol.df <- outcome.direction.df |> filter(High.or.low.means.resilient == "v")
    outcomes.neg.pol.vec <- outcomes.neg.pol.df$Outcome
    outcomes.neg.pol.vec
    pol.correct <- res.escalc |>
      filter(outcome %in% outcomes.neg.pol.vec) |>
      mutate(yi = yi * -1)

    res.escalc[res.escalc$id %in% pol.correct$id,] <- pol.correct

    # arrarnge by SMD
    res.escalc <- res.escalc |> arrange(yi)

    # reset id
    res.escalc <- res.escalc |> mutate(id = 1:nrow(res.escalc))

    if (!is.null(subset)){
      t.stud.ids <- 1:nrow(res.escalc)
      t.stud.ids <- t.stud.ids[subset]
      res.escalc <- res.escalc |>  filter(id %in% t.stud.ids)
    }
  
    if (return.data == "results.metafor"){
      if (mods == F){
        # get results (multi-variate if more than one study is onluded otherwise univariate)
        if (nrow(global.df) > 1){
          return(
            rma.mv(yi, vi, random = ~ 1 | outcome/study.id, data=res.escalc, test = "t", slab = outcome)
          )
        } else {
          return(rma.uni(yi, vi, measure="SMD", data = res.escalc))
        }
      } else {
        return(
          rma.mv(yi, vi, random = ~ 1 | outcome/study.id, data=res.escalc, test = "t", slab = outcome, mods = formula(paste("~", mods, "-1")))
        )
      }
    } else if (return.data == "results.escalc"){
      return(res.escalc)
    } else if (return.data != F){
      print("error in get.overall.res.metafor(): set parameter return.data to 'results.metafor' or 'results.escalc'")
    }
  } else {
    if (return.data == "results.metafor"){
      return(data.frame(k = 0))
    } else if (return.data == "results.escalc"){
      return(NULL)
    } else if (return.data != F){
      print("error in get.overall.res.metafor(): set parameter return.data to 'results.metafor' or 'results.escalc'")
    }
  }
}

# %% [markdown] hidden=true
# ### Plot Summary Forest Plot

# %% hidden=true vscode={"languageId": "r"}
intervention.comparisons.df.list[[1]]

# %% code_folding=[93] hidden=true vscode={"languageId": "r"}
# using random effect models
# fonts adjusted for plot size  fonts adjusted for plot size options(repr.plot.width = 25, repr.plot.height = 9, repr.plot.res = 350)
plot.summary.forest <- function(
  net.res.all = F,
  with.outliers = T,
  overall.method = "rma.mv",
  overall.measure = "net",
  outcome_vec = present.outcomes,
  title = "Summary Forest Plot",
  no.participants.df.n.total.imputed_ = no.participants.df.n.total.imputed,
  study.names.suff.data_ = study.names.suff.data
) {
  # parameter with.outliers is not implemented for the overall results of the network meta-analysis model 
  # get total effect sizes and confidence intervals of all outcomes
  outcomes <- c()
  k <- c()
  o <- c()  # total observations
  o.i <- c()  # observations in intervention
  o.c <- c()  # observations in control
  te <- c()
  ci.l <- c()
  ci.u <- c()
  I2 <- c()
  pow <- c()
  mean.int <- c()
  mean.control <- c()
  seTE.random <- c()
  
  present.outcomes.ordered.te <- outcome_vec
  
  for (outcome in present.outcomes.ordered.te){
    results.meta <- meta.analyze(
      outcome = outcome, meditation.types = meditation.type.all, m.data.list = m.data.list,
      return.data = "results.meta", preferred.scale = get.1st.preferred.scale(outcome), split.subgroups = FALSE,
      filter.forest..funnel.vec = if(length(outlier.list[[outcome]]) > 0 & !with.outliers){-outlier.list[[outcome]]}else{FALSE}
    )
    if (results.meta$k > 0){
      outcomes <- append(outcomes, outcome)
      k <- append(k, results.meta$k)
      o.i <- append(o.i, sum(results.meta$data[, "n.int"]))
      o.c <- append(o.c, sum(results.meta$data[, "n.control"]))
      o <- append(
        o, sum(results.meta$data[, "n.int"]) + sum(results.meta$data[, "n.control"])
      )
      te  <- append(te, results.meta$TE.random)
      ci.l  <- append(ci.l, results.meta$lower.random)
      ci.u <- append(ci.u, results.meta$upper.random)
      I2 <- append(I2, results.meta$I2)
      pow <- append(pow, results.meta$power)
      seTE.random <- append(seTE.random, results.meta$seTE.random)
    }
  }

  df.sum <- data.frame(
    outcomes = outcomes,
    k = k,
    o.i = o.i,
    o.c = o.c,
    o = o,
    te = te,
    ci.l = ci.l,
    ci.u = ci.u,
    I2 = I2,
    pow = pow,
    seTE.random = seTE.random
  )

  # correct scale  directions
  for (outcome in df.sum[,"outcomes"]){
    if (outcome.direction.df[outcome.direction.df[,"Outcome"] == outcome, "High.or.low.means.resilient"] == "v"){
      df.sum[df.sum[,"outcomes"] == outcome, c("te", "ci.l", "ci.u")] <- -1 * df.sum[df.sum[,"outcomes"] == outcome, c("te", "ci.u", "ci.l")]
    }
  }
  
  
  
  # insert overall metrics
  if (overall.measure == "net"){
    
    overall.name <- "Overall (network meta-analysis model)"

    # Get study names of studies including the outcomes
    study.names.of.outcomes <- outcome.names.df %>%
      filter(if_any(everything(), ~ . %in% outcome_vec)) %>%
      row.names()

    # Define number of participants
    n_participants_overall <- no.participants.df.n.total.imputed_ %>%
      # Select only columns of studies with sufficient data
      select(all_of(study.names.suff.data_)) %>%
      # Select only rows of studies including the outcomes
      select(any_of(study.names.of.outcomes)) %>%
      # Filter for sum row
      filter(row.names(.) == "Sum") %>% 
      sum()

    # sum(no.participants.df.n.total.imputed_["Sum", study.names.suff.data])
    
    if (is.logical(net.res.all)){
      print("error in plot.summary.forest(): set net.res.all to netmeta results object of network meta-analysis while overall.measure = 'net'")
    }
    
    df.overall <- data.frame(
      outcomes = overall.name,
      k = c(sum(net.res.all$k)),
      o.i = c(NA),
      o.c = c(NA),
      o = c(n_participants_overall),
      te = c(net.res.all$TE.random["meditation (exclusive)", "passive control"]),
      ci.l = c(net.res.all$lower.random["meditation (exclusive)", "passive control"]),
      ci.u = c(net.res.all$upper.random["meditation (exclusive)", "passive control"]),
      I2 = c(net.res.all$I2),
      pow = c(NA),
      seTE.random = c(net.res.all$seTE.random["meditation (exclusive)", "passive control"])
    )
  } else if (overall.measure == "pairwise"){
    if (overall.method == "rma.mv"){
      
      overall.name <- "Overall (multi-variate model)"
      
      # get results
      res.overall.mv <- get.overall.res.metafor()

      # generate one row df with metrics of interest
      df.overall <- data.frame(
        outcomes = overall.name,
        k = c(sum(df.sum$k)),
        o.i = c(sum(df.sum$o.i)),
        o.c = c(sum(df.sum$o.c)),
        o = c(sum(df.sum$o)),
        te = c(res.overall.mv$b[1, 1]),
        ci.l = c(res.overall.mv$ci.lb),
        ci.u = c(res.overall.mv$ci.ub),
        I2 = c(max(df.sum$I2, na.rm = T)),  # to make it red in plot (will be set to "NA" in table) 
        pow = c(NA),
        seTE.random = c(NA)
      )

    } else if (overall.method == "metamean"){
      # bad approach | better: with metafor::rma.mv(); see https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/multilevel-ma.html#multilevel-R
      res.overall.meta <- metamean(n = df.sum$o, mean = df.sum$te, sd = df.sum$seTE.random * sqrt(df.sum$o), studlab = df.sum$outcomes)
      df.overall <- data.frame(
        outcomes = overall.name,
        k = c(sum(df.sum$k)),
        o.i = c(sum(df.sum$o.i)),
        o.c = c(sum(df.sum$o.c)),
        o = c(sum(df.sum$o)),
        te = c(res.overall.meta$TE.random),
        ci.l = c(res.overall.meta$lower.random),
        ci.u = c(res.overall.meta$upper.random),
        I2 = c(res.overall.meta$I2),
        pow = c(NA),
        seTE.random = c(NA)
      )
    } else {
      print("error in plot.summary.forest(): set overall.method to 'rma.mv' or 'metamean'")
    }
  # Don't add an overall measure if overall.measure is set to 'none'
  } else if (tolower(overall.measure) == "none") {
    df.overall <- data.frame(
      outcomes = character(),
      k = numeric(),
      o.i = numeric(),
      o.c = numeric(),
      o = numeric(),
      te = numeric(),
      ci.l = numeric(),
      ci.u = numeric(),
      I2 = numeric(),
      pow = numeric(),
      seTE.random = numeric()
    )
  } else {
    print("error in plot.summary.forest(): set overall.measure to 'net', 'pairwise', or 'none'")
  }

  df.sum <- rbind(
    df.sum,
    df.overall
  )
  
  # create col with I^2 is 0 instead of NA
  df.sum$I2.trans <- df.sum$I2*100

  df.sum[is.na(df.sum$I2.trans), "I2.trans"] <- 0
  
  # addd column of te that does not get rounded (for correct order in plot/table later)
  df.sum$te.prec <- df.sum$te
  if (tolower(overall.measure) != "none") {
    df.sum[df.sum$outcomes == overall.name, "te.prec"] <- -100  # to set it at the bottom line
  }
  # roud values
  df.sum[,c("o", "te", "ci.l", "ci.u", "I2", "I2.trans", "pow")] <- round(df.sum[,c("o", "te", "ci.l", "ci.u", "I2", "I2.trans", "pow")], 2)
  df.sum[,c("I2", "I2.trans")] <- round(df.sum[,c("I2", "I2.trans")], 0)

  # create col of strings with te an ci vaulues
  df.sum$te.ci <- paste(
    as.character(df.sum$te), " (",
    as.character(df.sum$ci.l), ", ",
    as.character(df.sum$ci.u), ")",
    sep = ""
  )

  # create col of strings with observation and no. of interventions
  df.sum$o.k <- paste(
    as.character(df.sum$o), " (",
    as.character(df.sum$k), ")",
    sep = ""
  )
  
  if (!is.logical(net.res.all) && tolower(overall.measure) != "none") {
    df.sum[df.sum$outcomes == overall.name, "o.k"] <- paste(
      as.character(df.sum[df.sum$outcomes == overall.name, "o"]), " (",
      as.character(net.res.all$m), ")",
      sep = ""
    )
  } else if (tolower(overall.measure) != "none") {
    df.sum[df.sum$outcomes == overall.name, "o.k"] <- as.character(df.sum[df.sum$outcomes == overall.name, "o"])
  }
  
  # set band colours for table
  df.sum <- arrange(df.sum, te.prec)  # sort df.sum according to te
  if (nrow(df.sum) %% 2 == 1){
    df.sum$colour <- c("gray90", rep(c("white", "gray95"), nrow(df.sum) / 2))
  } else {
    df.sum$colour <- rep(c("white", "gray95"), nrow(df.sum) / 2)
  }
  
  
  # set color range for coloring heterogeneity
  fun_color_range <- colorRampPalette(c("chartreuse4", "gold", "red", "darkred"))  # Create color generating function
  my_colors <- fun_color_range(20)
    
  
  # drop column of power
  df.sum <- df.sum %>% select(!pow)
  # forest plot"
  p <- ggplot(df.sum, aes(y = te, x = reorder(outcomes, te.prec))) + 
    geom_vline(xintercept = df.sum$outcomes, colour = df.sum$colour, size = 19) +
    geom_pointrange(
      aes(
        ymin = ci.l, ymax = ci.u, colour = I2.trans, size = o
      ),
      alpha = 0.5, shape = 15
    ) +
    coord_flip() +
    guides(colour = guide_legend(override.aes = list(size=1)), reverse=TRUE) +
    scale_colour_gradientn(
      colors = my_colors, name=expression(paste("I"^{2}*" [%]")),
      limits = c(0,100), breaks = c(0, 25, 50, 75, 100)
    ) + 
    scale_size_continuous(
      name = "Data Points", range = c(0.5, 3),
      breaks = c(
        950, 450, 200, 50
      )
    ) +
    geom_hline(aes(yintercept=0), colour = 'red', lty=2) +
    geom_hline(aes(yintercept=0.2), colour = 'darkgray', lty=3) +
    geom_hline(aes(yintercept=0.5), colour = 'darkgray', lty=3) +
    geom_hline(aes(yintercept=0.8), colour = 'darkgray', lty=3) +
    geom_hline(aes(yintercept=-0.2), colour = 'darkgray', lty=3) +
    geom_hline(aes(yintercept=-0.5), colour = 'darkgray', lty=3) +
    geom_hline(aes(yintercept=-0.8), colour = 'darkgray', lty=3) +
    theme_bw() +
    theme(
      legend.key=element_rect(fill='cornsilk2'),
      axis.text.y = element_blank(), axis.title.y = element_blank(),
      axis.text=element_text(size=18), #change font size of axis text
      axis.title=element_text(size=18), #change font size of axis titles
      plot.title=element_text(size=18, hjust = 0.5),
      legend.title = element_text(size=19),
      legend.text = element_text(size=16),
    ) +
    ggtitle("\nSt. Mean Difference") + ylab("may reduce resilience                                 may improve resilience")
  # table
  font.size <- 7
  
  # Set I2 to "NA" where not applicable
  if (overall.measure != "net" && tolower(overall.measure) != "none") {
    df.sum[df.sum$outcomes == overall.name, c("I2", "I2.trans")] <- "NA"
  }
  df.sum[df.sum$k <= 1, c("I2", "I2.trans")] <- "NA"
  
  
  data_table <- ggplot(data = df.sum, aes(y = reorder(outcomes, te.prec))) +
    geom_hline(aes(yintercept = outcomes, colour = arrange(df.sum, te.prec)$colour), size = 19) +

    geom_text(aes(x = 0, label = outcomes), hjust = 0, size = font.size) +
    geom_text(aes(x = 1, label = o.k), nudge_x = 0.1, hjust = 0, size = font.size) +
    geom_text(aes(x = 2, label = te.ci), hjust = 0, nudge_x = - 0.1, size = font.size) +
    geom_text(aes(x = 3, label = I2.trans), hjust = 0, nudge_x = - 0.1, size = font.size) +
    # geom_text(aes(x = 4, label = pow), hjust = 0, nudge_x = - 0.5, size = font.size) +

    scale_colour_identity() +
    theme_void() +
    ggtitle(expression(paste("       Outcome                                   Data Points (Comparisons)   SMD (CI) [Hedge's g]               I"^{2}*" [%]"))) +
    theme(
      plot.margin = margin(6, 0, 48, 0),
      plot.title=element_text(size=23, face="bold"),
    )
  
  # combine forest plot and table with additional title
  combined_plot <- grid.arrange(
    data_table, p, ncol = 2,
    widths = c(4/7, 3/7),
    top = textGrob(title, gp = gpar(fontsize = 30, fontface = "bold"))
  )
  combined_plot
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Network meta-analysis

# %% hidden=true vscode={"languageId": "r"}
comparions.all <- control.all.list
comparions.all <- append(list(`exclusive meditation` = meditation.type.all), comparions.all)
names(comparions.all)
names(comparions.all) <- c(
  "meditation (exclusive)", "passive control", "rest", "cognitive control", "meditation with movement", "PMR", "Autogentic Training",
  "MBSR", "biofeedback", "sham bio feedback", "stress management", "dog therapy", "walking", "combinations"
)
names(comparions.all)

# %% code_folding=[] hidden=true vscode={"languageId": "r"}
net.meta.analyze <- function(
  # parameters for data collection
  outcome.vec = NULL, preferred.scale = F, net.df = F, net.res = F, filter.forest..funnel.vec = F, comparisons.skip.list = F,
  
  # for netmeta()
  details.chkmultiarm = T, tol.multiarm = 10,  ####### <-- do sensitivity analyis in which studies with higher multi-arm inconsitency than default value in netmeta() are cut out
  
  # plots
  plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
  
  # return
  silent = F, return.data = F,
  
  # additional
  reference.group = "passive control", random = T
  
  
  # set df or results object to net.df or net.res to skip the respective caluclation (directly plot the repective plots with given df or results object)
){
  
  if (is.logical(net.df) & is.logical(net.res)){
    net.df <- data.frame(
      studlab = c(),
      outcome = c(),
      TE = c(),
      seTE = c(),
      treat1 = c(),
      treat2 = c()
    )

    # Renomve all follow-up time points from m.data.list
    # (otherwise time points after the post-test would flow into the model as post-test data)
    m.data.list.no.T2.T3 <- m.data.list
    m.data.list.no.T2.T3$results.descriptive.array[,,c("T2", "T3"),,,] <- NA_real_
    
    for (outcome in outcome.vec){
      for (i in 1:length(comparions.all)){
        for (j in 1:length(comparions.all)){
          # skip if same intervention/control is compared to it self or if comparison was done already
          if (j <= i){
            next
          }

          if (preferred.scale == F){
            preferred.scale <- get.1st.preferred.scale(outcome)
          }

          if (!silent){
            cat("\n", "# ", names(comparions.all)[i], " vs. ", names(comparions.all)[j], "\n", sep = "")
          }

          results.meta <- meta.analyze(
            outcome, meditation.type.all, m.data.list.no.T2.T3, preferred.scale = preferred.scale, meta.df.list = F, comparison.list = list(comparions.all[[i]], comparions.all[[j]]),
            split.subgroups = F, return.data = "results.meta", silent = silent, filter.forest..funnel.vec = filter.forest..funnel.vec, comparisons.skip.list = comparisons.skip.list
          )

          if (results.meta$k > 0){
            # correct polarities of scales so high means more resilient
            pol <- outcome.direction.df[outcome.direction.df[, "Outcome"] == outcome, "High.or.low.means.resilient"]
            sign. <- ifelse(pol == "v", -1, 1)

            # append SMD data to net.df
            new.df <- data.frame(
              studlab = results.meta$studlab,
              outcome = outcome,
              TE = sign. * results.meta$TE,
              seTE = results.meta$seTE,
              treat1 = names(comparions.all[i]),
              treat2 = names(comparions.all[j])
            )
            net.df <- rbind(net.df, new.df)
          }
        }
      }
    }
    
    # add number to label if same intervention comparision is present more than one time in a study
    # in other words, pretend this intervention comparision is from another study
      # this approach leads to a lack of standard error adjustment that would have been done for multi-arm studies:
      # "Multi-Arm Studies
      # Usually, we have a number of multi-arm studies (i.e. studies with more than two
      # treatment groups) to include in our network meta-analysis. We can do this most
      # easily by including each multi-arm study in the dataset as a series of two-arm
      # comparisons. However, the standard error of each two-arm comparison from a
      # multi-arm study needs to be adjusted to reflect the fact that comparisons within
      # multi-arm studies are correlated." (Schwarzer, G. et al. (2015) Meta-Analysis with R. p. 192)
    
    if (nrow(net.df) > 1){
      dupls.i <- which(duplicated(net.df[,c("studlab", "treat1", "treat2")]))
      stud.labs.dupl <- unique(net.df[,"studlab"])

      for (stud.lab.dupl in stud.labs.dupl){
        i <- 1
        for (dupl.i in dupls.i){
          if (net.df[dupl.i, "studlab"] == stud.lab.dupl){
            net.df[dupl.i, "studlab"] <- paste(stud.lab.dupl, " #", i, "#", sep = "")
            i <- i + 1
          }
        }
      }
    }
  } else if (!is.data.frame(net.df) & is.logical(net.res)){
    cat("\nerror in net.meta.analyze(): set parameter net.df to a data frame of the format required for netmeta(), got class", class(net.df), "instead\n")
  }
  
  if (is.logical(net.res)){
    if (nrow(net.df) > 1){
      net.res <- netmeta(
        TE = TE,
        seTE = seTE,
        treat1 = treat1,
        treat2 = treat2,
        studlab = studlab,
        data = net.df,
        sm = "SMD",
        common = !random,
        random = random,
        reference.group = ifelse(
          reference.group %in% net.df$treat1 | reference.group %in% net.df$treat2,
          reference.group,
          "meditation (exclusive)"
        ),
        details.chkmultiarm = details.chkmultiarm,
        sep.trts = " vs. ",
        tol.multiarm = tol.multiarm
      )
      
      if (!(reference.group %in% net.df$treat1 | reference.group %in% net.df$treat2)){
        cat(
          "warning in net.meta.analyze() for calculation of net.res: parameter reference.group (", reference.group, ") was set to 'meditation (exclusive)' as ",
          reference.group, " was not present\n", sep = ""
        )
      }
      
    } else {
      print("error in net.meta.analyze(): calculation of net.res not possible as nrow(net.df) is not above 1 --> only 1 or 0 treatment comparisons are present")
    }
  } else if (class(net.res) != "netmeta"){
    cat("\nerror in net.meta.analyze(): set parameter net.res to an variable of class netmeta, got class", class(net.res), "instead\n")
  }
  
  if (plot.netgraph == T){
    if (is.logical(net.res)){
      print("error in net.meta.analyze(): plot of netgraph not possible as nrow(net.df) is not above 1 --> only 1 or 0 treatment comparisons are present")
    } else {
      netgraph(
        net.res,
        iterate=F, # start="prcomp", 
        col="darkgray", cex=1.5, multiarm=F,
        points=F, col.points="blue", cex.points=3,
        number.of.studies = T, pos.number.of.studies = 0.42
      )
    }
  }
  
  if (plot.direct.evidence == T){
    if (is.logical(net.res)){
      print("error in net.meta.analyze(): plot of direct.evidence.plot not possible as nrow(net.df) is not above 1 --> only 1 or 0 treatment comparisons are present")
    } else {
      d.evidence <- direct.evidence.plot(net.res, random = T)
      plot(d.evidence)
    }
  }
  
  if (plot.forest == T){
    if (is.logical(net.res)){
      print("error in net.meta.analyze(): plot of network forest plot not possible as nrow(net.df) is not above 1 --> only 1 or 0 treatment comparisons are present")
    } else {
      forest(
        net.res,
        reference.group = ifelse(
          reference.group %in% net.res$treat1 | reference.group %in% net.res$treat2,
          reference.group,
          "meditation (exclusive)"
        ),
        sortvar = TE,
        drop.reference.group = TRUE,
        label.left = "may reduce resilience",
        label.right = "may improve resilience"
  #       smlab = paste(  # headline of forest plot in the center
  #         "Therapy Formats vs. Care As Usual \n",
  #         "(Depressive Symptoms)"),
      #        xlim = c(-1.3, 0.5),
      #        labels = long.labels
      )
      
      if (!(reference.group %in% net.res$treat1 | reference.group %in% net.res$treat2)){
        cat(
          "warning in net.meta.analyze() for forest plot: parameter reference.group (", reference.group, ") was set to 'meditation (exclusive)' as ",
          reference.group, " was not present\n", sep = ""
        )
      }
    }
  }
  
  if (plot.netheat){
    if (is.logical(net.res)){
      print("error in net.meta.analyze(): plot of netheat plot not possible as nrow(net.df) is not above 1 --> only 1 or 0 treatment comparisons are present")
    } else {
      netheat(net.res, nchar.trts = 3)
    }
  }
  
  if (return.data != F){
    if (return.data == "net.df"){
      return(net.df)
    } else if (return.data == "net.res"){
      return(net.res)
    } else {
      print("error in net.meta.analyze(): set parameter return.data to 'net.df' or 'net.res'")
    }
  }
}

# %% [markdown] heading_collapsed=true hidden=true
# ## Scales used in oucomes of Summary of Findings table

# %% hidden=true vscode={"languageId": "r"}
# # find out scales used for the five most important outcomes
# all.measures.used <- c()
# for (outcome in present.outcomes.primary){
#   studlabs <- print.meta.results(
#     outcome, preferred.scale = ifelse(outcome == "Stress", "DASS", F),
#     regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
#     split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
#     return.data = "results.meta"
#   )$studlab
  
#   # get list of vectors specifying which oucome number is of interest per study 
#   outcome.no.list <- list()
#   i <- 1
#   for (studlab in studlabs){
#     j <- 1
#     outcome.no.vec <- c()
#     for (outcome.num in 1:7){
#       if (!is.na(outcome.names.df[studlab, outcome.num])){
#         if(outcome.names.df[studlab, outcome.num] == outcome){
# #           print(outcome.num)
# #           print(outcome.no.vec)
#           outcome.no.vec[j] <- outcome.num
# #           print(outcome.no.vec)
#           j <- j + 1
#         }
#       }
#     }
#     outcome.no.list[i] <- outcome.no.vec
#     i <- i + 1
#   }
#   names(outcome.no.list) <- studlabs
  
#   # get vector of unique measures per outcome
#   outcome.unique.measures.vec <- c()
#   i <- 1
#   for (studlab in studlabs){
#     outcome.unique.measures.vec[i] <- outcome.measures.df.list[[studlab]][
#       outcome.no.list[[studlab]],
#       "Measures.Name"
#     ]
#     i <- i + 1
#   }
#   outcome.unique.measures.vec <- unique(outcome.unique.measures.vec)
#   all.measures.used <- append(all.measures.used, outcome.unique.measures.vec)
#   print(outcome.unique.measures.vec)
#   cat("\n\n")
# }
# sort(all.measures.used)

# %% [markdown] heading_collapsed=true
# # Inter-rater reliability

# %% hidden=true vscode={"languageId": "r"}
irr.ft <- read.csv("Inter-Rater-Reliability_Paper_Full_Text_2023_12_10.csv")
irr.ta <- read.csv("Inter-Rater-Reliability_Paper_Title_Abstract_2023_12_10.csv")
irr.ta[, c("Reviewer.A", "Reviewer.B", "Proportionate.Agreement", "Cohen.s.Kappa")]
irr.ft[, c("Reviewer.A", "Reviewer.B", "Proportionate.Agreement", "Cohen.s.Kappa")]

# %% [markdown] heading_collapsed=true
# # Study Characteristics

# %% hidden=true vscode={"languageId": "r"}
# install.packages(c("stringr", "maps", "mapproj"))  # for splitting strings & map plot
library(stringr)
library(maps)
library(ggplot2)
library(mapproj)
library(colorspace)  # creating color palette

# %% [markdown] heading_collapsed=true hidden=true
# ## Create sorted vector of present outcomes

# %% hidden=true vscode={"languageId": "r"}
# define primary outcomes vector (with separation of state and trait)
# present.outcomes.primary <- c("Resilience Scale", "Anxiety (state)", "Anxiety (trait)", "Depression (trait)", "Stress", "Well-being or quality of life")

# without separation of trait and state
present.outcomes.primary <- c("Resilience Scale", "Anxiety", "Depression", "Stress", "Well-being")

# filter out primary outcomes
present.outcomes.secondary <- present.outcomes[
  -c(which(
    present.outcomes %in% present.outcomes.primary
  ))
]

# sort and append both together
present.outcomes.sorted <- append(present.outcomes.primary, sort(present.outcomes.secondary))
present.outcomes.sorted

# %% [markdown] heading_collapsed=true hidden=true
# ## Participants

# %% hidden=true vscode={"languageId": "r"}
no.participants.df <- data.frame(results.descriptive.array[,"n","T1","Outcome.1","Scale.1",])
no.participants.df["Sum",] <- colSums(no.participants.df, na.rm = TRUE)
no.participants.df["Sum", no.participants.df["Sum",] < 0] <- NA  # negative values indicate nm.placholder
no.participants.df["Sum", "Gupta.2020"] <- 94
colnames(no.participants.df) <- study.names  # reset column names as there were points instead of spaces before
no.participants.df

# %% hidden=true vscode={"languageId": "r"}
percent.symb.female <- rep(" %", study.no)
percent.symb.female[
  is.na(female.perc.used) |
  female.perc.used == nm.placeholder
] <- ""

percent.symb.male <- rep(" %", study.no)
percent.symb.male[
  is.na(male.perc.used) |
  male.perc.used == nm.placeholder
] <- ""

percent.symb.diverse <- rep(" %", study.no)
percent.symb.diverse[
  is.na(diverse.perc.used) |
  diverse.perc.used == nm.placeholder
] <- ""


participants.info <- paste(
  "N = ", no.participants.df["Sum",], "\n",
  "Mean age [years] = ", round(population.characteristics.array["Mean.Age", "Over.All", "T1",], digits = 2), "\n",
  "Female percentage = ", round(female.perc.used, digits = 2), percent.symb.female, "\n",
  "Male percentage = ", round(male.perc.used, digits = 2), percent.symb.male, "\n",
  "Diverse gender percentage = ", round(diverse.perc.used, digits = 2), percent.symb.diverse,
  sep = ""
)
cat(participants.info)

# %% [markdown] heading_collapsed=true hidden=true
# ## Intervention Description

# %% hidden=true vscode={"languageId": "r"}
count.int <- function(study, nm.placeholder){
  int.count <- 0
  for (int in 1:6){
    if(!(
      is.na(intervention.comparisons.df.list[[study]][int, "Name"]) |
      intervention.comparisons.df.list[[study]][int, "Name"] %in%
      c("NA", nm.placeholder, as.character(nm.placeholder))
    )){
      int.count <- int.count + 1
    }
  }
  return(int.count)
}

# %% hidden=true vscode={"languageId": "r"}
check.na.int.comp <- function(int, study, nm.placeholder){
  if (
      is.na(intervention.comparisons.df.list[[study]][int, "Name"]) |
      intervention.comparisons.df.list[[study]][int, "Name"] %in%
      c("NA", nm.placeholder, as.character(nm.placeholder))
  ){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# %% hidden=true vscode={"languageId": "r"}
meditation.techniques.df

# %% hidden=true vscode={"languageId": "r"}
intervention.comparisons.df.list

# %% hidden=true vscode={"languageId": "r"}
int.info <- c()

for (study in 1:study.no){

  int.vec.list <- list(c(), c(), c(), c(), c())
  
  names(int.vec.list) <- c("int1", "int2", "int3", "int4 or passiv control", "int5")
  
  int.no <- 1
  for (int in 1:length(int.vec.list)){
    
    int.name <- intervention.comparisons.df.list[[study]][int, "Name"]

    if (!check.na.int.comp(int, study, nm.placeholder)){
      pas.cont <- if (meditation.techniques.df[study, int] %in% c("No Intervention", "Wait-List Control")){
        T
      } else {
        F
      }
      int.vec.list[[int]] <- append(
        int.vec.list[[int]],
        paste(
          if(count.int(study, nm.placeholder) > 1){
            if (int == 4 & pas.cont){
              paste("Passive Control:\n", sep = "")
            } else {
              paste("Intervention ", int.no, ":\n", sep = "")
            }
          } else{""},
          
          if (pas.cont){
            meditation.techniques.df[study, int]
          } else {
            intervention.comparisons.df.list[[study]][int, "Short.Description"]
          },
          sep = ""
        )
      )
      int.no <- int.no + 1
    } else{
      int.vec.list[[int]] <- append(int.vec.list[[int]], c(""))
    }
  }
  
  
  if (T %in% (meditation.techniques.df[study,] %in% c("No Intervention", "Wait-List Control"))){
    order.int.4.5 <- c(5, 4)
  } else {
    order.int.4.5 <- c(4, 5)
  }
  
  int.info.per.study <- paste(
    int.vec.list[[1]], if(!check.na.int.comp(1, study, nm.placeholder)){"\n\n"}else{""},
    int.vec.list[[2]], if(!check.na.int.comp(2, study, nm.placeholder)){"\n\n"}else{""},
    int.vec.list[[3]], if(!check.na.int.comp(3, study, nm.placeholder)){"\n\n"}else{""},
    int.vec.list[[order.int.4.5[1]]], if(!check.na.int.comp(order.int.4.5[1], study, nm.placeholder)){"\n\n"}else{""},
    int.vec.list[[order.int.4.5[2]]],
    sep = ""
  )
  
  int.info <- append(int.info, int.info.per.study)
}

length(int.info)
cat(int.info)

# %% [markdown] heading_collapsed=true hidden=true
# ## Intervention Characteristics

# %% vscode={"languageId": "r"}
intervention.comparisons.df.list[[1]]

# %% hidden=true vscode={"languageId": "r"}
int.char.info <- c(rep(c(1), study.no))

for (study in 1:study.no){
  ints <- which(!(is.na.or.nm(meditation.techniques.df[study,]) | meditation.techniques.df[study,] == "None"))
  int.char.info.per.int <- c(rep(c(""), length(ints)))
  int.no <- 1
  for (int in ints){
    pas.cont <- if (meditation.techniques.df[study, int] %in% c("No Intervention", "Wait-List Control")){
      T
    } else {
      F
    }
    # cat(int.char.info.per.int[int.no], "---------------->>>>>-----------------\n")
    int.char.info.per.int[int.no] <- paste(
      if (int == 4 & pas.cont){
        "Passive Control:\n"
      } else {
        paste("Intervention ", int.no, ":\n", sep = "")
      },
      if(!check.na.int.comp(int, study, nm.placeholder)){
        int.dur <- intervention.comparisons.df.list[[study]][int, "Total.Duration.in.Days"]
        ses.dur <- intervention.comparisons.df.list[[study]][int, "Sessions.Duration.in.minutes"]
        freq <- intervention.comparisons.df.list[[study]][int, "Frequency.in.times.per.week"]
        del.mod <- intervention.comparisons.df.list[[study]][int, "Delivery.Mode"]
        med.tech <- gsub("Other: ", "", meditation.techniques.df[study, int])
        med.typ <- intervention.comparisons.df.list[[study]][int, "Meditation.Type"]
        
        paste(
          "Intervention's duration [days]: ", if(!int.dur %in% c(nm.placeholder, as.character(nm.placeholder))){int.dur}else{"NA"}, "\n",
          "Single Session's duration [minutes]: ", if(!ses.dur %in% c(nm.placeholder, as.character(nm.placeholder))){ses.dur}else{"NA"}, "\n",
          "Frequency [sessions per week]: ", if(!freq %in% c(nm.placeholder, as.character(nm.placeholder))){freq}else{"NA"}, "\n",
          "Delivery mode: ", if(!del.mod %in% c(nm.placeholder, as.character(nm.placeholder))){del.mod}else{"NA"}, "\n",
          "Meditation Technique: ", med.tech, "\n",
          "Meditation category: ", if(is.null(med.typ) || !med.typ %in% c(nm.placeholder, as.character(nm.placeholder))){med.typ}else{"NA"},
          sep = ""
        )
      } else {""},
      collapse = ""
    )
    int.no <- int.no + 1
    #cat(int.char.info.per.int[int.no], "------------------++++++---------------\n")
  }
  #cat(length(int.char.info.per.int), "-------------------------\n")
  if (length(int.char.info.per.int) == 2){
    int.char.info[study] <- paste(
      int.char.info.per.int[1], "\n\n",
      int.char.info.per.int[2], "\n\n",
      sep = ""
    )
  } else if (length(int.char.info.per.int) == 3){
    int.char.info[study] <- paste(
      int.char.info.per.int[1], "\n\n",
      int.char.info.per.int[2], "\n\n",
      int.char.info.per.int[3], "\n\n",
      sep = ""
    )
  } else if (length(int.char.info.per.int) == 4){
    int.char.info[study] <- paste(
      int.char.info.per.int[1], "\n\n",
      int.char.info.per.int[2], "\n\n",
      int.char.info.per.int[3], "\n\n",
      int.char.info.per.int[4], "\n\n",
      sep = ""
    )
  } else if (length(int.char.info.per.int) == 5){
    int.char.info[study] <- paste(
      int.char.info.per.int[1], "\n\n",
      int.char.info.per.int[2], "\n\n",
      int.char.info.per.int[3], "\n\n",
      int.char.info.per.int[4], "\n\n",
      int.char.info.per.int[5], "\n\n",
      sep = ""
    )
  }

}

cat(int.char.info)

# %% [markdown] heading_collapsed=true hidden=true
# ## Meditation Techniques

# %% hidden=true vscode={"languageId": "r"}
check.na.med.tech <- function(int, study, nm.placeholder){
  if (
      is.na(meditation.techniques.df[study, int]) |
      meditation.techniques.df[study, int] %in%
      c("NA", nm.placeholder, as.character(nm.placeholder), "None")
  ){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# %% hidden=true vscode={"languageId": "r"}
med.tech.info <- c()

for (study in 1:study.no){

  med.vec.list <- list(c(), c(), c(), c(), c(), c())
  
  names(med.vec.list) <- c("int1", "int2", "int3", "int4", "int5", "int6")
  
  int.no <- 1
  for (int in 1:6){

    if (!check.na.med.tech(int, study, nm.placeholder)){
      med.vec.list[[int]] <- append(
        med.vec.list[[int]],
        paste(
          if(count.int(study, nm.placeholder) > 1){
            paste("Intervention ", int.no, ":", "\n", sep = "")
          } else {""},
          meditation.techniques.df[study, int],
          sep = ""
        )
      )
      int.no <- int.no + 1
    } else{
      med.vec.list[[int]] <- append(med.vec.list[[int]], c(""))
    }
  }
  
  med.tech.info.per.study <- paste(
    med.vec.list[[1]], if(!check.na.med.tech(1, study, nm.placeholder)){"\n\n"}else{""},
    med.vec.list[[2]], if(!check.na.med.tech(2, study, nm.placeholder)){"\n\n"}else{""},
    med.vec.list[[3]], if(!check.na.med.tech(3, study, nm.placeholder)){"\n\n"}else{""},
    med.vec.list[[4]], if(!check.na.med.tech(4, study, nm.placeholder)){"\n\n"}else{""},
    med.vec.list[[5]], if(!check.na.med.tech(5, study, nm.placeholder)){"\n\n"}else{""},
    med.vec.list[[6]], if(!check.na.med.tech(6, study, nm.placeholder)){"\n\n"}else{""},
    sep = ""
  )
  
  med.tech.info <- append(med.tech.info, med.tech.info.per.study)
}

length(med.tech.info)
cat(med.tech.info)

# %% [markdown] heading_collapsed=true hidden=true
# ## Outcomes and Effectiveness

# %% hidden=true vscode={"languageId": "r"}
intervention.comparisons.df.list[[1]]

# %% hidden=true vscode={"languageId": "r"}
check.na.otucome <- function(outcome, study, nm.placeholder){
  if (
      is.na(outcome.names.df[study, outcome]) |
      outcome.names.df[study, outcome] %in%
      c("NA", nm.placeholder, as.character(nm.placeholder), "Other: ")
  ){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# %% hidden=true vscode={"languageId": "r"}
check.na.res.quant <- function(outcome, int, scale, study, nm.placeholder){
  if (
      is.na(results.quantitative.array[outcome, 'P.Value', "T1", int, scale, study]) |
      (results.quantitative.array[outcome, 'P.Value', "T1", int, scale, study] == nm.placeholder)
  ){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# %% hidden=true vscode={"languageId": "r"}
check.na.scales <- function(outcome, study, nm.placeholder){
  if (
      is.na(outcome.measures.df.list[[study]][outcome, "Measures.Name"]) |
      outcome.measures.df.list[[study]][outcome, "Measures.Name"] %in%
      c("NA", nm.placeholder, as.character(nm.placeholder))
  ){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# %% hidden=true vscode={"languageId": "r"}
outcome.names.df

# %% hidden=true vscode={"languageId": "r"}
outcome.measures.df.list

# %% hidden=true vscode={"languageId": "r"}
outc.eff.info <- c(rep(c(""), study.no))

for (study in 1:study.no){
  
  outc.no <- 1
  for (outcome in 1:7){
    
    # get p-values of between subject statitics of T1 (post-test) per outcome and intervention
    p.val.list.per.outc <- list(1, 1, 1)
    names(p.val.list.per.outc) <- c('Int1.X.Int4', 'Int2.X.Int4', 'Int3.X.Int4')
    
    p.val.dimnames <- c('Int1.X.Int4', 'Int2.X.Int4', 'Int3.X.Int4')
    for (int in p.val.dimnames){
      placeholder <- "space"
      scale1.val <- placeholder
      scale2.val <- placeholder
      for (scale in 1:2){
        if (!check.na.res.quant(outcome, int, scale, study, nm.placeholder)){
          if (scale == 1){
            scale1.val <- results.quantitative.array[outcome, 'P.Value', "T1", int, scale, study]
            
            p.val.list.per.outc[[int]] <- scale1.val
            
          } else if (scale == 2){
            scale2.val <- results.quantitative.array[outcome, 'P.Value', "T1", int, scale, study]
            
            if (scale1.val == placeholder & scale2.val != placeholder){
              scale1.val <- scale2.val
            }            
            
            if (scale1.val != scale2.val){
              cat(
                "\n\np-values of scale 1 and scale 2 are not equal --> differnt evidence of effectiveness?\n",
                "scale 1: ", scale1.val, "\n",
                "scale 2: ", scale2.val, "\n\n",
                sep = ""
              )
            }
          }
        } else {
          if (scale == 1){
            p.val.list.per.outc[[int]] <- 0.11  # just a value >= 0.05 (see code below)
          }
        }
      }
    }
    
    if (!check.na.otucome(outcome, study, nm.placeholder)){
      outc.eff.info[study] <- paste(
        outc.eff.info[study],
        outcome.names.df[study, outcome], " (",
        outcome.measures.df.list[[study]][outcome, "Measures.Name"], ")",
        if(!check.na.int.comp(1, study, nm.placeholder)){
          paste(
            if(count.int(study, nm.placeholder) > 1){"\nIntervention 1: "}else{": "},
            if(p.val.list.per.outc[[1]] <= 0.05){"Y"}else{"N"},
            "\n"
          )
        } else {""},
        if(!check.na.int.comp(2, study, nm.placeholder)){
          paste(
            if(count.int(study, nm.placeholder) > 1){
              if(check.na.int.comp(1, study, nm.placeholder)){"\nIntervention 1: "}else{"\nIntervention 2: "}
            } else{": "},
            if(p.val.list.per.outc[[2]] <= 0.05){"Y"}else{"N"},
            "\n"
          )
        } else {""},
        if(!check.na.int.comp(3, study, nm.placeholder)){
          paste(
            if(count.int(study, nm.placeholder) > 1){
              if(check.na.int.comp(1, study, nm.placeholder) & check.na.int.comp(2, study, nm.placeholder)){"\nIntervention 1: "}else if(check.na.int.comp(1, study, nm.placeholder)){"\nIntervention 2: "}else{"\nIntervention 3: "}
            } else{": "},
            if(p.val.list.per.outc[[3]] <= 0.05){"Y"}else{"N"},
            "\n"
          )
        } else {""}, "\n",
        sep = ""
      )
      outc.no <- outc.no + 1
    } else{
      outc.eff.info[study] <- paste(outc.eff.info[study], c(""))
    }
  }
}

length(outc.eff.info)
cat(outc.eff.info)

# %% [markdown] heading_collapsed=true hidden=true
# ## Authors Key Conclusion

# %% hidden=true vscode={"languageId": "r"}
key.conlcusions <- one.D.info.df[,'Key.Conclusions.of.Study.Authors']
key.conlcusions

# %% [markdown] heading_collapsed=true hidden=true
# ## Insert Values in Table

# %% hidden=true vscode={"languageId": "r"}
study.char.col.names <- c(
  "Participants", "Intervention Description(s)", "Intervention Characteristics", "Control",
  "Included Outcomes (Scale)",
  "Authors' Key Conlclusion"
)

study.char.df <- data.frame(matrix(
  ncol = length(study.char.col.names),
  nrow = study.no
))

dimnames(study.char.df) <- list(
  study.names,
  study.char.col.names
)

study.char.df$Participants <- participants.info
study.char.df$`Intervention Description(s)` <- int.info  # only included interventions

# %% hidden=true vscode={"languageId": "r"}
study.char.col.names <- c(
  "Participants", "Intervention Description(s)", "Intervention Characteristics",
  "Included Outcomes (Scale)",
  "Authors' Key Conlclusion"
)

study.char.df <- data.frame(matrix(
  ncol = length(study.char.col.names),
  nrow = study.no
))

dimnames(study.char.df) <- list(
  study.names,
  study.char.col.names
)

study.char.df$Participants <- participants.info

study.char.df$`Intervention Description(s)` <- int.info  # only included interventions

study.char.df$`Included Outcomes (Scale)` <- outc.eff.info  # evidence of effectiveness was not exctracted by p values but by Authors' Key Conlclusion

study.char.df$`Intervention Characteristics` <- int.char.info

study.char.df$`Authors' Key Conlclusion` <- key.conlcusions

study.char.df[sort(rownames(study.char.df)),]

# %% hidden=true vscode={"languageId": "r"}
# write.csv(study.char.df, "study.char.df.csv")

# %% hidden=true vscode={"languageId": "r"}
# get list of intervention names
stud.n.vec <- c()
i <- 1
for (study in study.names){
  t.stud.df <- intervention.comparisons.df.list[[study]]
  t.int.names <- paste(t.stud.df$Name, collapse = ", ")
  t.int.names <- gsub("NA", "", t.int.names)
  t.stud.apps <- intervention.comparisons.df.list[[study]] |>
    select(Meditation.App) |>
    filter(Meditation.App != "NA")
  t.int.names <- paste(t.int.names, "| Apps:", t.stud.apps)
  stud.n.vec[i] <- t.int.names
  t.stud.apps
  i <- i + 1
}
stud.n.vec

# %% hidden=true vscode={"languageId": "r"}
data.frame(studyname = names(intervention.comparisons.df.list), intnames = stud.n.vec) |> arrange(studyname)

# %% [markdown] heading_collapsed=true hidden=true
# ## Summary/descriptive statistics

# %% [markdown] heading_collapsed=true hidden=true
# ### Participants

# %% hidden=true vscode={"languageId": "r"}
# Function | Get participant descriptives by studies
get.part.desc.by.stud <- function(study.names.vec){
  # n
  n.mean.total <- mean(unlist(no.participants.df["Sum", study.names.vec]), na.rm = T)
  no.participants.df.n.total.imputed <- no.participants.df
  no.participants.df.n.total.imputed["Sum", which(is.na.or.nm(no.participants.df["Sum",]))] <- n.mean.total
  n.total <- sum(no.participants.df.n.total.imputed["Sum", study.names.vec], na.rm = T)
  no.participants.df["Sum", study.names.vec][no.participants.df["Sum", study.names.vec] == 0] <- NA  # studies with 0 paarcticipants should have NA instead
  n.range <- paste(
    as.character(min(no.participants.df["Sum", study.names.vec], na.rm = T)),
    as.character(max(no.participants.df["Sum", study.names.vec], na.rm = T)),
    sep = ", "
  )
  n.sd.total <- sd(no.participants.df["Sum", study.names.vec], na.rm = T)

  # age
  age.range <- paste(
    as.character(round(min(population.characteristics.array["Mean.Age", "Over.All", "T1", study.names.vec], na.rm = T), digits = 2)),
    as.character(round(max(population.characteristics.array["Mean.Age", "Over.All", "T1", study.names.vec], na.rm = T), digits = 2)),
    sep = ", "
  )
  age.mean.total <- round(sum(no.participants.df.n.total.imputed["Sum", study.names.vec] * population.characteristics.array["Mean.Age", "Over.All", "T1", study.names.vec], na.rm = T) / n.total, digits = 2)
  age.sd.total <- sd(population.characteristics.array["Mean.Age", "Over.All", "T1", study.names.vec], na.rm = T)

  # gender
  ## male
  male.range <- paste(
    as.character(min(round(population.characteristics.array["Males.Percent", "Over.All", "T1", study.names.vec], digits = 2), na.rm = T)),
    as.character(max(round(population.characteristics.array["Males.Percent", "Over.All", "T1", study.names.vec], digits = 2), na.rm = T)),
    sep = ", "
  )
  
  # get df with reported genders
  no.participants.reported.gernder.vec <- no.participants.df[
    "Sum",
    which(
      !is.na(population.characteristics.array["Males.Percent", "Over.All", "T1", study.names.vec]) |
      population.characteristics.array["Males.Percent", "Over.All", "T1", study.names.vec] %in% c("NA", nm.placeholder, as.character(nm.placeholder))
    )
  ]
  c.names.gender <- colnames(no.participants.reported.gernder.vec)  # get its column names
  no.participants.reported.gernder.vec <- no.participants.reported.gernder.vec[which(c.names.gender %in% study.names.vec)]  # filter by study.names.vec
  
  n.gender.present.total <- sum(
    no.participants.reported.gernder.vec,
    na.rm = T
  )  # total n of studies that has reported gender
  
  male.vec <- population.characteristics.array["Males.Percent", "Over.All", "T1", study.names.vec][!is.na.or.nm(population.characteristics.array["Males.Percent", "Over.All", "T1", study.names.vec])]
  male.mean.total <- round(mean(male.vec), digits = 2)
  male.sd.total <- round(sd(male.vec), digits = 2)

  ## female
  female.range <- paste(
    as.character(min(round(population.characteristics.array["Females.Percent", "Over.All", "T1", study.names.vec], digits = 2), na.rm = T)),
    as.character(max(round(population.characteristics.array["Females.Percent", "Over.All", "T1", study.names.vec], digits = 2), na.rm = T)),
    sep = ", "
  )
  female.vec <- population.characteristics.array["Females.Percent", "Over.All", "T1", study.names.vec][!is.na.or.nm(population.characteristics.array["Females.Percent", "Over.All", "T1", study.names.vec])]
  female.mean.total <- round(mean(female.vec), digits = 2)
  female.sd.total <- round(sd(female.vec), digits = 2)

  ## diverse
  diverse.range <- paste(
    as.character(min(round(population.characteristics.array["Diverse.Percent", "Over.All", "T1", study.names.vec], digits = 2), na.rm = T)),
    as.character(max(round(population.characteristics.array["Diverse.Percent", "Over.All", "T1", study.names.vec], digits = 2), na.rm = T)),
    sep = ", "
  )
  diverse.vec <- population.characteristics.array["Diverse.Percent", "Over.All", "T1", study.names.vec][!is.na.or.nm(population.characteristics.array["Diverse.Percent", "Over.All", "T1", study.names.vec])]
  diverse.mean.total <- round(mean(diverse.vec), digits = 2)
  diverse.sd.total <- round(sd(diverse.vec), digits = 2)

  # put data in data frame
  desc.stat.participants <- data.frame(
    Mean = round(c(n.mean.total, age.mean.total, male.mean.total, female.mean.total, diverse.mean.total), digits = 2),
    SD = round(c(n.sd.total, age.sd.total, male.sd.total, female.sd.total, diverse.sd.total), digits = 2),
    Range = c(n.range, age.range, male.range, female.range, diverse.range),
    row.names = c("Number of participants", "Mean age", "Male percentage", "Female percentage", "Diverse gender percentage")
  )
  desc.stat.participants
}


# %% hidden=true vscode={"languageId": "r"}
# participant characteristics of all studies
get.part.desc.by.stud(study.names)

# %% hidden=true vscode={"languageId": "r"}
n.mean.total <- mean(unlist(no.participants.df["Sum", ]), na.rm = T)
no.participants.df.n.total.imputed <- no.participants.df
no.participants.df.n.total.imputed["Sum", which(is.na.or.nm(no.participants.df["Sum",]))] <- n.mean.total %>% round(digits = 0)
n.total <- sum(no.participants.df.n.total.imputed["Sum", ], na.rm = T)
n.total

# %% vscode={"languageId": "r"}
no.participants.df.n.total.imputed

# %% code_folding=[] hidden=true vscode={"languageId": "r"}
# write.csv(study.char.df, "desc.stat.participants.csv")

# %% [markdown] heading_collapsed=true hidden=true
# #### Country distribution

# %% hidden=true vscode={"languageId": "r"}
# cut yout "Other: "
one.D.info.df[, "Country.in.which.the.study.conducted"] <- gsub("Other: ", "",one.D.info.df[, "Country.in.which.the.study.conducted"])
one.D.info.df[, "Country.in.which.the.study.conducted"]

# %% hidden=true vscode={"languageId": "r"}
# Count countries
country.df <- data.frame(region = one.D.info.df[, "Country.in.which.the.study.conducted"])
country.df <- data.frame(table(country.df))
country.df$region <- as.character(country.df$region)
country.df

# %% hidden=true vscode={"languageId": "r"}
# set plot size
options(repr.plot.width = 10, repr.plot.height = 4, repr.plot.res = 230)

# %% hidden=true vscode={"languageId": "r"}
WorldData <- map_data('world')

ggplot() +
  geom_map(
    data = WorldData, map = WorldData,
    aes(x = long, y = lat, group = group, map_id = region),
    fill = "white", colour = "#7f7f7f", size=0.5
  ) + 
  geom_map(
    data = country.df, map=WorldData,
    aes(fill=Freq, map_id=region),
    colour="#7f7f7f", size=0.5
  ) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  # scale_fill_continuous(type = "viridis") +  # alternative scale color
  scale_fill_continuous_diverging(
    palette = "Blue-Red 3",  # rev = TRUE,
    mid = 10
  ) +  # scale color
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="Legend [frequency]", x="", y="") +
  theme_bw()

# %% [markdown] heading_collapsed=true hidden=true
# ### Intervention

# %% [markdown] heading_collapsed=true hidden=true
# #### Distribution

# %% [markdown] heading_collapsed=true hidden=true
# ##### Meditation types

# %% hidden=true vscode={"languageId": "r"}
meditation.techniques.all.vec <- unlist(meditation.techniques.df,use.names = FALSE)  # get all elements of df
meditation.techniques.all.vec <- meditation.techniques.all.vec[which(!(
  is.na(meditation.techniques.all.vec) | meditation.techniques.all.vec %in% c("NA", "None", 'No Intervention', "Wait-List Control")
))]  # cut out NAs
meditation.techniques.all.vec <- sort(unlist(str_split(meditation.techniques.all.vec, "; "), use.names=FALSE))  # split by "; " and sort
meditation.techniques.freq.df <- data.frame(table(meditation.techniques.all.vec))  # create df of frequencies
colnames(meditation.techniques.freq.df) <- c("Meditation Techniques", "Frequency")

# recluster meditation types
other.count <- 0
open.monitoring.count <- 0
loving.kindness.count <- 0

for (med.tech in meditation.techniques.freq.df[, "Meditation Techniques"]){
  # others
  if (grepl(
    "Other: devotional readings to ponder, meditations taken from a widely used devotional book designed for Christian readers titled",
    med.tech
    ) | grepl(
    "Other: devotional readings to ponder, meditations taken from a widely used devotional book designed for Christian readers titled, forgiveness meditations",
    med.tech
    ) | grepl(
    "Other: mindfulness-based exercises through audio video or text files, grounding visualization, gratitude, imagining the life you want, finding meaning",
    med.tech
    ) | grepl(
    "Other: mindfulness toward sensations and experiences of the body, mindfulness toward the contents of the mind, skill of mental noting to label their mental contents, cultivating awareness and understanding of emotions as mental contents and the nonjudgmental stance toward emotions with a goal of optimizing the response to one’s emotions",
    med.tech
    ) | grepl(
    "Other: Readings reflecting meditative perspective",
    med.tech
  )){
    other.count <- other.count + 1
  
  # open monitoring
  } else if (grepl(
    "Other: \"awareness to any sensations in their body or in their immediate environment\", focus on \"thoughts and feelings\",  \"non-judgmental observation\"",
    med.tech
    ) | grepl(
    "Other: silent meditations with bells, personalized meditations with or without guided\nintro and bells",
    med.tech
  )){
    open.monitoring.count <- open.monitoring.count + 1
  
  # loving kindness
  } else if (grepl(
    "Other: focusing on meta-awareness, \"Mindfulness-based exercises [[]...[]] about happiness in life as the result of a benevolent attitude towards life and a positive relationship to oneself and others\", \"Methods of introspection, reflection and self-care\"",
    med.tech
  )){
    loving.kindness.count <- loving.kindness.count + 1
  }
}

# cut out others (others are not clustered properly)
meditation.techniques.freq.df <- meditation.techniques.freq.df[
  which(!grepl("Other", meditation.techniques.freq.df[,"Meditation Techniques"])),
]

# correct data types
meditation.techniques.freq.df[,"Meditation Techniques"] <- as.character(meditation.techniques.freq.df[,"Meditation Techniques"])
meditation.techniques.freq.df[,"Frequency"] <- as.double(meditation.techniques.freq.df[,"Frequency"])

# assign reclustered meditation types
meditation.techniques.freq.df[nrow(meditation.techniques.freq.df) + 1,] <- c("various techniques", other.count)  # add others' other.count

meditation.techniques.freq.df[which(meditation.techniques.freq.df[, "Meditation Techniques"] == "Open Monitoring Meditation"), "Frequency"] <-
as.double(meditation.techniques.freq.df[which(meditation.techniques.freq.df[, "Meditation Techniques"] == "Open Monitoring Meditation"), "Frequency"]) + open.monitoring.count

meditation.techniques.freq.df[which(meditation.techniques.freq.df[, "Meditation Techniques"] == "Loving-Kindness Meditation"), "Frequency"] <-
as.double(meditation.techniques.freq.df[which(meditation.techniques.freq.df[, "Meditation Techniques"] == "Loving-Kindness Meditation"), "Frequency"]) + loving.kindness.count

# sort by frequency
meditation.techniques.freq.df <- meditation.techniques.freq.df[order(-as.double(meditation.techniques.freq.df$Frequency)),]

rownames(meditation.techniques.freq.df) <- 1:nrow(meditation.techniques.freq.df)  # adjust indices
meditation.techniques.freq.df

# %% hidden=true vscode={"languageId": "r"}
# set plot size
options(repr.plot.width = 10, repr.plot.height = 9, repr.plot.res = 150)

# %% hidden=true vscode={"languageId": "r"}
# correct data type of $Frequency again (wasn't working above)
meditation.techniques.freq.df[,"Frequency"] <- as.double(meditation.techniques.freq.df[,"Frequency"])
meditation.techniques.freq.df

# %% hidden=true vscode={"languageId": "r"}
meditation.techniques.freq.df[1,"Meditation Techniques"] <- str_to_lower("Breathing Exercise")

# %% hidden=true vscode={"languageId": "r"}
# correct spelling
i <- 1
for (med.tech in meditation.techniques.freq.df[, "Meditation Techniques"]){
  meditation.techniques.freq.df[i,"Meditation Techniques"] <- str_to_lower(meditation.techniques.freq.df[i,"Meditation Techniques"])  # convert strings to lower case
  meditation.techniques.freq.df[i,"Meditation Techniques"] <- gsub("\"", "'", meditation.techniques.freq.df[i,"Meditation Techniques"])  # replace " with '
  i <- i + 1
}

meditation.techniques.freq.df <- replace_val_in_df(
  df = meditation.techniques.freq.df, col = 1,
  search = "transcendental meditation", replace = "transcendental meditation"
)

meditation.techniques.freq.df <- replace_val_in_df(
  df = meditation.techniques.freq.df, col = 1,
  search = "only 'mindfulness meditation' named", replace = "'mindfulness meditation'"
)

meditation.techniques.freq.df <- replace_val_in_df(
  df = meditation.techniques.freq.df, col = 1,
  search = "only 'focused attention meditation' named", replace = "'focused attention meditation'"
)

meditation.techniques.freq.df <- replace_val_in_df(
  df = meditation.techniques.freq.df, col = 1,
  search = "breathing exercise", replace = "breathing exercise"
)

meditation.techniques.freq.df

# %% hidden=true vscode={"languageId": "r"}
ggplot(meditation.techniques.freq.df, aes(x = reorder(`Meditation Techniques`, -Frequency), y = Frequency)) +
  geom_bar(stat="identity", fill = "cornflowerblue", alpha = 0.6) +
  theme(
    title = element_text(size = 17, face = "bold"),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(angle=75, vjust=1, hjust=1, size = 17),
    axis.text.y = element_text(size = 17)
  ) +
  labs(
    title = "Frequency of meditation techniques",
    x = "\nMeditation types",
    y = "Frequency\n"
  )
# others are not clustered properly --> statistic is biased

# %% [markdown] heading_collapsed=true hidden=true
# ##### Meditation families

# %% hidden=true vscode={"languageId": "r"}
# do same for meditation families
meditation.type.attentional.freq <- 0
meditation.type.constructive.freq <- 0
meditation.type.deconstructive.freq <- 0

for (col in 1:ncol(meditation.techniques.df)){
  for (row in 1:nrow(meditation.techniques.df)){
    med.tech <- meditation.techniques.df[row, col]
    
    if (med.tech %in% meditation.type.all.list[[1]]){
      meditation.type.attentional.freq <- meditation.type.attentional.freq + 1
    } else if (med.tech %in% meditation.type.all.list[[2]]){
      meditation.type.constructive.freq <- meditation.type.constructive.freq + 1
    } else if (med.tech %in% meditation.type.all.list[[3]]){
      meditation.type.attentional.freq <- meditation.type.attentional.freq + 1
      meditation.type.constructive.freq <- meditation.type.constructive.freq + 1
    } else if (med.tech %in% meditation.type.all.list[[4]]){
      meditation.type.attentional.freq <- meditation.type.attentional.freq + 1
      meditation.type.deconstructive.freq <- meditation.type.deconstructive.freq + 1
    } else if (med.tech %in% meditation.type.all.list[[5]]){
      meditation.type.attentional.freq <- meditation.type.attentional.freq + 1
      meditation.type.constructive.freq <- meditation.type.constructive.freq + 1
      meditation.type.deconstructive.freq <- meditation.type.deconstructive.freq + 1
    } else if (med.tech %in% meditation.type.all.list[[6]]){
      meditation.type.deconstructive.freq <- meditation.type.deconstructive.freq + 1
    }
  }
}

meditation.families.count.df <- data.frame(
  `Meditation Families` = c(
    "attentional familiy", "constructive family", "deconstructive familiy"
  ),
  Frequency = c(
    meditation.type.attentional.freq, meditation.type.constructive.freq, meditation.type.deconstructive.freq
  )
)
meditation.families.count.df <- meditation.families.count.df[order(meditation.families.count.df$Frequency),]
meditation.families.count.df

# %% hidden=true vscode={"languageId": "r"}
ggplot(meditation.families.count.df, aes(x = "", y = Frequency, fill=reorder(Meditation.Families, Frequency))) +
  geom_bar(stat="identity", color = "white", width=1, size = 0.9) +
  coord_polar("y", start=0) +
  geom_text(
    aes(label = Frequency),
    position = position_stack(vjust=0.5),
    col = c(rep("black", 2), "white"),
    size = c(rep(7, 2), 8)
  ) +  #text within pie pieces
  guides(fill = guide_legend(reverse = TRUE, override.aes = list(size = 10))) +  # reverse legend order
  labs(
    title = "          Frequency of meditation families",
    x = NULL, y = NULL, fill = "Meditation families"
  ) +  # labels
  theme_classic() +
  theme(
    title = element_text(size = 17, face = "bold"),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 17),
    legend.title = element_text(size = 17)
  ) +  # adjust texts
  scale_fill_brewer(palette="Blues")  # set color palette

# %% [markdown] heading_collapsed=true hidden=true
# ##### Intervention Characteristics

# %% hidden=true vscode={"languageId": "r"}
# Function | Get intervention characteristics by study
get.int.char.by.stud <- function(study.names.vec){
  # calculate mean sd and range of sessions' duration and frequency and programs duration
  sessions.durations.vec <- c()
  sessions.durations.report.count <- c()

  sessions.frequencies.vec <- c()
  sessions.frequencies.report.count <- c()

  programs.durations.vec <- c()
  programs.durations.report.count <- c()

  for (study in 1:study.no){
    
    # skip if study number is not presnet in study.names
    if (!study %in% which(study.names %in% study.names.vec)){
      next
    }
    
    df <- intervention.comparisons.df.list[[study]]

    sessions.durations.report.count <- append(
      sessions.durations.report.count,
      ifelse(
        F %in% is.na.or.nm(df[, "Sessions.Duration.in.minutes"]),
        1, 0
      )
    )

    sessions.frequencies.report.count <- append(
      sessions.frequencies.report.count,
      ifelse(
        F %in% is.na.or.nm(df[, "Frequency.in.times.per.week"]),
        1, 0
      )
    )

    programs.durations.report.count <- append(
      programs.durations.report.count,
      ifelse(
        F %in% is.na.or.nm(df[, "Total.Duration.in.Days"]),
        1, 0
      )
    )

    for (row in 1:nrow(df)){
      sessions.duration <- ifelse(is.na.or.nm(df[row, "Sessions.Duration.in.minutes"]), NA, df[row, "Sessions.Duration.in.minutes"])
      sessions.durations.vec <- append(sessions.durations.vec, sessions.duration)

      sessions.frequency <- ifelse(is.na.or.nm(df[row, "Frequency.in.times.per.week"]), NA, df[row, "Frequency.in.times.per.week"])
      sessions.frequencies.vec <- append(sessions.frequencies.vec, sessions.frequency)

      programs.duration <- ifelse(is.na.or.nm(df[row, "Total.Duration.in.Days"]), NA, df[row, "Total.Duration.in.Days"])
      programs.durations.vec <- append(programs.durations.vec, programs.duration)
    }
  }

  sessions.duration.mean.total <- round(mean(sessions.durations.vec, na.rm = T), digits = 2)
  sessions.duration.sd.total <- round(sd(sessions.durations.vec, na.rm = T), digits = 2)
  sessions.duration.range.total <- paste(as.character(c(min(sessions.durations.vec, na.rm = T), max(sessions.durations.vec, na.rm = T))), collapse = ", ")

  sessions.frequency.mean.total <- round(mean(sessions.frequencies.vec, na.rm = T), digits = 2)
  sessions.frequency.sd.total <- round(sd(sessions.frequencies.vec, na.rm = T), digits = 2)
  sessions.frequency.range.total <- paste(as.character(c(min(sessions.frequencies.vec, na.rm = T), max(sessions.frequencies.vec, na.rm = T))), collapse = ", ")

  programs.duration.mean.total <- round(mean(programs.durations.vec, na.rm = T), digits = 2)
  programs.duration.sd.total <- round(sd(programs.durations.vec, na.rm = T), digits = 2)
  programs.duration.range.total <- paste(as.character(c(min(programs.durations.vec, na.rm = T), max(programs.durations.vec, na.rm = T))), collapse = ", ")

  intervention.characteristics.descriptive.df <- data.frame(
    Mean = c(sessions.duration.mean.total, sessions.frequency.mean.total, programs.duration.mean.total),
    SD = c(sessions.duration.sd.total, sessions.frequency.sd.total, programs.duration.sd.total),
    Range = c(sessions.duration.range.total, sessions.frequency.range.total, programs.duration.range.total),
    `Percentage of reporting studies` = round(
      c(sum(sessions.durations.report.count), sum(sessions.frequencies.report.count), sum(programs.durations.report.count)) / length(study.names.vec) * 100,
      digits = 2
    ),
    row.names = list("Sessions' Duration [minutes]", "Sessions' Frequency [1/week]", "Interventions' Duration [days]")
  )
  intervention.characteristics.descriptive.df
}

# %% hidden=true vscode={"languageId": "r"}
# calculate mean sd and range of sessions' duration and frequency and programs duration
sessions.durations.vec <- c()
sessions.durations.report.count <- c()

sessions.frequencies.vec <- c()
sessions.frequencies.report.count <- c()

programs.durations.vec <- c()
programs.durations.report.count <- c()

for (study in 1:study.no){
  df <- intervention.comparisons.df.list[[study]]
  
  sessions.durations.report.count <- append(
    sessions.durations.report.count,
    ifelse(
      F %in% is.na.or.nm(df[, "Sessions.Duration.in.minutes"]),
      1, 0
    )
  )
  
  sessions.frequencies.report.count <- append(
    sessions.frequencies.report.count,
    ifelse(
      F %in% is.na.or.nm(df[, "Frequency.in.times.per.week"]),
      1, 0
    )
  )
  
  programs.durations.report.count <- append(
    programs.durations.report.count,
    ifelse(
      F %in% is.na.or.nm(df[, "Total.Duration.in.Days"]),
      1, 0
    )
  )
  
  for (row in 1:nrow(df)){
    sessions.duration <- ifelse(is.na.or.nm(df[row, "Sessions.Duration.in.minutes"]), NA, df[row, "Sessions.Duration.in.minutes"])
    sessions.durations.vec <- append(sessions.durations.vec, sessions.duration)
    
    sessions.frequency <- ifelse(is.na.or.nm(df[row, "Frequency.in.times.per.week"]), NA, df[row, "Frequency.in.times.per.week"])
    sessions.frequencies.vec <- append(sessions.frequencies.vec, sessions.frequency)
    
    programs.duration <- ifelse(is.na.or.nm(df[row, "Total.Duration.in.Days"]), NA, df[row, "Total.Duration.in.Days"])
    programs.durations.vec <- append(programs.durations.vec, programs.duration)
  }
}

sessions.duration.mean.total <- round(mean(sessions.durations.vec, na.rm = T), digits = 2)
sessions.duration.sd.total <- round(sd(sessions.durations.vec, na.rm = T), digits = 2)
sessions.duration.range.total <- paste(as.character(c(min(sessions.durations.vec, na.rm = T), max(sessions.durations.vec, na.rm = T))), collapse = ", ")

sessions.frequency.mean.total <- round(mean(sessions.frequencies.vec, na.rm = T), digits = 2)
sessions.frequency.sd.total <- round(sd(sessions.frequencies.vec, na.rm = T), digits = 2)
sessions.frequency.range.total <- paste(as.character(c(min(sessions.frequencies.vec, na.rm = T), max(sessions.frequencies.vec, na.rm = T))), collapse = ", ")

programs.duration.mean.total <- round(mean(programs.durations.vec, na.rm = T), digits = 2)
programs.duration.sd.total <- round(sd(programs.durations.vec, na.rm = T), digits = 2)
programs.duration.range.total <- paste(as.character(c(min(programs.durations.vec, na.rm = T), max(programs.durations.vec, na.rm = T))), collapse = ", ")

intervention.characteristics.descriptive.df <- data.frame(
  Mean = c(sessions.duration.mean.total, sessions.frequency.mean.total, programs.duration.mean.total),
  SD = c(sessions.duration.sd.total, sessions.frequency.sd.total, programs.duration.sd.total),
  Range = c(sessions.duration.range.total, sessions.frequency.range.total, programs.duration.range.total),
  `Percentage of reporting studies` = round(
    c(sum(sessions.durations.report.count), sum(sessions.frequencies.report.count), sum(programs.durations.report.count)) / study.no * 100,
    digits = 2
  ),
  row.names = list("Sessions' Duration [minutes]", "Sessions' Frequency [1/week]", "Interventions' Duration [days]")
)
intervention.characteristics.descriptive.df

# %% hidden=true vscode={"languageId": "r"}
length(sessions.durations.vec[!is.na.or.nm(sessions.durations.vec)])
length(sessions.frequencies.vec[!is.na.or.nm(sessions.frequencies.vec)])
length(programs.durations.vec[!is.na.or.nm(programs.durations.vec)])

# %% [markdown] heading_collapsed=true hidden=true
# ### Outcomes

# %% [markdown] heading_collapsed=true hidden=true
# ###### Outcome numbers

# %% hidden=true vscode={"languageId": "r"}
outcomes.no.ordered.freq.df <- outcomes.no.df[order(-outcomes.no.df$Freq),]
colnames(outcomes.no.ordered.freq.df) <- c("Outcome", "Frequency")

studies.suff.data.pas.vec <- c()
studies.suff.data.pas.num <- 0

studies.suff.data.act.vec <- c()
studies.suff.data.act.num <- 0

studies.suff.data.mix.vec <- c()
studies.suff.data.mix.num <- 0


i <- 1
for (outcome in outcomes.no.ordered.freq.df$Outcome){
  # get studies that supply sufficient data for outcomes to caclulate meta-analyses for comparison to passive controls
  uni.stud.p.outcome.pas <- unique(
    print.meta.results(
      outcome, preferred.scale = get.1st.preferred.scale(outcome),
      regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
      return.data = "meta.df.list"
    )[[1]][,1]
  )
  
  # get the same for all active control groups
  uni.stud.p.outcome.act <- unique(
    meta.analyze(
      outcome, meditation.type.all, m.data.list, preferred.scale = get.1st.preferred.scale(outcome), comparison.list = list(meditation.type.all, control.all.active),
      split.subgroups = F, return.data = "meta.df.list"
    )[[1]][,1]
  )
  
  uni.stud.p.outcome.pas.only <- uni.stud.p.outcome.pas[!uni.stud.p.outcome.pas %in% uni.stud.p.outcome.act]
  uni.stud.p.outcome.act.only <- uni.stud.p.outcome.act[!uni.stud.p.outcome.act %in% uni.stud.p.outcome.pas]
  uni.stud.p.outcome.mixed <- uni.stud.p.outcome.pas[uni.stud.p.outcome.pas %in% uni.stud.p.outcome.act]
  
  # insert number of these studies into df
  outcomes.no.ordered.freq.df[i, "Frequency sufficient data passive only"] <- length(uni.stud.p.outcome.pas.only)
  outcomes.no.ordered.freq.df[i, "Frequency sufficient data active only"] <- length(uni.stud.p.outcome.act.only)
  outcomes.no.ordered.freq.df[i, "Frequency sufficient data mixed"] <- length(uni.stud.p.outcome.mixed)
  
  # count number of studies that deliver sufficient data
  ## for passive only
  ### count
  for (study in uni.stud.p.outcome.pas.only){
    if (!study %in% studies.suff.data.pas.vec){
      studies.suff.data.pas.num <- studies.suff.data.pas.num + 1
    }
  }
  
  #### add study names to vec
  studies.suff.data.pas.vec <- append(studies.suff.data.pas.vec, uni.stud.p.outcome.pas)
  
  ## for passive only
  ### count
  for (study in uni.stud.p.outcome.act.only){
    if (!study %in% studies.suff.data.act.vec){
      studies.suff.data.act.num <- studies.suff.data.act.num + 1
    }
  }
  
  ### add study names to vec
  studies.suff.data.act.vec <- append(studies.suff.data.act.vec, uni.stud.p.outcome.act)
  
  ## for actvie and passive controls
  ### count
  for (study in uni.stud.p.outcome.mixed){
    if (!study %in% studies.suff.data.mix.vec){
      studies.suff.data.mix.num <- studies.suff.data.mix.num + 1
    }
  }
  
  ### add study names to vec
  studies.suff.data.mix.vec <- append(studies.suff.data.mix.vec, uni.stud.p.outcome.mixed)
  
  i <- i + 1
}

outcomes.no.ordered.freq.df$Outcome <- str_to_lower(as.character(outcomes.no.ordered.freq.df$Outcome))

outcomes.no.ordered.freq.df

# %% hidden=true vscode={"languageId": "r"}
# total freq of outcomes
outcomes.no.ordered.freq.plot.df <- outcomes.no.ordered.freq.df[,c(1,2)]

# freq of outcomes with sufficient data for active/passive controls and both respecitvely
outcomes.no.ordered.suff.dat.pas.plot.df <- outcomes.no.ordered.freq.df[,c(1,3)]
outcomes.no.ordered.suff.dat.act.plot.df <- outcomes.no.ordered.freq.df[,c(1,4)]
outcomes.no.ordered.suff.dat.mix.plot.df <- outcomes.no.ordered.freq.df[,c(1,5)]

# rename cols
names(outcomes.no.ordered.suff.dat.pas.plot.df) <- c("Outcome", "Frequency")
names(outcomes.no.ordered.suff.dat.act.plot.df) <- c("Outcome", "Frequency")
names(outcomes.no.ordered.suff.dat.mix.plot.df) <- c("Outcome", "Frequency")

# add col telling for what sufficient data is present
outcomes.no.ordered.suff.dat.pas.plot.df$`Sufficient data for meta-analysis` <- "04_for_passive_controls_only"
 
outcomes.no.ordered.suff.dat.act.plot.df$`Sufficient data for meta-analysis` <- "02_for_active_controls_only"

outcomes.no.ordered.suff.dat.mix.plot.df$`Sufficient data for meta-analysis` <- "03_for_active_and_passive_controls"

# df with freq of no sufficient data
outcomes.no.ordered.freq.plot.df$Frequency <- outcomes.no.ordered.freq.plot.df$Frequency - (
  outcomes.no.ordered.suff.dat.pas.plot.df$Frequency + outcomes.no.ordered.suff.dat.act.plot.df$Frequency + outcomes.no.ordered.suff.dat.mix.plot.df$Frequency
)

outcomes.no.ordered.freq.plot.df$`Sufficient data for meta-analysis` <- "01_for_no_case"


# outcomes.no.ordered.suff.dat.plot.df$Frequency <- outcomes.no.ordered.freq.plot.df$Frequency - (outcomes.no.ordered.freq.plot.df$Frequency - outcomes.no.ordered.suff.dat.plot.df$Frequency)

# bring all dfs together
outcomes.no.ordered.freq.plot.df <- rbind(
  outcomes.no.ordered.freq.plot.df,
  outcomes.no.ordered.suff.dat.act.plot.df,
  outcomes.no.ordered.suff.dat.mix.plot.df,
  outcomes.no.ordered.suff.dat.pas.plot.df
)

# rename rownames
rownames(outcomes.no.ordered.freq.plot.df) <- 1:nrow(outcomes.no.ordered.freq.plot.df)


# outcomes.no.ordered.freq.plot.df$`Sufficient data for meta-analysis` <- NA
# outcomes.no.ordered.freq.plot.df[1:(nrow(outcomes.no.ordered.freq.plot.df)/2),"Sufficient data for meta-analysis"] <- "yes"
# outcomes.no.ordered.freq.plot.df[20:nrow(outcomes.no.ordered.freq.plot.df), "Sufficient data for meta-analysis"] <- "no"

outcomes.no.ordered.freq.plot.df

# %% hidden=true vscode={"languageId": "r"}
# create labels shown in outcome frequency plots
outcome.freq.labels <- c()
iter.count <- nrow(outcomes.no.ordered.freq.plot.df)
for (i in 1:iter.count){
  freq <- outcomes.no.ordered.freq.plot.df[i,"Frequency"]
  if (freq %in% c(0, 1)){
    freq <- ""
  }
  outcome.freq.labels <- append(
    outcome.freq.labels,
    freq
  )
}
outcome.freq.labels

# %% hidden=true vscode={"languageId": "r"}
# set plot size
options(repr.plot.width = 15, repr.plot.height = 7, repr.plot.res = 150)

row.no <- nrow(outcomes.no.ordered.freq.plot.df)
ggplot(outcomes.no.ordered.freq.plot.df, aes(x = reorder(Outcome, -Frequency), y = Frequency, fill = `Sufficient data for meta-analysis`)) +
  geom_bar(stat="identity", alpha = 0.6, colour = "white") +
  geom_text(
    aes(label = outcome.freq.labels),
    position = position_stack(vjust=0.5),
    color = c(rep("white", length(outcome.freq.labels) / 2), rep("black", length(outcome.freq.labels) / 2))
  ) + 
  theme(
    title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(angle=75, vjust=1, hjust=1, size = 13)
  ) +
  labs(
    title = "Frequency of outcomes among all included studies",
    x = "\nOutcomes",
    y = "Frequency\n"
  ) +
  scale_fill_manual(values=c("darkblue", "blue","cornflowerblue", "lightblue"))

# %% [markdown] heading_collapsed=true hidden=true
# ###### Participants per outcome (only passive controls)

# %% hidden=true vscode={"languageId": "r"}
# participants per outcomes (with and without outliers)
part.p.group.p.outcome.df <- data.frame(
  Experimental.group = rep(NA, length(present.outcomes)),
  Experimental.group.no.outl. = rep(NA, length(present.outcomes)),
  Control.group = rep(NA, length(present.outcomes)),
  Control.group.no.outl. = rep(NA, length(present.outcomes)),
  Total = rep(NA, length(present.outcomes)),
  Total.no.outl. = rep(NA, length(present.outcomes)),
  row.names = present.outcomes.sorted
)

for (outcome in present.outcomes.sorted){
  # get data without outliers
  outliers <- outlier.list[[outcome]]
  results.meta.wo.o <-  print.meta.results(
    outcome, preferred.scale = get.1st.preferred.scale(outcome),
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "results.meta", filter.forest..funnel.vec = if(length(outliers) != 0){-outliers}else{FALSE}
  )
  n.e.wo.o <- if(results.meta.wo.o$k == 0){0}else{results.meta.wo.o$n.e.pooled}
  n.c.wo.o <- if(results.meta.wo.o$k == 0){0}else{results.meta.wo.o$n.c.pooled}
  part.p.group.p.outcome.df[outcome, "Experimental.group.no.outl."] <- n.e.wo.o
  part.p.group.p.outcome.df[outcome, "Control.group.no.outl."] <- n.c.wo.o
  part.p.group.p.outcome.df[outcome, "Total.no.outl."] <- n.e.wo.o + n.c.wo.o
  
  # get data with outliers included
  results.meta.w.o <-  print.meta.results(
    outcome, preferred.scale = get.1st.preferred.scale(outcome),
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "results.meta"
  )
  n.e.w.o <- if(results.meta.w.o$k == 0){0}else{results.meta.w.o$n.e.pooled}
  n.c.w.o <- if(results.meta.w.o$k == 0){0}else{results.meta.w.o$n.c.pooled}
  part.p.group.p.outcome.df[outcome, "Experimental.group"] <- n.e.w.o
  part.p.group.p.outcome.df[outcome, "Control.group"] <- n.c.w.o
  part.p.group.p.outcome.df[outcome, "Total"] <- n.c.w.o + n.e.w.o
}

# add row with sums
part.p.group.p.outcome.df <- rbind(
  part.p.group.p.outcome.df,
  data.frame(
    part.p.group.p.outcome.df %>%
      summarise(across(everything(), ~ sum(., na.rm = TRUE))),
    row.names = "Sum"
  )
)
part.p.group.p.outcome.df

# %% hidden=true vscode={"languageId": "r"}
part.p.group.p.outcome.df$Outcomes <- rownames(part.p.group.p.outcome.df)
part.p.group.p.outcome.df <- part.p.group.p.outcome.df %>%
  relocate(Outcomes)
part.p.group.p.outcome.df

# %% [markdown] heading_collapsed=true hidden=true
# ### Number of studies delivering sufficient data for meta-analyses

# %% hidden=true vscode={"languageId": "r"}
# all studies
study.no; study.names

# %% hidden=true vscode={"languageId": "r"}
# see calculation at ###### Outcome numbers
# studies with sufficient data with passive controls only
studies.suff.data.pas.num; unique(studies.suff.data.pas.vec)

# studies with sufficient data with active controls only
studies.suff.data.act.num; unique(studies.suff.data.act.vec)

# studies with sufficient data with active and passive controls
studies.suff.data.mix.num; unique(studies.suff.data.mix.vec)

# Number of studies with sufficient data for either active or passive controls
unique(c(studies.suff.data.pas.vec, studies.suff.data.act.vec)) %>% length();
unique(c(studies.suff.data.pas.vec, studies.suff.data.act.vec))

# %% [markdown]
# # Sensitivity Analysis

# %% vscode={"languageId": "r"}
# install.packages("gt")  # <-- manipulating table apperance
# install phantomjs <-- for saving tables as pictures
# install.packages("webshot2")
# webshot::install_phantomjs()
# install.packages("rmarkdown")  # <-- for saving table as docx
library(webshot2)
library(gt)
library(rmarkdown)

# %% [markdown] heading_collapsed=true
# ## Get all present outcomes names with sufficient data and passive controls and plots for outcomes in loop

# %% hidden=true vscode={"languageId": "r"}
#
present.outcomes.passive <- c()

overall.res.metafor.passive <- get.overall.res.metafor()
study.names.suff.data.passive <- unique(overall.res.metafor.passive$data$study.id)

outcome.names.df.passive <- outcome.names.df[study.names.suff.data.passive,]

for (row in 1:nrow(outcome.names.df.passive)){
  for (col in 1:ncol(outcome.names.df.passive)){
    if (!(
      is.na(outcome.names.df.passive[row, col]) |
      outcome.names.df.passive[row, col] == "NA"
    )){
      present.outcomes.passive <- append(present.outcomes.passive, outcome.names.df.passive[row, col])
    }
  }
}

present.outcomes.passive <- present.outcomes.passive[-c(which(present.outcomes.passive == "Other: "))]  # delete "Other: "
outcomes.no.df.passive <- data.frame(table(present.outcomes.passive))

present.outcomes.passive <- unique(present.outcomes.passive)
outcomes.no.df.passive

outcomes.no.10.plus.passive <- as.vector(outcomes.no.df.passive[
  outcomes.no.df.passive$Freq >= 10, "present.outcomes.passive"
])
outcomes.no.10.plus.passive

# %% [markdown] heading_collapsed=true
# ## Get results of network meta-analysis

# %% hidden=true vscode={"languageId": "r"}
# network meta-analysis results
message("Calculating network meta-analysis results for all outcomes...")
net.res.all <- net.meta.analyze(
  present.outcomes, preferred.scale = F, net.df = F, net.res = F,
  details.chkmultiarm = T, tol.multiarm = 1,
  plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
  return.data = "net.res", reference.group = "passive control", random = T, silent = T
)

# Smaller network models per outcome domain
## Direct Resilience
message("... For direct resilience...")
resilience.scale.outcomes <- c(
  "Resilience Scale"
)
net.res.resilience.scale <- net.meta.analyze(
  resilience.scale.outcomes, preferred.scale = F, net.df = F, net.res = F,
  details.chkmultiarm = T, tol.multiarm = 1,
  plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
  return.data = "net.res", reference.group = "passive control", random = T, silent = T
)

## Mental-health related outcomes
message("... For mental-health related outcomes...")
mental.health.outcomes <- c(
  "Depression", "Anxiety", "Stress", "Well-being"
)
net.res.mental.health <- net.meta.analyze(
  mental.health.outcomes, preferred.scale = F, net.df = F, net.res = F,
  details.chkmultiarm = T, tol.multiarm = 1,
  plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
  return.data = "net.res", reference.group = "passive control", random = T, silent = T
)

## Secondary Factors
message("... For secondary outcomes...")
net.res.secondary.outcomes <- net.meta.analyze(
  present.outcomes.secondary, preferred.scale = F, net.df = F, net.res = F,
  details.chkmultiarm = T, tol.multiarm = 1,
  plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
  return.data = "net.res", reference.group = "passive control", random = T, silent = T
)

# Define list containing all network meta-analysis results
net.res.list <- list(
  net.res.all = list(
    res.object = net.res.all,
    included.outcomes = present.outcomes
  ),
  net.res.resilience.scale = list(
    res.object = net.res.resilience.scale,
    included.outcomes = resilience.scale.outcomes
  ),
  net.res.mental.health = list(
    res.object = net.res.mental.health,
    included.outcomes = mental.health.outcomes
  ),
  net.res.secondary.outcomes = list(
    res.object = net.res.secondary.outcomes,
    included.outcomes = present.outcomes.secondary
  )
)

# studies delivering sufficient data for meta-analysis
study.names.suff.data <- sort(unique(gsub("\\ #.*","", net.res.all$studlab)))

# inconsistent multi-arm studies with levels of tolerance .001 and .01
inc.mult.arm.stud.001 <- c("Flett 2019a", "Messer 2016", "Spruin 2021", "Tloczynski 1994", "Waechter 2021", "Wang 2021", "Weytens 2014")
inc.mult.arm.stud.01 <- c("Messer 2016", "Spruin 2021", "Waechter 2021")

# Investigate if all outcomes are spelled correctly
outcomes.spelled.correctly.per.domain <- sapply(
  net.res.list,
  function(domain) domain$included.outcomes %in% present.outcomes
)
outcomes.spelled.correctly.per.domain.all <- sapply(
  net.res.list,
  function(domain) all(domain$included.outcomes %in% present.outcomes)
)
outcomes.spelled.correctly.all <- sapply(
  net.res.list,
  function(domain) all(domain$included.outcomes %in% present.outcomes)
) %>% all()

if (!outcomes.spelled.correctly.all) {
  misspelled_outcomes <- lapply(names(net.res.list), function(name) {
    domain <- net.res.list[[name]]
    incorrect <- domain$included.outcomes[!domain$included.outcomes %in% present.outcomes]
    if (length(incorrect) > 0) {
      paste0("  - ", name, ": ", paste(incorrect, collapse = ", "))
    }
  })
  misspelled_outcomes <- unlist(misspelled_outcomes[!sapply(misspelled_outcomes, is.null)])
  
  stop("Not all outcomes in net.res.list are spelled correctly. The following outcomes are misspelled:\n",
       paste(misspelled_outcomes, collapse = "\n"))
}

# %% vscode={"languageId": "r"}
# Missing outcomes
setdiff(
  present.outcomes,
  (
    sapply(
      net.res.list[-1],  # exclude overall network
      function(domain) domain$included.outcomes
    ) %>%
    unlist(use.names = F) %>%
    unique()
  )
)

# %% hidden=true vscode={"languageId": "r"}
# real number of studies included into the network meta-analysis model
length(study.names.suff.data)

# number of participants included in theses studies
sum(no.participants.df["Sum", study.names.suff.data], na.rm = T)

# %% [markdown]
# ## Functions

# %% [markdown] heading_collapsed=true
# ### Get vector to fill sensitivity analysis df

# %% hidden=true vscode={"languageId": "r"}
# with network analysis included
get.sens.anal.vec <- function(res, model = F, model.meta = NULL, subgroup.method = NULL, sub.model.metafor.fixed = F, model.metafor.fixed = F){ 
  
  if ("rma.uni" %in%  class(res)){  # for {metafor} results
    n.cont <- sum(res$data[, "n.control"])
    n.int <- sum(res$data[, "n.int"])
    n.total <- n.cont + n.int
    pval.Q <- res$QEp
    # pval.Q <- ifelse(pval.Q == 0, "< .0001" ,pval.Q)
    pval <- res$pval
    # pval <- ifelse(pval == 0, "< .0001" ,pval)
    
    if (model == "rand.fix"){
      
      if (model.metafor.fixed){
        het.metrics <- c(rep(NA, 9)[1:9])
        pred.inter.lb <- NA
        pred.inter.ub <- NA
      } else {
        het.metrics <- c(res$tau2, NA, NA, res$se.tau2, res$I2, NA, NA, res$QE, pval.Q)
        pred.inter.lb <- predict(res)$pi.lb
        pred.inter.ub <- predict(res)$pi.ub
      }

      sens.anal <- c(
        res$k, n.total, n.int, n.cont, res$b[1,1], res$ci.lb, res$ci.ub,
        pred.inter.lb, pred.inter.ub, res$se, res$zval, pval, het.metrics
      )
    } else if (model == "mixed"){
      Q.val.omnibus.test.moderators <- res$QM
      p.val.omnibus.test.moderators <- res$QMp
      is.categorical.mod <- grepl("delivery.mode", names(res$b[2,1])) | grepl("meditation.type", names(res$b[2,1]))
        # is true if moderator is delivery.mode or meditation.type

      # add number of studies per subgroup to results object if moderator is categorical
      if (is.categorical.mod){
        res$k.w <- c(0)
        for (lvl in 2:length(res$b)){  # length(res$b) = number of model coefficients
          res$k.w[lvl] <- sum(res$X[,lvl])
        }
        res$k.w[1] <- res$k - sum(res$k.w)
      } else {
        res$k.w <- NULL
      }
      
      # generate vector with model results
      mod.vals <- c()
      for (lvl in 1:length(res$b)){
        mod.vals <- append(
          mod.vals,
          as.double(c(
            res$b[lvl,1], res$ci.lb[lvl], res$ci.ub[lvl], res$se[lvl], res$zval[lvl], pval[lvl],
            if(!is.categorical.mod & length(res$b) == 2 & lvl == 2){rep(NA, 6)[1:6]}else{NULL}  # generate NAs for 2nd moderator coefficient of linear regressions of continous moderators
          ))
        )
      }
      
      # get heterogeneity metrics
      if (sub.model.metafor.fixed){
        het.metrics <- c(rep(NA, 5)[1:5])
      } else {
        het.metrics <- c(res$tau2, res$se.tau2, res$I2, res$QE, pval.Q)
      }
        
      sens.anal <- c(
        res$k, res$k.w, n.total, n.int, n.cont,
        mod.vals, res$R2, het.metrics, Q.val.omnibus.test.moderators, p.val.omnibus.test.moderators
      )
      
    } else {
      print("error in get.sens.anal.vec(): set parameter 'model' to 'rand.fix' or 'mixed' when using {metafor} results")
    }
    
  } else if ("metacont" %in%  class(res) | "metagen" %in%  class(res)){  # for {meta} resutls
    
    n.cont <- sum(res$n.c)
    n.int <- sum(res$n.e)
    n.total <- n.cont + n.int
    pval <- res$pval.random
    # pval <- ifelse(pval == 0, "< .0001" ,pval)
    # pval.Q <- ifelse(pval.Q == 0, "< .0001" ,pval.Q)
    pred.inter.lb <- res$lower.predict
    pred.inter.ub <- res$upper.predict
    
    
    
    if (model == "rand.fix"){
      if (model.meta == "random"){
        sens.anal <- c(
          res$k, n.total, n.int, n.cont, res$TE.random, res$lower.random, res$upper.random, pred.inter.lb, pred.inter.ub,
          res$seTE.random, res$zval.random, pval,
          res$tau2, res$lower.tau2, res$upper.tau2, NA,
          res$I2 * 100, res$lower.I2 * 100, res$upper.I2 * 100,
          res$Q, res$pval.Q
        )
      } else if (model.meta == "common"){
        sens.anal <- c(
          res$k, n.total, n.int, n.cont, res$TE.common, res$lower.common, res$upper.common, pred.inter.lb, pred.inter.ub,
          res$seTE.common, res$zval.common, pval,
          NA, NA, NA, NA,
          NA, NA, NA,
          NA, NA
        )
      } else {
        print("error in get.sens.anal.vec(): set parameter 'model.meta' to 'common' or 'random' if {meta} results are passed")
      }
    } else if (model == "mixed"){
      print("error in get.sens.anal.vec(): there is no regression calculated by {meta} on its own (only a wrapper function working with {metafor}). Set parameter 'model' to 'rand.fix' or 'subgroup' instead.")
    } else if (model == "subgroup"){
      
      # get number of studies per subgroup, treatment effect, and within group/between study heterogeneity metrics from result object (res)
      k.w <- c()
      TE.metrics <- c()
      het.metrics.w <- c()
      for (i in 1:length(res$k.w)){  # iterate over number of subgroup levls
        k.w[i] <- res$k.w[i]

        if (subgroup.method == "fixed"){
          TE.metrics <- append(TE.metrics, c(res$TE.common.w[i], res$lower.common.w[i], res$upper.common.w[i], res$seTE.common.w[i], res$zval.common.w[i]))
          het.metrics.w <- append(het.metrics.w, c(rep(NA, 8)[1:8]))
        } else {
          TE.metrics <- append(TE.metrics, c(res$TE.random.w[i], res$lower.random.w[i], res$upper.random.w[i], res$seTE.random.w[i], res$zval.random.w[i]))

          if ((subgroup.method == "random.common.tau2" & i == 1) | subgroup.method %in% c("random.separate.tau2", "random")){
            Q.stats.within <- if(subgroup.method == "random.common.tau2"){c(res$Q, res$pval.Q)}else if(subgroup.method %in% c("random.separate.tau2", "random")){c(res$Q.w[i], res$pval.Q.w[i])}
            tau2.metrics.within <- if(subgroup.method %in% c("random.common.tau2", "random")){c(res$tau2.w[i], NA, NA)}else if(subgroup.method == "random.separate.tau2"){c(res$tau2.w[i], res$lower.tau2.w[i], res$upper.tau2.w[i])}
             # tau2 confidence intervals of subgroups seem to be not reported  by {meta} result object for subgroup.method = "random.common.tau2"
            het.metrics.w <- append(
              het.metrics.w,
              c(
                tau2.metrics.within,
                res$I2.w[i] * 100, res$lower.I2.w[i] * 100, res$upper.I2.w[i] * 100,
                Q.stats.within
              )
            )
          } else {
            het.metrics.w <- append(het.metrics.w, c(rep(NA, 8)[1:8]))
          }
        }
      }
      
      # get between group Q-statistics
      if (subgroup.method == "fixed"){
        Q.stats.between <- c(res$Q.b.common, res$pval.Q.b.common)
      } else if (subgroup.method == "random.separate.tau2"){
        Q.stats.between <- c(res$Q, res$pval.Q)
      } else if (subgroup.method %in% c("random.common.tau2", "random")){
        Q.stats.between <- c(res$Q.b.random, res$pval.Q.b.random)
      }
      sens.anal <- as.double(c(
        res$k, k.w, n.total, n.int, n.cont,
        TE.metrics, het.metrics.w, Q.stats.between
      ))

    } else {
      print("error in get.sens.anal.vec(): set parameter 'model' to 'rand.fix' or 'subgroup' when using {meta} results")
    }
  } else if (class(res) == "netmeta"){  # for network analysis
    
    trts <- net.res.all$trts
    
    k <- length(unique(gsub("\\ #.*","", res$studlab)))  # gsub("\\ #.*","", studlab) cuts out every " #" and the following
    
    sens.anal <- c(
      k, res$m, res$n, res$d,
      as.double(res$pval.random[, "passive control"][trts][-6]),  # pvals comparing interventions with passive control
      as.double(res$pval.random[, "meditation (exclusive)"][trts][-c(4, 6)]),  # pvals comparing interventions with exclusive meditation
      as.double(res$TE.random[, "meditation (exclusive)"][trts][-4]) * -1,  # differences in SMD to exclusive meditation
        # index 4 = passive control, index 6 = meditation (exclusive)
      res$tau2, res$I2 * 100, res$lower.I2 * 100, res$upper.I2 * 100,
      res$Q, res$df.Q, res$pval.Q,
      res$Q.heterogeneity, res$df.Q.heterogeneity, res$pval.Q.heterogeneity,
      res$Q.inconsistency, res$df.Q.inconsistency, res$pval.Q.inconsistency
    )
  } else {
    cat("error in get.sens.anal.vec(): unknown input object of class:", class(res))
  }
  return(sens.anal)
}

# %% [markdown]
# ### Get sensitivity analysis df

# %% code_folding=[] vscode={"languageId": "r"}
get.sens.anal.df <- function(
  outcome, model, moderator.vec = c("sessions.duration", "sessions.frequency", "programs.duration", "meditation.total", "follow.up.period", "delivery.mode", "meditation.type"),
  save.as = F, saving.path = "", subgroup = F,
  .sens.anal.df = F  # works only for model = "net"
){
  
  if (model == "rand.fix"){
    
    # get results
    restuls.meta.o. <- print.meta.results(
      outcome = outcome, preferred.scale = get.1st.preferred.scale(outcome),
      regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
      return.data = "results.meta"
    )
    
    if (restuls.meta.o.$k > 0){
      
      outliers <- outlier.list[[outcome]]
      
      # get column names of sensitivity analysis data frame
      cnames <- c(
        "Number of included studies", "Number of obeservations", "... in experimental groups", "... in control groups", "SMD [Hedge's g]",
        "SMD CI lower threshold", "SMD CI upper threshold", "SMD PI lower threshold", "SMD PI upper threshold",
        "SMD's standard errror", "SMD's z-value", "p-value (testing SMD differs from zero)", "tau^2", "tau^2 CI lower threshold", "tau^2 CI upper threshold", "tau^2's standard error", "I^2 [%]",
        "I^2 CI lower threshold", "I^2 CI upper threshold", "Q-value", "p-value (testing between-study heterogeneity)"
      )
      
      # get empty sensitivity analysis data frame
      sens.anal.df <- t(data.frame(
        rep(NA, length(cnames)),
        row.names = cnames
      ))
      
      rnames <- c()
      
      # fill sensitivity analysis data frame
      if (outcome == "Stress"){
        for (preferred.scale in c("DASS", "PSS")){
          for (out.inf in c("out.inf.incl", "out.inf.excl")){
            if (length(outliers) == 0 & out.inf == "out.inf.excl"){
              next  # skip if no outliers/influential cases are present
            }
            for (lib in c("meta", "metafor")){
              for (model.used in c("random", "common")){
                # get results object
                res <- print.meta.results(
                  outcome = outcome, preferred.scale = preferred.scale,
                  regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
                  return.data = paste("results.", lib, sep = ""), results.metafor.fixed = ifelse(model.used == "common" & lib == "metafor", T, F),
                  filter.forest..funnel.vec = if(out.inf == "out.inf.incl"){F}else{if(length(outliers) == 0){FALSE}else{-outliers}}
                )

                # get vector of result values
                sens.anal.vec.df <- t(data.frame(
                  get.sens.anal.vec(res, model, model.used, model.metafor.fixed = ifelse(model.used == "common" & lib == "metafor", T, F)),
                  row.names = cnames
                ))

                sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

                # generate codes for rownames
                rname <- paste(
                  ifelse(preferred.scale == "DASS", "1", "0"),
                  ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
                  ifelse(lib == "meta", "1", "0"),
                  ifelse(model.used == "random", "1", "0")
                )

                # add rowname to rownames of sens.anal.df
                rnames <- append(rnames, rname)
              }
            }
          }
        }
      } else {
        for (out.inf in c("out.inf.incl", "out.inf.excl")){
          if (length(outliers) == 0 & out.inf == "out.inf.excl"){
            next  # skip if no outliers/influential cases are present
          }
          for (lib in c("meta", "metafor")){
            for (model.used in c("random", "common")){
             
              # get results object
              res <- print.meta.results(
                outcome = outcome, preferred.scale = F,
                regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
                return.data = paste("results.", lib, sep = ""), results.metafor.fixed = ifelse(model.used == "common" & lib == "metafor", T, F),
                filter.forest..funnel.vec = if(out.inf == "out.inf.incl"){F}else{if(length(outliers) == 0){F}else{-outliers}}
              )
              
              # get vector of result values
              sens.anal.vec.df <- t(data.frame(
                get.sens.anal.vec(res, model, model.used, model.metafor.fixed = ifelse(model.used == "common" & lib == "metafor", T, F)),
                row.names = cnames
              ))

              sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

              # generate codes for rownames
              rname <- paste(
                ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
                ifelse(lib == "meta", "1", "0"),
                ifelse(model.used == "random", "1", "0")
              )

              # add rowname to rownames of sens.anal.df
              rnames <- append(rnames, rname)
            }
          }
        }
      }
      
      ## delete first NA row and delete rownames 
      sens.anal.df <- sens.anal.df[-1, 1:ncol(sens.anal.df)]
      rownames(sens.anal.df) <- NULL  # delete rownames
      
      ## add rownames column
      choices.df <- data.frame(
        rnames,
        row.names = NULL
      )
      
      colnames(choices.df) <- "Decision Codes"
      
      sens.anal.df <- cbind(choices.df, sens.anal.df)
      
      ## round values
      ### n
      i.start <- 2
      i.end <- 5
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 0)
      
      ### SMD
      i.start <- i.end + 1
      i.end <- i.start + 6
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
      
      ### tau2
      i.start <- i.end + 1
      i.end <- i.start + 4
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
      
      ### I2 and Q val
      i.start <- i.end + 1
      i.end <- i.start + 3
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 2)
      
      ### pval (Q stats)
      sens.anal.df[, i.end + 1] <- round(sens.anal.df[, i.end + 1], 4)
      
      # save colored table
      if (save.as != F){
        ## cut out not wanted columns for coloring
        colrange <- 2:ncol(sens.anal.df)

        for (col in colrange){  # cut cols for coloring that are NA only
          if (all_na(sens.anal.df[, col])){
            colrange <- colrange[!colrange == col]
          }
        }
      
        gt.object <- sens.anal.df |>
          gt() |>
          data_color(
            columns = colrange,
            palette = "RdYlGn",
            na_color = "gray85",
            alpha = .8
          ) |>
          tab_footnote(
            footnote = paste(
              "Meaning of digits in decision codes (same digit order): ",
              ifelse(outcome == "Stress", "preferred Scale - DASS = 1, PSS = 0; ", ""),
              ifelse(length(outliers) == 0, "", "outliers and influential cases included - yes = 1, no = 0; "),
              "library - meta = 1, metafor = 0; model - random-effects = 1, fixed-effects = 0",
              sep = ""
            ),
            cells_column_labels(columns = `Decision Codes`)
          ) |>
          tab_footnote(
            footnote = "SMD = standardized mean difference",
            cells_column_labels(columns = `SMD [Hedge's g]`:`p-value (testing SMD differs from zero)`)
          ) |>
          tab_footnote(
            footnote = "CI = 95% confidence interval",
            cells_column_labels(columns = c(
              `SMD CI lower threshold`, `SMD CI upper threshold`, `tau^2 CI lower threshold`, `tau^2 CI upper threshold`,
              `I^2 CI lower threshold`, `I^2 CI upper threshold`
            ))
          ) |>
          tab_footnote(
            footnote = "PI = 95% prediction interval",
            cells_column_labels(columns = c(`SMD PI lower threshold`, `SMD PI upper threshold`))
          )
        
        if (save.as == "png"){
          gtsave(
            gt.object,
            paste(
              saving.path, "Sens.anal.table.", outcome, ".",
              save.as, sep = ""
            ),
            vwidth = 1750,
            vheight = round(nrow(sens.anal.df) / 16 * 2000, 0)
          )
        } else {
          gtsave(
            gt.object,
            paste(
              saving.path, "Sens.anal.table.", outcome, ".",
              save.as, sep = ""
            )
          )
        }
      }
      
    } else {
      sens.anal.df <- NA
    }
    
# Regression
  } else if (model == "mixed"){
    for (moderator in moderator.vec){
      
      # create sensitivity analysis data frame
      if (moderator %in% c("delivery.mode", "meditation.type")){  # in this case cnames without columns of second moderator coefficient

        res <- print.meta.results(
          outcome, preferred.scale = ifelse(outcome == "Stress", "DASS", F),
          basic = F, moderator.vec = c(moderator), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
          regression.label = T, return.data = "regression.results.linear"
        )

        lvl.names <- unique(res$data[, moderator])
        
        mod.res.names <- c()
        k.w.names <- c()
        for (lvl.name in lvl.names){
          # generate column names for moderator results of lvl.name
          col.sub.names <- c("coefficient", "coefficient CI lower threshold", "coefficient CI upper threshold", "coefficient's standard errror", "coefficient's z-value", "coefficient's p-value")
          i <- 1
          for (col.sub.name in col.sub.names){
            col.sub.names[i] <- paste(lvl.name, col.sub.name)
            i <- i + 1
          }
          mod.res.names <- append(mod.res.names, col.sub.names)
          k.w.names <- append(k.w.names, paste("Number of studies in subroup", lvl.name))
        }
        
        cnames <- c(
          "Number of included studies", k.w.names, "Number of obeservations", "... in experimental groups", "... in control groups",
          mod.res.names,
          "R^2 [%]", "tau^2", "tau^2's standard error",
          "I^2 [%]", "Q-value (testing residual heterogeneity)", "p-value (testing residual heterogeneity)", "Q-value for omnibus-test of moderators", "p-value for omnibus-test of moderators"        
        )
      } else {
        cnames <- c(
          "Number of included studies", "Number of obeservations", "... in experimental groups", "... in control groups",
          "Intercept", "Intercept CI lower threshold", "Intercept CI upper threshold", "Intercept's standard errror", "Intercept's z-value", "Intercept's p-value",
          "Moderator's coefficient", "Moderator's coefficient CI lower threshold", "Moderator's coefficient CI upper threshold",
          "Moderator's coefficient's standard errror", "Moderator's coefficient's z-value", "Moderator's coefficient's p-value",
          "2nd Moderator's coefficient", "2nd Moderator's coefficient CI lower threshold", "2nd Moderator's coefficient CI upper threshold",
          "2nd Moderator's coefficient's standard errror", "2nd Moderator's coefficient's z-value", "2nd Moderator's coefficient's p-value",
          "R^2 [%]", "tau^2", "tau^2's standard error",
          "I^2 [%]", "Q-value (testing residual heterogeneity)", "p-value (testing residual heterogeneity)", "Q-value for omnibus-test of moderators", "p-value for omnibus-test of moderators"        
        )
      }
      
      sens.anal.df <- t(data.frame(
        rep(NA, length(cnames)),
        row.names = cnames
      ))
      
      rnames <- c()
      
      i <- 1
      
      if (outcome == "Stress"){
        for (preferred.scale in c("DASS", "PSS")){
          for (out.inf in c("out.inf.incl", "out.inf.excl")){
            if (length(outliers) == 0 & out.inf == "out.inf.excl"){
              next  # skip if no outliers/influential cases are present
            }
            for (sub.model in c("random", "common")){
              for (mean.range in c("mean.range.incl", "mean.range.excl")){
                if (moderator %in% c("delivery.mode", "meditation.type") & mean.range == "mean.range.excl"){
                   next  # categorical moderators are not influenced by mean range values
                }
                for (degree in c("linear", "squared")){
                  if (moderator %in% c("delivery.mode", "meditation.type") & degree == "squared"){
                    next  # squared models shell not be calculated for categorical moderators
                  }

                  if (moderator %in% c("delivery.mode", "meditation.type")){
                    outliers <- outlier.list[[moderator]][[outcome]]
                  } else{
                    outliers <- outlier.list[[paste(moderator, ifelse(degree == "linear", ".lin", ".sq"), sep = "")]][[outcome]]
                  }
                  
                  res <- print.meta.results(
                    outcome, preferred.scale = preferred.scale,
                    basic = F, moderator.vec = c(moderator), print.regplot = F, print.baujat.regression = F, print.regression.results = F,
                    regression.degree.1 = ifelse(degree == "linear", T, F), regression.degree.2 = ifelse(degree == "squared", T, F),
                    regression.label = T, return.data = ifelse(degree == "linear", "regression.results.linear", "regression.results.poly"),
                    without.mean.r = ifelse(mean.range == "mean.range.incl", F, T), results.metafor.fixed = ifelse(sub.model == "random", F, T),
                    filter.regression.linear.list = ifelse(out.inf == "out.inf.incl" & degree == "linear", F, if(length(outliers) == 0){F}else{list(-outliers)}),
                    filter.regression.poly.list = ifelse(out.inf == "out.inf.incl" & degree == "squared", F, if(length(outliers) == 0){F}else{list(-outliers)})
                  )
                  
                  sens.anal.vec.df <- t(data.frame(
                    get.sens.anal.vec(
                      res, "mixed",
                      sub.model.metafor.fixed = ifelse(sub.model == "random", F, T)
                    ),
                    row.names = cnames
                  ))

                  sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

                  # generate codes for rownames
                  rname <- paste(
                    ifelse(preferred.scale == "DASS", "1", "0"),
                    ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
                    ifelse(sub.model == "random", "1", "0"),
                    ifelse(moderator %in% c("delivery.mode", "meditation.type"), "", ifelse(mean.range == "mean.range.incl", "1", "0")),
                    if(moderator %in% c("delivery.mode", "meditation.type")){""}else if(degree == "squared"){"0"}else if(degree == "linear"){"1"}
                  )

                # add rowname to rownames of sens.anal.df
                rnames[i] <- rname
                i <- i + 1
                }
              }
            }
          }
        }
      } else {
        for (out.inf in c("out.inf.incl", "out.inf.excl")){
          if (length(outliers) == 0 & out.inf == "out.inf.excl"){
            next  # skip if no outliers/influential cases are present
          }
          for (sub.model in c("random", "common")){
            for (mean.range in c("mean.range.incl", "mean.range.excl")){
              for (degree in c("linear", "squared")){
                if (moderator %in% c("delivery.mode", "meditation.type") & degree == "squared"){
                  next  # squared models shell not be calculated for categorical moderators
                }
                
                if (moderator %in% c("delivery.mode", "meditation.type")){
                  outliers <- outlier.list[[moderator]][[outcome]]
                } else{
                  outliers <- outlier.list[[paste(moderator, ifelse(degree == "linear", ".lin", ".sq"), sep = "")]][[outcome]]
                }

                res <- print.meta.results(
                  outcome,
                  basic = F, moderator.vec = c(moderator), print.regplot = F, print.baujat.regression = F, print.regression.results = F,
                  regression.degree.1 = ifelse(degree == "linear", T, F), regression.degree.2 = ifelse(degree == "squared", T, F),
                  regression.label = T, return.data = ifelse(degree == "linear", "regression.results.linear", "regression.results.poly"),
                  without.mean.r = ifelse(mean.range == "mean.range.incl", F, T), results.metafor.fixed = ifelse(sub.model == "random", F, T),
                  filter.regression.linear.list = ifelse(out.inf == "out.inf.incl", F, if(length(outliers) == 0){F}else{list(-outliers)}),
                  filter.regression.poly.list = ifelse(out.inf == "out.inf.incl" & degree == "squared", F, if(length(outliers) == 0){F}else{list(-outliers)})
                )

                sens.anal.vec.df <- t(data.frame(
                  get.sens.anal.vec(res, "mixed"),
                  row.names = cnames
                ))

                sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

                # generate codes for rownames
                rname <- paste(
                  ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
                  ifelse(sub.model == "random", "1", "0"),
                  ifelse(mean.range == "mean.range.incl", "1", "0"),
                  if(moderator %in% c("delivery.mode", "meditation.type")){""}else if(degree == "squared"){"0"}else if(degree == "linear"){"1"}
                )

                # add rowname to rownames of sens.anal.df
                rnames[i] <- rname
                i <- i + 1
              }
            }
          }
        }
      }
      
      ## delete first NA row and delete rownames 
      sens.anal.df <- sens.anal.df[-1, 1:ncol(sens.anal.df)]
      rownames(sens.anal.df) <- NULL  # delete rownames
      
      ## add rownames column
      choices.df <- data.frame(
        rnames,
        row.names = NULL
      )
      
      colnames(choices.df) <- "Decision Codes"
      
      sens.anal.df <- cbind(choices.df, sens.anal.df)
      
      ## round values
      ### k and n
      lo.lim <- 2
      up.lim <- ifelse(moderator %in% c("delivery.mode", "meditation.type"), 5 + length(res$b), 5)
      sens.anal.df[, lo.lim:up.lim] <- round(sens.anal.df[, lo.lim:up.lim], 0)
      
      ### coefficients
      for (i in 1:length(res$b)){
        lo.lim <- up.lim + 1
        up.lim <- lo.lim + 5
        sens.anal.df[, lo.lim:up.lim] <- round(sens.anal.df[, lo.lim:up.lim], 4)
      }
      
      ### R^2
      R.index <- which(colnames(sens.anal.df) == "R^2 [%]")
      sens.anal.df[, R.index] <- round(sens.anal.df[, R.index], 2)
      
      ### tau^2 and its se
      lo.lim <- R.index + 1
      up.lim <- lo.lim + 1
      sens.anal.df[, lo.lim:up.lim] <- round(sens.anal.df[, lo.lim:up.lim], 4)
      
      ### I^2 and Q-value
      lo.lim <- up.lim + 1
      up.lim <- lo.lim + 1
      sens.anal.df[, lo.lim:up.lim] <- round(sens.anal.df[, lo.lim:up.lim], 2)
      
      ### p-values of Q-statistics and omnibus test
      lo.lim <- up.lim + 1
      up.lim <- lo.lim + 2
      sens.anal.df[, lo.lim:up.lim] <- round(sens.anal.df[, lo.lim:up.lim], 4)
      
      # save colored table
      if (save.as != F){
        ## cut out not wanted columns for coloring
        colrange <- 2:ncol(sens.anal.df)

        for (col in 1:ncol(sens.anal.df)){  # cut cols for coloring that are NA only
          if (all_na(sens.anal.df[, col])){
            colrange <- colrange[!colrange == col]
          }
        }
        
        # get column names of CI
        cnames.CI <- cnames[grepl("CI", cnames)]
        
        gt.object <- sens.anal.df |>
          gt() |>
          data_color(
            columns = colrange,
            palette = "RdYlGn",
            na_color = "gray85",
            alpha = .8
          ) |>
          tab_footnote(
            footnote = if(outcome == "Stress"){
              paste(
                "Meaning of digits in decision codes (same digit order): ",
                "preferred Scale - DASS = 1, PSS = 0; ",
                ifelse(length(outliers) == 0, "", "outliers and influential cases included - yes = 1, no = 0; "),
                "'sub-model' used in mixed-effects model - random-effects = 1, fixed-effects = 0",
                ifelse(moderator %in% c("delivery.mode", "meditation.type"), "", "; mean range values - included = 1, excluded = 0; degree of model - linear = 1, squared = 0"),
                sep = ""
              )
            } else if (outcome == "Anxiety"){
              paste(
                "Meaning of digits in decision codes (same digit order): ",
                ifelse(length(outliers) == 0, "", "outliers and influential cases included - yes = 1, no = 0; "),
                "'sub-model' used in mixed-effects model - random-effects = 1, fixed-effects = 0",
                ifelse(moderator %in% c("delivery.mode", "meditation.type"), "", "; mean range values - included = 1, excluded = 0; degree of model - linear = 1, squared = 0"),
                sep = ""
              )
            } else {""},
            cells_column_labels(columns = "Decision Codes")
          ) |>
          tab_footnote(
            footnote = "CI = confidence interval",
            cells_column_labels(columns = cnames.CI)
          )
        
        if (save.as == "png"){
          gtsave(
            gt.object,
            paste(
              saving.path, "Sens.anal.table.", outcome, ".",
              moderator, ".reg.", save.as, sep = ""
            ),
            vwidth = round(ncol(sens.anal.df) / 31 * 3300, 0),
            vheight = round(sqrt(nrow(sens.anal.df) / 32) * 2000, 0)
          )
        } else {
          gtsave(
            gt.object,
            paste(
              saving.path, "Sens.anal.table.", outcome, ".",
              moderator, ".reg.", save.as, sep = ""
            )
          )
        }
      }
    }
# Subgroup Analysis
  } else if (model == "subgroup"){
    
    # get results
    restuls.meta.o. <- print.meta.results(
      outcome = outcome, preferred.scale = ifelse(outcome == "Stress", "DASS", F),
      regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
      split.subgroups = T, print.forest.sub.single = subgroup, subgroup.method = "fixed", print.meta.results = F,
      return.data = "results.meta"
    )
    
    if (restuls.meta.o.$k > 0){
      
      if (subgroup != F){
        outliers <- outlier.list[[subgroup]][[outcome]]
        if (!subgroup %in% c("delivery.mode", "meditation.type")){
          print("error in get.sens.anal.df(): set parameter'subgroup' to 'delivery.mode' or 'meditation.type'")
        }
      } else {
        print("error in get.sens.anal.df(): set parameter'subgroup' to 'delivery.mode' while parameter 'model' == 'subgroup'")
      }
      
      # generate column names for study numbers, treatment effects, and heterogeneity metrics of all subgroup levels
      cnames.k.w <- c()
      cnames.TE.w <- c()
      cnames.TE.w.base <- c(
        "SMD [Hedge's g]", "SMD CI lower threshold", "SMD CI upper threshold", "SMD's standard errror", "SMD's z-value"
      )
      cnames.het.w <- c()
      cnames.het.w.base <- c(
        "tau^2", "tau^2 CI lower threshold", "tau^2 CI upper threshold",
        "I^2 [%]", "I^2 CI lower threshold", "I^2 CI upper threshold", "Q-value", "p-value (testing between-study heterogeneity)"
      )
      
      for (sub.lvl in restuls.meta.o.$subgroup.levels){
        
        # study numbers
        cnames.k.w <- append(cnames.k.w, paste("Study number in", sub.lvl))
        
        # treatment effect metrics       
        cnames.TE.w.lvl <- c()
        for (i in 1:length(cnames.TE.w.base)){
          cnames.TE.w.lvl[i] <- paste(cnames.TE.w.base[i], " (", sub.lvl, ")", sep = "")
        }
        cnames.TE.w <- append(cnames.TE.w, cnames.TE.w.lvl)
        
        # heterogeneity metrics
        cnames.het.w.lvl <- c()
        for (i in 1:length(cnames.het.w.base)){
          cnames.het.w.lvl[i] <- paste(cnames.het.w.base[i], " (", sub.lvl, ")", sep = "")
        }
        cnames.het.w <- append(cnames.het.w, cnames.het.w.lvl)
      }
      
      # column names of sensitivity analysis data frame
      cnames <- c(
        "Number of included studies", cnames.k.w, "Number of obeservations", "... in experimental groups", "... in control groups",
        cnames.TE.w, cnames.het.w, "Q-value (between group)", "p-value (between group)"
      )
      
      # get empty sensitivity analysis data frame
      sens.anal.df <- t(data.frame(
        rep(NA, length(cnames)),
        row.names = cnames
      ))
      
      rnames <- c()
      
      # fill sensitivity analysis data frame
      if (outcome == "Stress"){
        for (preferred.scale in c("DASS", "PSS")){
          for (out.inf in c("out.inf.incl", "out.inf.excl")){
            if (length(outliers) == 0 & out.inf == "out.inf.excl"){
              next  # skip if no outliers/influential cases are present
            }
            for (subgroup.method in c("random.separate.tau2", "random.common.tau2", "fixed")){
              # get results object
              res <- print.meta.results(
                outcome = outcome, preferred.scale = preferred.scale,
                regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
                split.subgroups = T, print.forest.sub.single = subgroup, subgroup.method = subgroup.method, print.meta.results = F,
                return.data = "results.meta",
                filter.forest..funnel.vec = if(out.inf == "out.inf.incl"){F}else{if(length(outliers) == 0){FALSE}else{-outliers}}
              )
              sens.anal.vec.df <- t(data.frame(
                get.sens.anal.vec(res, model, subgroup.method = subgroup.method),
                row.names = cnames
              ))

              sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

              # generate codes for rownames
              rname <- paste(
                ifelse(preferred.scale == "DASS", "1", "0"),
                ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
                if(subgroup.method == "random.separate.tau2"){"1"}else if(subgroup.method == "random.common.tau2"){"2"}else if(subgroup.method ==  "fixed"){"0"}
              )

              # add rowname to rownames of sens.anal.df
              rnames <- append(rnames, rname)
            }
          }
        }
      } else {
        for (out.inf in c("out.inf.incl", "out.inf.excl")){
          if (length(outliers) == 0 & out.inf == "out.inf.excl"){
            next  # skip if no outliers/influential cases are present
          }
          for (subgroup.method in c("random.separate.tau2", "random.common.tau2", "fixed")){

            # get results object
            res <- print.meta.results(
              outcome = outcome, preferred.scale = F,
              regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
              split.subgroups = T, print.forest.sub.single = subgroup, subgroup.method = subgroup.method, print.meta.results = F,
              return.data = "results.meta",
              filter.forest..funnel.vec = if(out.inf == "out.inf.incl"){F}else{if(length(outliers) == 0){FALSE}else{-outliers}}
            )

            # get vector of result values
            sens.anal.vec.df <- t(data.frame(
              get.sens.anal.vec(res, model, subgroup.method = subgroup.method),
              row.names = cnames
            ))

            sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

            # generate codes for rownames
            rname <- paste(
              ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
              if(subgroup.method == "random.separate.tau2"){"1"}else if(subgroup.method == "random.common.tau2"){"2"}else if(subgroup.method ==  "fixed"){"0"}
            )

            # add rowname to rownames of sens.anal.df
            rnames <- append(rnames, rname)
          }
        }
      }
      
      ## delete first NA row and delete rownames 
      sens.anal.df <- sens.anal.df[-1, 1:ncol(sens.anal.df)]
      rownames(sens.anal.df) <- NULL  # delete rownames
      
      ## add rownames column
      choices.df <- data.frame(
        rnames,
        row.names = NULL
      )
      
      colnames(choices.df) <- "Decision Codes"
      
      sens.anal.df <- cbind(choices.df, sens.anal.df)
      
      ## round values
      lvl.n <- length(restuls.meta.o.$subgroup.levels)
      
      # n        
      i.start <- 2
      i.end <- 5 + lvl.n
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 0)
      
      # TE vals
      for (i in 1:lvl.n){
        i.start <- i.end + 1
        i.end <- i.start + 4
        sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
      }
      
      # heterogeneity vals
      for (i in 1:lvl.n){
        # tau2 vals
        i.start <- i.end + 1
        i.end <- i.start + 2
        sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
        
        # I2 vals
        i.start <- i.end + 1
        i.end <- i.start + 2
        sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 2)
        
        # Q stats between study
        i.start <- i.end + 1
        i.end <- i.start + 1
        sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
      }
      
      # Q stats between groups
      i.start <- i.end + 1
      i.end <- i.start + 1
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
      
      # save colored table
      if (save.as != F){
        ## cut out not wanted columns for coloring
        colrange <- 2:ncol(sens.anal.df)

        for (col in colrange){  # cut cols for coloring that are NA only
          if (all_na(sens.anal.df[, col])){
            colrange <- colrange[!colrange == col]
          }
        }
        
        
        # get column names of SMD, CI, and PI
        cnames.SMD <- cnames[grepl("SMD", cnames)]
        cnames.CI <- cnames[grepl("CI", cnames)]
        
        gt.object <- sens.anal.df |>
          gt() |>
          data_color(
            columns = colrange,
            palette = "RdYlGn",
            na_color = "gray85",
            alpha = .8
          ) |>
          tab_footnote(
            footnote = paste(
              "Meaning of digits in decision codes (same digit order): ",
              ifelse(outcome == "Stress", "preferred Scale - DASS = 1, PSS = 0; ", ""),
              ifelse(length(outliers) == 0, "", "outliers and influential cases included - yes = 1, no = 0; "),
              "subgroup method - random-effects with separate tau^2 = 1, random-effects with common tau^2 = 2, fixed = 0",
              sep = ""
            ),
            cells_column_labels(columns = "Decision Codes")
          ) |>
          tab_footnote(
            footnote = "SMD = standardized mean difference",
            cells_column_labels(columns = cnames.SMD)
          ) |>
          tab_footnote(
            footnote = "CI = confidence interval",
            cells_column_labels(columns = cnames.CI)
          )
        
        if (save.as == "png"){
          gtsave(
            gt.object,
            paste(
              saving.path, "Sens.anal.table.", outcome, ".", subgroup, ".sub.",
              save.as, sep = ""
            ),
            vwidth = round(ncol(sens.anal.df) / 35 * 4000, 0),
            vheight = round(nrow(sens.anal.df) / 12 * 1000, 0)
          )
        } else {
          gtsave(
            gt.object,
            paste(
              saving.path, "Sens.anal.table.", outcome, ".", subgroup, ".sub.",
              save.as, sep = ""
            )
          )
        }
      }
      
    } else {
      sens.anal.df <- NA
    }
    
# network meta-analysis
  } else if (model == "net"){
    
    if (is.logical(.sens.anal.df)){
      outliers <- outlier.list[["net.overall"]]

      # get column names of sensitivity analysis data frame
      cnames <- c(
        "Number of included studies", "... pairwise comparisons", "... treatments", "... designs",
        "p-val. pas. vs. bio.", "p-val. pas. vs. cog.", "p-val. pas. vs. dog", "p-val. pas. vs. med(x)", "p-val. pas. vs. med(m)", "p-val. pas. vs. PMR",
        "p-val. pas. vs. rest", "p-val. pas. vs. stress", "p-val. pas. vs. walk",
        "p-val. med(x) vs. bio.", "p-val. med(x) vs. cog.", "p-val. med(x) vs. dog", "p-val. med(x) vs. med(m)", "p-val. med(x) vs. PMR",
        "p-val. med(x) vs. rest", "p-val. med(x) vs. stress", "p-val. med(x) vs. walk",
        "SMD diff. med(x) vs. bio.", "SMD diff. med(x) vs. cog.", "SMD diff. med(x) vs. dog", "SMD diff. med(x) vs. med(m)", "SMD diff. med(x) vs. pas.", "SMD diff. med(x) vs. PMR",
        "SMD diff. med(x) vs. rest", "SMD diff. med(x) vs. stress", "SMD diff. med(x) vs. walk",
        "tau^2", "I^2 [%]", "I^2 CI lower threshold [%]", "I^2 CI upper threshold [%]",
        "Total Q", "Total Q df", "Total Q p-value",
        "Q of for heterogeneity (het.)", "Q (het.) df", "Q (het.) p-value", 
        "Q of for inconsistancy (inc.)", "Q (inc.) df", "Q (inc.) p-value"
      )

      rnames <- c()

      # get empty sensitivity analysis data frame
      sens.anal.df <- t(data.frame(
        rep(NA, length(cnames)),
        row.names = cnames
      ))

      for (preferred.scale in c("DASS", "PSS")){
        for (out.inf in c("out.inf.incl", "out.inf.excl")){
          for (tol.multiarm in c(1, 0.01, 0.001)){

            res <- net.meta.analyze(
              outcome, preferred.scale = preferred.scale, net.df = F, net.res = F, comparisons.skip.list = F,
              plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
              reference.group = "passive control", random = T, return.data = "net.res", silent = T, tol.multiarm = tol.multiarm,
              filter.forest..funnel.vec = if (out.inf == "out.inf.incl"){
                if(tol.multiarm == 0.001){inc.mult.arm.stud.001}else if(tol.multiarm == 0.01){inc.mult.arm.stud.01}else{F}
              } else {
                if(length(outliers) == 0){
                  if(tol.multiarm == 0.001){inc.mult.arm.stud.001}else if(tol.multiarm == 0.01){inc.mult.arm.stud.01}else{F}
                } else {
                  if(tol.multiarm == 0.001){c(inc.mult.arm.stud.001, outliers)}else if(tol.multiarm == 0.01){c(inc.mult.arm.stud.01, outliers)}else{outliers}
                }
              }
            )

            # get vector of result values
            sens.anal.vec.df <- t(data.frame(
              get.sens.anal.vec(res),
              row.names = cnames
            ))

            sens.anal.df <- rbind(sens.anal.df, sens.anal.vec.df)

            # generate codes for rownames
            rname <- paste(
              ifelse(preferred.scale == "DASS", "1", "0"),
              ifelse(length(outliers) == 0, "", ifelse(out.inf == "out.inf.incl", "1", "0")),
              if(tol.multiarm == 1){"1"}else if(tol.multiarm == 0.01){"2"}else{"0"}
            )

            # add rowname to rownames of sens.anal.df
            rnames <- append(rnames, rname)
          }
        }
      }

      ## delete first NA row and delete rownames 
      sens.anal.df <- sens.anal.df[-1, 1:ncol(sens.anal.df)]
      rownames(sens.anal.df) <- NULL  # delete rownames

      ## add rownames column
      choices.df <- data.frame(
        rnames,
        row.names = NULL
      )

      colnames(choices.df) <- "Decision Codes"

      sens.anal.df <- cbind(choices.df, sens.anal.df)
    } else {
      if (is.data.frame(sens.anal.df) != T){
        cat("error in get.sens.anal.df(): set paramter sens.anal.df to an data frame, got object of type", class(sens.anal.df), "instead")
      }
      sens.anal.df <- .sens.anal.df
    }

    ## round values
    ### k, m, n, d
    i.start <- 2
    i.end <- 5
    sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 0)

    ### pvals
    i.start <- i.end + 1
    i.end <- i.start + 16
    sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)

    ### SMD differences
    i.start <- i.end + 1
    i.end <- i.start + 8
    sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 2)

    ### tau2
    i.start <- i.end + 1
    i.end <- i.start + 0
    sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
    
    ### I2
    i.start <- i.end + 1
    i.end <- i.start + 2
    sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 2)
    
    ### Q and its pvals
    for (metric in c("Total Q", "Q heterogeneity", "Q inconsistancy")){
      # Q
      i.start <- i.end + 1
      i.end <- i.start + 0
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 2)
    
      # df
      i.start <- i.end + 1
      i.end <- i.start + 0
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 2)
    
      # p
      i.start <- i.end + 1
      i.end <- i.start + 0
      sens.anal.df[, i.start:i.end] <- round(sens.anal.df[, i.start:i.end], 4)
    }


    # save colored table
    if (save.as != F){
      ## cut out not wanted columns for coloring
      colrange <- 2:ncol(sens.anal.df)

      for (col in colrange){  # cut cols for coloring that are NA only
        if (all_na(sens.anal.df[, col])){
          colrange <- colrange[!colrange == col]
        }
      }
      
      gt.object <- sens.anal.df |>
        gt() |>
        data_color(
          columns = colrange,
          palette = "RdYlGn",
          na_color = "gray85",
          alpha = .8
        ) |>
        tab_footnote(
          footnote = paste(
            "Meaning of digits in decision codes (same digit order): ",
            "preferred Scale - DASS = 1, PSS = 0; ",
            ifelse(length(outliers) == 0, "", "outliers, influential cases, and inconsistant treatment comparisons included - yes = 1, no = 0; "),
            "tolerance of multi-arm study inconsistency - 1 (no inconsitant multi-arm studies cut) = 1, .01 (some respective studies cut) = 2, .001 (all respective studies cut) = 0",
            sep = ""
          ),
          cells_column_labels(columns = `Decision Codes`)
        ) |>
        tab_footnote(
          footnote = "bio. = biofeedback",
          cells_column_labels(columns = c(
            `p-val. pas. vs. bio.`, `p-val. med(x) vs. bio.`, `SMD diff. med(x) vs. bio.`
          ))
        ) |>
        tab_footnote(
          footnote = "pas. = passive control",
          cells_column_labels(columns = c(
            `p-val. pas. vs. cog.`:`p-val. pas. vs. walk`, `SMD diff. med(x) vs. pas.`
          ))
        ) |>
        tab_footnote(
          footnote = "cog. = cognitive control",
          cells_column_labels(columns = c(
            `p-val. pas. vs. cog.`, `p-val. med(x) vs. cog.`, `SMD diff. med(x) vs. cog.`
          ))
        ) |>
        tab_footnote(
          footnote = "dog = dog threapy",
          cells_column_labels(columns = c(
            `p-val. pas. vs. dog`, `p-val. med(x) vs. dog`, `SMD diff. med(x) vs. dog`
          ))
        ) |>
        tab_footnote(
          footnote = "med(x) = exclusive meditation",
          cells_column_labels(columns = c(
            `p-val. pas. vs. med(x)`, `p-val. med(x) vs. cog.`:`SMD diff. med(x) vs. walk`
          ))
        ) |>
        tab_footnote(
          footnote = "med(m) = meditation with movment",
          cells_column_labels(columns = c(
            `p-val. pas. vs. med(m)`, `p-val. med(x) vs. med(m)`, `SMD diff. med(x) vs. med(m)`
          ))
        ) |>
        tab_footnote(
          footnote = "PMR = Progressive Muscle Relaxation (by Jacobson)",
          cells_column_labels(columns = c(
            `p-val. pas. vs. PMR`, `p-val. med(x) vs. PMR`, `SMD diff. med(x) vs. PMR`
          ))
        ) |>
        tab_footnote(
          footnote = "stress = stress management",
          cells_column_labels(columns = c(
            `p-val. pas. vs. stress`, `p-val. med(x) vs. stress`, `SMD diff. med(x) vs. stress`
          ))
        ) |>
        tab_footnote(
          footnote = "SMD diff. = differnce of standardized mean differences",
          cells_column_labels(columns = `SMD diff. med(x) vs. cog.`:`SMD diff. med(x) vs. walk`)
        )
      
        gtsave(
          gt.object,
          paste(
            saving.path, "Sens.anal.table.network.all", ".",
            save.as, sep = ""
          )
        )      
    }
  } else {
    print("error in get.sens.anal.df(): set parameter 'model' to 'rand.fix', 'mixed', 'subgroup', or 'net'")
  }
  return(sens.anal.df)
}

# %% [markdown] heading_collapsed=true
# ## Get all sensitivity data frames

# %% hidden=true vscode={"languageId": "r"}
# # for all outcomes / for the comparison of exclusive meditation vs. passive control; Google Chrome has to be installed to get png images
# saving.path <- r"(C:\Users\anonymous\Documents\GitHub\MA_Meta_Analyses\Sensitivity Analysis tables\)"
# for (outcome in present.outcomes.sorted){
#   for (model in c("rand.fix", "mixed", "subgroup")){
#     if (model == "rand.fix"){
#       get.sens.anal.df(outcome, model, c(), "png", saving.path)
#     } else if (model == "mixed" & outcome %in% c("Anxiety", "Depression", "Stress", "Mindfulness")){
#       for (moderator in c("sessions.duration", "sessions.frequency", "programs.duration", "follow.up.period", "delivery.mode", "meditation.type")){
#         if (
#           (outcome == "Anxiety" & moderator == "follow.up.period") |  # moderator follow.up.period has not enough data for anxiety
#           (outcome == "Mindfulness" & moderator != "follow.up.period")  # only moderator follow.up.period has enough data for mindfulness
#         ){next}
#         get.sens.anal.df(outcome, model, c(moderator), "png", saving.path)
#       }
#     } else if (model == "subgroup" & outcome %in% c("Anxiety", "Stress", "Mindfulness", "Depression")){
#       for (subgroup in c("delivery.mode", "meditation.type")){
#         get.sens.anal.df(outcome, model, c(), "png", saving.path, subgroup)
#       }
#     } else if (model == "mixed" & outcome != "Stress"){
#       # no regressions for these outcomes
#     } else if (model == "subgroup" & outcome != "Stress"){
#       # no subgroup analysis for these outcomes
#     } else {
#       cat("unmentioned case:", outcome, model, "\n")
#     }
#   }
# }

# # for network meta-analysis of all outcomes in one model
# net.sens.anal.df <- get.sens.anal.df(present.outcomes, "net", save.as = "html", saving.path = saving.path)

# %% hidden=true vscode={"languageId": "r"}
# for network meta-analysis of all outcomes in one model
net.sens.anal.df <- get.sens.anal.df(present.outcomes, "net", save.as = "html", saving.path = r"(C:\Users\anonymous\Documents\GitHub\MA_Meta_Analyses\Sensitivity Analysis tables\)")

# %% [markdown] vscode={"languageId": "r"}
# ## Summary table for different subgroups

# %% vscode={"languageId": "r"}
# define rownames and rownames NA vector for sub_summary_df
rnames <- c(	
  "Overall",
  "synchronous guiding",
  "asynchronous guiding",
  "constructive family",
  "attentional family",
  "hybrid form"
)

NAs = rep(NA, length(rnames))

i <- 1
for (outcome in c("Anxiety", "Depression", "Stress", "Mindfulness")){

  # create empty df
  sub_summary_df <- data.frame(
    `Hedge’s g` = NAs,
    `95%CI` = NAs,
    P_value = NAs,
    Int_n = NAs,
    Con_n = NAs,
    Total_n = NAs,
    Q = NAs,
    I2 = NAs,
    K = NAs,
    row.names = rnames
  )

  # calculate results
  res <- print.meta.results(
    outcome, preferred.scale = get.1st.preferred.scale(outcome),
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
    split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "results.meta"
  )

  res_fam <- print.meta.results(
    outcome, preferred.scale = get.1st.preferred.scale(outcome),
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
    split.subgroups = T, print.forest.sub.single = "meditation.type", print.meta.results = F,
    return.data = "results.meta"
  )

  res_del <- print.meta.results(
    outcome, preferred.scale = get.1st.preferred.scale(outcome),
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
    split.subgroups = T, print.forest.sub.single = "delivery.mode", print.meta.results = F,
    return.data = "results.meta"
  )

  # insert values into sub_summary_df
  for (row_i in seq_row(sub_summary_df)){
    group <- rownames(sub_summary_df)[row_i]

    if (group == "Overall"){
      if (!is.null(res)){
        row_overall <- c(
          res$TE.random, paste(round(res$lower.random, 2), "-", round(res$upper.random, 2)), res$pval.random,
          sum(res$n.e), sum(res$n.c), sum(res$n.e) + sum(res$n.c), res$Q, res$I2 * 100, res$k
        )
        sub_summary_df[row_i,] <- row_overall
      }
      

    } else if (group %in% c("synchronous guiding", "asynchronous guiding")){
      if (!is.null(res_del)){
        row_group <- c(
          res_del$TE.random.w[group], paste(round(res_del$lower.random.w[group], 2), "-", round(res_del$upper.random.w[group], 2)), res_del$pval.random.w[group],
          sum(res_del$n.e.w[group]), sum(res_del$n.c.w[group]), sum(res_del$n.e.w[group]) + sum(res_del$n.c.w[group]),
          res_del$Q.w[group], res_del$I2.w[group] * 100, res_del$k.w[group]
        )
        sub_summary_df[row_i,] <- row_group
      }
      

    } else if (group %in% c("hybrid form", "attentional family", "deconstructive family", "constructive family")){
      if (!is.null(res_fam)){
        row_group <- c(
          res_fam$TE.random.w[group], paste(round(res_fam$lower.random.w[group], 2), "-", round(res_fam$upper.random.w[group], 2)), res_fam$pval.random.w[group],
          sum(res_fam$n.e.w[group]), sum(res_fam$n.c.w[group]), sum(res_fam$n.e.w[group]) + sum(res_fam$n.c.w[group]),
          res_fam$Q.w[group], res_fam$I2.w[group] * 100, res_fam$k.w[group]
        )
        sub_summary_df[row_i,] <- row_group
      }
      
    }
  }

  rownames(sub_summary_df) <- paste(outcome, rownames(sub_summary_df))

  if (i == 1){
    total_sub_summary_df <- sub_summary_df
  } else {
    total_sub_summary_df <- rbind(total_sub_summary_df, sub_summary_df)
  }
  i <- i + 1
}
total_sub_summary_df <- total_sub_summary_df |>
  mutate(
    across(everything(), ~ round(as.numeric(.), 2)),
    P_value = round(as.numeric(P_value), 3),
    X95.CI = total_sub_summary_df$X95.CI
  )

total_sub_summary_df

# %% [markdown] heading_collapsed=true
# ## Robustness Tables

# %% [markdown] heading_collapsed=true hidden=true
# ### Random and fixed effects models

# %% hidden=true vscode={"languageId": "r"}
# get df of all sensitivity analysis
i <- 1
for (outcome in present.outcomes.sorted){
  res.meta <- print.meta.results(
    outcome, preferred.scale = ifelse(outcome == "Stress", "DASS", F),
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
    split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "results.meta" # ,
    # filter.forest..funnel.vec = - outlier.list[["Stress"]]
  )
  if (res.meta$k >= 1){
    if (i == 1)
      sens.anal.df.all <- get.sens.anal.df(outcome, "rand.fix") |> mutate(outcome = outcome)
    else {
      sens.anal.df <- get.sens.anal.df(outcome, "rand.fix") |> mutate(outcome = outcome)
      sens.anal.df.all <- rbind(sens.anal.df.all, sens.anal.df)
    }
    i <- i + 1
  }
}
sens.anal.df.all

# %% hidden=true vscode={"languageId": "r"}
# generate df that shows if primary analyses are robust against analyzerd choices
sens.summary.df <- data.frame(
  c(unique(sens.anal.df.all$outcome)),
  c(NA),
  c(NA),
  c(NA),
  c(NA)
)
colnames(sens.summary.df) <- c(
  "Outcome", "SMD diff. from 0 in primary analysis", "SMD robust in sensitivity analysis",
  "Q-statistic suggest heterogeneity in primary analysis", "Q-statistic robust in sensitivity analysis"
)

i <- 1
yes <- "Y"
no <- "N"
for (outcome in unique(sens.anal.df.all$outcome)){
  sens.anal.df <- sens.anal.df.all[sens.anal.df.all$outcome == outcome,]
  
  # check significance of SMD in prim. analysis
  if (sens.anal.df[1, "p-value (testing SMD differs from zero)"] <= 0.05){
    sens.summary.df[i, "SMD diff. from 0 in primary analysis"] <- yes
    sig <- T
  } else {
    sens.summary.df[i, "SMD diff. from 0 in primary analysis"] <- no
    sig <- F
  }
  
  # check significances of SMD in sensitivity analysis and rate robustnes
  if (length(unique(sens.anal.df[-1, "p-value (testing SMD differs from zero)"] <= 0.05)) > 1){
    sens.summary.df[i, "SMD robust in sensitivity analysis"] <- no
  } else if (unique(sens.anal.df[-1, "p-value (testing SMD differs from zero)"] <= 0.05) == sig){
    sens.summary.df[i, "SMD robust in sensitivity analysis"] <- yes
  } else if (unique(sens.anal.df[-1, "p-value (testing SMD differs from zero)"] <= 0.05) != sig){
    sens.summary.df[i, "SMD robust in sensitivity analysis"] <- no
  } else {
    cat("not mentioned case in", outcome, "\n")
  }
  
  # check significance of Q-stats in prim. analysis
  if (sens.anal.df[1, "Number of included studies"] <= 1){
    sens.summary.df[i, "Q-statistic suggest heterogeneity in primary analysis"] <- "NA"
    sens.summary.df[i, "Q-statistic robust in sensitivity analysis"] <- "NA"
  } else {
    if (sens.anal.df[1, "p-value (testing between-study heterogeneity)"] <= 0.05){
      sens.summary.df[i, "Q-statistic suggest heterogeneity in primary analysis"] <- yes
      sig <- T
    } else {
      sens.summary.df[i, "Q-statistic suggest heterogeneity in primary analysis"] <- no
      sig <- F
    }

    # check significances of Q-stats in sensitivity analysis and rate robustness
    uni.q.s.05 <- unique(sens.anal.df[-1, "p-value (testing between-study heterogeneity)"] <= 0.05)
    uni.q.s.05 <- uni.q.s.05[which(!is.na(uni.q.s.05))]
  
    if (length(uni.q.s.05) > 1){
      sens.summary.df[i, "Q-statistic robust in sensitivity analysis"] <- no
    } else if (uni.q.s.05 == sig){
      sens.summary.df[i, "Q-statistic robust in sensitivity analysis"] <- yes
    } else if (uni.q.s.05 != sig){
      sens.summary.df[i, "Q-statistic robust in sensitivity analysis"] <- no
    } else {
      cat("not mentioned case in", outcome, "\n")
    }
  }
  i <- i + 1
}
sens.summary.df

# %% [markdown] heading_collapsed=true hidden=true
# ### Mixed effects models

# %% hidden=true vscode={"languageId": "r"}
# get df of all sensitivity analysis
i <- 1
for (outcome in c("Stress", "Anxiety")){
  for (moderator in c("sessions.duration", "sessions.frequency", "programs.duration", "follow.up.period")){
    if (outcome == "Anxiety" & moderator == "follow.up.period"){
      next
    }
    if (i == 1)
      sens.anal.df.all <- get.sens.anal.df(outcome, "mixed", moderator) |> mutate(outcome = outcome) |> mutate(moderator = moderator)
    else {
      sens.anal.df <- get.sens.anal.df(outcome, "mixed", moderator) |> mutate(outcome = outcome) |> mutate(moderator = moderator)
      sens.anal.df.all <- rbind(sens.anal.df.all, sens.anal.df)
    }
    i <- i + 1
  }
}
sens.anal.df.all

# %% hidden=true vscode={"languageId": "r"}
# generate df that shows if primary analyses are robust against analyzerd choices
sens.summary.df <- data.frame(
  c(NA), c(NA), c(NA),
  c(NA), c(NA),
  c(NA), c(NA),
  c(NA), c(NA)
)
colnames(sens.summary.df) <- c(
  "Outcome", "Moderator", "lowest number of included studies",
  "Test of mod. sig. in primary analysis", "Test of mod. robust in sensitivity analysis",
  "Sign of mod. CI in primary analysis", "Sign of mod. CI robust in sensitivity analysis",
  "Q-statistic suggest heterogeneity in primary analysis", "Q-statistic for het. robust in sensitivity analysis"
)

i <- 1
yes <- "Y"
no <- "N"
minus <- "-"
plus <- "+"
plus.minus <- "+/-"

for (outcome in unique(sens.anal.df.all$outcome)){
  for (moderator in unique(sens.anal.df.all$moderator)){
    if (outcome == "Anxiety" & moderator == "follow.up.period"){
      next
    }
    
    sens.summary.df[i, 1] <- outcome
    sens.summary.df[i, 2] <- moderator
    
    sens.anal.df <- sens.anal.df.all[
      sens.anal.df.all$outcome == outcome & sens.anal.df.all$moderator == moderator,
    ]
    
    sens.summary.df[i, 3] <- min(sens.anal.df$`Number of included studies`)
    
    # check significance of Test of mod. in prim. analysis
    if (sens.anal.df[1, "p-value for omnibus-test of moderators"] <= 0.05){
      sens.summary.df[i, "Test of mod. sig. in primary analysis"] <- yes
      sig <- T
    } else {
      sens.summary.df[i, "Test of mod. sig. in primary analysis"] <- no
      sig <- F
    }

    # check significances of Test of mod. in sensitivity analysis and rate robustnes
    if (length(unique(sens.anal.df[-1, "p-value for omnibus-test of moderators"] <= 0.05)) > 1){
      sens.summary.df[i, "Test of mod. robust in sensitivity analysis"] <- no
    } else if (unique(sens.anal.df[-1, "p-value for omnibus-test of moderators"] <= 0.05) == sig){
      sens.summary.df[i, "Test of mod. robust in sensitivity analysis"] <- yes
    } else if (unique(sens.anal.df[-1, "p-value for omnibus-test of moderators"] <= 0.05) != sig){
      sens.summary.df[i, "Test of mod. robust in sensitivity analysis"] <- no
    } else {
      cat("not mentioned case in", outcome, moderator, "\n")
    }

    # check significance of Q-stat in prim. analysis
    if (sens.anal.df[1, "Number of included studies"] <= 1){
      sens.summary.df[i, "Q-statistic suggest heterogeneity in primary analysis"] <- "NA"
      sens.summary.df[i, "Q-statistic for het. robust in sensitivity analysis"] <- "NA"
    } else {
      if (sens.anal.df[1, "p-value (testing residual heterogeneity)"] <= 0.05){
        sens.summary.df[i, "Q-statistic suggest heterogeneity in primary analysis"] <- yes
        sig <- T
      } else {
        sens.summary.df[i, "Q-statistic suggest heterogeneity in primary analysis"] <- no
        sig <- F
      }

      # check significances of Q-stat in sensitivity analysis and rate robustness
      uni.q.s.05 <- unique(sens.anal.df[-1, "p-value (testing residual heterogeneity)"] <= 0.05)
      uni.q.s.05 <- uni.q.s.05[which(!is.na(uni.q.s.05))]

      if (length(uni.q.s.05) > 1){
        sens.summary.df[i, "Q-statistic for het. robust in sensitivity analysis"] <- no
      } else if (uni.q.s.05 == sig){
        sens.summary.df[i, "Q-statistic for het. robust in sensitivity analysis"] <- yes
      } else if (uni.q.s.05 != sig){
        sens.summary.df[i, "Q-statistic for het. robust in sensitivity analysis"] <- no
      } else {
        cat("not mentioned case in", outcome, moderator, "\n")
      }
    }
    
    # check for sign within CI of moderator
    mod.CI.low <- sens.anal.df[1, 'Moderator\'s coefficient CI lower threshold']
    mod.CI.up <- sens.anal.df[1, 'Moderator\'s coefficient CI upper threshold']
    if (mod.CI.low < 0 & mod.CI.up < 0){
      sens.summary.df[i, "Sign of mod. CI in primary analysis"] <- minus
      sig <- minus
    } else if (mod.CI.low > 0 & mod.CI.up > 0){
      sens.summary.df[i, "Sign of mod. CI in primary analysis"] <- plus
      sig <- plus
    } else {
      sens.summary.df[i, "Sign of mod. CI in primary analysis"] <- plus.minus
      sig <- plus.minus
    }
    
    sens.anal.df.no.sq <- sens.anal.df[substr(sens.anal.df$`Decision Codes`, nchar(sens.anal.df$`Decision Codes`) - 1, nchar(sens.anal.df$`Decision Codes`)) != "0",]
      # cut squared models as meaning of sign is other than in linear model
    uni.m.sig <- unique(sens.anal.df.no.sq[-1, c('Moderator\'s coefficient CI lower threshold', 'Moderator\'s coefficient CI upper threshold')] < 0)
    uni.m.sig <- uni.m.sig[which(!is.na(uni.m.sig))]

    if (length(uni.m.sig) > 1 & sig != plus.minus){
      sens.summary.df[i, "Sign of mod. CI robust in sensitivity analysis"] <- no
    } else if (length(uni.m.sig) > 1 & sig == plus.minus){
      sens.summary.df[i, "Sign of mod. CI robust in sensitivity analysis"] <- yes
    } else if (uni.m.sig == sig){
      sens.summary.df[i, "Sign of mod. CI robust in sensitivity analysis"] <- yes
    } else if (uni.m.sig != sig){
      sens.summary.df[i, "Sign of mod. CI robust in sensitivity analysis"] <- no
    } else {
      cat("not mentioned case in", moderator, outcome, "\n")
    }
    
    i <- i + 1
  }
}
rownames(sens.summary.df) <- NULL
sens.summary.df  # check sign robustness

# %% [markdown] heading_collapsed=true hidden=true
# ### Tests of subgroup differences

# %% hidden=true vscode={"languageId": "r"}
# get df of all sensitivity analysis
i <- 1
sens.anal.df.list <- list()
for (outcome in outcomes.no.10.plus.passive){
  for (moderator in c("delivery.mode", "meditation.type")){
    for (model in c("mixed", "subgroup")){
      sens.anal.df.list[[i]] <- get.sens.anal.df(outcome, model, moderator, subgroup = moderator)
      names(sens.anal.df.list)[i] <- paste(outcome, moderator, model)
      i <- i + 1
    }
  }
}
sens.anal.df.list

# %% hidden=true vscode={"languageId": "r"}
# generate df that shows if primary analyses are robust against analyzerd choices (works only for the mixed-effects and subgroup differences models)
sens.summary.df <- data.frame(
  c(NA), c(NA), c(NA),
  c(NA), c(NA),
  c(NA), c(NA)
)
colnames(sens.summary.df) <- c(
  "Outcome", "Moderator", "Lowest number of included studies",
  "Test of sub. diff. in primary analysis", "Test of mod./test of sub. diff. robust in sensitivity analysis",
  "Q-statistic suggest heterogeneity in primary analysis", "Q-statistic for het. robust in sensitivity analysis"
)

i <- 1
yes <- "Y"
no <- "N"

# get present outcomes, mdoerators, and models by naming of sens.anal.df.list (these thre dimensions has to be divided by " " with in the names of the list)
names.df <- data.frame(
  str_split(names(sens.anal.df.list), " ")  # split names of sens.anal.df.list by ""
)

outcomes <- unique(unlist(names.df[1,]))  # present outcomes are in first row...
moderators <- unique(unlist(names.df[2,]))
models <- unique(unlist(names.df[3,]))


for (outcome in outcomes){
  for (moderator in moderators){
    
    
    sens.summary.df[i, 1] <- outcome
    sens.summary.df[i, 2] <- moderator
    
    df.l.names <- names(sens.anal.df.list)
    
    # data frame for subgroup differences model (which was used in primary analysis)
    sens.anal.df.sub <- sens.anal.df.list[[
      which(grepl(outcome, df.l.names) & grepl(moderator, df.l.names) & grepl("subgroup", df.l.names))
    ]]
    
    # data frame for mixed-effects model 
    sens.anal.df.mix <- sens.anal.df.list[[
      which(grepl(outcome, df.l.names) & grepl(moderator, df.l.names) & grepl("mixed", df.l.names))
    ]]

    sens.summary.df[i, 3] <- min(c(
      unlist(sens.anal.df.sub$`Number of included studies`),
      unlist(sens.anal.df.mix$`Number of included studies`)
    ))

    # check significance of Test of mod. in prim. analysis
    if (sens.anal.df.sub[1, "p-value (between group)"] <= 0.05){
      sens.summary.df[i, "Test of sub. diff. in primary analysis"] <- yes
      sig <- T
    } else {
      sens.summary.df[i, "Test of sub. diff. in primary analysis"] <- no
      sig <- F
    }
    
    no.flag <- F
    for (model in models){
      if (!no.flag){
        if (model == "subgroup"){
          sens.anal.df <- sens.anal.df.sub
          p.diff.name <- "p-value (between group)"
        } else if (model == "mixed") {
          sens.anal.df <- sens.anal.df.mix
          p.diff.name <- "p-value for omnibus-test of moderators"
        } else {
          print("ERROR #1")
        }
        # check significances of Test of mod. or test of sub. diff. in sensitivity analysis and rate robustnes
        if (length(unique(sens.anal.df[-1, p.diff.name] <= 0.05)) > 1){
          sens.summary.df[i, "Test of mod./test of sub. diff. robust in sensitivity analysis"] <- no
          no.flag <- T
        } else if (unique(sens.anal.df[-1, p.diff.name] <= 0.05) == sig){
          sens.summary.df[i, "Test of mod./test of sub. diff. robust in sensitivity analysis"] <- yes
        } else if (unique(sens.anal.df[-1, p.diff.name] <= 0.05) != sig){
          sens.summary.df[i, "Test of mod./test of sub. diff. robust in sensitivity analysis"] <- no
          no.flag <- T
        } else {
          cat("not mentioned case in", outcome, moderator, "\n")
        }
      }
    }

    # check significance of Q-stat in prim. analysis
    if (sens.anal.df.sub[1, "Number of included studies"] <= 1){
      sens.summary.df[i, "Q-statistic suggest heterogeneity in primary analysis"] <- "NA"
      sens.summary.df[i, "Q-statistic for het. robust in sensitivity analysis"] <- "NA"
    } else {

      # columns in which the respective p-values are present
      col.i.Q.stat <- which(
        grepl(
          gsub("([()])","\\\\\\1", "p-value (testing between-study heterogeneity)"),  # gsub("([()])","\\\\\\1", ) makes parenthesis searchable
          colnames(sens.anal.df.sub)
        )
      )

      # if one of the between study p-values is below or equal to 0.05
      if (T %in% (unlist(sens.anal.df.sub[1, col.i.Q.stat]) <= 0.05)){
        sens.summary.df[i, "Q-statistic suggest heterogeneity in primary analysis"] <- yes
        sig <- T
      } else {
        sens.summary.df[i, "Q-statistic suggest heterogeneity in primary analysis"] <- no
        sig <- F
      }

      # check significances of Q-stat in sensitivity analysis and rate robustness
      no.flag <- F
      for (model in models){
        if (!no.flag){
          if (model == "subgroup"){
            uni.q.s.05 <- unique(unlist(sens.anal.df.sub[-1, col.i.Q.stat]) <= 0.05)
          } else {
            uni.q.s.05 <- unique(unlist(sens.anal.df.mix[-1, "p-value (testing residual heterogeneity)"]) <= 0.05)
          }

          uni.q.s.05 <- uni.q.s.05[which(!is.na(uni.q.s.05))]

          if (length(uni.q.s.05) > 1){
            sens.summary.df[i, "Q-statistic for het. robust in sensitivity analysis"] <- no
            no.flag <- T
          } else if (uni.q.s.05 == sig){
            sens.summary.df[i, "Q-statistic for het. robust in sensitivity analysis"] <- yes
          } else if (uni.q.s.05 != sig){
            sens.summary.df[i, "Q-statistic for het. robust in sensitivity analysis"] <- no
            no.flag <- T
          } else {
            cat("not mentioned case in", outcome, moderator, "\n")
          }
        }
      }
    }

    i <- i + 1
  }
}
rownames(sens.summary.df) <- NULL
sens.summary.df

# %% [markdown] heading_collapsed=true
# ## Investigation of outlier/influential case characteristics compared to rest of included studies of the respective outcomes (random-effects models)

# %% [markdown] heading_collapsed=true hidden=true
# ### Devillers-Réolon 2022

# %% hidden=true vscode={"languageId": "r"}
study.names.stress.anx.depr <- sort(unique(c(
  print.meta.results(
    "Stress", preferred.scale = "DASS",
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "results.meta"
  )$studlab,
  print.meta.results(
    "Anxiety", preferred.scale = get.1st.preferred.scale("Anxiety"),
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "results.meta"
  )$studlab,
  print.meta.results(
    "Depression", preferred.scale = get.1st.preferred.scale("Depression"),
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "results.meta"
  )$studlab
)))
study.names.stress.anx.depr

# %% hidden=true vscode={"languageId": "r"}
get.part.desc.by.stud(study.names.stress.anx.depr)
get.int.char.by.stud(study.names.stress.anx.depr)

# %% hidden=true vscode={"languageId": "r"}
get.part.desc.by.stud("Devillers-Réolon 2022")
get.int.char.by.stud("Devillers-Réolon 2022")

# %% [markdown] heading_collapsed=true hidden=true
# ### Bonamo 2015

# %% hidden=true vscode={"languageId": "r"}
study.names.mindf <- sort(unique(
  print.meta.results(
    "Mindfulness",
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "results.meta"
  )$studlab
))
study.names.mindf

# %% hidden=true vscode={"languageId": "r"}
get.part.desc.by.stud(study.names.mindf)
get.int.char.by.stud(study.names.mindf)

# %% hidden=true vscode={"languageId": "r"}
get.part.desc.by.stud("Bonamo 2015")
get.int.char.by.stud("Bonamo 2015")

# %% hidden=true vscode={"languageId": "r"}
print.meta.results(
  "Mindfulness",
  basic = F, moderator.vec = c("female.percent"), print.regplot = T, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
  regression.label = T, return.data = "regression.results.linear"
)

# %% hidden=true vscode={"languageId": "r"}
print.meta.results(
  "Mindfulness",
  basic = F, moderator.vec = c("female.percent"), print.regplot = T, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
  regression.label = T, return.data = "regression.results.linear",
  filter.regression.linear.list = list(-c(9, 10))
)

# %% [markdown] heading_collapsed=true
# ## Overall results

# %% hidden=true vscode={"languageId": "r"}
res.overall <- get.overall.res.metafor()

# %% hidden=true vscode={"languageId": "r"}
res.overall

# %% hidden=true vscode={"languageId": "r"}
# Get influence plot for overall results
options(repr.plot.width = 20, repr.plot.height = 7, repr.plot.res = 150)
plot.influnece(res.overall)

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 20, repr.plot.height = 7, repr.plot.res = 150)
plot.influnece(res.overall, cluster = "study.id")

# %% hidden=true vscode={"languageId": "r"}
plot.influnece(res.overall, cluster = "outcome")

# %% hidden=true vscode={"languageId": "r"}
# overall results with and without outliers/influential cases
res.overall.n.o. <- get.overall.res.metafor(-outlier.list$overall)


# %% hidden=true vscode={"languageId": "r"}
forest(res.overall.n.o.)

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 6, repr.plot.height = 7, repr.plot.res = 150)
forest(res.overall)

# %% hidden=true vscode={"languageId": "r"}
# use trim and fill metheod to insert potential missing studies due to publication bias
options(repr.plot.width = 15, repr.plot.height = 7, repr.plot.res = 150)
study.labels <- res.overall$data$id
study.labels[-outlier.list$overall] <- ""
funnel(  # with paramter slab adjusted
  res.overall, legend = T,  yaxis="seinv", label = T, slab = study.labels,
  level=c(90, 95, 99), shade=c("white", "gray55", "gray75"),
  refline=0, lty = 0, refline2 = res.overall$b[1,1], lty2 = 3
    # results.meta$TE.random = overall effect size of the random effects model
)

# %% hidden=true vscode={"languageId": "r"}
# funnel plot asymmetry
# regtest() is not available for multivariate models in metafor
ranktest(res.overall)

# %% hidden=true vscode={"languageId": "r"}
forest(get.overall.res.metafor(outlier.list$overall))

# %% hidden=true vscode={"languageId": "r"}
res.overall; res.overall.n.o.

# %% [markdown] heading_collapsed=true
# ## [All Outcomes] Overall network meta-analysis

# %% hidden=true vscode={"languageId": "r"}
# install.packages("igraph")
library(igraph)

# %% [markdown] heading_collapsed=true hidden=true
# ### Investigating different levels of tolerance for consistency of treatment estimates in multi-arm studies

# %% hidden=true vscode={"languageId": "r"}
# net.res.all.no.inc <- net.meta.analyze(
#   present.outcomes, preferred.scale = F, net.df = F, net.res = F, filter.forest..funnel.vec = c(
#     "Flett 2019a", "Messer 2016", "Spruin 2021", "Tloczynski 1994", "Waechter 2021", "Wang 2021", "Weytens 2014"  # studies with multi-arm inconsistancy
#   ),
#   details.chkmultiarm = T, tol.multiarm = 0.001,
#   plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
#   return.data = "net.res", reference.group = "passive control", random = T, silent = T
# )

# %% hidden=true vscode={"languageId": "r"}
# net.res.all.no.inc.01 <- net.meta.analyze(
#   present.outcomes, preferred.scale = F, net.df = F, net.res = F, filter.forest..funnel.vec = c(
#     "Messer 2016", "Spruin 2021", "Waechter 2021"  # studies with multi-arm inconsistancy
#   ),
#   details.chkmultiarm = T, tol.multiarm = 0.01,
#   plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
#   return.data = "net.res", reference.group = "passive control", random = T, silent = T
# )

# %% hidden=true vscode={"languageId": "r"}
# netgraph(net.res.all); forest(net.res.all); net.res.all

# %% hidden=true vscode={"languageId": "r"}
# netgraph(net.res.all.no.inc.01); forest(net.res.all.no.inc.01, order = "fit"); net.res.all.no.inc.01

# %% hidden=true vscode={"languageId": "r"}
# netgraph(net.res.all.no.inc); forest(net.res.all.no.inc, order = "fit"); net.res.all.no.inc

# %% hidden=true vscode={"languageId": "r"}
# results:
# relaxing tolerance for consistency of treatment estimates in multi-arm studies to .01 let to ...
# ... non sig. inconsistancy compared to relxing it totally (to 10)
# ... no loss of precision (range of CI) in rest vs. pas cont and cog. cont vs. pas cont
#
# conclusion:
# setting this tolerance to .01 seems to be a good trade of betwen keeping data and inconsistancy

# %% [markdown] heading_collapsed=true hidden=true
# ### Investigating inconsistancy

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 10, repr.plot.height = 10, repr.plot.res = 200)
netheat(net.res.all, nchar.trts = 3)

# %% hidden=true vscode={"languageId": "r"}
# --> comparison passive control vs. stressmanagement causes inconsistancy

# %% hidden=true vscode={"languageId": "r"}
net.res.all.split <- netsplit(net.res.all)
net.res.all.split

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 10, repr.plot.height = 20, repr.plot.res = 150)
plot(net.res.all.split)

# %% hidden=true vscode={"languageId": "r"}
direct.evidence.plot(net.res.all, random = T)

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 27, repr.plot.height = 9, repr.plot.res = 150)
plot(direct.evidence.plot(net.res.all, random = T, subplot.ratio=c(5, 1.3, 1.3)))

# %% hidden=true vscode={"languageId": "r"}
# comparisons with heterogneity
# - biofeedback vs. meditation (exclusive)
# - biofeedback vs. passive control
# - meditation (exclusive) vs. passive control
# - meditation (exclusive) vs. rest
# - meditation (exclusive) vs. rest

# inconsistancy
# dog therapy

# %% [markdown] heading_collapsed=true hidden=true
# ### Funnel Plot

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 10, repr.plot.height = 7, repr.plot.res = 150)
funnel(
  net.res.all, order = "passive control", # method.bias = "Egger",
  legend = T,  yaxis="invse", col = c(
    "blue", "red", "purple", "forestgreen", "aquamarine", 
    "gold4", "black", "brown"
#     "orange", "pink", 
#     "khaki", "plum", , "sandybrown", 
#     "coral", "gold4"
  )
)
#legend("topright", legend = levels(as.factor(net.res.all$comparison)), cex = 0.6)

# %% [markdown] heading_collapsed=true hidden=true
# ### Reporting (League Table and nettable)

# %% hidden=true vscode={"languageId": "r"}
# table of p-values comparing all treatments
round(net.res.all$pval.random, 2)

# %% hidden=true vscode={"languageId": "r"}
# league table
league.tab <- netleague(net.res.all)
league.tab$random

# %% hidden=true vscode={"languageId": "r"}
# netimpact.res <- netimpact(net.res.all)

# %% hidden=true vscode={"languageId": "r"}
# reporting
nettable(net.res.all)

# %% hidden=true vscode={"languageId": "r"}
# get table of SMDs
net.smd.df <- data.frame(matrix(".", nrow = net.res.all$n, ncol = net.res.all$n))

for(i in 1:nrow(net.smd.df)){
  for(j in 1:ncol(net.smd.df)){
    if (i < j){
      next
    }
    # Concatenate the contents and assign to the new data frame
    net.smd.df[i,j] <- paste(
      as.character(round(net.res.all$TE.random[i,j], 2)), " [",
      as.character(round(net.res.all$lower.random[i,j], 2)), ", ",
      as.character(round(net.res.all$upper.random[i,j], 2)), "]",
      sep = ""
    )
  }
}
rownames(net.smd.df) <- net.res.all$trts
colnames(net.smd.df) <- net.res.all$trts
net.smd.df

# %% [markdown] heading_collapsed=true hidden=true
# ### Cutting out Studies due to results of netheat and netsplit

# %% [markdown] heading_collapsed=true hidden=true
# #### Inconsistancy

# %% hidden=true vscode={"languageId": "r"}
net.res.n.dt.pc <- net.meta.analyze(
  present.outcomes, preferred.scale = F, net.df = F, net.res = F, comparisons.skip.list = list(
    list(cont.active.dog, cont.active.dog)
  ),
  plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = T,
  reference.group = "passive control", random = T, return.data = "net.res"
)

# %% hidden=true vscode={"languageId": "r"}
net.res.n.dt.pc

# %% hidden=true vscode={"languageId": "r"}
# inconsistancy was due to dog therapy (Spruin 2021 = only study)

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 10, repr.plot.height = 20, repr.plot.res = 150)
plot(netsplit(net.res.all))

# %% [markdown] heading_collapsed=true hidden=true
# #### Heterogeneity

# %% hidden=true vscode={"languageId": "r"}
# comparisons with heterogneity
# - biofeedback vs. meditation (exclusive)
# - biofeedback vs. passive control
# - meditation (exclusive) vs. passive control
# - meditation (exclusive) vs. rest

# %% [markdown] heading_collapsed=true hidden=true
# ##### Biofeedback

# %% hidden=true vscode={"languageId": "r"}
overall.res.m.vs.b <- get.overall.res.metafor(comparison.list = list(meditation.type.all, cont.active.bio.feedback))
overall.res.m.vs.b$data  # Ratanasiripong 2015 only study using biofeedback

# %% [markdown] heading_collapsed=true hidden=true
# ##### Meditation (exclusive) vs. passive control

# %% hidden=true vscode={"languageId": "r"}
unique(res.overall$data[outlier.list$overall,"study.id"])

# %% [markdown] heading_collapsed=true hidden=true
# ##### Meditation (exclusive) vs. rest

# %% hidden=true vscode={"languageId": "r"}
overall.res.m.vs.r <- get.overall.res.metafor(comparison.list = list(meditation.type.all, cont.active.rest))
overall.res.m.vs.r$data

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 10, repr.plot.height = 8, repr.plot.res = 150)
forest(overall.res.m.vs.r)

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 10, repr.plot.height = 4, repr.plot.res = 150)
plot.influnece(overall.res.m.vs.r, cluster = "study.id")

# %% hidden=true vscode={"languageId": "r"}
# use trim and fill metheod to insert potential missing studies due to publication bias
options(repr.plot.width = 15, repr.plot.height = 7, repr.plot.res = 150)
study.labels <- overall.res.m.vs.r$data$id
funnel(  # with paramter slab adjusted
  overall.res.m.vs.r, legend = T,  yaxis="seinv", label = T, slab = study.labels,
  level=c(90, 95, 99), shade=c("white", "gray55", "gray75"),
  refline=0, lty = 0, refline2 = overall.res.m.vs.r$b[1,1], lty2 = 3
    # results.meta$TE.random = overall effect size of the random effects model
)

# %% hidden=true vscode={"languageId": "r"}
overall.res.m.vs.r$data[3,]

# %% hidden=true vscode={"languageId": "r"}
overall.res.m.vs.r.n.o <- get.overall.res.metafor(comparison.list = list(meditation.type.all, cont.active.rest), filter.forest..funnel.vec = c("Silvestre-López 2021", "Ramsburg 2014"))
overall.res.m.vs.r.n.o; overall.res.m.vs.r
# cuting out 4 of 10 studies to reduce heterogeneity
# --> heterogneity can not be aussumed to relay on small porportion of dataset 
# maybe to less outcomes included to map resilience

# %% [markdown] heading_collapsed=true hidden=true
# ##### Meditation (exclusive) vs. stress management

# %% hidden=true vscode={"languageId": "r"}
overall.res.m.vs.str <- get.overall.res.metafor(comparison.list = list(meditation.type.all, cont.active.stress.man))
overall.res.m.vs.str

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 10, repr.plot.height = 9, repr.plot.res = 200)
forest(overall.res.m.vs.str)

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 10, repr.plot.height = 4, repr.plot.res = 150)
plot.influnece(overall.res.m.vs.str, cluster = "study.id")

# %% hidden=true vscode={"languageId": "r"}
# use trim and fill metheod to insert potential missing studies due to publication bias
options(repr.plot.width = 15, repr.plot.height = 7, repr.plot.res = 150)
study.labels <- overall.res.m.vs.str$data$id
funnel(  # with paramter slab adjusted
  overall.res.m.vs.str, legend = T,  yaxis="seinv", label = T, slab = study.labels,
  level=c(90, 95, 99), shade=c("white", "gray55", "gray75"),
  refline=0, lty = 0, refline2 = overall.res.m.vs.r$b[1,1], lty2 = 3
    # results.meta$TE.random = overall effect size of the random effects model
)

# %% hidden=true vscode={"languageId": "r"}
overall.res.m.vs.str$data[c(1, 17),]

# %% hidden=true vscode={"languageId": "r"}
overall.res.m.vs.str$data[overall.res.m.vs.str$data$study.id == "Weytens 2014",]

# %% hidden=true vscode={"languageId": "r"}
overall.res.m.vs.str.n.o <- get.overall.res.metafor(
  comparison.list = list(meditation.type.all, cont.active.stress.man), filter.forest..funnel.vec = c('Klibert 2022', 'Spruin 2021')
)
overall.res.m.vs.str.n.o; overall.res.m.vs.str
# results are highly influenced by Weytens 2014 and Kim 2021 but cutting both out would lead to a too high data loss

# %% [markdown] heading_collapsed=true hidden=true
# ### Both

# %% hidden=true vscode={"languageId": "r"}
net.res.n.o <- net.meta.analyze(
  present.outcomes, preferred.scale = F, net.df = F, net.res = F,
  
  filter.forest..funnel.vec = c(
    "Spruin 2021",  # causing inconsistancy in comparisons with dog therapy
    "Ratanasiripong 2015", # causing heterogeneity in comparisons with biofeedback
    'Bultas 2021', 'Devillers-Réolon 2022', 'Huberty 2019', 'Messer 2016', 'Bonamo 2015',  # causing heterogeneity or have high influence in meditation (exclusive) vs. passive control
    "Silvestre-López 2021", "Ramsburg 2014",  # causing heterogeneity or have high influence  in meditation (exclusive) vs. rest
    'Klibert 2022', 'Spruin 2021'  # causing heterogeneity in meditation (exclusive) vs. stress management
  ),
  
  plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F, silent = T,
  reference.group = "passive control", random = T, return.data = "net.res"
)
net.res.n.o

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 10, repr.plot.height = 9, repr.plot.res = 200)
netheat(net.res.n.o, nchar.trts = 3)

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 10, repr.plot.height = 15, repr.plot.res = 200)
plot(netsplit(net.res.n.o))

# %% [markdown] heading_collapsed=true hidden=true
# ### Comparing network meta-analysis results with and without studies causing inconsistancy, heterogeneity, or have high influence on specific comparison results 

# %% hidden=true jupyter={"outputs_hidden": true} vscode={"languageId": "r"}
net.res.n.o; net.res.all

# %% hidden=true vscode={"languageId": "r"}
# plot forest plots
options(repr.plot.width = 6, repr.plot.height = 4, repr.plot.res = 200)

# passive control
forest(
  net.res.all, sortvar = TE,
  label.left = "may reduce resilience  ", label.right = "  may improve resilience",
  layout = "meta"
)
forest(
  net.res.n.o, sortvar = TE,
  label.left = "may reduce resilience  ", label.right = "  may improve resilience",
  layout = "meta"
)

# cognitive contorl
forest(
  reference.group = "cognitive control",
  net.res.all, sortvar = TE,
  label.left = "may reduce resilience  ", label.right = "  may improve resilience",
  layout = "meta"
)
forest(
  reference.group = "cognitive control",
  net.res.n.o, sortvar = TE,
  label.left = "may reduce resilience  ", label.right = "  may improve resilience",
  layout = "meta"
)

# rest
forest(
  reference.group = "rest",
  net.res.all, sortvar = TE,
  label.left = "may reduce resilience  ", label.right = "  may improve resilience",
  layout = "meta"
)
forest(
  reference.group = "rest",
  net.res.n.o, sortvar = TE,
  label.left = "may reduce resilience  ", label.right = "  may improve resilience",
  layout = "meta"
)

# %% hidden=true vscode={"languageId": "r"}
# net.res.all$TE.nma.random
net.res.all$TE.random[-c(1, 3, 4),"meditation (exclusive)"]

# %% hidden=true vscode={"languageId": "r"}
# comparing differences between interventions without specical cases
# data.frame(meditation = net.res.n.o$pval.random[, "meditation (exclusive)"])
cat("with special cases included")
data.frame(
  dif.to.pas.con.sig = net.res.all$pval.random[-c(1, 3, 4), "passive control"],
  dif.to.med.sig = net.res.all$pval.random[-c(1, 3, 4), "meditation (exclusive)"], # rows that do not occur in df below cut
  SMD.dif.to.med = round(net.res.all$TE.random[-c(1, 3, 4),"meditation (exclusive)"], 2)
)
data.frame(
  dif.to.pas.con.sig = net.res.all$pval.random[-c(1, 3, 4), "passive control"] <.05,
  dif.to.med.sig = net.res.all$pval.random[-c(1, 3, 4), "meditation (exclusive)"] <.05, # rows that do not occur in df below cut
  SMD.dif.to.med = round(net.res.all$TE.random[-c(1, 3, 4),"meditation (exclusive)"], 2)
)
cat("without special cases")
data.frame(
  dif.to.pas.con.sig = net.res.n.o$pval.random[- 2, "passive control"] <.05,  # is difference of these group to passive control significant?
  dif.to.med.sig = net.res.n.o$pval.random[- 2, "meditation (exclusive)"] <.05,  # row meditation (exclusive) cut
  SMD.dif.to.med = round(net.res.n.o$TE.random[-2,"meditation (exclusive)"], 2)
)

# %% [markdown] heading_collapsed=true hidden=true
# ### Investigate duplication of study labels due to multiple outcomes or interventions clustered together

# %% [markdown] heading_collapsed=true hidden=true
# #### comparison to passive controls

# %% hidden=true vscode={"languageId": "r"}
# find out present outcomes per treatment compared with passive control
outcomes.per.trt.df <- data.frame(matrix(NA, nrow = 2, ncol = net.res.all$n))  # NA data.frame for outcomes per treatment and respective number
rownames(outcomes.per.trt.df) <- list("included outcomes", "number of outcomes")
colnames(outcomes.per.trt.df) <- net.res.all$trts

for (trt in net.res.all$trts){
  outcomes.per.trt.vec <- sort(unique(
    (net.res.all$data |> filter(
      (treat1 == trt | treat2 == trt) &
      (treat1 == "passive control" | treat2 == "passive control")
    ))$outcome
  ))
  
  outcomes.per.trt.df[1, trt] <- paste(outcomes.per.trt.vec, collapse = ", ")
  outcomes.per.trt.df[2, trt] <- length(outcomes.per.trt.vec)
}
outcomes.per.trt.df

# %% hidden=true vscode={"languageId": "r"}
# get inflated study number and proportion of direct evidence
data.frame(
  comparison = net.res.all.split$comparison,
  k = net.res.all.split$k,
  prop = net.res.all.split$prop.random
) |> filter(
  grepl("exclusive", comparison) &
  grepl("passive", comparison) &
  k > 0
)

# %% hidden=true vscode={"languageId": "r"}
# get number of study labels of studies comparing meditation to passive controls
studlabs.med.vs.pas <- sort(unique(
  gsub(
    "\\ #.*","",  # searchs for " #" and deletes it and everything after it in the following strings of study labels comparing meditation to passive controls to which " #..." was added before
    (net.res.all$data |> filter(
      ("meditation (exclusive)" == treat1 | "meditation (exclusive)" == treat2) &
      ("passive control" == treat1 | "passive control" == treat2)
    ))$studlab
  )
))
studlabs.med.vs.pas
length(studlabs.med.vs.pas)

# %% hidden=true vscode={"languageId": "r"}
# get number of outcomes of studies comparing meditation to passive controls
outcomes.med.vs.pas <- sort(unique(
  (net.res.all$data |> filter(
    ("meditation (exclusive)" == treat1 | "meditation (exclusive)" == treat2) &
    ("passive control" == treat1 | "passive control" == treat2)
  ))$outcome
))
outcomes.med.vs.pas
length(outcomes.med.vs.pas)

# %% hidden=true vscode={"languageId": "r"}
# get number of split multi-arm studies comparing meditation to passive controls
## get df of studies at which study labels were duplicated
studlabs.med.vs.pas.df <- net.res.all$data |>
  filter(
    # filter for meditation and passive control
    ("meditation (exclusive)" == treat1 | "meditation (exclusive)" == treat2) &
    ("passive control" == treat1 | "passive control" == treat2) &
    
    # filter for # indicating duplication
    grepl("#", studlab)
  ) |>

  # cut " #" and everything after it in studlab
  mutate(studlab = gsub("\\ #.*","", studlab)) |>
  arrange(.studlab)

## filter out studies that were duplicated due to two interventions clustered to one
studlabs.med.vs.pas.no.muli.arm.df <-  studlabs.med.vs.pas.df |>
  distinct(studlab, outcome, .keep_all = T)

## get study label of these studies filtered out in the last step
studlabs.med.vs.pas.muli.arm <- unique(studlabs.med.vs.pas.df$studlab[
  !studlabs.med.vs.pas.df$.studlab %in% studlabs.med.vs.pas.no.muli.arm.df$.studlab
])
studlabs.med.vs.pas.muli.arm
length(studlabs.med.vs.pas.muli.arm)

# %% hidden=true vscode={"languageId": "r"}
# get inflated study number and proportion of direct evidence for comparisons against passive controls
data.frame(
  comparison = net.res.all.split$comparison,
  k = net.res.all.split$k,
  prop = net.res.all.split$prop.random
) |> filter(
  grepl("passive", comparison) &
  k > 0
)

# %% [markdown] heading_collapsed=true hidden=true
# #### cognitve controls

# %% hidden=true vscode={"languageId": "r"}
# get inflated study number and proportion of direct evidence for comparisons against cognitve controls
data.frame(
  comparison = net.res.all.split$comparison,
  k = net.res.all.split$k,
  prop = net.res.all.split$prop.random
) |> filter(
  grepl("cognitive", comparison) &
  k > 0
)

# %% hidden=true vscode={"languageId": "r"}
# get number of study labels of studies comparing meditation to cognitive controls
studlabs.med.vs.pas <- sort(unique(
  gsub(
    "\\ #.*","",  # searchs for " #" and deletes it and everything after it in the following strings of study labels comparing meditation to cognitive controls to which " #..." was added before
    (net.res.all$data |> filter(
      ("meditation (exclusive)" == treat1 | "meditation (exclusive)" == treat2) &
      ("cognitive control" == treat1 | "cognitive control" == treat2)
    ))$studlab
  )
))
studlabs.med.vs.pas
length(studlabs.med.vs.pas)

# %% hidden=true vscode={"languageId": "r"}
# get number of outcomes of studies comparing meditation to cognitive controls
outcomes.med.vs.pas <- sort(unique(
  (net.res.all$data |> filter(
    ("meditation (exclusive)" == treat1 | "meditation (exclusive)" == treat2) &
    ("cognitive control" == treat1 | "cognitive control" == treat2)
  ))$outcome
))
outcomes.med.vs.pas
length(outcomes.med.vs.pas)

# %% hidden=true vscode={"languageId": "r"}
# get number of split multi-arm studies comparing meditation to cognitive controls
## get df of studies at which study labels were duplicated
studlabs.med.vs.cog.df <- net.res.all$data |>
  filter(
    # filter for meditation and cognitive control
    ("meditation (exclusive)" == treat1 | "meditation (exclusive)" == treat2) &
    ("cognitive control" == treat1 | "cognitive control" == treat2) &
    
    # filter for # indicating duplication
    grepl("#", studlab)
  ) |>

  # cut " #" and everything after it in studlab
  mutate(studlab = gsub("\\ #.*","", studlab)) |>
  arrange(.studlab)

## filter out studies that were duplicated due to two interventions clustered to one
studlabs.med.vs.cog.no.muli.arm.df <-  studlabs.med.vs.cog.df |>
  distinct(studlab, outcome, .keep_all = T)

## get study label of these studies filtered out in the last step
studlabs.med.vs.cog.muli.arm <- unique(studlabs.med.vs.cog.df$studlab[
  !studlabs.med.vs.cog.df$.studlab %in% studlabs.med.vs.cog.no.muli.arm.df$.studlab
])
studlabs.med.vs.cog.muli.arm
length(studlabs.med.vs.cog.muli.arm)

# %% [markdown] heading_collapsed=true hidden=true
# #### rest

# %% hidden=true vscode={"languageId": "r"}
# get inflated study number and proportion of direct evidence for comparisons against rest
data.frame(
  comparison = net.res.all.split$comparison,
  k = net.res.all.split$k,
  prop = net.res.all.split$prop.random
) |> filter(
  grepl("rest", comparison) &
  k > 0
)

# %% hidden=true vscode={"languageId": "r"}
# get number of study labels of studies comparing meditation to rest
studlabs.med.vs.rest <- sort(unique(
  gsub(
    "\\ #.*","",  # searchs for " #" and deletes it and everything after it in the following strings of study labels comparing meditation to rest to which " #..." was added before
    (net.res.all$data |> filter(
      ("meditation (exclusive)" == treat1 | "meditation (exclusive)" == treat2) &
      ("rest" == treat1 | "rest" == treat2)
    ))$studlab
  )
))
studlabs.med.vs.rest
length(studlabs.med.vs.rest)

# %% hidden=true vscode={"languageId": "r"}
# get number of outcomes of studies comparing meditation to rest
outcomes.med.vs.rest <- sort(unique(
  (net.res.all$data |> filter(
    ("meditation (exclusive)" == treat1 | "meditation (exclusive)" == treat2) &
    ("rest" == treat1 | "rest" == treat2)
  ))$outcome
))
outcomes.med.vs.rest
length(outcomes.med.vs.rest)

# %% hidden=true vscode={"languageId": "r"}
# get number of split multi-arm studies comparing meditation to rests
## get df of studies at which study labels were duplicated
studlabs.med.vs.rest.df <- net.res.all$data |>
  filter(
    # filter for meditation and rest
    ("meditation (exclusive)" == treat1 | "meditation (exclusive)" == treat2) &
    ("rest" == treat1 | "rest" == treat2) &
    
    # filter for # indicating duplication
    grepl("#", studlab)
  ) |>

  # cut " #" and everything after it in studlab
  mutate(studlab = gsub("\\ #.*","", studlab)) |>
  arrange(.studlab)

## filter out studies that were duplicated due to two interventions clustered to one
studlabs.med.vs.rest.no.muli.arm.df <-  studlabs.med.vs.rest.df |>
  distinct(studlab, outcome, .keep_all = T)

## get study label of these studies filtered out in the last step
studlabs.med.vs.rest.muli.arm <- unique(studlabs.med.vs.rest.df$studlab[
  !studlabs.med.vs.rest.df$.studlab %in% studlabs.med.vs.rest.no.muli.arm.df$.studlab
])
studlabs.med.vs.rest.muli.arm
length(studlabs.med.vs.rest.muli.arm)

# %% [markdown] heading_collapsed=true hidden=true
# #### Get overall number of split multi-arm treatments

# %% hidden=true vscode={"languageId": "r"}
## get df of studies at which study labels were duplicated
studlabs.net.df <- net.res.all$data |>
  filter(grepl("#", studlab)) |>  # filter for # indicating duplication
  mutate(studlab = gsub("\\ #.*","", studlab)) |>
  arrange(.studlab)

## filter out studies that were duplicated due to two interventions clustered to one
studlabs.net.no.muli.arm.df <-  studlabs.net.df |>
  distinct(studlab, outcome, .keep_all = T)

## get study label of these studies filtered out in the last step
studlabs.net.muli.arm <- unique(studlabs.net.df$studlab[
  !studlabs.net.df$.studlab %in% studlabs.net.no.muli.arm.df$.studlab
])
studlabs.net.muli.arm
length(studlabs.net.muli.arm)

# %% [markdown]
# ## [Resilience Scales] Overall network meta-analysis

# %% [markdown]
# ### Investigating inconsistancy

# %% [markdown]
# ### Investigate duplication of study labels due to multiple outcomes, interventions, or time points with the same treatment comparison

# %% vscode={"languageId": "r"}
# Get multi-arm split studies and their number
multi_arm_split_studies <- net.res.resilience.scale$studlab %>%
  grep("#", ., value = TRUE) %>%
  str_remove_all(" #.*") %>%
  unique()

multi_arm_split_studies
length(multi_arm_split_studies)

# %% vscode={"languageId": "r"}
results.descriptive.array %>% dimnames()

# %% vscode={"languageId": "r"}
options(repr.matrix.max.rows=20, repr.matrix.max.cols=20)
net.res.resilience.scale$data %>% filter(grepl("Flett 2019b", .studlab))

# %% vscode={"languageId": "r"}
outcome.names.df %>% filter(row.names(.) == "Flett 2019b")

# %% vscode={"languageId": "r"}
print.array.not.na(results.descriptive.array[,,,"Outcome.4","Scale.1","Flett 2019b"])

# %% [markdown]
# ## [Mental health-related outcomes] Overall network meta-analysis

# %% [markdown]
# ### Investigating inconsistancy

# %% vscode={"languageId": "r"}
# Get multi-arm split studies and their number
multi_arm_split_studies <- net.res.mental.health$studlab %>%
  grep("#", ., value = TRUE) %>%
  str_remove_all(" #.*") %>%
  unique()

multi_arm_split_studies
length(multi_arm_split_studies)

# %% [markdown]
# # Create Shiny Dashboard (of inference statistics)

# %% vscode={"languageId": "r"}
# install.packages("shinydashboard")
library(shinydashboard)
library(shiny)
conflicts_prefer(shinydashboard::box)

# %% [markdown] heading_collapsed=true
# ## Calculate data frame lists and results for meta-analyses once for speeding up the repitative loading of the dashboard

# %% hidden=true vscode={"languageId": "r"}
meta.df.lists <- list()
meta.df.lists.w.o.mean.r <- list()
net.ress <- list()

i <- 1
for (outcome in present.outcomes.sorted){
  # with mean ranges
  meta.df.list.temp <- print.meta.results(
    outcome, preferred.scale = get.1st.preferred.scale(outcome),
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "meta.df.list"
  )
  
  meta.df.lists[[i]] <- meta.df.list.temp
  
  # without mean ranges
  meta.df.list.w.o.mean.r.temp <- print.meta.results(
    outcome, preferred.scale = get.1st.preferred.scale(outcome),
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "meta.df.list", without.mean.r = T
  )
  
  meta.df.lists.w.o.mean.r[[i]] <- meta.df.list.w.o.mean.r.temp
  
  # for network meta-analyses of single outcomes
  net.ress[[i]] <- net.meta.analyze(
    outcome, preferred.scale = get.1st.preferred.scale(outcome), net.df = F, net.res = F,
    plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
    return.data = "net.res", reference.group = "passive control", random = T, silent = T
  )
  
  i <- i + 1
}

names(meta.df.lists) <- present.outcomes.sorted
names(meta.df.lists.w.o.mean.r) <- present.outcomes.sorted
names(net.ress) <- present.outcomes.sorted

# # for network meta-analyses of all outcomes included in one model
# net.res.all <- net.meta.analyze(
#   present.outcomes, preferred.scale = F, net.df = F, net.res = F,
#   plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
#   return.data = "net.res", reference.group = "passive control", random = T, silent = T
# )

# %% [markdown]
# ## Functions

# %% [markdown] heading_collapsed=true
# ### Get regression results from print.meta.results()

# %% hidden=true vscode={"languageId": "r"}
get.regression.results <- function(
  meta.df.list, outcome, moderator, degree, filter = FALSE, preferred.scale = FALSE, without.mean.r = FALSE
){
  if (degree == 1){
    regression.results <- print.meta.results(
      outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list, return.data = "regression.results.linear",
      basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F,
      regression.degree.1 = T, regression.degree.2 = F, print.regplot = F, without.mean.r = without.mean.r,
      filter.regression.linear.list = if(filter){if(length(outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]])}}else{F}
    )
  } else if (degree == 2){
    regression.results <- print.meta.results(
      outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list, return.data = "regression.results.poly",
      basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F,
      regression.degree.1 = F, regression.degree.2 = T, print.regplot = F, without.mean.r = without.mean.r,
      filter.regression.poly.list = if(filter){if(length(outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]])}}else{F}
    )
  } else {
    cat(
      'No regression results returned from get.regression.results() for outcome "', outcome, '" and moderator "', moderator,
      "\nset degree to 1 or 2\n", sep= ""
    )
  }
  
  return(regression.results)
}

# %% [markdown] heading_collapsed=true
# ### Create fluidRows of moderators (with outliers)

# %% hidden=true vscode={"languageId": "r"}
set.moderator.rows.w.o. <- function(
  meta.df.list, outcome, preferred.scale, outcome.alias, sig.0.05.color, sig.0.1.color,
  moderator.vec = c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")
){
  
  # create variable testing if there is enough data for the moderator "follow.up.period"
  if(nrow(meta.df.list[[1]]) > 0){
    mod.data.f.up.t1 <- meta.df.list[[1]][, "follow.up.period"]
  } else {
    mod.data.f.up.t1 <- c()
  }
  
  if(nrow(meta.df.list[[2]]) > 0){
    mod.data.f.up.t2 <- meta.df.list[[2]][, "follow.up.period"]
  } else {
    mod.data.f.up.t2 <- c()
  }
  
  if(nrow(meta.df.list[[3]]) > 0){
    mod.data.f.up.t3 <- meta.df.list[[3]][, "follow.up.period"]
  } else {
    mod.data.f.up.t3 <- c()
  }
  
  f.up.isnt.na <- 
  if (          
    length(mod.data.f.up.t1[which(!is.na.or.nm(mod.data.f.up.t1))]) >= 4 &
    (
      length(mod.data.f.up.t2[which(!is.na.or.nm(mod.data.f.up.t2))]) >= 1 |
      length(mod.data.f.up.t3[which(!is.na.or.nm(mod.data.f.up.t3))]) >= 1
    )
  ){
    !is.na(print.meta.results(
      outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list, return.data = "regression.results.linear",
      basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
      print.regplot = F, filter.regression.linear.list = F
    )[1])
  } else {FALSE}
  
  fluidRow.list <- list()
  for (moderator in moderator.vec){
    if(nrow(meta.df.list[[1]]) > 0){
      mod.data <- meta.df.list[[1]][, moderator]
    } else {
      mod.data <- c()
    }
    k.4 <- length(mod.data[which(!is.na.or.nm(mod.data))]) >= 4  # check if 4 or more studies are included
    
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        width = 12,
        class = "well",
        h3(moderator)
      )
    ))
    
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        title = "linear",
        width = 6,
        background = if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
          if(
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$QMp <= 0.05 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.05
          ){
            sig.0.05.color
          } else if (
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$QMp <= 0.1 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.1
          ){
            sig.0.1.color
          } else {NULL}
        } else {NULL},
        class = "well",
        plotOutput(paste(moderator, ".lin", outcome.alias, sep = "")),
        footer = p(paste(
          "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
          "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
          "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
          "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
          "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
          sep = ""
        ), style="color:black"),
        align="center",
        collapsible = TRUE
      ),
      box(
        title = "squared",
        width = 6,
        background = if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
          if(
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$QMp <= 0.05 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.05
          ){
            sig.0.05.color
          } else if (
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$QMp <= 0.1 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.1
          ){
            sig.0.1.color
          } else {NULL}
        } else {NULL},
        class = "well",
        plotOutput(paste(moderator, ".sq", outcome.alias, sep = "")),
        footer = p(paste(
          "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
          "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
          "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
          "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
          "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
          sep = ""
        ), style="color:black"),
        align="center",
        collapsible = TRUE
      )
    ))
  }
  return(fluidRow.list)
}

# %% [markdown] heading_collapsed=true
# ### Create fluidRows of moderators (for sensitivity analysis)

# %% hidden=true vscode={"languageId": "r"}
set.moderator.rows.comp <- function(
  meta.df.list, outcome, preferred.scale, outcome.alias, sig.0.05.color, sig.0.1.color, without.mean.r = F,
  moderator.vec = c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")
){
  
  # create variable testing if there is enough data for the moderator "follow.up.period"
  if(nrow(meta.df.list[[1]]) > 0){
    mod.data.f.up.t1 <- meta.df.list[[1]][, "follow.up.period"]
  } else {
    mod.data.f.up.t1 <- c()
  }
  
  if(nrow(meta.df.list[[2]]) > 0){
    mod.data.f.up.t2 <- meta.df.list[[2]][, "follow.up.period"]
  } else {
    mod.data.f.up.t2 <- c()
  }
  
  if(nrow(meta.df.list[[3]]) > 0){
    mod.data.f.up.t3 <- meta.df.list[[3]][, "follow.up.period"]
  } else {
    mod.data.f.up.t3 <- c()
  }
  
  f.up.isnt.na <- 
  if (          
    length(mod.data.f.up.t1[which(!is.na.or.nm(mod.data.f.up.t1))]) >= 4 &
    (
      length(mod.data.f.up.t2[which(!is.na.or.nm(mod.data.f.up.t2))]) >= 1 |
      length(mod.data.f.up.t3[which(!is.na.or.nm(mod.data.f.up.t3))]) >= 1
    )
  ){
    !is.na(print.meta.results(
      outcome, preferred.scale = p>eferred.scale, meta.df.list = meta.df.list, return.data = "regression.results.linear",
      basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
      print.regplot = F, filter.regression.linear.list = F
    )[1])
  } else {FALSE}
  
  fluidRow.list <- list()
  
  fluidRow.list <- append(fluidRow.list, fluidRow(
    box(
      title = "Correlation Matrix",
      width = 12,
      class = "well",
      tableOutput(paste("cor.matrix.", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
      align="right",
      collapsible = TRUE,
      collapsed = TRUE
    )
  ))
  
  for (moderator in moderator.vec){
    if(nrow(meta.df.list[[1]]) > 0){
      mod.data <- meta.df.list[[1]][, moderator]
    } else {
      mod.data <- c()
    }
    k.4 <- length(mod.data[which(!is.na.or.nm(mod.data))]) >= 4  # check if 4 or more studies are included
    
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        width = 12,
        class = "well",
        h3(moderator)
      )
    ))
    
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        width = 12,
        class = "well",
        h4(paste(
          "Outliers [linear; squared]:",
          paste(
            paste(outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]], collapse = ", "), "; ",
            paste(outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]], collapse = ", "),
            sep = ""
          )
        ))
      )
    ))
    
    # Outlier and influence pltos
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        title = paste("Baujat Plot for linear", moderator),
        width = 6,
        class = "well",
        plotOutput(paste(moderator, ".baujat", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        align="center",
        collapsible = T,
        collapsed = T
      ),
      box(
        title = paste("Influence Plots for linear", moderator),
        width = 6,
        class = "well",
        plotOutput(paste(moderator, ".influence", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        align="center",
        collapsible = T,
        collapsed = T
      )
    ))
    
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        title = paste("Baujat Plot for squared", moderator),
        width = 6,
        class = "well",
        plotOutput(paste(moderator, ".baujat.sq", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        align="center",
        collapsible = T,
        collapsed = T
      ),
      box(
        title = paste("Influence Plots for squared", moderator),
        width = 6,
        class = "well",
        plotOutput(paste(moderator, ".influence.sq", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        align="center",
        collapsible = T,
        collapsed = T
      )
    ))
    
    # Normal Q-Q plot
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        title = paste("Q-Q Plot for", moderator),
        width = 12,
        class = "well",
        plotOutput(paste(moderator, ".qq", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        align="center",
        collapsible = T,
        collapsed = T
      )
    ))
    
    # Rregression Plots
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        title = "linear (with outliers)",
        width = 6,
        background = if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
          if(
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$QMp <= 0.05 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.05
          ){
            sig.0.05.color
          } else if (
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$QMp <= 0.1 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.1
          ){
            sig.0.1.color
          } else {NULL}
        } else {NULL},
        class = "well",
        plotOutput(paste(moderator, ".lin.w.o.", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        footer = p(paste(
          "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
          "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
          "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
          "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
          "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = F, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
          sep = ""
        ), style="color:black"),
        align="center",
        collapsible = TRUE
      ),
      box(
        title = "squared (with outliers)",
        width = 6,
        background = if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
          if(
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$QMp <= 0.05 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.05
          ){
            sig.0.05.color
          } else if (
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$QMp <= 0.1 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$pval[2] <= 0.1
          ){
            sig.0.1.color
          } else {NULL}
        } else {NULL},
        class = "well",
        plotOutput(paste(moderator, ".sq.w.o.", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        footer = p(paste(
          "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
          "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
          "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
          "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
          "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = F, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
          sep = ""
        ), style="color:black"),
        align="center",
        collapsible = TRUE
      )
    ))
    
    fluidRow.list <- append(fluidRow.list, fluidRow(
      box(
        title = "linear (without outliers)",
        width = 6,
        background = if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
          if(
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$QMp <= 0.05 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$pval[2] <= 0.05
          ){
            sig.0.05.color
          } else if (
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$QMp <= 0.1 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$pval[2] <= 0.1
          ){
            sig.0.1.color
          } else {NULL}
        } else {NULL},
        class = "well",
        plotOutput(paste(moderator, ".lin.n.o.", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        footer = p(paste(
          "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
          "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
          "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
          "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
          "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 1, filter = T, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
          sep = ""
        ), style="color:black"),
        align="center",
        collapsible = TRUE
      ),
      box(
        title = "squared (without outliers)",
        width = 6,
        background = if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
          if(
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$QMp <= 0.05 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$pval[2] <= 0.05
          ){
            sig.0.05.color
          } else if (
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$QMp <= 0.1 &
            get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$pval[2] <= 0.1
          ){
            sig.0.1.color
          } else {NULL}
        } else {NULL},
        class = "well",
        plotOutput(paste(moderator, ".sq.n.o.", outcome.alias, ifelse(without.mean.r, "w.o.m.r", ""), sep = "")),
        footer = p(paste(
          "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
          "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
          "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
          "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
          "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round(get.regression.results(meta.df.list, outcome, moderator, degree = 2, filter = T, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
          sep = ""
        ), style="color:black"),
        align="center",
        collapsible = TRUE
      )
    ))

  }
  return(fluidRow.list)
}

# %% code_folding=[] hidden=true vscode={"languageId": "r"}
# # function not verified
# set.moderator.rows.comp <- function(
#   meta.df.list, outcome, preferred.scale, outcome.alias, sig.0.05.color, sig.0.1.color,
#   moderator.vec = c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")){
  
#   # set variables for better readability of code below
#   f.up.isnt.na <- 
#   if (
#     nrow(meta.df.list[[1]]) >= 4 &
#     (
#       nrow(meta.df.list[[2]]) >= 1 |
#       nrow(meta.df.list[[3]]) >= 1
#     )
#   ){
#     !is.na(print.meta.results(
#       outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list, return.data = "regression.results.linear",
#       basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
#       print.regplot = F, filter.regression.linear.list = F
#     )[1])
#   } else {FALSE}
  
#   k.4 <- nrow(meta.df.list[[1]]) >= 4  # check if 4 or more studies are included
  
#   fluidRow.list <- list()
#   for (moderator in moderator.vec){
# # Headings
#     fluidRow.list <- append(fluidRow.list, fluidRow(
#       box(
#         width = 12,
#         class = "well",
#         h3(moderator)
#       )
#     ))
    
#     for (without.mean.r in c(FALSE, TRUE)){
#       if (without.mean.r){
#         fluidRow.list <- append(fluidRow.list, fluidRow(
#           box(
#             width = 12,
#             class = "well",
#             h4("Without mean ranges")
#           )
#         ))
#       } else {
#         fluidRow.list <- append(fluidRow.list, fluidRow(
#           box(
#             width = 12,
#             class = "well",
#             h4("With mean ranges")
#           )
#         ))
#       }
#       for (filter in c(TRUE, FALSE)){
#         if (filter){
#           fluidRow.list <- append(fluidRow.list, fluidRow(
#             box(
#               width = 12,
#               class = "well",
#               h5("Without outliers")
#             )
#           ))
#         } else {
#           fluidRow.list <- append(fluidRow.list, fluidRow(
#             box(
#               width = 12,
#               class = "well",
#               h5("With outliers")
#             )
#           ))
#         }
        
# # Plots
#         fluidRow.list <- append(fluidRow.list, fluidRow(
#           box(
#             title = "linear",
#             width = 6,
#             background = if(nrow(meta.df.list[[1]]) >= 4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
#               if(
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$QMp <= 0.05 &
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$pval[2] <= 0.05
#               ){
#                 sig.0.05.color
#               } else if (
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$QMp <= 0.1 &
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$pval[2] <= 0.1
#               ){
#                 sig.0.1.color
#               } else {NULL}
#             } else {NULL},
#             class = "well",
#             plotOutput(paste(
#               moderator, ".lin", outcome.alias, if(filter){".filt"}else{".unfilt"}, if(without.mean.r){".w.o.mean.r"}else{".w.mean.r"},
#               sep = ""
#             )),
#             footer = p(paste(
#               "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){ get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
#               "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
#               "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
#               "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
#               "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 1, filter = filter, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
#               sep = ""
#             ), style="color:black"),
#             align="center",
#             collapsible = TRUE
#           ),
#           box(
#             title = "squared",
#             width = 6,
#             background = if(nrow(meta.df.list[[1]]) >= 4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){
#               if(
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$QMp <= 0.05 &
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$pval[2] <= 0.05
#               ){
#                 sig.0.05.color
#               } else if (
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$QMp <= 0.1 &
#                  get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$pval[2] <= 0.1
#               ){
#                 sig.0.1.color
#               } else {NULL}
#             } else {NULL},
#             class = "well",
#             plotOutput(paste(
#               moderator, ".sq", outcome.alias, if(filter){".filt"}else{".unfilt"}, if(without.mean.r){".w.o.mean.r"}else{".w.mean.r"},
#               sep = ""
#             )),
#             footer = p(paste(
#               "k: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){ get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$k}else{"NA"}, "; ",
#               "Test of moderators p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$QMp, digits = 4)}else{"NA"}, "; ",
#               "Regression coefficient p-value: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$pval[2], digits = 4)}else{"NA"}, "; ",
#               "R^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$R2, digits = 2)}else{"NA"}, "%; ",
#               "I^2: ", if(k.4 & if(moderator == "follow.up.period"){f.up.isnt.na}else{T}){round( get.regression.results(meta.df.list, outcome, moderator, without.mean.r = without.mean.r, degree = 2, filter = filter, preferred.scale = preferred.scale)$I2, digits = 2)}else{"NA"}, "%",
#               sep = ""
#             ), style="color:black"),
#             align="center",
#             collapsible = TRUE
#           )
#         ))
#       }
#     }
#   }
#   return(fluidRow.list)
# }

# %% [markdown] heading_collapsed=true
# ### Create outcome page for the ui

# %% code_folding=[] hidden=true vscode={"languageId": "r"}
# graphical parameters stress (because shiny fails to recognize the size of subgroub forest plots)
forest.plot.box.height.Stress <- "height: 65vh;"
  # for 16:9 (1920 x 1080) screen = "height: 65vh;"
  # for 3:2 (2256 x 1504) screen = "height: 99vh;"

# Tab names per outcome page
forest.tab.name <- "Forest & Funnel Plot (with outliers)"
forest.no.outliers.tab.name <- "Forest & Funnel Plot (comparision with/without outliers)"
regression.single.with.outliers.tab.name <- "Linear Regressions (with outliers)"
regression.single.comp.tab.name <- "Linear Regressions (comparison with/without outliers; with mean ranges)"
regression.single.comp.tab.name.no.mean.r <- "Linear Regressions (comparison with/without outliers; without mean ranges)"
net.meta.tab.name <- "Network Meta-Analysis"
# regression.multiple.tab.name <- "Multiple Regressions"
# regression.multiple.no.outliers.tab.name <- "M. Regressions (w.o.)"

# set colors
sig.0.05.color <- "red"
sig.0.1.color <- "yellow"

set.outcome.page <- function(outcome, preferred.scale = FALSE){
  
# Collect data of outcome 
  meta.df.list <- meta.df.lists[[outcome]]
  
# Get p-value of Egger's regression test for funnel plot asymmetry
  funnel.asym.p.egger <- print.meta.results(
    outcome, preferred.scale = preferred.scale,  meta.df.list = meta.df.list,
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "funnel.asym.p.egger"
  )
  if(length(funnel.asym.p.egger) == 0){
    funnel.asym.p.egger <- 1
  }
  
  funnel.asym.p.egger.n.o. <- print.meta.results(
    outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "funnel.asym.p.egger", filter.forest..funnel.vec = if(length(outlier.list[[outcome]]) != 0){-outlier.list[[outcome]]}else{FALSE}
  )
  if(length(funnel.asym.p.egger.n.o.) == 0){
    funnel.asym.p.egger.n.o. <- 1
  }
  
# Get p-value of rank correlation test for funnel plot asymmetry
  funnel.asym.p.rank <- print.meta.results(
    outcome, preferred.scale = preferred.scale,  meta.df.list = meta.df.list,
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "funnel.asym.p.rank"
  )
  if(length(funnel.asym.p.rank) == 0){
    funnel.asym.p.rank <- 1
  }
  
  funnel.asym.p.rank.n.o. <- print.meta.results(
    outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "funnel.asym.p.rank", filter.forest..funnel.vec = if(length(outlier.list[[outcome]]) != 0){-outlier.list[[outcome]]}else{FALSE}
  )
  if(length(funnel.asym.p.rank.n.o.) == 0){
    funnel.asym.p.rank.n.o. <- 1
  }
  
  # get number of control and experiment groups' total observations
  results.meta <-  print.meta.results(
    outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "results.meta"  # , filter.forest..funnel.vec = if(length(outlier.list[[outcome]]) != 0){-outlier.list[[outcome]]}else{FALSE}
  )
  o.exp <- results.meta$n.e.pooled
  o.con <- results.meta$n.c.pooled
  o.total <- o.exp + o.con
  k <- results.meta$k
  pval.random <- round(as.double(results.meta$pval.random), digits = 4)
  lower.I2 <- round(as.double(results.meta$lower.I2) * 100, digits = 2)
  upper.I2 <- round(as.double(results.meta$upper.I2) * 100, digits = 2)
  
  # get results for network meta-analysis
  net.res <- net.ress[[outcome]]
  
# Adjust outcome name because spaces and brackets could cause trouble later on
  outcome.alias <- gsub("([()])", "", gsub(" ", "_", outcome))
  
  
  tabItem(
    tabName = paste(outcome.alias, "_page", sep = ""),
    tabsetPanel(
      type = "tabs",
      
# Main Results (with outliers)
      tabPanel(
        forest.tab.name,
        fluidRow(
          box(
            title = "Forest Plot",
            footer = paste(
              "Exp. Observations: ", o.exp, "; Con. Observations: ", o.con, "; Total: ", o.total, "; k: ", k,
              "; I2 CI: (", lower.I2, ", ", upper.I2, "); p (SMD diff. from 0): ", pval.random, sep = ""
            ),
            width = 8,
            class = "well",
            plotOutput(paste("forest.", outcome.alias, sep = "")),
            align="center",
            collapsible = TRUE
          ),
          box(
            title = "Funnel Plot (with Trim and Fill Method)",
            background = if(funnel.asym.p.egger <= 0.05){sig.0.05.color}else{NULL},
            footer = paste(
              "P-value of Egger's regression and rank correlation tests for funnel plot asymmetry: ",
              round(funnel.asym.p.egger, digits = 4), ", ", round(funnel.asym.p.rank, digits = 4), sep = ""
            ),
            width = 4,
            class = "well",
            plotOutput(paste("funnel.", outcome.alias, sep = "")),
            collapsible = TRUE
          )
        ),
        
        fluidRow(
          box(
            title = "Forest Plot (devided by Delivery Mode)",
            width = 6,
            class = "well",
            plotOutput(paste("subgroup.delivery.", outcome.alias, sep = "")),
            align="center",
            style = forest.plot.box.height.Stress,
              # adjust height of grey box around the plot, as the high number of studies hinders shiny to recognize the actual plot size
            collapsible = TRUE
          ),
          box(
            title = "Forest Plot (devided by Meditation Type)",
            width = 6,
            plotOutput(paste("subgroup.type.", outcome.alias, sep = "")),
            class = "well",
            align = "center",
            style = forest.plot.box.height.Stress,
            collapsible = TRUE
          )
        )
      ),
      
# Main Results comparison with/without outliers
      tabPanel(
        forest.no.outliers.tab.name,
        fluidRow(
          box(
            title = "Influence Data",
            width = 7,
            class = "well",
            tableOutput(paste("influence.df.", outcome.alias, ".forest", sep = "")),
            align="right",
            collapsible = TRUE,
            collapsed = TRUE
          ),
          box(
            title = "Influence Plots",
            width = 5,
            class = "well",
            plotOutput(paste("influence.", outcome.alias, ".forest", sep = "")),
            align="center",
            collapsible = TRUE,
            collapsed = TRUE
          ),
        ),
        fluidRow(
          box(
            title = "Baujat Plot",
            width = 6,
            class = "well",
            plotOutput(paste("baujat.", outcome.alias, ".forest.comp", sep = "")),
            align="center",
            collapsible = TRUE,
            collapsed = TRUE
          )
        ),
        # Gosh Plots
        fluidRow(
          box(
            title = "Gosh Plot (outlier 1)",
            width = 4,
            class = "well",
            imageOutput(paste("gosh.1.", outcome.alias, ".forest.comp", sep = "")),
            align="center",
            collapsible = TRUE,
            collapsed = TRUE
          ),
          box(
            title = "Gosh Plot (outlier 2)",
            width = 4,
            class = "well",
            imageOutput(paste("gosh.2.", outcome.alias, ".forest.comp", sep = "")),
            align="center",
            collapsible = TRUE,
            collapsed = TRUE
          ),
          box(
            title = "Gosh Plot (outlier 3)",
            width = 4,
            class = "well",
            imageOutput(paste("gosh.3.", outcome.alias, ".forest.comp", sep = "")),
            align="center",
            collapsible = TRUE,
            collapsed = TRUE
          )
        ),
        
        # Comparison Plots
        fluidRow(
          box(
            width = 6,
            class = "well",
            h4(paste(
              "Outliers:",
              paste(outlier.list[[outcome]], collapse = ", ")
            ))
          )
        ),
        fluidRow(
          box(
            title = "Forest Plot (with outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("forest.o.", outcome.alias, sep = "")),
            align="right",
            collapsible = TRUE
          ),
          box(
            title = "Forest Plot (without outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("forest.n.o.comp", outcome.alias, sep = "")),
            align="right",
            collapsible = TRUE
          )
        ),
        
        fluidRow(
          box(
            title = "Forest Plot (devided by Delivery Mode with outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("subgroup.delivery.", outcome.alias, ".o.", sep = "")),
            align="center",
            style = forest.plot.box.height.Stress,
            collapsible = TRUE
          ),
          box(
            title = "Forest Plot (devided by Delivery Mode without outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("subgroup.delivery.", outcome.alias, ".n.o.comp", sep = "")),
            align="center",
            style = forest.plot.box.height.Stress,
            collapsible = TRUE
          )
        ),
        
        fluidRow(
          box(
            title = "Forest Plot (devided by Meditation Type with outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("subgroup.type.", outcome.alias, ".o.", sep = "")),
            align="center",
            style = forest.plot.box.height.Stress,
            collapsible = TRUE
          ),
          box(
            title = "Forest Plot (devided by Meditation Type without outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("subgroup.type.", outcome.alias, ".n.o.comp", sep = "")),
            align="center",
            style = forest.plot.box.height.Stress,
            collapsible = TRUE
          )
        ),
        
        fluidRow(
          box(
            title = "Funnel Plot (with outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("funnel.o.", outcome.alias, sep = "")),
            footer = paste(
              "P-value of Egger's regression and rank correlation tests for funnel plot asymmetry: ",
              round(funnel.asym.p.egger, digits = 4), ", ", round(funnel.asym.p.rank, digits = 4), sep = ""
            ),
            align="right",
            collapsible = TRUE
          ),
          box(
            title = "Funnel Plot (without outliers)",
            width = 6,
            class = "well",
            plotOutput(paste("funnel.n.o.comp", outcome.alias, sep = "")),
            footer = paste(
              "P-value of Egger's regression and rank correlation tests for funnel plot asymmetry: ",
              round(funnel.asym.p.egger.n.o., digits = 4), ", ", round(funnel.asym.p.rank.n.o., digits = 4), sep = ""
            ),
            align="right",
            collapsible = TRUE
          )
        ),
        
        fluidRow(
          box(
            title = "Sensitivity Dataframe",
            width = 12,
            class = "well",
            tableOutput(paste("sens.df.rand.fix.", outcome.alias, sep = "")),
            align="right",
            collapsible = TRUE
          )
        )
      ),
      
# Single Regressions
  # (without outliers)
      tabPanel(
        regression.single.with.outliers.tab.name,
        set.moderator.rows.w.o.(meta.df.list, outcome, preferred.scale, outcome.alias, sig.0.05.color, sig.0.1.color)
      ),
      
      tabPanel(
        regression.single.comp.tab.name,
        set.moderator.rows.comp(meta.df.list, outcome, preferred.scale, outcome.alias, sig.0.05.color, sig.0.1.color)
      ),
      
      tabPanel(
        regression.single.comp.tab.name.no.mean.r,
        set.moderator.rows.comp(meta.df.list, outcome, preferred.scale, outcome.alias, sig.0.05.color, sig.0.1.color, without.mean.r = T)
      ),
      
# Network Analysis
      tabPanel(
        net.meta.tab.name,
        fluidRow(
          box(
            title = "Network Plot",
            footer = if(is.logical(net.res)){""}else{paste(
              "Studies = ", net.res$k, "; Pairwise comparisons = ", net.res$m, "; Treatments = ", net.res$n, "; Designs", net.res$d,
              "; tau2 = ", round(net.res$tau2, digit = 4), "; I2 = ", round(net.res$I2, digit = 2), " [", round(net.res$lower.I2, digit = 2), ", ", round(net.res$upper.I2, digit = 2), "]",
              "; Total Q = ", round(net.res$Q, digit = 2), "; df = ", round(net.res$df.Q, digit = 2), "; paval = ", round(net.res$pval.Q, digit = 4),
              "; Hetero Q = ", round(net.res$Q.heterogeneity, digit = 2), "; df = ", round(net.res$df.Q.heterogeneity, digit = 2), "; paval = ", round(net.res$pval.Q.heterogeneity, digit = 4),
              "; Incons Q = ", round(net.res$Q.inconsistency, digit = 2), "; df = ", round(net.res$df.Q.inconsistency, digit = 2), "; paval = ", round(net.res$pval.Q.inconsistency, digit = 4),
              sep = ""
            )},
            width = 6,
            class = "well",
            plotOutput(paste("network.", outcome.alias, sep = "")),
            align="center",
            collapsible = TRUE
          ),
          box(
            title = "Network Forest Plot",
            width = 6,
            class = "well",
            plotOutput(paste("net.forest.", outcome.alias, sep = "")),
            collapsible = TRUE
          )
        ),
        
        fluidRow(
          box(
            title = "Direct Evidence Plot",
            width = 8,
            class = "well",
            plotOutput(paste("net.dir.evidence.", outcome.alias, sep = "")),
            align="center",
            collapsible = TRUE
          ),
          box(
            title = "Netheat Plot",
            width = 4,
            plotOutput(paste("net.heat.", outcome.alias, sep = "")),
            class = "well",
            align = "center",
            collapsible = TRUE
          )
        )
      ),
      fluidRow(
        box(
          title = "P-val table",
          width = 12,
          class = "well",
          tableOutput(paste("net.p.df.", outcome.alias, sep = "")),
          align="right",
          collapsible = TRUE,
          collapsed = TRUE
        )
      )
    )
  )
}

# %% [markdown]
# ### Create summary pages for the ui

# %% vscode={"languageId": "r"}
set.summary.page <- function(net.res.object, domain_name, tabName) {

  # Check if net.res.object is a netmeta object
    if (!inherits(net.res.object, "netmeta")) {
      stop("Error: net.res.object must be a netmeta object")
    }

  # Define number of included studies
  included_studies_per_model <- net.res.object$studlab %>%
    # In case, there is one study with the same treatment comparison occuring multiple times in the model
    # (e.g., because of multiple outcomes for this treatment comparison), there was added a label such as " #1#"
    # we remove these labels to count unique studies only
    str_remove_all(" #\\d+#") %>%
    unique()
  n_included_studies_per_model <- length(included_studies_per_model)

  # Return the tabItem with the tabsetPanels and tabPanels
  tabItem(
    tabName = tabName,
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Summary without outliers",
        fluidRow(
          box(
            title = "Summary Forest Plot",
            width = 12,
            class = "well",
            plotOutput(paste0("forest.summary.", domain_name)),
            align="center",
            collapsible = TRUE
          )
        )
      ),
      tabPanel(
        "Comparison with/without outliers",
        fluidRow(
          box(
            title = "Summary Forest Plot (with outliers)",
            width = 12,
            class = "well",
            plotOutput(paste0("forest.summary.o..", domain_name)),
            align="center",
            collapsible = TRUE
          )
        ),
        fluidRow(
          box(
            title = "Summary Forest Plot (without outliers)",
            width = 12,
            class = "well",
            plotOutput(paste0("forest.summary.n.o..", domain_name)),
            align="center",
            collapsible = TRUE
          )
        )
      ),
      tabPanel(
        "Network Analysis",
        fluidRow(
          box(
            title = "Network Plot",
            footer = paste(
              "N included unique studies = ", n_included_studies_per_model,
              "; Inflated study count (mixture of number of outcomes, actual studies, and split multi-arm designs) = ", net.res.object$k,
              "; Pairwise comparisons = ", net.res.object$m,
              "; Treatments = ", net.res.object$n, "; Designs = ", net.res.object$d,
              "; tau2 = ", round(net.res.object$tau2, digit = 4), "; I2 = ", round(net.res.object$I2, digit = 2), " [", round(net.res.object$lower.I2, digit = 2), ", ", round(net.res.object$upper.I2, digit = 2), "]",
              "; Total Q = ", round(net.res.object$Q, digit = 2), "; df = ", round(net.res.object$df.Q, digit = 2), "; paval = ", round(net.res.object$pval.Q, digit = 4),
              "; Hetero Q = ", round(net.res.object$Q.heterogeneity, digit = 2), "; df = ", round(net.res.object$df.Q.heterogeneity, digit = 2), "; paval = ", round(net.res.object$pval.Q.heterogeneity, digit = 4),
              "; Incons Q = ", round(net.res.object$Q.inconsistency, digit = 2), "; df = ", round(net.res.object$df.Q.inconsistency, digit = 2), "; paval = ", round(net.res.object$pval.Q.inconsistency, digit = 4),
              "; \nlist of included studies: \n'", paste(included_studies_per_model, collapse = "', '"), "'",
              sep = ""
            ),
            width = 6,
            class = "well",
            plotOutput(paste0("network.all.", domain_name)),
            align="center",
            collapsible = TRUE
          ),
          box(
            title = "Network Forest Plot",
            # background = if(funnel.asym.p.egger <= 0.05){sig.0.05.color}else{NULL},
            # footer = "",
            width = 6,
            class = "well",
            plotOutput(paste0("net.forest.all.", domain_name)),
            collapsible = TRUE
          )
        ),

        fluidRow(
          box(
            title = "Direct Evidence Plot",
            width = 8,
            class = "well",
            plotOutput(paste0("net.dir.evidence.all.", domain_name)),
            align="center",
            # style = forest.plot.box.height.Stress,
              # adjust height of grey box around the plot, as the high number of studies hinders shiny to recognize the actual plot size
            collapsible = TRUE
          ),
          box(
            title = "Netheat Plot",
            width = 4,
            plotOutput(paste0("net.heat.all.", domain_name)),
            class = "well",
            align = "center",
            # style = forest.plot.box.height.Stress,
            collapsible = TRUE
          )
        ),
        fluidRow(
          box(
            title = "P-val table",
            width = 12,
            class = "well",
            tableOutput(paste0("net.p.df.all.", domain_name)),
            align="right",
            collapsible = TRUE,
            collapsed = TRUE
          )
        )
      ),
      tabPanel(
        "Node Splitting Forest Plot",
        fluidRow(
          box(
            title = "Node Splitting Forest Plot",
            width = 12,
            class = "well",
            plotOutput(paste0("netsplit.summary.", domain_name), height = "1700px"),
            align="center",
            collapsible = TRUE
          )
        )
      )
    )
  )
}

# %% [markdown] heading_collapsed=true
# ### Set outcome outputs for server

# %% hidden=true vscode={"languageId": "r"}
# without loop for moderators & incl regression comparison
moderator.vec <- c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")

return.outcome.output <- function(output, outcome.vec, preferred.scale = FALSE){
  moderator.vec <- c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")
  
  picture.size <- 400
  
  for (outcome in outcome.vec){
    # this for regreloop was inteded to set outputs for all outcomes, but the outpunts of all outcomes got set with the plots of the last outcome
    # passing only one outcome as outcome.vec works
    outliers <- outlier.list[[outcome]]
    
    meta.df.list <- meta.df.lists[[outcome]]
    
    meta.df.list.w.o.mean.r <- meta.df.lists.w.o.mean.r[[outcome]]
    
    outcome.alias <- gsub("([()])", "", gsub(" ", "_", outcome))  # because spaces and brackets could cause trouble later on
    
  # Main Results
    output[[paste("forest.", outcome.alias, sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = T, forest.add.fix.eff.mod = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F  # ,
          # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
        )
      }
    )
    
    output[[paste("subgroup.delivery.", outcome.alias, sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = F, forest.add.fix.eff.mod = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "delivery.mode", print.meta.results = F  # ,
          # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
        )
      },
      height = 600  # adjust height of plot (independently of from the ui frame)
    )

    output[[paste("subgroup.type.", outcome.alias, sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = F, forest.add.fix.eff.mod = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "meditation.type", print.meta.results = F  # ,
          # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
        )
      },
      height = 600
    )

    output[[paste("funnel.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = F, print.funnel = T, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
        funnel.label = F  # ,
        # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
      )
    })


  # Main Results comparison with/without outliers
    output[[paste("baujat.", outcome.alias, ".forest.comp", sep = "")]] <- renderPlot({  # n.o. = no outliers (included)
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = T, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
      )
    })
    
    output[[paste("influence.", outcome.alias, ".forest", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = F, print.funnel = F, print.influence = T, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
      )
    })
    
    output[[paste("influence.df.", outcome.alias, ".forest", sep = "")]] <- renderTable({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
        return.data = "influence.df"
      )
    })

    output[[paste("forest.o.", outcome.alias, sep = "")]] <- renderPlot({  # o. = (with) outliers
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = T, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
      )
    })

    output[[paste("forest.n.o.comp", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = T, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
        filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
      )
    })
    
    output[[paste("subgroup.delivery.", outcome.alias, ".o.", sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "delivery.mode", print.meta.results = F
        )
      },
      height = 600  # adjust height of plot (independently of from the ui frame)
    )

    output[[paste("subgroup.delivery.", outcome.alias, ".n.o.comp", sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "delivery.mode", print.meta.results = F,
          filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
        )
      },
      height = 600  # adjust height of plot (independently of from the ui frame)
    )
    
    output[[paste("subgroup.type.", outcome.alias, ".o.", sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "meditation.type", print.meta.results = F
        )
      },
      height = 600  # adjust height of plot (independently of from the ui frame)
    )

    output[[paste("subgroup.type.", outcome.alias, ".n.o.comp", sep = "")]] <- renderPlot(
      {
        print.meta.results(
          outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
          regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "meditation.type", print.meta.results = F,
          filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
        )
      },
      height = 600  # adjust height of plot (independently of from the ui frame)
    )

    output[[paste("funnel.o.", outcome.alias, sep = "")]] <- renderPlot({  # o. = (with) outliers
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = F, print.funnel = T, funnel.label.out.only = T, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
      )
    })

    output[[paste("funnel.n.o.comp", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        regression = F, print.forest = F, print.funnel = T, funnel.label.out.only = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
        filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
      )
    })

    output[[paste("gosh.1.", outcome.alias, ".forest.comp", sep = "")]] <- renderImage({
      list(
        src = paste(gsub("/", "\\\\", getwd()), "\\", "Gosh Plots", "\\", "Gosh.Plot.", outcome, ".outlier.1", ".png", sep = ""),
        contentType = 'image/png',
        width = picture.size,
        height = picture.size
      )
    }, deleteFile = F)

    output[[paste("gosh.2.", outcome.alias, ".forest.comp", sep = "")]] <- renderImage({
      list(
        src = paste(gsub("/", "\\\\", getwd()), "\\", "Gosh Plots", "\\", "Gosh.Plot.", outcome, ".outlier.2", ".png", sep = ""),
        contentType = 'image/png',
        width = picture.size,
        height = picture.size
      )
    }, deleteFile = F)

    output[[paste("gosh.3.", outcome.alias, ".forest.comp", sep = "")]] <- renderImage({
      list(
        src = paste(gsub("/", "\\\\", getwd()), "\\", "Gosh Plots", "\\", "Gosh.Plot.", outcome, ".outlier.3", ".png", sep = ""),
        contentType = 'image/png',
        width = picture.size,
        height = picture.size
      )
    }, deleteFile = F)
    
    output[[paste("sens.df.rand.fix.", outcome.alias, sep = "")]] <- renderTable(
      get.sens.anal.df(outcome, "rand.fix", ifelse(outcome == "Stress", "DASS", F))
    )
    
# Regression
## with mean ranges
## =============================================================================================================  
    
    output[[paste("cor.matrix.", outcome.alias, sep = "")]] <- renderTable({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, regression = F, return.data = "correlation.matrix"
      )
    })
    
    ## "programs.duration"
    ## =============================================================================================================
    output[[paste("programs.duration", ".baujat", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("programs.duration", ".influence", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("programs.duration", ".baujat.sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("programs.duration", ".influence.sq", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("programs.duration", ".qq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("programs.duration", ".lin", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = F  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = F  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".lin.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".sq.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".lin.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".sq.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]])}
      )
    })
    
    ## "sessions.duration"
    ## =============================================================================================================
    output[[paste("sessions.duration", ".baujat", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.duration", ".influence", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.duration", ".baujat.sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.duration", ".influence.sq", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.duration", ".qq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.duration", ".lin", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = F  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = F  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".lin.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".sq.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".lin.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".sq.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]])}
      )
    })
    
    ## "sessions.frequency"
    ## =============================================================================================================
    output[[paste("sessions.frequency", ".baujat", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.frequency", ".influence", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.frequency", ".baujat.sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.frequency", ".influence.sq", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.frequency", ".qq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.frequency", ".lin", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = F  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = F  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".lin.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".sq.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".lin.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".sq.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]])}
      )
    })
    
    ## "follow.up.period"
    ## =============================================================================================================
    output[[paste("follow.up.period", ".baujat", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("follow.up.period", ".influence", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("follow.up.period", ".baujat.sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("follow.up.period", ".influence.sq", outcome.alias, sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("follow.up.period", ".qq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("follow.up.period", ".lin", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = F  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".sq", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = F  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".lin.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".sq.w.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".lin.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".sq.n.o.", outcome.alias, sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]])}
      )
    })
    
## without mean ranges
## =============================================================================================================  
    
    output[[paste("cor.matrix.", outcome.alias, "w.o.m.r", sep = "")]] <- renderTable({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, regression = F, return.data = "correlation.matrix"
      )
    })
    
    ## "programs.duration"
    ## =============================================================================================================
    output[[paste("programs.duration", ".baujat", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("programs.duration", ".influence", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("programs.duration", ".baujat.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("programs.duration", ".influence.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("programs.duration", ".qq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("programs.duration", ".lin", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".lin.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".sq.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".lin.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("programs.duration", ".sq.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("programs.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".sq", sep = "")]][[outcome]])}
      )
    })
    
    ## "sessions.duration"
    ## =============================================================================================================
    output[[paste("sessions.duration", ".baujat", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.duration", ".influence", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.duration", ".baujat.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.duration", ".influence.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.duration", ".qq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.duration"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.duration", ".lin", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".lin.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".sq.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".lin.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.duration", ".sq.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.duration"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.duration", ".sq", sep = "")]][[outcome]])}
      )
    })
    
    ## "sessions.frequency"
    ## =============================================================================================================
    output[[paste("sessions.frequency", ".baujat", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.frequency", ".influence", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.frequency", ".baujat.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.frequency", ".influence.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("sessions.frequency", ".qq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("sessions.frequency"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("sessions.frequency", ".lin", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".lin.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".sq.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".lin.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("sessions.frequency", ".sq.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("sessions.frequency"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("sessions.frequency", ".sq", sep = "")]][[outcome]])}
      )
    })
    
    ## "follow.up.period"
    ## =============================================================================================================
    output[[paste("follow.up.period", ".baujat", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("follow.up.period", ".influence", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, return.data = "regression.results.linear"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("follow.up.period", ".baujat.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("follow.up.period", ".influence.sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      plot.influnece(print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T, return.data = "regression.results.poly"  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      ))
    })
    
    output[[paste("follow.up.period", ".qq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
        basic = F, moderator.vec = c("follow.up.period"), print.regplot = F, print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T, print.qq.norm = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][[outcome]])}
      )
    })
    
    output[[paste("follow.up.period", ".lin", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".sq", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".lin.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T  # ,
        # filter.regression.linear.list = if(length(outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".sq.w.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T  # ,
        # filter.regression.poly.list = if(length(outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".lin.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results(
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
        regression.label = T,
        filter.regression.linear.list = if(length(outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".lin", sep = "")]][[outcome]])}
      )
    })

    output[[paste("follow.up.period", ".sq.n.o.", outcome.alias, "w.o.m.r", sep = "")]] <- renderPlot({
      print.meta.results( 
        outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list.w.o.mean.r,
        basic = F, moderator.vec = c("follow.up.period"), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
        regression.label = T,
        filter.regression.poly.list = if(length(outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste("follow.up.period", ".sq", sep = "")]][[outcome]])}
      )
    })
    
# Network Meta-Analysis
    output[[paste("network.", outcome.alias, sep = "")]] <- renderPlot({
      net.meta.analyze(
        outcome, preferred.scale = preferred.scale, net.df = F, net.res = net.ress[[outcome]],
        plot.netgraph = T, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
        return.data = F, reference.group = "passive control", random = T
      )
    })
    
    output[[paste("net.forest.", outcome.alias, sep = "")]] <- renderPlot({
      net.meta.analyze(
        outcome, preferred.scale = preferred.scale, net.df = F, net.res = net.ress[[outcome]],
        plot.netgraph = F, plot.forest = T, plot.direct.evidence = F, plot.netheat = F,
        return.data = F, reference.group = "passive control", random = T
      )
    })
    
    output[[paste("net.dir.evidence.", outcome.alias, sep = "")]] <- renderPlot({
      net.meta.analyze(
        outcome, preferred.scale = preferred.scale, net.df = F, net.res = net.ress[[outcome]],
        plot.netgraph = F, plot.forest = F, plot.direct.evidence = T, plot.netheat = F,
        return.data = F, reference.group = "passive control", random = T
      )
    })
    
    output[[paste("net.heat.", outcome.alias, sep = "")]] <- renderPlot({
      net.meta.analyze(
        outcome, preferred.scale = preferred.scale, net.df = F, net.res = net.ress[[outcome]],
        plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = T,
        return.data = F, reference.group = "passive control", random = T
      )
    })
    
    try( 
      output[[paste("net.p.df.", outcome.alias, sep = "")]] <- renderTable(
        data.frame(
          net.meta.analyze(
            outcome, preferred.scale = preferred.scale, net.df = F, net.res = net.ress[[outcome]],
            plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
            return.data = "net.res", reference.group = "passive control", random = T
          )$pval.random
        ),
        digits = 4
      ),
      silent = T
    )
    
  }
  return(output)
}

# %% vscode={"languageId": "r"}
# # with loop for moderators (error: shows only follow.up period plot for every moderator yet)
# moderator.vec <- c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")

# return.outcome.output <- function(output, outcome.vec, preferred.scale = FALSE){
#   moderator.vec <- c("programs.duration", "sessions.duration", "sessions.frequency", "follow.up.period")
  
#   picture.size <- 400
  
#   for (outcome in outcome.vec){
#     # this for loop was inteded to set outputs for all outcomes, but the outpunts of all outcomes got set with the plots of the last outcome
#     # passing only one outcome as outcome.vec works
#     outliers <- outlier.list[[outcome]]
    
#     meta.df.list <- print.meta.results(
#       outcome, preferred.scale = preferred.scale,
#       regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
#       return.data = "meta.df.list"
#     )
    
#     outcome.alias <- gsub("([()])", "", gsub(" ", "_", outcome))  # because spaces and brackets could cause trouble later on
    
#   # Main Results
#     output[[paste("forest.", outcome.alias, sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = T, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F  # ,
#           # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#         )
#       }
#     )
    
#     output[[paste("subgroup.delivery.", outcome.alias, sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "delivery.mode", print.meta.results = F  # ,
#           # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#         )
#       },
#       height = 600  # adjust height of plot (independently of from the ui frame)
#     )

#     output[[paste("subgroup.type.", outcome.alias, sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "meditation.type", print.meta.results = F  # ,
#           # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#         )
#       },
#       height = 600
#     )

#     output[[paste("funnel.", outcome.alias, sep = "")]] <- renderPlot({
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = F, print.funnel = T, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
#         funnel.label = F  # ,
#         # filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#       )
#     })


#   # Main Results comparison with/without outliers
#     output[[paste("baujat.", outcome.alias, ".forest.comp", sep = "")]] <- renderPlot({  # n.o. = no outliers (included)
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = T, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
#       )
#     })
    
#     output[[paste("influence.", outcome.alias, ".forest", sep = "")]] <- renderPlot({
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = F, print.funnel = F, print.influence = T, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
#       )
#     })
    
#     output[[paste("influence.df.", outcome.alias, ".forest", sep = "")]] <- renderTable({
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
#         return.data = "influence.df"
#       )
#     })

#     output[[paste("forest.o.", outcome.alias, sep = "")]] <- renderPlot({  # o. = (with) outliers
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = T, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
#       )
#     })

#     output[[paste("forest.n.o.comp", outcome.alias, sep = "")]] <- renderPlot({
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = T, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
#         filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#       )
#     })
    
#     output[[paste("subgroup.delivery.", outcome.alias, ".o.", sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "delivery.mode", print.meta.results = F
#         )
#       },
#       height = 600  # adjust height of plot (independently of from the ui frame)
#     )

#     output[[paste("subgroup.delivery.", outcome.alias, ".n.o.comp", sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "delivery.mode", print.meta.results = F,
#           filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#         )
#       },
#       height = 600  # adjust height of plot (independently of from the ui frame)
#     )
    
#     output[[paste("subgroup.type.", outcome.alias, ".o.", sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "meditation.type", print.meta.results = F
#         )
#       },
#       height = 600  # adjust height of plot (independently of from the ui frame)
#     )

#     output[[paste("subgroup.type.", outcome.alias, ".n.o.comp", sep = "")]] <- renderPlot(
#       {
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = T, print.forest.sub.single = "meditation.type", print.meta.results = F,
#           filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#         )
#       },
#       height = 600  # adjust height of plot (independently of from the ui frame)
#     )

#     output[[paste("funnel.o.", outcome.alias, sep = "")]] <- renderPlot({  # o. = (with) outliers
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = F, print.funnel = T, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F
#       )
#     })

#     output[[paste("funnel.n.o.comp", outcome.alias, sep = "")]] <- renderPlot({
#       print.meta.results(
#         outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#         regression = F, print.forest = F, print.funnel = T, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
#         filter.forest..funnel.vec = if(length(outliers) == 0){FALSE}else{-outliers}
#       )
#     })

#     output[[paste("gosh.1.", outcome.alias, ".forest.comp", sep = "")]] <- renderImage({
#       list(
#         src = paste(gsub("/", "\\\\", getwd()), "\\", "Gosh Plots", "\\", "Gosh.Plot.", outcome, ".outlier.1", ".png", sep = ""),
#         contentType = 'image/png',
#         width = picture.size,
#         height = picture.size
#       )
#     }, deleteFile = F)

#     output[[paste("gosh.2.", outcome.alias, ".forest.comp", sep = "")]] <- renderImage({
#       list(
#         src = paste(gsub("/", "\\\\", getwd()), "\\", "Gosh Plots", "\\", "Gosh.Plot.", outcome, ".outlier.2", ".png", sep = ""),
#         contentType = 'image/png',
#         width = picture.size,
#         height = picture.size
#       )
#     }, deleteFile = F)

#     output[[paste("gosh.3.", outcome.alias, ".forest.comp", sep = "")]] <- renderImage({
#       list(
#         src = paste(gsub("/", "\\\\", getwd()), "\\", "Gosh Plots", "\\", "Gosh.Plot.", outcome, ".outlier.3", ".png", sep = ""),
#         contentType = 'image/png',
#         width = picture.size,
#         height = picture.size
#       )
#     }, deleteFile = F)
    
# # Regression
#     for (moderator in moderator.vec){
#       output[[paste(moderator, ".lin", outcome.alias, sep = "")]] <- renderPlot({
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
#           regression.label = T  # ,
#           # filter.regression.linear.list = if(length(outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]])}
#         )
#       })
      
#       output[[paste(moderator, ".sq", outcome.alias, sep = "")]] <- renderPlot({
#         print.meta.results( 
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
#           regression.label = T  # ,
#           # filter.regression.poly.list = if(length(outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]])}
#         )
#       })
      
#       output[[paste(moderator, ".lin.w.o.", outcome.alias, sep = "")]] <- renderPlot({
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
#           regression.label = T  # ,
#           # filter.regression.linear.list = if(length(outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]])}
#         )
#       })
      
#       output[[paste(moderator, ".sq.w.o.", outcome.alias, sep = "")]] <- renderPlot({
#         print.meta.results( 
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
#           regression.label = T  # ,
#           # filter.regression.poly.list = if(length(outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]])}
#         )
#       })
      
#       output[[paste(moderator, ".lin.n.o.", outcome.alias, sep = "")]] <- renderPlot({
#         print.meta.results(
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
#           regression.label = T,
#           filter.regression.linear.list = if(length(outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".lin", sep = "")]][[outcome]])}
#         )
#       })
      
#       output[[paste(moderator, ".sq.n.o.", outcome.alias, sep = "")]] <- renderPlot({
#         print.meta.results( 
#           outcome, preferred.scale = preferred.scale, meta.df.list = meta.df.list,
#           basic = F, moderator.vec = c(moderator), print.baujat.regression = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
#           regression.label = T,
#           filter.regression.poly.list = if(length(outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]]) == 0){FALSE}else{list(-outlier.list[[paste(moderator, ".sq", sep = "")]][[outcome]])}
#         )
#       })
#     }
#   }
#   return(output)
# }

# %% [markdown]
# ### Set summary domain outputs for server

# %% vscode={"languageId": "r"}
return.summary.output <- function(output, net.res.object, domain_name, outcome_vec) {
  # output for summary section (all outcomes included)
  output[[paste0("forest.summary.", domain_name)]] <- renderPlot(plot.summary.forest(net.res.object, outcome_vec = outcome_vec))
  output[[paste0("forest.summary.o..", domain_name)]] <- renderPlot(plot.summary.forest(net.res.object, outcome_vec = outcome_vec))
  output[[paste0("forest.summary.n.o..", domain_name)]] <- renderPlot(plot.summary.forest(net.res.object, outcome_vec = outcome_vec, with.outliers = F))
  
  output[[paste0("network.all.", domain_name)]] <- renderPlot({
    net.meta.analyze(
      outcome_vec, preferred.scale = F, net.df = F, net.res = net.res.object,
      plot.netgraph = T, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
      return.data = F, reference.group = "passive control", random = T
    )
  })
  output[[paste0("net.forest.all.", domain_name)]] <- renderPlot({
    net.meta.analyze(
      outcome_vec, preferred.scale = F, net.df = F, net.res = net.res.object,
      plot.netgraph = F, plot.forest = T, plot.direct.evidence = F, plot.netheat = F,
      return.data = F, reference.group = "passive control", random = T
    )
  })
  output[[paste0("net.dir.evidence.all.", domain_name)]] <- renderPlot({
    net.meta.analyze(
      outcome_vec, preferred.scale = F, net.df = F, net.res = net.res.object,
      plot.netgraph = F, plot.forest = F, plot.direct.evidence = T, plot.netheat = F,
      return.data = F, reference.group = "passive control", random = T
    )
  })
  output[[paste0("net.heat.all.", domain_name)]] <- renderPlot({
    net.meta.analyze(
      outcome_vec, preferred.scale = F, net.df = F, net.res = net.res.object,
      plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = T,
      return.data = F, reference.group = "passive control", random = T
    )
  })
  
  output[[paste0("net.p.df.all.", domain_name)]] <- renderTable(data.frame(net.res.object$pval.random), digits = 4)

  net.res.split <- netsplit(net.res.object)
  output[[paste0("netsplit.summary.", domain_name)]] <- renderPlot({plot(net.res.split)})
  
  return(output)
}

# %% [markdown] heading_collapsed=true
# ## Save all Gosh Plots as .png (making dashboard faster)

# %% hidden=true vscode={"languageId": "r"}
# plot.group.list <- list(
#   main.effects = outlier.list,
#   regressions.single = list(
#     list(
#       # only outcomes for that a regression gets calculated
#       # per outcome...
#       programs.duration = c(),
#       sessions.duration = c(),
#       programs.frequency = c(),
#       follow.up.period = c()
#     )
#   )
# )

# preferred.scale <- "PSS"
# frame.size <- 11
# resolution.png <- 600

# for (outcome in present.outcomes){  # index over present.outcomes all outcomes
#   results.metafor <- print.meta.results(
#     outcome, preferred.scale = if(outcome == "Stress"){preferred.scale}else{FALSE}, return.data = "results.metafor",
#     print.forest = F, print.funnel = F, print.meta.results = F, split.subgroups = F, print.influence = F, regression = F, print.baujat = F
#   )
  
#   if (is.null(results.metafor)) {
#     next  # skip to the next iteration if results.metafor is NULL as there are no studies for this outcome
#   }

#   for (plot.group.name in names(plot.group.list)){ # index over names(plot.group.list) instead of "main.effects" to get also the regression gosh plots
#     i <- 1
#     for (outlier in plot.group.list[[plot.group.name]][[outcome]]){  # index over plot.group.name insead of "main.effects" to get also the regression gosh plots
#       png(
#         file.path(
#           "Gosh Plots",
#           paste0(
#             "Gosh.Plot.",
#             outcome,
#             if(outcome == "Stress"){paste0(".", preferred.scale)}else{""},
#             ".outlier.", i, ".png"
#           )
#         ),
#         width=frame.size, height=frame.size, units="in", res=resolution.png
#       )
#       par(mar=c(frame.size,frame.size,1,1))
#       sav <- gosh(results.metafor)
#       plot(sav, out = outlier)
#       dev.off()
#       i <- i + 1
#     }
#   }
# }

# %% [markdown]
# ## Dashboard

# %% vscode={"languageId": "r"}
# without separation of trait/state
ui <- dashboardPage(
  dashboardHeader(title = "Meta-Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "Summary_page", icon = icon("flash", lib = 'glyphicon')),
      menuItem("Summary_Resilience_Scale", tabName = "Summary_Resilience_Scale_page", icon = icon("flash", lib = 'glyphicon')),
      menuItem("Summary_Mental_Health", tabName = "Summary_Mental_Health_page", icon = icon("flash", lib = 'glyphicon')),
      menuItem("Summary_Secondary_Outcomes", tabName = "Summary_Secondary_Outcomes_page", icon = icon("flash", lib = 'glyphicon')),
      menuItem("Resilience_Scale", tabName = "Resilience_Scale_page", icon = icon("line-chart")),
      menuItem("Anxiety", tabName = "Anxiety_page", icon = icon("line-chart")),
      menuItem("Depression", tabName = "Depression_page", icon = icon("line-chart")),
      menuItem("Stress", tabName = "Stress_page", icon = icon("line-chart")),
      menuItem("Well-being", tabName = "Well-being_page", icon = icon("line-chart")),
      menuItem("Acceptance", tabName = "Acceptance_page", icon = icon("line-chart")),
      menuItem("Active_coping", tabName = "Active_coping_page", icon = icon("line-chart")),
      menuItem("Cognitive_control", tabName = "Cognitive_control_page", icon = icon("line-chart")),
      menuItem("Empathy", tabName = "Empathy_page", icon = icon("line-chart")),
      menuItem("Hope", tabName = "Hope_page", icon = icon("line-chart")),
      menuItem("Mindfulness", tabName = "Mindfulness_page", icon = icon("line-chart")),
      menuItem("Optimism", tabName = "Optimism_page", icon = icon("line-chart")),
      menuItem("Positive_affect", tabName = "Positive_affect", icon = icon("line-chart")),
      menuItem("Positive_emotion", tabName = "Positive_emotion_page", icon = icon("line-chart")),
      menuItem("Religious_coping", tabName = "Religious_coping_page", icon = icon("line-chart")),
      menuItem("Self-acceptance", tabName = "Self-acceptance_page", icon = icon("line-chart")),
      menuItem("Self-compassion", tabName = "Self-compassion_page", icon = icon("line-chart")),
      menuItem("Self-efficacy", tabName = "Self-efficacy_page", icon = icon("line-chart")),
      menuItem("Self-esteem", tabName = "Self-esteem_page", icon = icon("line-chart"))
    ),
    sidebarMenuOutput("menu")
  ),
  dashboardBody(tabItems(
    
  # Summary Pages
    set.summary.page(
      net.res.object = net.res.list$net.res.all$res.object,
      domain_name = "all",
      tabName = "Summary_page"
    ),
    set.summary.page(
      net.res.object = net.res.list$net.res.resilience.scale$res.object,
      domain_name = "resilience.scale",
      tabName = "Summary_Resilience_Scale_page"
    ),
    set.summary.page(
      net.res.object = net.res.list$net.res.mental.health$res.object,
      domain_name = "mental.health",
      tabName = "Summary_Mental_Health_page"
    ),
    set.summary.page(
      net.res.object = net.res.list$net.res.secondary.outcomes$res.object,
      domain_name = "secondary.outcomes",
      tabName = "Summary_Secondary_Outcomes_page"
    ),
  # Outcomes' pages

    set.outcome.page("Resilience Scale"),
    set.outcome.page("Anxiety"),
    set.outcome.page("Depression"),
    set.outcome.page("Stress", preferred.scale = "DASS"),
    set.outcome.page("Well-being"),
    set.outcome.page("Acceptance"),
    set.outcome.page("Active coping"),
    set.outcome.page("Cognitive control"),
    set.outcome.page("Empathy"),
    set.outcome.page("Hope"),
    set.outcome.page("Mindfulness"),
    set.outcome.page("Optimism"),
    set.outcome.page("Positive affect"),
    set.outcome.page("Positive emotion"),
    set.outcome.page("Religious coping"),
    set.outcome.page("Self-acceptance"),
    set.outcome.page("Self-compassion"),
    set.outcome.page("Self-efficacy"),
    set.outcome.page("Self-esteem")
  ))
)


# Server -------------------------------------------------------
server <- function(input, output, session) {
  
  # end session by closing the window / tab
  session$onSessionEnded(function() {
    stopApp()
  })

  # outputs for summary sections per domain
  output <- return.summary.output(
    output,
    net.res.object = net.res.list$net.res.all$res.object,
    domain_name = "all",
    outcome_vec = net.res.list$net.res.all$included.outcomes
  )
  output <- return.summary.output(
    output,
    net.res.object = net.res.list$net.res.resilience.scale$res.object,
    domain_name = "resilience.scale",
    outcome_vec = net.res.list$net.res.resilience.scale$included.outcomes
  )
  output <- return.summary.output(
    output,
    net.res.object = net.res.list$net.res.mental.health$res.object,
    domain_name = "mental.health",
    outcome_vec = net.res.list$net.res.mental.health$included.outcomes
  )
  output <- return.summary.output(
    output,
    net.res.object = net.res.list$net.res.secondary.outcomes$res.object,
    domain_name = "secondary.outcomes",
    outcome_vec = net.res.list$net.res.secondary.outcomes$included.outcomes
  )
  
  # output$net.p.df.all <- renderTable(data.frame(net.res.all$pval.random), digits = 4)
  
  # outputs for outcomes
  output <- return.outcome.output(output, "Resilience Scale")
  output <- return.outcome.output(output, "Anxiety")  # "Only equal moderator values present for regression of outcome:\"Anxiety\" and moderator: \"follow.up.period\""
  output <- return.outcome.output(output, "Depression")
  output <- return.outcome.output(output, "Stress", preferred.scale = "DASS")
  output <- return.outcome.output(output, "Well-being")
  output <- return.outcome.output(output, "Acceptance")
  output <- return.outcome.output(output, "Active coping")
  output <- return.outcome.output(output, "Cognitive control")
  output <- return.outcome.output(output, "Empathy")
  output <- return.outcome.output(output, "Hope")
  output <- return.outcome.output(output, "Mindfulness")
  output <- return.outcome.output(output, "Optimism")
  output <- return.outcome.output(output, "Positive affect", preferred.scale = "IPANAT")
  output <- return.outcome.output(output, "Positive emotion")
  output <- return.outcome.output(output, "Religious coping")
  output <- return.outcome.output(output, "Self-acceptance")
  output <- return.outcome.output(output, "Self-compassion", preferred.scale = "SCS")
  output <- return.outcome.output(output, "Self-efficacy")
  output <- return.outcome.output(output, "Self-esteem")
}

shinyApp(ui, server)

# %% vscode={"languageId": "r"}
# Plot summary forest plots for once without overall estimate and once for all domains with overall estimate
options(repr.plot.width = 25, repr.plot.height = 9, repr.plot.res = 350)

plot.summary.forest(
  overall.measure = "none", outcome_vec = present.outcomes,
  title = ""
)

for (net.res.name in names(net.res.list)) {
  net.res.object <- net.res.list[[net.res.name]]$res.object
  if (is.null(net.res.object) || class(net.res.object) != "netmeta") {
    message(paste0("Skipping ", net.res.name, " as net.res.object is NULL or not of class 'netmeta' (is likely to have no results)."))
    next  # skip to the next iteration if net.res.object is NULL
  }
  message(paste0("--- Plotting summary forest for ", net.res.name), " -------------------------------------")
  plot.summary.forest(
    net.res.object, outcome_vec = net.res.list[[net.res.name]]$included.outcomes,
    title = paste("Summary Forest Plot for", net.res.name)
  )
}

# %% [markdown] heading_collapsed=true
# # Risk of Bias Assessment

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 6, repr.plot.height = 20, repr.plot.res = 150)

# %% hidden=true vscode={"languageId": "r"}
# devtools::install_github("mcguinlu/robvis")  # install robvis from github so it includes the rob_forest function
library("robvis")

# %% hidden=true vscode={"languageId": "r"}
outcome.names.df

# %% hidden=true vscode={"languageId": "r"}
rob.df

# %% vscode={"languageId": "r"}

# %% hidden=true vscode={"languageId": "r"}
rob_traffic_light(
  data = rob.df,
  tool = "ROB2",
  psize = 5
)

# %% hidden=true vscode={"languageId": "r"}
# print numbers and study labels
i <- 1
for (label in rownames(rob.df)){
  cat(i, " = ", label, ", ", sep = "")
  i <- i + 1
}

# %% hidden=true vscode={"languageId": "r"}
# outcomes with high RoB
outcomes.high.rob <- as.character(unique(unlist(outcome.names.df[studies.high.rob,])))  # get unique values from df
outcomes.high.rob <- outcomes.high.rob[!(outcomes.high.rob == "Other: " | is.na.or.nm(outcomes.high.rob))]  # filter out "NA", "Other: "
outcomes.high.rob

# %% hidden=true vscode={"languageId": "r"}
# get study labels per outcome in which high rob studies could be present within the analyses
studs.p.high.rob.outc.list <- list()

i <- 1
for (outcome in outcomes.high.rob){
  studs.h.r <- print.meta.results(
    outcome, preferred.scale = if (outcome == "Stress"){
      "DASS"
    } else if (outcome == "Positive affect"){
      "IPANAT"
    } else if (outcome == "Self-compassion"){
      "SCS"
    } else {F},
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
    split.subgroups = F, print.forest.sub.single = F, subgroup.method = "fixed", print.meta.results = F,
    return.data = "results.meta"
  )$studlab
  
  studs.p.high.rob.outc.list[[i]] <- studs.h.r
  i <- i + 1
}
names(studs.p.high.rob.outc.list) <- outcomes.high.rob
studs.p.high.rob.outc.list

# %% hidden=true vscode={"languageId": "r"}
# check if high rob studies are included in calculations
for(outcome in outcomes.high.rob){
  for (study in studies.high.rob){
    if (study %in% studs.p.high.rob.outc.list[[outcome]]){
      cat(outcome, study, "\n")
    }
  }
}
# --> only Mindfulness could be influence by a high rob study (Johnson-Waddell 2018)

# %% hidden=true vscode={"languageId": "r"}
studies.high.rob

# %% hidden=true vscode={"languageId": "r"}
options(repr.plot.width = 9, repr.plot.height = 3, repr.plot.res = 200)
rob_summary(
  data = rob.df,
  tool = "ROB2",
  overall = T,
  weighted = F
)

# %% [markdown] heading_collapsed=true
# # Generate List of all used Packages and versions

# %% hidden=true vscode={"languageId": "r"}
session <-sessionInfo()

# %% hidden=true vscode={"languageId": "r"}
session

# %% hidden=true vscode={"languageId": "r"}
session$R.version$version.string

# %% hidden=true vscode={"languageId": "r"}
packages.loaded <- names(session$otherPkgs)
versions <- c()
i <- 1
for (pkg in packages.loaded){
  versions[i] <- session$otherPkgs[[pkg]]$Version
  i <- i + 1
}
version.df <- data.frame(
  Package = c(packages.loaded),
  Version = c(versions)
) |> arrange(Package)

write.csv(t(version.df), "lib.versions.csv")
version.df

# %% hidden=true vscode={"languageId": "r"}

# %% [markdown]
# # Meta-Analysis Plots

# %% vscode={"languageId": "r"}
# set plot size
# options(repr.plot.width = 12, repr.plot.height = 8, repr.plot.res = 400)

# unversal functions
# forest etc.
# print.meta.results(
#   "Stress", preferred.scale = "DASS",
#   regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
#   split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
#   return.data = "results.meta" # ,
#   # filter.forest..funnel.vec = - outlier.list[["Stress"]]
# )

# regression etc.
# print.meta.results(
#   "Stress", preferred.scale = "DASS",
#   basic = F, moderator.vec = c("programs.duration"), print.regplot = F, print.baujat.regression = F, print.influence = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
#   regression.label = T, return.data = "regression.results.linear"  # ,
#   # filter.regression.linear.list = if(length(outlier.list[[paste("programs.duration", ".lin", sep = "")]][["Stress"]]) == 0){FALSE}else{list(-outlier.list[[paste("programs.duration", ".lin", sep = "")]][["Stress"]])}
# )

# net.meta.analyze(
#   c("Stress"), preferred.scale = "DASS", net.df = F, net.res = F, comparisons.skip.list = F,
#   plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = T,
#   reference.group = "passive control", random = T, return.data = F
# )

# %% vscode={"languageId": "r"}
# forest plots
options(repr.plot.width = 12, repr.plot.height = 8, repr.plot.res = 400)
for (outcome in present.outcomes.sorted){
  print.meta.results(
    outcome, preferred.scale = get.1st.preferred.scale(outcome),
    regression = F, print.forest = ifelse(outcome %in% outcomes.no.10.plus.passive, F, T), print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = ifelse(outcome %in% outcomes.no.10.plus.passive, T, F), print.forest.sub.single = F, print.meta.results = F,
    # return.data = "results.meta" # ,
    # filter.forest..funnel.vec = - outlier.list[["Stress"]]
  )
}

# %% vscode={"languageId": "r"}
# netgraph
net.meta.analyze(
  present.outcomes, net.df = F, net.res = net.res.all, comparisons.skip.list = F,
  plot.netgraph = T, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
  reference.group = "passive control", random = T, return.data = F
)

# %% [markdown]
# ## Save plots as png

# %% vscode={"languageId": "r"}
# set plot size
options(repr.plot.width = 7, repr.plot.height = 4, repr.plot.res = 400)

# %% [markdown]
# ### Network forest plots per outcome

# %% vscode={"languageId": "r"}
# save all funnel plots
i <- 1
j <- 0
for (outcome in present.outcomes.sorted){
  cat("\n", outcome, j, "\n")
  net.res.outcome <- net.meta.analyze(
    outcome, net.df = F, net.res = F, comparisons.skip.list = F,
    plot.netgraph = F, plot.forest = F, plot.direct.evidence = F, plot.netheat = F,
    reference.group = "passive control", random = T, return.data = "net.res", silent = T
  )
  
  if (!is.logical(net.res.outcome)){
    png(paste("Network Forest Plots/", i, ".", outcome, ".net.forest.png", sep = ""))

    net.meta.analyze(
      outcome, net.df = F, net.res = net.res.outcome, comparisons.skip.list = F,
      plot.netgraph = F, plot.forest = T, plot.direct.evidence = F, plot.netheat = F,
      reference.group = "passive control", random = T, return.data = F, silent = T
    )
    dev.off()
    i <- i + 1
  }
  j <- j + 1
}

# %% [markdown] heading_collapsed=true
# ### Funnel plots

# %% hidden=true vscode={"languageId": "r"}
# save all funnel plots
i <- 1
for (outcome in present.outcomes.sorted){
  
  meta.df.list <- print.meta.results(
    outcome, preferred.scale = get.1st.preferred.scale(outcome),
    regression = F, print.forest = F, print.funnel = T, print.influence = F, print.baujat = F,
    split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "meta.df.list"
  )
  
  if (nrow(meta.df.list[[1]]) >= 3){
    png(paste("Funnel Plots/", i, ".", outcome, ".funnel.png", sep = ""), width = 1000)

    print.meta.results(
      outcome, preferred.scale = get.1st.preferred.scale(outcome), , meta.df.list = meta.df.list,
      regression = F, print.forest = F, print.funnel = T, print.influence = F, print.baujat = F,
      split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    )
    dev.off()
    i <- i + 1
  }
}

# %% [markdown] heading_collapsed=true
# ### Baujat plots

# %% hidden=true jupyter={"outputs_hidden": true} vscode={"languageId": "r"}
# save all Baujat Plots plots (random-effects models)
i <- 1
for (outcome in present.outcomes.sorted){
  
  meta.df.list <- print.meta.results(
    outcome, preferred.scale = get.1st.preferred.scale(outcome),
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
    split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "meta.df.list"
  )
  
  if (nrow(meta.df.list[[1]]) >= 3){
    png(paste("Baujat Plots/", i, ".", outcome, ".baujat.png", sep = ""), width = 1000)

    print.meta.results(
      outcome, preferred.scale = get.1st.preferred.scale(outcome), meta.df.list = meta.df.list,
      regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = T,
      split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    )
    dev.off()
    i <- i + 1
  }
}

# %% hidden=true vscode={"languageId": "r"}
# save all Baujat Plots plots (linear mixed-effects meta-regression)
for (outcome in outcomes.no.10.plus.passive){
  for (moderator in c('programs.duration', 'sessions.duration', 'sessions.frequency', 'follow.up.period', "delivery.mode", "meditation.type")){
    if (outcome == "Anxiety" & moderator == "follow.up.period"){
      next
    }
    png(paste("Baujat Plots/", outcome, ".", moderator, ".baujat.png", sep = ""), width = 1000)

    print.meta.results(
      outcome, preferred.scale = get.1st.preferred.scale(outcome),
      basic = F, moderator.vec = c(moderator), print.regplot = F, print.baujat.regression = T, print.influence = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
      regression.label = T
    )
    dev.off()
  }
}

# %% hidden=true vscode={"languageId": "r"}
# save all Baujat Plots plots (quadratic/squared mixed-effects meta-regression)
for (outcome in c("Anxiety", "Stress")){
  for (moderator in c('programs.duration', 'sessions.duration', 'sessions.frequency', 'follow.up.period')){
    if (outcome == "Anxiety" & moderator == "follow.up.period"){
      next
    }
    png(paste("Baujat Plots/", outcome, ".", moderator, ".baujat.squared.png", sep = ""), width = 1000)

    print.meta.results(
      outcome, preferred.scale = get.1st.preferred.scale(outcome),
      basic = F, moderator.vec = c(moderator), print.regplot = F, print.baujat.regression = T, print.influence = F, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
      regression.label = T
    )
    dev.off()
  }
}

# %% [markdown] heading_collapsed=true
# ### Influence plots

# %% hidden=true jupyter={"outputs_hidden": true} vscode={"languageId": "r"}
# save all Influence Plots plots
i <- 1
for (outcome in present.outcomes.sorted){
  
  meta.df.list <- print.meta.results(
    outcome, preferred.scale = get.1st.preferred.scale(outcome),
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F,
    split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "meta.df.list"
  )
  
  if (nrow(meta.df.list[[1]]) >= 3){
    png(paste("Influence Plots/", i, ".", outcome, ".influence.png", sep = ""), width = 1000)

    print.meta.results(
      outcome, preferred.scale = get.1st.preferred.scale(outcome), meta.df.list = meta.df.list,
      regression = F, print.forest = F, print.funnel = F, print.influence = T, print.baujat = F,
      split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    )
    dev.off()
    i <- i + 1
  }
}

# %% hidden=true vscode={"languageId": "r"}
# save all Baujat Plots plots (linear mixed-effects meta-regression)
for (outcome in outcomes.no.10.plus.passive){
  for (moderator in c('programs.duration', 'sessions.duration', 'sessions.frequency', 'follow.up.period', "delivery.mode", "meditation.type")){
    if (outcome == "Anxiety" & moderator == "follow.up.period"){
      next
    }
    png(paste("Influence Plots/", outcome, ".", moderator, ".influence.png", sep = ""), width = 1000)

    print.meta.results(
      outcome, preferred.scale = get.1st.preferred.scale(outcome),
      basic = F, moderator.vec = c(moderator), print.regplot = F, print.baujat.regression = F, print.influence = T, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
      regression.label = T
    )
    dev.off()
  }
}

# %% hidden=true vscode={"languageId": "r"}
# save all Baujat Plots plots (quadratic/squared mixed-effects meta-regression)
for (outcome in outcomes.no.10.plus.passive){
  for (moderator in c('programs.duration', 'sessions.duration', 'sessions.frequency', 'follow.up.period')){
    if (outcome == "Anxiety" & moderator == "follow.up.period"){
      next
    }
    png(paste("Influence Plots/", outcome, ".", moderator, ".influence.squared.png", sep = ""), width = 1000)

    print.meta.results(
      outcome, preferred.scale = get.1st.preferred.scale(outcome),
      basic = F, moderator.vec = c(moderator), print.regplot = F, print.baujat.regression = F, print.influence = T, print.regression.results = F, regression.degree.1 = F, regression.degree.2 = T,
      regression.label = T
    )
    dev.off()
  }
}

# %% [markdown] heading_collapsed=true
# ### QQ plots

# %% [markdown] heading_collapsed=true
# ## Plots per single outcomes

# %% hidden=true vscode={"languageId": "r"}
for (outcome in c("Anxiety", "Stress")){
  for (moderator in c('programs.duration', 'sessions.duration', 'sessions.frequency', 'follow.up.period')){
    if (outcome == "Anxiety" & moderator == "follow.up.period"){
      next
    }
    png(paste("QQ Plots/", outcome, ".", moderator, ".QQ.plot.png", sep = ""), width = 1000)

    print.meta.results(
      outcome, preferred.scale = get.1st.preferred.scale(outcome),
      basic = F, moderator.vec = c(moderator), print.regplot = F, print.baujat.regression = F, print.influence = F, print.regression.results = F, regression.degree.1 = T, regression.degree.2 = F,
      regression.label = T, print.qq.norm = T
    )
    dev.off()
  }
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Resilience Scale

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Resilience Scale")

# %% [markdown] heading_collapsed=true hidden=true
# ### Mental Health-related Outcomes

# %% [markdown] heading_collapsed=true hidden=true
# #### Anxiety

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Anxiety")

# %% hidden=true vscode={"languageId": "r"}
results.metafor.anxiety.t <- print.meta.results("Anxiety", return.data = "results.metafor", regression = T, regression.multiple = F)

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Anxiety", filter.forest..funnel.vec = -c(6), regression = F, split.subgroups = F, print.influence = F)

# %% hidden=true vscode={"languageId": "r"}
for (outlier in c(6)){
  sav <- gosh(results.metafor.anxiety.t)
  plot(sav, out = outlier)
}

# %% [markdown] heading_collapsed=true hidden=true
# #### Depression

# %% hidden=true vscode={"languageId": "r"}
results.metafor.depression.t <- print.meta.results("Depression", return.data = "results.metafor")

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Depression", filter.forest..funnel.vec = -c(1, 2, 7), regression = F, split.subgroups = F, print.influence = F)

# %% hidden=true vscode={"languageId": "r"}
for (outlier in c(1, 2, 7)){
  sav <- gosh(results.metafor.depression.t)
  plot(sav, out = outlier)
}

# %% [markdown] heading_collapsed=true hidden=true
# #### Stress

# %% [markdown] heading_collapsed=true hidden=true
# ##### DASS as preferred scale

# %% hidden=true vscode={"languageId": "r"}
results.metafor.stress.dass <- print.meta.results("Stress", preferred.scale = "DASS", return.data = "results.metafor")

# %% hidden=true vscode={"languageId": "r"}
results.meta.stress.dass <- print.meta.results(
    "Stress", preferred.scale = "DASS",
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "results.meta"
)
results.meta.stress.dass$k
results.meta.stress.dass$n.e.pooled
results.meta.stress.dass$n.c.pooled
results.meta.stress.dass$n.e.pooled + results.meta.stress.dass$n.c.pooled

# %% [markdown] heading_collapsed=true hidden=true
# ###### Sensitivity Analysis

# %% hidden=true vscode={"languageId": "r"}
# exclude high RoB studies
print.meta.results("Stress", preferred.scale = "DASS", exclude.high.rob.vec = studies.high.rob)  # is same for PSS

# %% hidden=true vscode={"languageId": "r"}
# without outliers of Stress (DASS) + gosh plots (for regression)
print.meta.results(
  "Stress", preferred.scale = "DASS", basic = T, filter.forest..funnel.vec = -c(1, 12, 14),
  filter.regression.linear.list = list(-c(10), -c(11), -c(1), -c(1, 12, 14), "", ""),
  filter.regression.poly.list = list(-c(1, 10), -c(11), -c(1), -c(1, 12, 14), "", ""),
  regression = T, split.subgroups = T, print.influence = F, print.baujat.regression = F, print.gosh.regression = F
)

# order of regression filter
# "sessions.duration":1, "sessions.frequency":2, "programs.duration":3, "follow.up.period":4, "delivery.mode":5, "meditation.type":6

# %% hidden=true vscode={"languageId": "r"}
# without outliers of Stress (DASS) + gosh plots (for regression)   ###### copy for flag.x.s.r = 1 #####
print.meta.results(
  "Stress", preferred.scale = "DASS", basic = F, filter.forest..funnel.vec = -c(1, 12, 14),
  filter.regression.linear.list = list(c(""), -c(10), -c(1), -c(1, 11, 13), "", ""),
  filter.regression.poly.list = list(c(""), -c(10), -c(1), -c(11, 13), "", ""),
  regression = T, split.subgroups = F, print.influence = F, print.baujat.regression = F, print.gosh.regression = T
)

# order of regression filter
# "sessions.duration":1, "sessions.frequency":2, "programs.duration":3, "follow.up.period":4, "delivery.mode":5, "meditation.type":6

# %% hidden=true vscode={"languageId": "r"}
results.metafor.stress.dass <- print.meta.results(
    "Stress", preferred.scale = "DASS",
    regression = F, print.forest = F, print.funnel = F, print.influence = F, print.baujat = F, split.subgroups = F, print.forest.sub.single = F, print.meta.results = F,
    return.data = "results.metafor"
)

# %% hidden=true vscode={"languageId": "r"}
# gosh analysis by dmetar
res.gosh <- gosh(results.metafor.stress.dass)
res.gosh.diag <- gosh.diagnostics(
  res.gosh,
  km.params = list(centers = 2),
  db.params = list(
    eps = 0.08,
    MinPts = 50)
)
res.gosh.diag

# %% hidden=true vscode={"languageId": "r"}
plot(res.gosh)

# %% hidden=true vscode={"languageId": "r"}
res.gosh.diag.default <- gosh.diagnostics(res.gosh)

# %% hidden=true vscode={"languageId": "r"}
res.gosh.diag.default

# %% hidden=true vscode={"languageId": "r"}
plot(res.gosh.diag.default)

# %% hidden=true vscode={"languageId": "r"}
plot(res.gosh.diag)

# %% hidden=true vscode={"languageId": "r"}
sav <- gosh(results.metafor.stress.dass)
plot(sav, out = 1)

# %% hidden=true vscode={"languageId": "r"}
sav <- gosh(results.metafor.stress.dass)
plot(sav, out = 12)

# %% hidden=true vscode={"languageId": "r"}
sav <- gosh(results.metafor.stress.dass)
plot(sav, out = 14)

# %% hidden=true vscode={"languageId": "r"}
# # gosh plots for forest plots
# for (outlier in c(1, 12, 14)){
#   sav <- gosh(results.metafor.stress.dass)
#   plot(sav, out = outlier)
# }

# %% hidden=true vscode={"languageId": "r"}
# gosh plots for regression (only for programs duration as one wasnt printed above)
print.meta.results(
  "Stress", preferred.scale = "DASS",
  filter.regression.linear.list = list("", "", -c(1), "", "", ""),
  filter.regression.poly.list = list("", "", -c(1), "", "", ""),
  basic = F, regression = T, print.baujat.regression = F,
  print.regplot = F, print.regression.results = F,
  print.gosh.regression = T
)

# %% hidden=true vscode={"languageId": "r"}
# multiple regressions without interaction
options(repr.plot.width = 12, repr.plot.height = 9, repr.plot.res = 100)
print.meta.results(
  outcome = "Stress", preferred.scale = "DASS", basic = F, regression = F, regression.multiple = T, regression.multiple.degree.2 = F,
  moderator.multiple.list = list(
    c("programs.duration", "sessions.duration"), c("programs.duration", "sessions.frequency"),
    c("sessions.duration", "programs.duration"), c("sessions.duration", "sessions.frequency"),
    c("sessions.frequency", "programs.duration"), c("sessions.frequency", "sessions.duration"),
    c("sessions.frequency", "programs.duration", "sessions.duration")
  )
)
# "sessions.duration":1, "sessions.frequency":2, "programs.duration":3, "follow.up.period":4, "delivery.mode":5, "meditation.type":6
options(repr.plot.width = 10, repr.plot.height = 9, repr.plot.res = 100)

# %% hidden=true vscode={"languageId": "r"}
# multiple regressions without interaction (without outliers)

options(repr.plot.width = 12, repr.plot.height = 9, repr.plot.res = 100)

print.meta.results(
  outcome = "Stress", preferred.scale = "DASS", basic = F, regression = F, regression.multiple = T, regression.multiple.degree.2 = F,
  print.gosh.regression = F, print.baujat.regression = F,
  moderator.multiple.list = list(
                                                  c("programs.duration", "sessions.duration"), c("programs.duration", "sessions.frequency"),
    c("sessions.duration", "programs.duration"),                                               c("sessions.duration", "sessions.frequency"),
    c("sessions.frequency", "programs.duration"), c("sessions.frequency", "sessions.duration"),
    c("sessions.frequency", "programs.duration", "sessions.duration")
    
  ),
  filter.multiple.regression.linear.list = list(
                -c(1),   -c(10, 7),
    -c(1),               -c(9),
    -c(10, 7), -c(9),
    -c(5)
  )
)

options(repr.plot.width = 10, repr.plot.height = 9, repr.plot.res = 100)

# %% hidden=true vscode={"languageId": "r"}
# multiple regressions without interaction (without outliers)  ##### copy for without mean ranges #####

options(repr.plot.width = 12, repr.plot.height = 9, repr.plot.res = 100)

print.meta.results(
  outcome = "Stress", preferred.scale = "DASS", basic = F, regression = F, regression.multiple = T, regression.multiple.degree.2 = F,
  print.gosh.regression = T, print.baujat.regression = F,
  moderator.multiple.list = list(
                                                  c("programs.duration", "sessions.duration"), c("programs.duration", "sessions.frequency"),
    c("sessions.duration", "programs.duration"),                                               c("sessions.duration", "sessions.frequency"),
    c("sessions.frequency", "programs.duration"), c("sessions.frequency", "sessions.duration"),
    c("sessions.frequency", "programs.duration", "sessions.duration")
    
  ),
  filter.multiple.regression.linear.list = list(
              -c(1, 7), "",
    -c(1, 7),            -c(1,7),
    "",       -c(1,7),
    ""
  )
)

options(repr.plot.width = 10, repr.plot.height = 9, repr.plot.res = 100)

# %% hidden=true vscode={"languageId": "r"}
# print gosh plot separately, as it came to an error within do.multiple.regression()
moderator.multiple.list = list(
                                                c("programs.duration", "sessions.duration"), c("programs.duration", "sessions.frequency"),
  c("sessions.duration", "programs.duration"),                                               c("sessions.duration", "sessions.frequency"),
  c("sessions.frequency", "programs.duration"), c("sessions.frequency", "sessions.duration")
)

filter.multiple.regression.linear.list = list(
              -c(1),   -c(10, 7),
  -c(1),               -c(9),
  -c(10, 7), -c(9)
)

mod.comb.no <- 1
for (moderator.combination in moderator.multiple.list){
    
  regression.df <- meta.analyze(
    "Stress", meditation.type.all, m.data.list, preferred.scale = "DASS",
    split.subgroups = FALSE, return.data = "regression.all"
  )

  # cut out data after post-test (that have follow.up.period > 0) if follow.up.period is not in moderator.combination
  if (!"follow.up.period" %in% moderator.combination){
    regression.df <- regression.df[
      which(regression.df[,"follow.up.period"] == 0),
    ]
  }

  # eliminate NAs
  regression.df <- regression.df %>%
    filter(!(
      is.na(!!sym(moderator.combination[1])) |
      !!sym(moderator.combination[1]) %in% c("NA", nm.placeholder, as.character(nm.placeholder)) |
      is.na(!!sym(moderator.combination[2])) |
      !!sym(moderator.combination[2]) %in% c("NA", nm.placeholder, as.character(nm.placeholder))
  ))
  
  if (
    is.list(filter.multiple.regression.linear.list) &
    if(is.list(filter.multiple.regression.linear.list)){filter.multiple.regression.linear.list[[mod.comb.no]][1] != ""}else{FALSE}
  ){

    for (outlier in -filter.multiple.regression.linear.list[[mod.comb.no]]){

      results.regression.linear.unfiltered <- get.results.metafor(
        regression.df, moderator = moderator.combination
      )

      cat("\n Gosh plot for", paste(moderator.combination, sep = ", "), "(outlier number:", outlier, "linear)")
      sav <- gosh(results.regression.linear.unfiltered, subsets = 15000)
      plot(sav, out = outlier)
    }
  } else {
    cat("\nno Gosh plot printed for", moderator.combination, "(linear)\n")
  }
  mod.comb.no <- mod.comb.no+1
}

# %% hidden=true vscode={"languageId": "r"}
# multiple regression with interaction (no outliers detected)

options(repr.plot.width = 12, repr.plot.height = 9, repr.plot.res = 100)

print.meta.results(
  outcome = "Stress", preferred.scale = "DASS", basic = F, regression = F, regression.multiple = T, regression.multiple.degree.2 = F,
  non.interaction = F, interaction = T,
  moderator.multiple.list = list(
    c("programs.duration", "sessions.duration"), c("programs.duration", "sessions.frequency"),
    c("sessions.duration", "programs.duration"), c("sessions.duration", "sessions.frequency"),
    c("sessions.frequency", "programs.duration"), c("sessions.frequency", "sessions.duration"),
    c("sessions.frequency", "programs.duration", "sessions.duration")
  )
)

options(repr.plot.width = 10, repr.plot.height = 9, repr.plot.res = 100)
# "sessions.duration":1, "sessions.frequency":2, "programs.duration":3, "follow.up.period":4, "delivery.mode":5, "meditation.type":6

# %% [markdown] heading_collapsed=true hidden=true
# ##### PSS as preferred scale

# %% hidden=true vscode={"languageId": "r"}
results.metafor.stress.pss <- print.meta.results("Stress", preferred.scale = "PSS", return.data = "results.metafor")

# %% hidden=true vscode={"languageId": "r"}
# without outliers of Stress (PSS) + gosh plots (for regression)
print.meta.results(
  "Stress", preferred.scale = "PSS", basic = F, filter.forest..funnel.vec = -c(1, 12, 14),
  filter.regression.linear.list = list(-c(1, 10), -c(11), -c(1), -c(1, 12, 14), "", ""),
  filter.regression.poly.list = list(-c(1, 10), -c(11), -c(1), -c(1, 12, 14), "", ""),
  regression = T, split.subgroups = F, print.influence = F, print.baujat.regression = F, print.gosh.regression = T
)

# order of regression filter
# "sessions.duration":1, "sessions.frequency":2, "programs.duration":3, "follow.up.period":4, "delivery.mode":5, "meditation.type":6

# %% hidden=true vscode={"languageId": "r"}
# without outliers of Stress (PSS) + gosh plots (for regression)  ###### copy for flag.x.s.r = 1 #####
print.meta.results(
  "Stress", preferred.scale = "PSS", basic = F, filter.forest..funnel.vec = -c(1, 12, 14),
  filter.regression.linear.list = list("", -c(10), -c(1), -c(1, 11, 13), "", ""),
  filter.regression.poly.list = list("", -c(10), -c(1), -c(1, 11, 13), "", ""),
  regression = T, split.subgroups = F, print.influence = F, print.baujat.regression = F, print.gosh.regression = T
)

# order of regression filter
# "sessions.duration":1, "sessions.frequency":2, "programs.duration":3, "follow.up.period":4, "delivery.mode":5, "meditation.type":6

# %% hidden=true vscode={"languageId": "r"}
for (outlier in c(1, 12, 14)){
  sav <- gosh(results.metafor.stress.pss)
  plot(sav, out = outlier)
}

# %% hidden=true vscode={"languageId": "r"}
# multiple regressions without interaction
print.meta.results(
  outcome = "Stress", preferred.scale = "PSS", basic = F, regression = F, regression.multiple = T, regression.multiple.degree.2 = F,
  moderator.multiple.list = list(
    c("programs.duration", "sessions.duration"), c("programs.duration", "sessions.frequency"),
    c("sessions.duration", "programs.duration"), c("sessions.duration", "sessions.frequency"),
    c("sessions.frequency", "programs.duration"), c("sessions.frequency", "sessions.duration"),
    c("sessions.frequency", "programs.duration", "sessions.duration")
  )
)
# "sessions.duration":1, "sessions.frequency":2, "programs.duration":3, "follow.up.period":4, "delivery.mode":5, "meditation.type":6

# %% hidden=true vscode={"languageId": "r"}
# multiple regressions without interaction (without outliers)

options(repr.plot.width = 12, repr.plot.height = 9, repr.plot.res = 100)

print.meta.results(
  outcome = "Stress", preferred.scale = "PSS", basic = F, regression = F, regression.multiple = T, regression.multiple.degree.2 = F,
  print.gosh.regression = F, print.baujat.regression = F,
  moderator.multiple.list = list(
                                                  c("programs.duration", "sessions.duration"), c("programs.duration", "sessions.frequency"),
    c("sessions.duration", "programs.duration"),                                               c("sessions.duration", "sessions.frequency"),
    c("sessions.frequency", "programs.duration"), c("sessions.frequency", "sessions.duration"),
    c("sessions.frequency", "programs.duration", "sessions.duration")
    
  ),
  filter.multiple.regression.linear.list = list(
                -c(1),   -c(10, 7),
    -c(1),               -c(9),
    -c(10, 7), -c(9),
    -c(5, 8)
  )
)

options(repr.plot.width = 10, repr.plot.height = 9, repr.plot.res = 100)

# %% hidden=true vscode={"languageId": "r"}
# print gosh plot separately, as it came to an error within do.multiple.regression()
moderator.multiple.list = list(
                                                c("programs.duration", "sessions.duration"), c("programs.duration", "sessions.frequency"),
  c("sessions.duration", "programs.duration"),                                               c("sessions.duration", "sessions.frequency"),
  c("sessions.frequency", "programs.duration"), c("sessions.frequency", "sessions.duration")
)

filter.multiple.regression.linear.list = list(
              -c(1),   -c(10, 7),
  -c(1),               -c(9),
  -c(10, 7), -c(9)
)

mod.comb.no <- 1
for (moderator.combination in moderator.multiple.list){
    
  regression.df <- meta.analyze(
    "Stress", meditation.type.all, m.data.list, preferred.scale = "PSS",
    split.subgroups = FALSE, return.data = "regression.all"
  )

  # cut out data after post-test (that have follow.up.period > 0) if follow.up.period is not in moderator.combination
  if (!"follow.up.period" %in% moderator.combination){
    regression.df <- regression.df[
      which(regression.df[,"follow.up.period"] == 0),
    ]
  }

  # eliminate NAs
  regression.df <- regression.df %>%
    filter(!(
      is.na(!!sym(moderator.combination[1])) |
      !!sym(moderator.combination[1]) %in% c("NA", nm.placeholder, as.character(nm.placeholder)) |
      is.na(!!sym(moderator.combination[2])) |
      !!sym(moderator.combination[2]) %in% c("NA", nm.placeholder, as.character(nm.placeholder))
  ))
  
  if (
    is.list(filter.multiple.regression.linear.list) &
    if(is.list(filter.multiple.regression.linear.list)){filter.multiple.regression.linear.list[[mod.comb.no]][1] != ""}else{FALSE}
  ){

    for (outlier in -filter.multiple.regression.linear.list[[mod.comb.no]]){

      results.regression.linear.unfiltered <- get.results.metafor(
        regression.df, moderator = moderator.combination
      )

      cat("\n Gosh plot for", paste(moderator.combination, sep = ", "), "(outlier number:", outlier, "linear)")
      sav <- gosh(results.regression.linear.unfiltered, subsets = 15000)
      plot(sav, out = outlier)
    }
  } else {
    cat("\nno Gosh plot printed for", moderator.combination, "(linear)\n")
  }
  mod.comb.no <- mod.comb.no+1
}

# %% hidden=true vscode={"languageId": "r"}
# multiple regression with interaction (no outliers detected)

options(repr.plot.width = 12, repr.plot.height = 9, repr.plot.res = 100)

print.meta.results(
  outcome = "Stress", preferred.scale = "PSS", basic = F, regression = F, regression.multiple = T, regression.multiple.degree.2 = F,
  non.interaction = F, interaction = T,
  moderator.multiple.list = list(
    c("programs.duration", "sessions.duration"), c("programs.duration", "sessions.frequency"),
    c("sessions.duration", "programs.duration"), c("sessions.duration", "sessions.frequency"),
    c("sessions.frequency", "programs.duration"), c("sessions.frequency", "sessions.duration"),
    c("sessions.frequency", "programs.duration", "sessions.duration")
  )
)

options(repr.plot.width = 10, repr.plot.height = 9, repr.plot.res = 100)
# "sessions.duration":1, "sessions.frequency":2, "programs.duration":3, "follow.up.period":4, "delivery.mode":5, "meditation.type":6

# %% [markdown] heading_collapsed=true hidden=true
# #### Well-Being

# %% hidden=true vscode={"languageId": "r"}
results.metafor.well.being <- print.meta.results("Well-being or quality of life", return.data = "results.metafor")

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Well-being or quality of life", filter.forest..funnel.vec = -c(1), regression = F, split.subgroups = F, print.influence = F)

# %% hidden=true vscode={"languageId": "r"}
for (outlier in c(1)){
  sav <- gosh(results.metafor.well.being)
  plot(sav, out = outlier)
}

# %% [markdown] heading_collapsed=true hidden=true
# ### Resilience Factors

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Acceptance")

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Active coping")

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Empathy")

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Hope")

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Mindfulness (state)")

# %% hidden=true vscode={"languageId": "r"}
results.metfor.mindfulness.t <- print.meta.results("Mindfulness", return.data = "results.metafor")

# %% hidden=true vscode={"languageId": "r"}
results.metfor.mindfulness.t.ex.rob <- print.meta.results("Mindfulness", exclude.high.rob.vec = studies.high.rob, return.data = "results.metafor")  # exclude Johnson-Waddell 2018 (No. 3)

# %% hidden=true vscode={"languageId": "r"}
# exclude outlier
print.meta.results("Mindfulness", filter.forest..funnel.vec = -c(9), regression = F, split.subgroups = F, print.influence = F)

# %% hidden=true vscode={"languageId": "r"}
for (outlier in c(9)){
  sav <- gosh(results.metfor.mindfulness.t)
  plot(sav, out = outlier)
}

# %% hidden=true vscode={"languageId": "r"}
# exclude both
print.meta.results("Mindfulness", filter.forest..funnel.vec = -c(3, 9))

# %% hidden=true vscode={"languageId": "r"}
for (outlier in c(8)){
  sav <- gosh(results.metfor.mindfulness.t.ex.rob)
  plot(sav, out = outlier)
}

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Optimism or positive attributional style")

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Positive emotion")

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Religiosity or spirituality or religious coping")

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Self-acceptance")

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Self-compassion")

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Self-efficacy")

# %% hidden=true vscode={"languageId": "r"}
print.meta.results("Self-esteem")

# %% [markdown] heading_collapsed=true hidden=true
# ### All Outcomes (Summary)

# %% hidden=true vscode={"languageId": "r"}
# set plot size
options(repr.plot.width = 25, repr.plot.height = 9, repr.plot.res = 120)

# %% hidden=true vscode={"languageId": "r"}
plot.summary.forest(with.outliers = F)
plot.summary.forest(with.outliers = T)

# %% [markdown] heading_collapsed=true
# # Get messages to request authors for data

# %% hidden=true vscode={"languageId": "r"}
# install.packages("openxlsx")
library(openxlsx)

# %% hidden=true vscode={"languageId": "r"}
one.D.info.df["Devillers-Réolon 2022", "Lead.Author.Email.Adress"] <- "rita.sleimen-malkoun@univ-amu.fr"
one.D.info.df["Devillers-Réolon 2022", "Name.of.this.Author"] <- "Rita Sleimen-Malkoun"
req.df <- one.D.info.df |>
  select(Covidence..:Further.Information.inserted.in.Extraction.Form.) |>
  filter(
    Requirement.of.Correspondence.for.further.Study.Information == "Yes" &
    Further.Information.requested. == "No" &
    !Lead.Author.Email.Adress %in% c("NA", "nm")
  )
req.df


# %% hidden=true vscode={"languageId": "r"}
req.mes.draft <- "
Name.of.this.Author

Lead.Author.Email.Adress

Request for additional data - Study.ID - #Covidence..
_

Dear Name.of.this.Author.1,

Our team is conducting a meta-analysis on meditation and mindfulness training. Your study, 'Title' (Study.ID), meets our inclusion criteria. For our analyses, we would like to ask for additional data not included in your article:

What.further.Study.Information.is.needed.

Thank you in advance for sharing this data.

Best regards,
"

# %% hidden=true vscode={"languageId": "r"}
replacments <- c("Name.of.this.Author.1", "Lead.Author.Email.Adress", "Study.ID", "Covidence..", "Name.of.this.Author", "Title", "What.further.Study.Information.is.needed.")
req.mess <- c()
for (i in 1:nrow(req.df)){
  req.mes.tmp <- req.mes.draft
  for (replacment in replacments){
    fill.str <- req.df[i, replacment]
    if (replacment == "Name.of.this.Author.1"){
      fill.str <- gsub( " .*$", "", req.df[i, "Name.of.this.Author"])
    }
    req.mes.tmp <- gsub(replacment, fill.str, req.mes.tmp)
    
  }
  req.mess[i] <- req.mes.tmp
}
names(req.mess) <- req.df[,"Study.ID"]
cat(req.mess, sep = "____________________________________________\n")

# %% hidden=true vscode={"languageId": "r"}
# data.request.excel <- req.df |> select(Covidence..:What.further.Study.Information.is.needed.)
# write.xlsx(data.request.excel, "data.request.excel.xlsx")

# %% [markdown]
# # Save Environment

# %% vscode={"languageId": "r"}
# # --------------------------------------------------------------

# #  setup_source_renv.R

# #  Purpose: Capture the exact R version and the complete package

# #           snapshot of the current project using renv.

# # --------------------------------------------------------------

# # ---- 1. Install renv if it is not already available -----------------

# if (!requireNamespace("renv", quietly = TRUE)) {

#   install.packages("renv", repos = "https://cloud.r-project.org")

# }

# # ---- 2. Initialise renv in the current directory --------------------

# #    This creates a private library (renv/library/) and a lockfile

# #    (renv.lock) that records:

# #      • R version

# #      • Exact package versions (CRAN, Bioconductor, GitHub, etc.)

# #      • Source URLs / commit hashes

# #

# #    If a renv project already exists, init() will simply

# #    activate it without overwriting anything.

# renv::init(bare = FALSE) # bare = FALSE => keep existing packages

# # ---- 3. OPTIONAL: Clean the private library (remove unused pkgs) -----

# #    This step is useful if you have many packages installed globally

# #    but only a subset is required for the notebook.

# #    Comment out if you prefer to keep everything.

# # renv::clean()

# # ---- 4. Snapshot the current state -----------------------------------

# #    After init() you usually already have a lockfile, but calling

# #    snapshot() ensures it reflects the *exact* versions that are

# #    loaded in the session right now.

# renv::snapshot(force = TRUE)

# # ---- 5. Confirm that the lockfile was created -----------------------

# lockfile_path <- file.path(getwd(), "renv.lock")

# if (file.exists(lockfile_path)) {

#  cat("\n✅ renv.lock created successfully at:", lockfile_path, "\n")

# } else {

#  stop("\n❌ Failed to create renv.lock – check the console for errors.")

# }

# # ---- 6. (Optional) Export the private library as a tarball ------------

# #    This lets you avoid downloading packages again on the target

# #    machine, which can be handy on machines without internet access.

# #    The archive will be named `renv_library.tar.gz`.

# #

# #    Uncomment the lines below if you want to ship the binary cache.

# #

# tar(

#  tarfile = "renv_library.tar.gz",

#  files = "renv/library",

#  compression = "gzip",

#  tar = "internal"

# )

# # cat("\n📦 Library archived as renv_library.tar.gz (optional).\n")

# %% vscode={"languageId": "r"}
# Show current R version
R.Version()$version.string
