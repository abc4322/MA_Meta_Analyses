# -*- coding: utf-8 -*-
# ---
# jupyter:
#   jupytext:
#     formats: ../Archive//ipynb,../jupytext_files/Archive//R:percent
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

# %% [markdown] heading_collapsed=true id="P6k0D__1dtvS"
# # Convert .Rmd to .ipynb
#

# %% hidden=true id="IX2XeTmudtvd"
# install.packages("devtools")
# remove.packages("rlang")
# install.packages("rlang")
# devtools::install_github("mkearney/rmd2jupyter")
# install.packages("rmd2jupyter")
# library("rmd2jupyter")

# %% hidden=true id="jtNlFnrgdtvm"
# rmd to ipynb
# rmd2jupyter("Preprocessing Data for Meta-Analyses.Rmd")

# %% hidden=true id="NFmgRsKRdtvr"
# install.packages("rmarkdown")

# %% [markdown] id="CN7cVG-zdtvz"
# # Preprocess data of raw covidence Export
#
#

# %% id="L5OqXycudtv0"
raw.df <- read.csv("final_data_export.csv")

# %% [markdown] id="TVlGoZT1dtv2"
#
# ## Install and load Dplyr
#

# %% colab={"base_uri": "https://localhost:8080/"} id="noEhheyudtv3" outputId="7bbb5020-fc0c-4f0c-99b2-37d479d3686a"
# install.packages("dplyr")
library("dplyr")
install.packages("sjmisc")
library("sjmisc")
# for data manipulation with dplyr see: https://www.youtube.com/watch?v=Gvhkp-Yw65U
# for splitting 2 values in 1 cell see: https://www.youtube.com/watch?v=DiY8EqZDwoI at 3:17 (e.g. if 2 scales for 1 outcome)
# for joining 2 data frames see:        https://www.youtube.com/watch?v=DiY8EqZDwoI at 11:57

# %% id="fr3yuOGbdtv9"
my.raw.df <- raw.df %>%
  filter(Reviewer.Name == "Robin Jacob", Study.design == "Passive RCT")

# %% [markdown] id="9IzRygfTdtv_"
# ## Drop unimportant columns by name

# %% [markdown] id="YjOLv1chBdoB"
# ### Remove column rages (first columns)

# %% id="fVJh-WRLdtwA"
my.df <- my.raw.df %>%
  select(-Reviewer.Name:-Further.Information.inserted.in.Extraction.Form.)  # "-" indicates deleting these columns

# %% [markdown] id="mJ0I76q5dtwB"
# ### Remove single column names and repeating names with ascending numbers (table headlines)
#

# %% id="fpJhMxcsdtwC"
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

# %% id="nZ1K97-UVbN0"
# set options to print entire df (extend max. rows end cols)
options(repr.matrix.max.rows=30, repr.matrix.max.cols=1100)

# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} id="yM0QVCjedDmJ" outputId="c15832aa-678f-4ec2-f8c3-2120fdd60013"
my.df

# %% [markdown] id="22dJtedvdtwE"
#
# ## Create arrays and data frame lists from Covidence tables
#

# %% [markdown] id="ZpNZyCCKdtwR"
#
# ### Set basic parameters
#

# %% id="zd8iL_EwdtwU"
nm.placeholder <- -999  # placeholder for values marked as nm (not mentioned)

flag.x.s.r <- 2
  # x.s.r = exact value (0), mean s (1), or mean r (2)
  # flag.x.s.r = 2 --> include all
  # flag.x.s.r = 1 --> only include mean s and exact values
  # flag.x.s.r = 0 --> include only exact vaules

study.no <- length(my.df[,"Study.ID"])

# %% [markdown] id="L5zVk_c8dtwV"
# ### Functions

# %% [markdown] id="zMsGw6SrBoS1"
# #### Checking for digits and characters

# %% id="-E3epanVdtwW"
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

# %% colab={"base_uri": "https://localhost:8080/", "height": 34} id="clFW77sUVbOC" outputId="d7eb1528-d1a8-4437-ca4c-e868e1add7d1"
char.inside("mindfulness meditation")

# %% [markdown] id="BhtFUMqudtwW"
#
# #### For mean values
#

# %% id="YIjOsZEWdtwX"
# extracts mean r and mean s values as double
get.all.means <- function(value){
  if (grepl("mean r", value)){
    extracted.value <- as.double(sub(" mean r.*", "", value))
        # extracts anything before " mean r" as double                 
  } else if (grepl("mean s", value)){
    extracted.value <- as.double(sub(" mean s.*", "", value))
  } else if (grepl("mean", value)){
    extracted.value <- NA
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

# %% [markdown] id="mLXTMZ4QdtwY"
#
# #### Convert value from nm, NA, digit, or char
#

 # %% colab={"base_uri": "https://localhost:8080/", "height": 34} id="6LzWIl1YbheI" outputId="d162ce31-5e74-40c7-b9fd-7c0cb4004f66"
 round(1.56565, digits = 3)

# %% id="Q0dN9yZOdtwZ"
convert.value <- function(value, nm.placeholder, only.double = TRUE){
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
      extracted.value <- nm.placeholder
    } else if (value == ""){
      extracted.value <- NA
    } else if (value == "^"){
      extracted.value <- value
        # extracts value after "<" as double 
    } else if (grepl("<", value)){
      extracted.value <- as.double(sub(".*<", "", value))
        # extracts value after "<" as double 
    } else if (grepl(">", value)){
      extracted.value <- as.double(sub(".*>", "", value))

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
        extracted.value <- get.all.means(value)
      } else if (flag.x.s.r == 1){
        extracted.value <- get.mean.s.only(value)
      } else if ((flag.x.s.r == 0) & !(grepl("mean r", value) | grepl("mean s", value))){
        extracted.value <- NA
        if (grepl("mean", value)){
          cat("value not added because mean only: ", value)
          cat("\n")
        }
      } else {
        if (only.double == TRUE){
          extracted.value <- NA
          cat("unknown case with char inside: ", value)
          cat("\n")
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

# %% [markdown] id="1d-upnnG2Fup"
# #### Check if multiple commas are present in string

# %% id="soDFylve2Fbm"
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
      # message(cond)
      return("")
      # "except" part for errors
    },
    warning=function(cond) {
      message(paste("Warning of multiple.commas.present() for input:", input.string, "| Type:", class(input.string)))
      # message(cond)
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

# %% [markdown] id="HvqEIc9Ndtwb"
#
# #### For arrays
#

# %% id="ln3DQzW-dtwb"
clean.and.shape.data.to.array <- function(
  my.df, start, end, dims, dimname.list, nm.placeholder, flag.x.s.r, study.no
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
                    value.before.comma, nm.placeholder
                    )
                  value.after.comma <- sub(".*,", "", value)
                    # extracts anything after "," as double
                  my.array[row, col, mtrx, dim.4.elmnt, 2, study] <- convert.value(
                    value.after.comma, nm.placeholder
                    )
                } else {
                    cat(value, "has multiple commas, find solution")
                  }
              } else {
                my.array[row, col, mtrx, dim.4.elmnt, 1, study] <- convert.value(
                  value, nm.placeholder
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
              value, nm.placeholder
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


# %% [markdown] id="DmwjedIRdtwc"
# #### For data frame lists
#

# %% id="b-hKzmmDdtwd"
# create data frames out of 2D Tables (with rows and cols swapped)
clean.data.to.df.list.swap <- function(
  my.df, start, end, dims, list.names, dimname.list, check.multiple.commas = FALSE
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
  for (i in 1:length(list.names)){
    study.df.list <- append(study.df.list, list(study.df), 0)
  }
  names(study.df.list) <- list.names
  
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
          value, nm.placeholder, only.double = FALSE
          )
      }
    }
  }
  study.df.list
}


# %% id="3boc2pzJdtwg"
# create data frames out of 2D Tables
clean.data.to.df.list <- function(
  my.df, start, end, dims, list.names, dimname.list, check.multiple.commas = FALSE
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
  for (i in 1:length(list.names)){
    study.df.list <- append(study.df.list, list(study.df), 0)
  }
  names(study.df.list) <- list.names
  
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
            cat("multiple commas in value, find solution:\n", value, "\n")
          }
        }
        study.df.list[[study]][row, col] <- convert.value(
          value, nm.placeholder, only.double = FALSE
          )
      }
    }
  }
  study.df.list
}


# %% [markdown] id="IY3HznuXWgrY"
# #### Print arrays

# %% id="RnZcGBSqWtKz"
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

# %% [markdown] id="VBH-GtDNdtwk"
# ### Arrays
#

# %% [markdown] id="p1aqcyGEhuWI"
# #### Population Characteristics

# %% colab={"base_uri": "https://localhost:8080/"} id="sTgpJVklh1FW" outputId="77911dab-3203-4d60-bf48-35bfb8c6ffd9"
dims <- c(4, 5, 4, study.no)

dimname.list <- list(c("No.Participants", "Mean.Age", "No.Females", "No.Males"),
                     c("Intervention.1", "Intervention.2", "Intervention.3", "Intervention.4", "Over.All"),
                     c("T0", "T1", "T2", "T3"),
                     my.df[,"Study.ID"]
                     )

start <- "Number.of.Participants.Intervention.1.T0"
end <- "Number.of.Males.Over.All.T3"

population.characteristics.array <- clean.and.shape.data.to.array(
  my.df, start, end, dims, dimname.list, nm.placeholder, flag.x.s.r, study.no
)

my.df <- my.df %>%
  select(-Number.of.Participants.Intervention.1.T0:-Number.of.Males.Over.All.T3)

# %% id="vZJx4dJABh_n"
# correct input mistakes
if (
  is.na(population.characteristics.array["No.Participants", "Intervention.4", 1, "Janowiak 1994"]) &
  population.characteristics.array["No.Participants", "Intervention.3", 1, "Janowiak 1994"] == 21
){
  population.characteristics.array["No.Participants", "Intervention.4", 1, "Janowiak 1994"] <- 21
  population.characteristics.array["No.Participants", "Intervention.3", 1, "Janowiak 1994"] <- NA

  population.characteristics.array[, "Intervention.4", 1, "Smith 2021"] <- population.characteristics.array[, "Intervention.2", 1, "Smith 2021"]
  population.characteristics.array[, "Intervention.2", 1, "Smith 2021"] <- NA
}



# %% colab={"base_uri": "https://localhost:8080/"} id="SqV3Ew2FiQBQ" outputId="3b0460d1-4e93-4906-96e2-d622e43cb443"
print.array.not.na(population.characteristics.array)

# %% [markdown] id="xaAo8PmDSSbb"
# #### Results Descriptive

# %% id="XlPWYl4tdtwl"
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
    my.df, start, end, dims, dimname.list, nm.placeholder, flag.x.s.r, study.no
)

my.df <- my.df %>%
  select(-Intervention.1.Mean.O1T0:-Control.or.Intervention.4.n.in.case.of.period.O7T3)

# %% id="Nd4uXwuhdtwn"
# use my.pop.char.array[,,"T0","Johnson-Waddell 2018"] as test for "", NA, "nm", "double"
#      [,1]  [,2] [,3] [,4]  [,5]
# [1,] "197" NA   NA   "197" 394 
# [2,] ""    ""   NA   ""    "nm"
# [3,] ""    NA   NA   ""    "nm"
# [4,] ""    NA   NA   ""    "" 
# make flag.x.s.r work --> done


# %% colab={"base_uri": "https://localhost:8080/"} id="Bd_7r-Esgmvz" outputId="395ad7f5-eab4-4b23-ce83-6c93245f0b75"
print.array.not.na(results.descriptive.array)

# %% [markdown] id="2QTPOlqldtwq"
#
# #### Results Quantitative
#

# %% id="qmAGEv3Qdtws"
dims <- c(7, 3, 3, 6, 2, study.no)

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
    my.df, start, end, dims, dimname.list, nm.placeholder, flag.x.s.r, study.no
)
my.df <- my.df %>%
  select(-Outcome.1.P.Value.1x2T1:-Outcome.7.Confidence.Interval.of.Effect.Size.3x4T3)

# %% [markdown] id="HfyamaAkdtwt"
# ### Data frame lists

# %% [markdown] id="QLuCkxIfVbOx"
# #### Intervention Comparison

# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} id="6V5Rp0Drdtwu" outputId="ebc13385-cae1-472c-a528-e1a7de67b976"
dims <- c(4, 7)

list.names <- my.df[,"Study.ID"]

dimname.list <- list(c("Intervention.1", "Intervention.2", "Intervention.3", "Control"),
                     c("Name", "Short.Description", "Delivery.Mode", "Meditation.App", "Sessions.Duration.in.minutes",
                       "Frequency.in.times.per.week", "Total.Duration.in.Days")
                     )
start <- "Name.Intervention.1"
end <- "Total.Duration.of.Intervention.in.Days.Control.or.Intervention.4"

intervention.comparisons.df.list <- clean.data.to.df.list.swap(
 my.df, start, end, dims, list.names, dimname.list
)

my.df <- my.df %>%
  select(-Name.Intervention.1:-Total.Duration.of.Intervention.in.Days.Control.or.Intervention.4)

intervention.comparisons.df.list

# %% [markdown] id="1ssi56Ffdtwv"
#
# #### Dates of Measuring Time Points
#

# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} id="nF-WvDM0dtww" outputId="98781609-a070-4759-ac30-7a93fb008c33"
dims <- c(4, 1)

list.names <- my.df[,"Study.ID"]

dimname.list <- list(c("Time.Point.0", "Time.Point.1", "Time.Point.2", "Time.Point.3"),
                     c("Date")
                     )
start <- "Time.Point.0.Date"
end <- "Time.Point.3.Date"

dates.measuring.time.points.df.list <- clean.data.to.df.list(
 my.df, start, end, dims, list.names, dimname.list
)

my.df <- my.df %>%
  select(-Time.Point.0.Date:-Time.Point.3.Date)

dates.measuring.time.points.df.list


# %% [markdown] id="wjV0e9Exdtwy"
#
# #### Between-Measuring Time Points Duration
#

# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} id="cCsuJj6rdtwz" outputId="5c414251-f627-45b2-8bce-d3ce21952633"
dims <- c(3, 1)

list.names <- my.df[,"Study.ID"]

dimname.list <- list(c("Time.Point.0...1", "Time.Point.0...2", "Time.Point.0...3"),
                     c("Duration.in.Days")
                     )
start <- "Time.Point.0...1.Duration.in.Days"
end <- "Time.Point.0...3.Duration.in.Days"

between.T.duration.df.list <- clean.data.to.df.list(
 my.df, start, end, dims, list.names, dimname.list
)

my.df <- my.df %>%
  select(-Time.Point.0...1.Duration.in.Days:-Time.Point.0...3.Duration.in.Days)

between.T.duration.df.list


# %% [markdown] id="UAh7lkDLdtw0"
#
# #### Definition of Outcomes
#

# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} id="L2vCxatldtw1" outputId="5febb8f8-094b-403d-afd3-ae845780279f"
dims <- c(7, 1)

list.names <- my.df[,"Study.ID"]

dimname.list <- list(c("Outcome.1", "Outcome.2", "Outcome.3", "Outcome.4", "Outcome.5", "Outcome.6", "Outcome.7"),
                     c("Definition")
                     )
start <- "Outcome.1.Definition"
end <- "Outcome.7.Definition"

outcome.definitions.df.list <- clean.data.to.df.list(
 my.df, start, end, dims, list.names, dimname.list
)

my.df <- my.df %>%
  select(-Outcome.1.Definition:-Outcome.7.Definition)

outcome.definitions.df.list


# %% [markdown] id="twxkiSZRdtw2"
#
# #### Measures of Outcomes
#

# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} id="fwFdJjUIdtw3" outputId="0361cfd9-d331-4a84-cb2c-d46449035a6e"
dims <- c(7, 3)

list.names <- my.df[,"Study.ID"]

dimname.list <- list(c("Outcome.1", "Outcome.2", "Outcome.3", "Outcome.4", "Outcome.5", "Outcome.6", "Outcome.7"),
                     c("Measures.Name", "Start.and.End.Point", "High.or.low.means.resilient")
                     )
start <- "Outcome.1.Scale.s.or.other.Measure.s.Name"
end <- "Outcome.7.High.or.low.means.resilient"

outcome.measures.df.list <- clean.data.to.df.list(
 my.df, start, end, dims, list.names, dimname.list, check.multiple.commas = TRUE
)

my.df <- my.df %>%
  select(-Outcome.1.Scale.s.or.other.Measure.s.Name:-Outcome.7.High.or.low.means.resilient)

outcome.measures.df.list


# %% [markdown] id="5joO9oxVdtw4"
#
# #### Qualitative Results of Data Analyses
#

# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} id="cpOokvFRdtw5" outputId="ba9abfc8-c07d-4809-9fb0-3a1db9df8b95"
dims <- c(7, 3)

list.names <- my.df[,"Study.ID"]

dimname.list <- list(c("Key.Theme.1", "Key.Theme.2", "Key.Theme.3", "Key.Theme.4", "Key.Theme.5", "Key.Theme.6", "Key.Theme.7"),
                     c("Name.of.Key.Theme", "Description.of.Key.Theme", "Results")
                     )
start <- "Key.Theme.1.Name.of.Key.Theme"
end <- "Key.Theme.7.Results"

results.qualitative.df.list <- clean.data.to.df.list(
 my.df, start, end, dims, list.names, dimname.list
)

my.df <- my.df %>%
  select(-Key.Theme.1.Name.of.Key.Theme:-Key.Theme.7.Results)

results.qualitative.df.list


# %% [markdown] id="eZKlbjHMVbPB"
# ### Data frame of left 1D Data

# %% id="UvagubmWVbPB"
my.df[my.df == 'None' | my.df == '' | my.df == NA] <- "NA"
one.D.info.df <- my.df
rownames(one.D.info.df) <- list.names

# %% colab={"base_uri": "https://localhost:8080/", "height": 260} id="TOTcTcARK3cS" outputId="fb89ae9b-e4bc-4d74-ebc6-42b7b13fbf02"
colnames(one.D.info.df)

# %% [markdown] id="HGMi3f0FV2Jb"
# ## Make scale and meditation type names unique

# %% [markdown] id="qo7Bneh2BdI5"
# ### Meditation types

# %% colab={"base_uri": "https://localhost:8080/", "height": 147} id="Gt-NW-sYcYDd" outputId="36811956-a16f-426a-aa8a-f228e830e096"
one.D.info.df["Huberty 2019",41:44]

# %% id="nVKJjyISWEmI"
# # get all unique meditation types
# meditation.types <- c()

# colnames.med.type <- colnames(one.D.info.df)[41:44]

# for (colname.med.type in colnames.med.type) {
#   for (meditation.type in one.D.info.df[,colname.med.type]){
#     if (!meditation.type == "NA"){
#       meditation.types <- append(meditation.types, meditation.type)
#     }
#   }
# }

# unique.meditation.types <- unique(meditation.types)
# for (meditation.type in unique.meditation.types){
#   print(meditation.type)
# }

# %% id="JUwRbuo6M_f1"
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
  "Zen Meditation",
  "Breathing Exercise (no further Info); Other: \"awareness to any sensations in their body or in their immediate environment\", focus on \"thoughts and feelings\",  \"non-judgmental observation\"",
  "Shavasana",
  "Mantra Meditation; Only \"Focused Attention Meditation\" named; Other: Readings reflecting meditative perspective",
  "Breathing Exercise (no further Info); Open Monitoring Meditation",
  "Body Scan; Other: \"become aware of their sensations, including sights, sounds and somatic sensations\", relaxing body areas, observe their physical sensations thoughts and emotions without reaction or judgment,"
)

meditation.type.constructive <- c(
  "Loving-Kindness Meditation"
)

meditation.type.attentional.and.constructive <- c(
  "Body Scan; Breathing Exercise (no further Info); Loving-Kindness Meditation"
)

meditation.type.attentional.constructive.and.deconstructive <- c(
  "Other: mindfulness-based exercises through audio video or text files, grounding visualization, gratitude, imagining the life you want, finding meaning",
  "Breathing Exercise (no further Info); Other: focusing on meta-awareness, \"Mindfulness-based exercises [...] about happiness in life as the result of a benevolent attitude towards life and a positive relationship to oneself and others\", \"Methods of introspection, reflection and self-care\"",
  "Prayer; Other: devotional readings to ponder, meditations taken from a widely used devotional book designed for Christian readers titled, forgiveness meditations"  # due to "forgiveness" --> strengthens cognitive and affective patterns (constructive), "reading to ponder" --> deconstructive
)

# %% [markdown] id="suhKn26IBgdG"
# ### Scales

# %% id="DPsdl0wv7hXn"
# # find out all unique scale cell entries
# measures <- c()

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

# unique.measures <- unique(measures)

# i <- 1
# for (j in unique.measures){
#   print(i)
#   print(j)
#   cat("\n")
#   i <- i + 1
# }

# %% [markdown] id="4tkp2rxMIrED"
# #### Resilience

# %% id="AY42c7L3IW7S"
scale.CD.RISC.10.synonyms <- c(
  "Connor-Davidson Resilience Scale (CD-RISC-10) (Connor & Davidson, 2003)"
)

# %% [markdown] id="iiYIyLWYIwAE"
# #### Mental health-related

# %% [markdown] id="a0C2z3Xmzq9L"
# ##### Including Subscales

# %% id="QWceMIVPH82r"
scale.DASS.synonyms <- c(
  "Depression, Anxiety\nand Stress Scale (DASS) 26",
  "Depression Anxiety and Stress Scale (DASS) - Depression Subcale",
  "Depression Anxiety and Stress Scale (DASS) - Anxiety Subcale",
  "Depression Anxiety and Stress Scale [32] - depression subcale",
  "Depression Anxiety and Stress Scale [32] - anxiety subcale",
  "Depression Anxiety and Stress Scale [32] - stress subcale",
  "Depression, Anxiety, and Stress Scale – 21 (DASS-21)"
)

scale.POMS.synonyms <- c(
  "Profile of Mood States total mood disturbance main scale 23",
  "Profile of Mood States anxiety subscale",
  "Profile of Mood States depression subscale"
)

# %% [markdown] id="yWVcnzO40j4y"
# ##### 2 Scales per Cell

# %% id="HVY5_Eaj0oMP"
scales.DASS.PSS.synonyms <- c(
  "Depression, Anxiety\nand Stress Scale (DASS) 26, Perceived Stress Scale\n(PSS)27,28",
  "Depression, Anxiety, and Stress Scale – 21 (DASS-21),\nPerceived Stress Scale (PSS)"
)

scales.PSS.DASS.synonyms <- c(
  "Perceived Stress Scale (PSS), Depression Anxiety and Stress Scale (DASS) - Stress Subcale"
)

# %% [markdown] id="csaEiELtzwRZ"
# ##### Anxiety

# %% id="1eO77-XCzB20"
scale.SAS.synonyms <- c(
  "Self-Rating Anxiety Scale (SAS) (Zung, 1971)"
)

scale.STAI.synonyms <- c(
  "State Anxiety Scale from the State–Trait Anxiety Inventory (Spielberger, Gorsuch, Lushene, Vagg, & Jacobs, 1983)",
  "State-Trait Anxiety Inventory (STAI)",
  "State-Trait Anxiety Inventory (S-TAI) - state (SAI)",
  "State-Trait Anxiety Inventory (STAI) - state subscale",
  "State-Trait Anxiety Inventory (STAI) - trait subscale"
)

# %% [markdown] id="_uIJGt86zzVJ"
# ##### Depression

# %% id="rDCZR5aJIP-F"
scale.BDI.synonyms <- c(
  "Beck Depression Inventory (BDI [72])"
)

scale.PHQ.9.synonyms <- c(
  "Patient Health Questionnaire-9 (PHQ-9; Kroenke & Spitzer, 2002)"
)

scale.QIDS.SR.synonyms <- c(
  "Quick Inventory of Depressive Symptomatology Self-Report (QIDS-SR)"
)

scale.SDS.synonyms <- c(
  "Self-Rating Depression Scale (SDS) (Zung et al., 1965)"
)

# %% [markdown] id="828kbyUbz1LN"
# ##### Stress

# %% id="SkYmlm1FH0OD"
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
  "erceived Stress Scale (PSS [74])"
)

# %% [markdown] id="zrlO4eS6z5ew"
# ##### Quality of life

# %% id="WqC-XpLOJZSh"
scale.LSS.synonyms <- c(
  "Questionnaire for the Assessment of Happiness ( ger.: Lebensglückskala, LSS) (Ciccarello & Reinhard, 2014)"
)

scale.SWLS.synonyms <- c(
  "Satisfaction with Life Scale (SWLS [71])",
  "Satisfaction with Life Scale (SWLS)"
)

scale.WHO.QOL.BREF.synonyms <- c(
  "World Health Organization Quality of Life-Brief (WHO-QOL-BREF)"
)

# %% [markdown] id="xusmfMsOI3t2"
# #### Resilience Factors

# %% [markdown] id="tNsa9WRlz-_F"
# ##### Coping, positve

# %% id="k-og6O0yIfGl"
scale.not.common.positive.coping.synonyms <- c(
  "scales adapted from a\nwell-known set of brief measures developed by the Fetzer Institute (1999, pp. 86–87) - positve coping subscale"
)

# %% [markdown] id="Em4Uu9Ay0BN8"
# ##### Empathy

# %% id="EMSUl-nFInPg"
scale.BEA.synonyms <- c(
  "Batson Empathy Adjectives (BEA; Batson, 1986; Coke, Batson, & McDavis, 1978)"
)

# %% [markdown] id="J1mZVAEI0DQb"
# ##### Mindfulness

# %% id="ISVYIeknI_nw"
scale.CAMS.R.synonyms <- c(
  "Cognitive and Affective Mindfulness Scale-Revised (CAMS-R)",
  "Cognitive and Affective Mindfulness Scale-Revised (CAMS-R) - acceptace subscale",
  "Cognitive and Affective Mindfulness Scale–Revised (CAMS-R; Feldman, Hayes, Kumar, Greeson, & Laurenceau, 2007)",
  "Cognitive and Affective Mindfulness Scale – Revised (CAMS-R)",
  "Cognitive and Affective Mindfulness\nScale-Revised (CAMS-R; Feldman et al., 2007)"
)

scale.FFMQ.synonyms <- c(
  "Five Factor Mindfulness Questionnaire (FFMQ) [60].",  # mistake in paper --> Factor = Facet
  "Five Facet Mindfulness Questionnaire (FFMQ; Baer, Smith, Hopkins, Krietemeyer, & Toney, 2006)"
)

scale.FMI.14.synonyms <- c(
  "Freiburg Mindfulness Inventory (FMI-14) (Buchheld & Walach, 2002)"
)

scale.KIMS.synonyms <- c(
  "Kentucky Inventory of Mindfulness Skills (KIMS; Baer et al., 2004)"
)

scale.MAAS.synonyms <- c(
  "Mindfulness Attention Awareness Scale (MAAS) (Brown and Ryan (2003)",
  "Mindfulness Attention Awareness Scale (MAAS) (Brown & Ryan, 2003)"
)

scale.TMS.synonyms <- c(
  "The Toronto Mindfulness Scale (TMS)"
)

# %% [markdown] id="9mUabHEA0NlP"
# ##### Psychological Capital

# %% id="VjmrAnJ0KIH6"
scale.PCQ.synonyms <- c(
  "Psychological Capital\nQuestionnaire (PCQ) 19"
)

# %% [markdown] id="y1sLz_A70Pge"
# ##### Self-acceptance

# %% id="VkL_GWL8JGHd"
scale.CPI.synonyms <- c(
  "California Psychological Inventory French version (CPI) (Gough 1957) - Subscale Self-acceptance (Sa)"
)

# %% [markdown] id="d3ftu1Yb0RrZ"
# ##### Self-compassion

# %% id="KtfYnKU6zItB"
scale.SCS.synonyms <- c(
  "Self-Compassion Scale (SCS; Neff, 2003a)"
)

scale.SCS.SF.synonyms <- c(
  "Self-Compassion Survey Short-Form (SCS-SF)"
)

# %% [markdown] id="L4lc2cVy0T8f"
# ##### Self-esteem

# %% id="6UsBRYUOysPx"
scale.RSES.synonyms <- c(
  "Rosenberg Self-Esteem Scale (RSES; Rosenberg, 1965)"
)

# %% [markdown] id="4whkx7Wm1atg"
# #### Set unique scale names

# %% id="WgVfk7ul1h51"
for (study in 1:study.no) {
  for (outcome in 1:7){
    outcome.measure <- outcome.measures.df.list[[study]][outcome,"Measures.Name"]
    if (!(is.na(outcome.measure) | outcome.measure == "NA" | outcome.measure == nm.placeholder)){
# Resilience
      if (outcome.measure %in% scale.CD.RISC.10.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Connor-Davidson Resilience Scale (CD-RISC-10)"
# Mental health-related
  ## Including Subscales
      } else if (outcome.measure %in% scale.DASS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Depression Anxiety and Stress Scale (DASS)"
      } else if (outcome.measure %in% scale.POMS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Profile of Mood States (POMS)"
  ## 2 Scales per cell
      } else if (outcome.measure %in% scales.DASS.PSS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Depression Anxiety and Stress Scale (DASS), Perceived Stress Scale (PSS)"
      } else if (outcome.measure %in% scales.PSS.DASS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Perceived Stress Scale (PSS), Depression Anxiety and Stress Scale (DASS)"
  ## Anxiety
      } else if (outcome.measure %in% scale.SAS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Self-Rating Anxiety Scale (SAS)"
      } else if (outcome.measure %in% scale.STAI.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "State-Trait Anxiety Inventory (STAI)"
  ## Depression
      } else if (outcome.measure %in% scale.BDI.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Beck Depression Inventory (BDI)"
      } else if (outcome.measure %in% scale.PHQ.9.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Patient Health Questionnaire-9 (PHQ-9)"
      } else if (outcome.measure %in% scale.QIDS.SR.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Quick Inventory of Depressive Symptomatology Self-Report (QIDS-SR)"
      } else if (outcome.measure %in% scale.SDS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Self-Rating Depression Scale (SDS)"
  ## Stress
      } else if (outcome.measure %in% scale.PSS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Perceived Stress Scale (PSS)"
  ## Quality of Life
      } else if (outcome.measure %in% scale.LSS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Lebensglueckskala (LSS)"
      } else if (outcome.measure %in% scale.SWLS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Satisfaction with Life Scale (SWLS)"
      } else if (outcome.measure %in% scale.WHO.QOL.BREF.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "World Health Organization Quality of Life-Brief (WHO-QOL-BREF)"
# Resilience factors
  ## Coping, positve
      } else if (outcome.measure %in% scale.not.common.positive.coping.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Positve coping subscale (adapted from scales of the Fetzer Institute)"
  ## Empathy
      } else if (outcome.measure %in% scale.BEA.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Batson Empathy Adjectives (BEA; Batson, 1986; Coke, Batson, & McDavis, 1978)"
  ## Mindfulness
      } else if (outcome.measure %in% scale.CAMS.R.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Cognitive and Affective Mindfulness Scale-Revised (CAMS-R)"
      } else if (outcome.measure %in% scale.FFMQ.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Five Facet Mindfulness Questionnaire (FFMQ)"
      } else if (outcome.measure %in% scale.FMI.14.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Freiburg Mindfulness Inventory (FMI-14)"
      } else if (outcome.measure %in% scale.KIMS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Kentucky Inventory of Mindfulness Skills (KIMS)"
      } else if (outcome.measure %in% scale.MAAS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Mindfulness Attention Awareness Scale (MAAS)"
      } else if (outcome.measure %in% scale.TMS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "The Toronto Mindfulness Scale (TMS)"
  ## Psychological Capital
      } else if (outcome.measure %in% scale.PCQ.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Psychological Capital Questionnaire (PCQ)"
  ## Self-acceptance
      } else if (outcome.measure %in% scale.CPI.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "California Psychological Inventory French version (CPI)"
  ## Self-compassion
      } else if (outcome.measure %in% scale.SCS.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Self-Compassion Scale (SCS)"
      } else if (outcome.measure %in% scale.SCS.SF.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Self-Compassion Survey Short-Form (SCS-SF)"
  ## Self-esteem
      } else if (outcome.measure %in% scale.RSES.synonyms){
        outcome.measures.df.list[[study]][outcome,"Measures.Name"] <- "Rosenberg Self-Esteem Scale (RSES)"
      } else {
        cat(
          "unknown case:", outcome.measure, "\n",
          "study: ", list.names[study], "\n\n"
          )
      }
    }
  }
}


# %% [markdown] id="YY0xZxAgePtS"
# ## Fill empty n, mean age, and sex values

# %% [markdown] id="BCW7blVJfAf3"
# ### n

# %% colab={"base_uri": "https://localhost:8080/", "height": 260} id="G23yVcV4N1B6" outputId="37fd360e-4875-4ff7-d73e-9e3367528abe"
colnames(one.D.info.df)

# %% colab={"base_uri": "https://localhost:8080/"} id="iYJ3f1JIeNgc" outputId="8806809f-e176-428c-9edc-8c5df61dc811"
# set correct n from Population Characteristics to descriptive results
methods.excluding.subjects <- c(
  "Per-protocol analysis",
  "Listwise or case deletion",
  "Pairwise deletion",
  "NA",
  NA,
  "Not mentioned",
  "not mentioned"
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
        for (intervention in 1:4){
          int.name <- intervention.comparisons.df.list[[study]][intervention, "Name"]
          no.incl.outcomes <- one.D.info.df[study, "Number.of.included.Outcomes"]
          if (grepl(",", outcome.measures.df.list[[study]][outcome, "Measures.Name"])){
            # the value of "Measures.Name" is devided in 2 scale names by a comma
            no.used.scales.per.outcome <- 2
          } else {
            no.used.scales.per.outcome <- 1
          }
          if (
            !(is.na(int.name) | int.name == "NA") &
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
                          "ERROR#1: no n of present intervention of study:", list.names[study], "\n",
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
                        "ERROR#2: no n of present intervention of study:", list.names[study], "\n",
                        "intervention:", intervention, "\n",
                        "n:", n, "\n\n"
                      )
                    }
                  }
                } else if ((is.na(n)) & t == 2){
                  n <- population.characteristics.array["No.Participants", intervention, t - 1, study]
                  if (is.na(n)){
                    cat(
                      "ERROR#3: no n of present intervention of study:", list.names[study], "\n",
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
                  cat("ERROR#5: no n of present intervention of study:", list.names[study], "\n")
                }

              } else {
                cat(
                  "Unknown case #1, find solution\n",
                  "Study: ", list.names[study], "\n",
                  "Imputation of missing Data:", one.D.info.df[study,"Imputation.of.missing.Data"], "\n",
                  "ITT or Per-Protocol-Analysis:", one.D.info.df[study,"ITT.or.Per.Protocol.Analysis.present."], "\n",
                  "n:", n, "\n\n"
                   )
              }
              results.descriptive.array[intervention, "n", t, outcome, scale, study] <- n
            }
          }
        }
      }
    }
  }
}

# %% [markdown] id="mK6iGfTCHYy-"
# --> Remove Messer 2016 from data for Meta-Analyses

# %% colab={"base_uri": "https://localhost:8080/"} id="rZqyWSYE3ETC" outputId="c66474a5-373d-4d1a-c53c-35e6aaa41875"
print.array.not.na(population.characteristics.array)

# %% [markdown] id="R6gDeqegVbPC"
# ## Put total data into a list

# %% id="tO7ZASXqVbPD"
m.data.list <- list(
    one.D.info.df,
    population.characteristics.array,
    intervention.comparisons.df.list,
    dates.measuring.time.points.df.list,
    between.T.duration.df.list,
    outcome.definitions.df.list,
    outcome.measures.df.list,
    results.descriptive.array,
    results.quantitative.array,
    results.qualitative.df.list
)

names(m.data.list) <- c(
    "one.D.info.df",
    "population.characteristics.array",
    "intervention.comparisons.df.list",
    "dates.measuring.time.points.df.list",
    "between.T.duration.df.list",
    "outcome.definitions.df.list",
    "outcome.measures.df.list",
    "results.descriptive.array",
    "results.quantitative.array",
    "results.qualitative.df.list"
)

# %% [markdown] id="JMb2giHBfawn"
# # Forest and Funnelplots

# %% [markdown] id="wrlelJx6hEGy"
# ## Function - Print forest plot clustered by outcome and meditation type

# %% colab={"base_uri": "https://localhost:8080/"} id="EPvplRN3U9eS" outputId="12c10740-1159-49a5-b954-1e83f220f032"
install.packages("esc")
install.packages("meta")
library("esc")
library("meta")

# %% id="s2R6D_b2feWt"
forest.plt.by.outcome..med.type <- function(
  outcome, meditation.types, m.data.list, time.point, double.scale = FALSE, prefered.scale = FALSE
    #  meditation.types = vector of meditation types
    # double.scale != FALSE only for Stress --> only outcome with 2 Scales per study
    # prefered.scale = prefered scale in case of 2 scales for one outcome (Stress)
  ){
  study.id <- c()
  n.int <- c()
  n.control <- c()
  mean.int <- c()
  mean.control <- c()
  sd.int <- c()
  sd.control <- c()
  pooled.sd <- c()
  mean.diff.T <- c()


  if (double.scale == FALSE){
    for (study in 1:study.no){
      int.per.study <- 0
      for (outcome.no in 1:7){
        for (intervention.no in 1:4){
          results.descriptive.array <- m.data.list[["results.descriptive.array"]]
          if (
            m.data.list[["one.D.info.df"]][study, sprintf("Name.of.Outcome.%d", outcome.no)] == outcome &
            !(
              is.na(m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"]) |
              m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == "NA" |
              m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == nm.placeholder
            ) &
            (
              m.data.list[["one.D.info.df"]][study, 40 + intervention.no] %in% meditation.types |
                # 41:44 are indices of Practiced Meditation Types in Interventon x
              intervention.no
            ) &
            !(
              NA %in% results.descriptive.array[intervention.no,,time.point, outcome.no, "Scale.1", study] |
              nm.placeholder %in% results.descriptive.array[intervention.no,,time.point, outcome.no, "Scale.1", study]
            ) &
            !(
              NA %in% results.descriptive.array[4,,time.point, outcome.no, "Scale.1", study] |
              nm.placeholder %in% results.descriptive.array[4,,time.point, outcome.no, "Scale.1", study]
            )
          ){

# for control group
            if (intervention.no == 4){
              for (i in 1:int.per.study){
                # if n interventions are included, these refer to the control n times
                study.id <- append(
                  study.id,
                  m.data.list[["one.D.info.df"]][study,"Study.ID"]
                )

                n.control <- append(
                  n.control,
                  results.descriptive.array[intervention.no, "n", time.point, outcome.no, "Scale.1", study]
                )
                mean.control <- append(
                  mean.control,
                  results.descriptive.array[intervention.no, "Mean", time.point, outcome.no, "Scale.1", study]
                )
                sd.control <- append(
                  sd.control,
                  results.descriptive.array[intervention.no, "SD", time.point, outcome.no, "Scale.1", study]
                )
              }
            
# for intervention
            } else {
              n.int <- append(
                n.int,
                results.descriptive.array[intervention.no, "n", time.point, outcome.no, "Scale.1", study]
              )
              mean.int <- append(
                mean.int,
                results.descriptive.array[intervention.no, "Mean", time.point, outcome.no, "Scale.1", study]
              )
              sd.int <- append(
                sd.int,
                results.descriptive.array[intervention.no, "SD", time.point, outcome.no, "Scale.1", study]
              )
              int.per.study <- int.per.study + 1
            }
          }
        }
      }
    }
  } else if (double.scale == TRUE){
    if (prefered.scale == FALSE){
      print("set prefered scale")
    }
    for (study in 1:study.no){
      int.per.study <- 0
      for (scale in 1:2){
        for (outcome.no in 1:7){
          for (intervention.no in 1:4){
            results.descriptive.array <- m.data.list[["results.descriptive.array"]]

            if (
              m.data.list[["one.D.info.df"]][study, sprintf("Name.of.Outcome.%d", outcome.no)] == outcome &
              !(
                is.na(m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"]) |
                m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == "NA" |
                m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == nm.placeholder
              ) &
              (
                m.data.list[["one.D.info.df"]][study, 40 + intervention.no] %in% meditation.types |
                  # 41:44 are indices of Practiced Meditation Types in Interventon x
                intervention.no
              ) &
              !(
                NA %in% results.descriptive.array[intervention.no,,time.point, outcome.no, scale, study] |
                nm.placeholder %in% results.descriptive.array[intervention.no,,time.point, outcome.no, scale, study]
              ) &
              !(
                NA %in% results.descriptive.array[4,,time.point, outcome.no, scale, study] |
                nm.placeholder %in% results.descriptive.array[4,,time.point, outcome.no, scale, study]
              )
            ){

# skip loop if 2 scales per outcome are present and current scale is not the prefered one
              scale.name <- m.data.list[["outcome.measures.df.list"]][[study]][sprintf("Outcome.%d", outcome.no), "Measures.Name"]
              if (multiple.commas.present(scale.name)){
                cat("multiple commas in scale name, find solution:", scale.name, "\n\n")
                next
              } else if (grepl(",", scale.name)){  # comma = 2 scales per outcome
                if (scale == 1){
                  scale.name.substring <- sub(",.*", "", scale.name)
                    # extracts substring before comma
                  if (!grepl(prefered.scale, scale.name.substring)){
                    next
                  }
                } else if (scale == 2){
                  scale.name.substring <- sub(".*,", "", scale.name)
                    # extracts substring after comma
                  if (!grepl(prefered.scale, scale.name.substring)){
                    next
                  }
                }
              }

# for control group
              if (intervention.no == 4){
                for (i in 1:int.per.study){
                  # if n interventions are included, these refer to the control n times
                  study.id <- append(
                    study.id,
                    m.data.list[["one.D.info.df"]][study,"Study.ID"]
                  )

                  n.control <- append(
                    n.control,
                    results.descriptive.array[intervention.no, "n", time.point, outcome.no, scale, study]
                  )
                  mean.control <- append(
                    mean.control,
                    results.descriptive.array[intervention.no, "Mean", time.point, outcome.no, scale, study]
                  )
                  sd.control <- append(
                    sd.control,
                    results.descriptive.array[intervention.no, "SD", time.point, outcome.no, scale, study]
                  )
                }
              
# for intervention
              } else {
                n.int <- append(
                  n.int,
                  results.descriptive.array[intervention.no, "n", time.point, outcome.no, scale, study]
                )
                mean.int <- append(
                  mean.int,
                  results.descriptive.array[intervention.no, "Mean", time.point, outcome.no, scale, study]
                )
                sd.int <- append(
                  sd.int,
                  results.descriptive.array[intervention.no, "SD", time.point, outcome.no, scale, study]
                )
                int.per.study <- int.per.study + 1
              }
            }
          }
        }
      }
    }
  }

  pooled.sd = sqrt((n.int - 1) * sd.int^2 + (n.control - 1) * sd.control^2) / (n.int + sd.int-2)
  mean.diff = mean.int - mean.control

  meta.df <- data.frame(
    study.id =	study.id,
    n.control =	n.control,
    mean.control =	mean.control,
    sd.control =	sd.control,
    n.int =	n.int,
    mean.int	= mean.int,
    sd.int	= sd.int,
    pooled.sd = pooled.sd,
    mean.diff = mean.diff
  )

  # meta.df$hedges.g <- (meta.df[, "pooled.sd"] / meta.df[, "mean.diff"])

  res.meta.df =  metacont(n.int, mean.int, sd.int, 
                          n.control, n.control, sd.control,
                          fixed = T, random = T, studlab = study.id,
                          data = meta.df, sm = "SMD")

  # plot forest plot
  forest(res.meta.df, leftcols = c('studlab'))

  # plot funnel plot
  funnel(res.meta.df)

  # print inserted data to check it?
}

# %% [markdown] id="ZrqOKXPIZLGT"
# ## Get all present outcomes

# %% colab={"base_uri": "https://localhost:8080/", "height": 52} id="AwYPRCAmVgC_" outputId="43bdf975-57a2-4d96-acdc-66a7aa68b41e"
outcome.names.df <- one.D.info.df[, which(colnames(one.D.info.df)=="Name.of.Outcome.1"):which(colnames(one.D.info.df)=="Name.of.Outcome.7")]
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

present.outcomes <- unique(present.outcomes)
present.outcomes

# %% [markdown] id="QxzxWruAcvdB"
# ## Plots for posttest

# %% [markdown] id="ABPQ7D6Pakid"
# ### Attentional Family

# %% [markdown] id="pYjEsSGD0-70"
# #### Resilience Scale

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="DFkAcg4d38O5" outputId="553383a4-39ab-4930-8b90-b630c1c241d6"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Resilience Scale", meditation.type.attentional, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Resilience Scale"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Resilience Scale"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="e8v6rjOe1GhX"
# #### Anxiety (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="2T_B87wY4IUO" outputId="9f41c877-b283-4b5f-b707-b02850a2bf5c"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Anxiety (trait)", meditation.type.attentional, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (trait)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (trait)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="Q51xPYYe1KBH"
# #### Anxiety (state)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="Pmbhu6yD4aJ9" outputId="f8d10e43-40cb-4f93-aceb-6ace528db69b"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Anxiety (state)", meditation.type.attentional, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (state)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (state)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="H0wWgFFL1DzK"
# #### Depression (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="YkVA1eS-4_fL" outputId="31a58a7b-ed0a-4e83-b504-3976bc962c6c"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Depression (trait)", meditation.type.attentional, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Depression (trait)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Depression (trait)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="kmtEx7Fh1O3r"
# #### Stress

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="KIMx0T3v5LT2" outputId="53fe87e9-1094-4a11-a6f1-3123869d4d77"
prefered.scales <- c("Depression Anxiety and Stress Scale (DASS)", "Perceived Stress Scale (PSS)")

forest.plt.by.outcome..med.type(
  "Stress", meditation.type.attentional, m.data.list, time.point = 2, double.scale = TRUE, prefered.scale = prefered.scales[1]
)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="0pEMK1J75oJo" outputId="c4c0e492-2601-4c4c-ec58-9fe178601a1c"
forest.plt.by.outcome..med.type(
  "Stress", meditation.type.attentional, m.data.list, time.point = 2, double.scale = TRUE, prefered.scale = prefered.scales[2]
)

# %% [markdown] id="IhttK7-b0uEC"
# #### Well-being or quality of life

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="SDKNxmht7o02" outputId="c0ead708-039e-4cbb-e56d-31fdf87be7d6"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Well-being or quality of life", meditation.type.attentional, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Well-being or quality of life"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Well-being or quality of life"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="0gU1K19i0qks"
# #### Acceptance

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="jTWmzeqH5yjd" outputId="10b9e4c4-5ed5-4de2-dbc4-a5f246501701"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Acceptance", meditation.type.attentional, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Acceptance"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Acceptance"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="gqrJT0740brR"
# #### Empathy

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="z-Lq4ItO6Mto" outputId="7c6d099d-5dc4-411e-d428-e34e02e82c73"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Empathy", meditation.type.attentional, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Empathy"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Empathy"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="PQRs58z607y7"
# #### Hope

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="wHbBmzpW6qlR" outputId="5a960153-5f66-4ff6-831e-271172b050c6"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Hope", meditation.type.attentional, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Hope"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Hope"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="ioTzEW9i0iLX"
# #### Mindfulness (state)

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="CjnVuuWR64Li" outputId="2c91dcfa-6b5a-4987-80ab-d8fef6f6252e"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Mindfulness (state)", meditation.type.attentional, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (state)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (state)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="-XnSButV0ywH"
# #### Mindfulness (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="K0e_hPpU6-V2" outputId="f224680e-ad87-4535-8e73-9b361cca96fc"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Mindfulness (trait)", meditation.type.attentional, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (trait)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (trait)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="Iz4cDns5042k"
# #### Optimism or positive attributional style

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="3dANn3JL7IAT" outputId="599b5915-d904-4e1c-f706-a3d17e035909"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Optimism or positive attributional style", meditation.type.attentional, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Optimism or positive attributional style"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Optimism or positive attributional style"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="tie4axJh0ljV"
# #### Religiosity or spirituality or religious coping

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="BnBrb7_b7PJG" outputId="5df65fe4-7fb0-4b08-aefd-5e6de1d3ca49"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Religiosity or spirituality or religious coping", meditation.type.attentional, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Religiosity or spirituality or religious coping"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Religiosity or spirituality or religious coping"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="CftTky4L1Bbg"
# #### Self-efficacy

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="ZtCxzKl57T-8" outputId="fc4f0b68-1247-4dfb-8c6e-88950605f062"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Self-efficacy", meditation.type.attentional, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-efficacy"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-efficacy"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="0f_5cHOC0eqZ"
# #### Self-acceptance

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="4I3vtZgm7YHY" outputId="d6716ff0-dbf5-4e8b-a3b4-6446e8167869"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Self-acceptance", meditation.type.attentional, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-acceptance"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-acceptance"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="rU5h12XG0YkI"
# #### Self-esteem

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="ZncBs8NL7eIV" outputId="4f63d2fc-dd57-4d9d-a89a-1915579e6554"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Self-esteem", meditation.type.attentional, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-esteem"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-esteem"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="DaoMK_039wkO"
# ### Constructive Family

# %% [markdown] id="lUI4sq51-_eL"
# #### Resilience Scale

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="I9Ml2t5B-Qci" outputId="e7e3e44c-4db4-4b7e-8096-105a1aefb73c"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Resilience Scale", meditation.type.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Resilience Scale"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Resilience Scale"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="ZKrVDQ_k-Qcl"
# #### Anxiety (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="YTycod3n-Qco" outputId="0ce16f2e-384b-406f-df8b-49ca3e5dcd17"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Anxiety (trait)", meditation.type.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (trait)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (trait)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="O2Kuf8Iq-Qcq"
# #### Anxiety (state)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="Gj6nBzF6-Qcs" outputId="11d7600b-0b7d-4fa3-b8c4-99daf6f4eb52"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Anxiety (state)", meditation.type.constructive, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (state)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (state)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="67WIqoUW-Qcu"
# #### Depression (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="Rcen3MPc-Qcx" outputId="3ad2c279-3a49-40e1-ef28-b20b01bbc059"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Depression (trait)", meditation.type.constructive, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Depression (trait)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Depression (trait)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="I1KTzaPu-Qcz"
# #### Stress

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="l_r60pw1-QdA" outputId="d9c9ff41-c427-4564-f5f3-fbfd2b3c3fde"
prefered.scales <- c("Depression Anxiety and Stress Scale (DASS)", "Perceived Stress Scale (PSS)")

forest.plt.by.outcome..med.type(
  "Stress", meditation.type.constructive, m.data.list, time.point = 2, double.scale = TRUE, prefered.scale = prefered.scales[1]
)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="CQS2TQ0O-QdE" outputId="cbb14dee-00ac-4eae-821f-2a496a63dfa5"
forest.plt.by.outcome..med.type(
  "Stress", meditation.type.constructive, m.data.list, time.point = 2, double.scale = TRUE, prefered.scale = prefered.scales[2]
)

# %% [markdown] id="pP07uE6M-QdG"
# #### Well-being or quality of life

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="kJ7I7aeP-QdO" outputId="5c2885ab-6673-44c9-c0cc-0c426fc2690f"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Well-being or quality of life", meditation.type.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Well-being or quality of life"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Well-being or quality of life"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="rZ4VaI4c-QdT"
# #### Acceptance

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="qrTr1hEb-QdU" outputId="5b00edf2-ebb9-4447-e063-8f1f496df02c"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Acceptance", meditation.type.constructive, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Acceptance"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Acceptance"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="xeYo_d9N-QdZ"
# #### Empathy

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="naYvlsCR-Qda" outputId="07835fef-72de-47cb-94f0-09f19305a223"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Empathy", meditation.type.constructive, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Empathy"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Empathy"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="uxUFj9vM-Qdc"
# #### Hope

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="H75RkFLx-Qdd" outputId="c38ccb22-777c-44f9-dde8-cea233025cf5"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Hope", meditation.type.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Hope"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Hope"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="wATiNqep-Qde"
# #### Mindfulness (state)

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="IwsvqhUS-Qdf" outputId="5acf52ff-b69c-44c9-dd45-c21ef5143d48"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Mindfulness (state)", meditation.type.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (state)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (state)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="wjMihDNn-Qdh"
# #### Mindfulness (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="00azfbdM-Qdj" outputId="84577749-3afb-4269-ddb7-ae9aea7a85f4"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Mindfulness (trait)", meditation.type.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (trait)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (trait)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="YQE8M38H-Qdj"
# #### Optimism or positive attributional style

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="YQ6X8LVv-Qdl" outputId="4512c6d5-5457-4e8a-acdb-74c44295cd5a"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Optimism or positive attributional style", meditation.type.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Optimism or positive attributional style"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Optimism or positive attributional style"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="8pTMirPp-Qdm"
# #### Religiosity or spirituality or religious coping

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="dwcj0s7F-Qdn" outputId="6829d270-05cc-4990-aa46-bb19482beb8d"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Religiosity or spirituality or religious coping", meditation.type.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Religiosity or spirituality or religious coping"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Religiosity or spirituality or religious coping"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="V6KJ62RK-Qdo"
# #### Self-efficacy

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="nfahMwfu-Qdp" outputId="7a26f1fd-3b5b-40ec-b526-fcb78fac023c"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Self-efficacy", meditation.type.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-efficacy"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-efficacy"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="kz0cZmlp-Qdq"
# #### Self-acceptance

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="EOCN9ubE-Qdr" outputId="a3602f59-54ab-4bbd-eaeb-0bc61c2648f8"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Self-acceptance", meditation.type.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-acceptance"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-acceptance"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="Q9-Bfcb1-Qdr"
# #### Self-esteem

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="fM48ipLB-Qds" outputId="849a0d19-8299-4a43-9c16-ab0acc7f3599"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Self-esteem", meditation.type.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-esteem"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-esteem"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="gtgg5E70-D_A"
# ### Attentional and Contructive Family mixed

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="GwGAS37O-X3g" outputId="ab18b5b9-fc9b-4f4c-b77d-0ac5b13079e3"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Resilience Scale", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Resilience Scale"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Resilience Scale"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="IVHOVFIK-X3i"
# #### Anxiety (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="gGNHgf-4-X3j" outputId="31bcf8a6-3c8c-4c8d-d938-94ae3d168181"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Anxiety (trait)", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (trait)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (trait)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="mOqVR5ow-X3k"
# #### Anxiety (state)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="TfuzFgIA-X3k" outputId="e8dfff9c-88f0-40bc-ce35-08614f1be745"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Anxiety (state)", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (state)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (state)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="MU3fNsal-X3l"
# #### Depression (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="htRpzPjJ-X3m" outputId="1c0ad2a5-ab25-4760-c285-4a33840d6eaa"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Depression (trait)", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Depression (trait)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Depression (trait)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="GvjvSOd--X3n"
# #### Stress

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="Q_Qz-9Sf-X3n" outputId="627be0f3-b49b-4ec2-fe93-c799992f224c"
prefered.scales <- c("Depression Anxiety and Stress Scale (DASS)", "Perceived Stress Scale (PSS)")

forest.plt.by.outcome..med.type(
  "Stress", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, double.scale = TRUE, prefered.scale = prefered.scales[1]
)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="UMu7ilMy-X3o" outputId="7b4ab26c-9947-435e-b3c8-a621709c9edd"
forest.plt.by.outcome..med.type(
  "Stress", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, double.scale = TRUE, prefered.scale = prefered.scales[2]
)

# %% [markdown] id="oCuWmamp-X3p"
# #### Well-being or quality of life

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="5Fk8df4L-X3q" outputId="607e808d-2f5d-4e18-8cfd-6a1aaa6ee228"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Well-being or quality of life", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Well-being or quality of life"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Well-being or quality of life"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="QPeBombl-X3r"
# #### Acceptance

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="lpy9rtrL-X3s" outputId="49d64a85-296c-433d-8960-4a79603e1d37"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Acceptance", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Acceptance"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Acceptance"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="X1KA7PsI-X3s"
# #### Empathy

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="klBbXAQb-X3t" outputId="b7a6aca3-ce93-48ca-a841-741e814d7c77"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Empathy", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Empathy"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Empathy"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="eL7iiA3L-X3u"
# #### Hope

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="58ACb8rX-X3v" outputId="e8d4f957-b498-458e-a7c7-544c91fc677d"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Hope", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Hope"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Hope"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="tscTfMDL-X3x"
# #### Mindfulness (state)

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="OHngZLrC-X3y" outputId="ae65dda5-ad94-4e71-8b88-c57f4a9eb2e4"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Mindfulness (state)", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (state)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (state)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="GI3IimV_-X3z"
# #### Mindfulness (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="oFmpTRGh-X3z" outputId="0e4d5649-0bf1-433b-eab1-a72f24ba3f98"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Mindfulness (trait)", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (trait)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (trait)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="fwC2No57-X30"
# #### Optimism or positive attributional style

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="UAOpQLX7-X31" outputId="1eefc090-12bc-4c5f-fda7-a74881d9e3d7"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Optimism or positive attributional style", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Optimism or positive attributional style"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Optimism or positive attributional style"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="iIRaJOtn-X33"
# #### Religiosity or spirituality or religious coping

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="mA4NuofZ-X33" outputId="1ab78703-8c84-4f4b-aac5-f032cb2bb979"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Religiosity or spirituality or religious coping", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Religiosity or spirituality or religious coping"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Religiosity or spirituality or religious coping"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="BhUv7mIW-X34"
# #### Self-efficacy

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="ydfQWvk_-X34" outputId="e9a02c5b-a742-4fce-fa5c-4919ed712ec6"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Self-efficacy", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-efficacy"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-efficacy"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="12RJHoj5-X35"
# #### Self-acceptance

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="KrURF2sj-X36" outputId="53d7ce87-4fa5-419d-9cf2-5e8690c5c785"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Self-acceptance", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-acceptance"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-acceptance"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="Sr2vERTw-X37"
# #### Self-esteem

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="n-peyxsK-X37" outputId="fe520393-df8d-4819-b5ae-1595e18eb734"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Self-esteem", meditation.type.attentional.and.constructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-esteem"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-esteem"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="W7w_hj6WwyI4"
# ### All Families mixed 

# %% [markdown] id="iaSp_tKC8Q7v"
# #### Resilience Scale

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="nthCodWA8Q7y" outputId="f749355e-744d-4b0e-cb23-fb4a9220bf22"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Resilience Scale", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Resilience Scale"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Resilience Scale"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="kuTh7rAK8Q72"
# #### Anxiety (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="8b9UOKZK8Q74" outputId="450cc513-f935-44b8-b39e-b2645878f888"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Anxiety (trait)", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (trait)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (trait)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="6y-M6ogl8Q77"
# #### Anxiety (state)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="uFtO0KBN8Q78" outputId="d7425eaa-0111-4425-ad48-f536f55f7e8d"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Anxiety (state)", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (state)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (state)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="qCfWpzc08Q8A"
# #### Depression (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="ai-IodCC8Q8B" outputId="ff01ab28-c4f9-4451-c2a2-28a948e3461a"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Depression (trait)", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Depression (trait)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Depression (trait)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="P4gp5Ebx8Q8C"
# #### Stress

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="WIXqXudq8Q8E" outputId="b96fd354-dc2d-4703-959b-f5d35df7d78c"
prefered.scales <- c("Depression Anxiety and Stress Scale (DASS)", "Perceived Stress Scale (PSS)")

forest.plt.by.outcome..med.type(
  "Stress", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, double.scale = TRUE, prefered.scale = prefered.scales[1]
)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="ySNjAWCf8Q8H" outputId="dc7269be-1948-49e5-f1f0-f599ff8d101b"
forest.plt.by.outcome..med.type(
  "Stress", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, double.scale = TRUE, prefered.scale = prefered.scales[2]
)

# %% [markdown] id="SdNxc6eD8Q8I"
# #### Well-being or quality of life

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="bUPxm1Wu8Q8J" outputId="187e15d3-8b4a-4be9-b395-c0a982938071"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Well-being or quality of life", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Well-being or quality of life"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Well-being or quality of life"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="JMbgYwO58Q8M"
# #### Acceptance

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="uWM4pioT8Q8N" outputId="5dfc8564-fd20-4622-956d-518c768ad466"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Acceptance", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Acceptance"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Acceptance"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="VFe20pW48Q8O"
# #### Empathy

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="vZMIoZlR8Q8P" outputId="715e0169-f83f-4d42-b742-f3f5699bda08"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Empathy", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Empathy"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Empathy"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="TXkuwh8k8Q8R"
# #### Hope

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="PRpZDcfg8Q8S" outputId="cc40f97e-89d4-42e5-cfbd-5dc675d181d2"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Hope", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Hope"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Hope"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="HlhH4cdc8Q8U"
# #### Mindfulness (state)

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="GyPnZc6R8Q8V" outputId="3dbecb1e-1d47-460b-f620-2ad27c4ba0f4"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Mindfulness (state)", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (state)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (state)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="_EU8SuxA8Q8W"
# #### Mindfulness (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="OPJprcZ78Q8X" outputId="60cdeab5-bee7-442f-b532-98a3a4d3fc90"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Mindfulness (trait)", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (trait)"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (trait)"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="t8UuOziS8Q8Z"
# #### Optimism or positive attributional style

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="Y9rna4qH8Q8a" outputId="363bded7-cfbe-409b-f158-ea5d38ec9052"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Optimism or positive attributional style", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Optimism or positive attributional style"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Optimism or positive attributional style"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="-naiZped8Q8b"
# #### Religiosity or spirituality or religious coping

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="i4h0pfKN8Q8d" outputId="3ec3e5ee-82ef-4268-8abb-ef7a59cba42d"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Religiosity or spirituality or religious coping", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Religiosity or spirituality or religious coping"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Religiosity or spirituality or religious coping"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="-xv3bczh8Q8e"
# #### Self-efficacy

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="WlH6QiCr8Q8f" outputId="83d9f46a-22da-4b57-a813-8fb680e26576"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Self-efficacy", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-efficacy"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-efficacy"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="cFjZAA3j8Q8h"
# #### Self-acceptance

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="Rx2N7-kr8Q8h" outputId="6900430c-5894-426b-b006-0d85a9d2a6ac"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Self-acceptance", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-acceptance"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-acceptance"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="TdS3a4Ki8Q8j"
# #### Self-esteem

# %% colab={"base_uri": "https://localhost:8080/", "height": 70} id="WoAZv33g8Q8k" outputId="df16199e-4dec-44e1-d669-1e79d8749d50"
tryCatch(
  {
  forest.plt.by.outcome..med.type(
    "Self-esteem", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-esteem"))
    # message(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-esteem"))
    # message(cond)
    return("")
    # except part for warnings
  }
)

# %% id="E779-Tov8GUU"

# %% [markdown] id="_H3LjqlU71Fn"
# # Old Code

# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} id="zbkRBPppUC9A" outputId="9a5960f7-2558-4033-c76a-3cacccfbc8d7"
prefered.scales <- c("Depression Anxiety and Stress Scale (DASS)", "Perceived Stress Scale (PSS)")

for (outcome in present.outcomes){
  print(outcome)
  if (outcome == "Stress"){
    for (prefered.scale in prefered.scales){
      print(prefered.scale)
      forest.plt.by.outcome..med.type(
        outcome, meditation.type.attentional, m.data.list, time.point = 2, double.scale = TRUE, prefered.scale = prefered.scale
      )
      Sys.sleep(1)
      cat("\n\n")
    }
  } else {
    print(outcome)
    tryCatch(
      {
      forest.plt.by.outcome..med.type(
        outcome, meditation.type.attentional, m.data.list, time.point = 2
      )
      Sys.sleep(1)
          # try part
      },
      error=function(cond) {
        message(paste("Error in outome:", outcome))
        # message(cond)
        return("")
        # except part for errors
      },
      warning=function(cond) {
        message(paste("Warning in outome:", outcome))
        # message(cond)
        return("")
        # except part for warnings
      }
    )
    cat("\n\n")
  }
  print("#########################################")
}

# %% id="qf7M02WXwmIy"
prefered.scales <- c("Depression Anxiety and Stress Scale (DASS)", "Perceived Stress Scale (PSS)")

for (outcome in present.outcomes){
  print(outcome)
  if (outcome == "Stress"){
    for (prefered.scale in prefered.scales){
      cat(" prefered scale:", prefered.scale)
      forest.plt.by.outcome..med.type(
        outcome, meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, double.scale = TRUE, prefered.scale = prefered.scale
      )
      cat("\n")
    }
  } else {
    print(outcome)
    tryCatch(
      {
      forest.plt.by.outcome..med.type(
        outcome, meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
      )
          # try part
      },
      error=function(cond) {
        message(paste("Error in outome:", outcome))
        # message(cond)
        return("")
        # except part for errors
      },
      warning=function(cond) {
        message(paste("Warning in outome:", outcome))
        # message(cond)
        return("")
        # except part for warnings
      }
    )
    cat("\n")
  }
}

# %% [markdown] id="wbNsWX_iLMOP"
# # Code of Test-Meta-Analyses

# %% id="APL7Y8ojLLoR"
library('readxl')
# import csv file into R
# data <- read_excel('review_47966_20220515233722.xlsx')
# write.csv(data, file = 'review_47966_20220515233722.csv')
data <- read.csv('review_47966_20220515233722.csv')

# create df of all Meditation 1 studies
data.med1 =  data[data$Practiced.Techniques.in.Intervention.1 == 'Meditation 1', ] # select only rows with meditation 1 as intervetion 1


# pooled.sd <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n1-2))
# mean.diff <- mean.1 - mean.2
# cohens'd <- mean.diff/pooled.sd
# calculate hedges g out of cohens d with hedges_g()

# create df for meta analysis with Meditation 1
library('esc')
df.meta <- data.frame(
  study.id =	data.med1[, 'Study.ID'],
  number.meditation =	data.med1[, 'Sample.size.Intervention.1.T1'],  # number of participants in meditation condition
  mean.meditation =	data.med1[, 'Intervention.1.Mean.T2'],
  sd.meditation =	data.med1[, 'Intervention.1.SD.T2'],
  number.control =	data.med1[, 'Sample.size.Intervention.2.T1'],
  mean.conttrol	= data.med1[, 'Intervention.2.Mean.T2'],
  sd.control	= data.med1[, 'Intervention.2.SD.T2'],
  med.frequency = data.med1[, 'Frequency'],
  pooled.sd = sqrt((data.med1[, 'Sample.size.Intervention.1.T1']-1)*data.med1[, 'Intervention.1.SD.T2']^2 + (data.med1[, 'Sample.size.Intervention.2.T1']-1)*data.med1[, 'Intervention.2.SD.T2']^2) / (data.med1[, 'Sample.size.Intervention.1.T1']+data.med1[, 'Sample.size.Intervention.1.T1']-2),
  mean.diff.T2 = data.med1[, 'Intervention.1.Mean.T2']-data.med1[, 'Intervention.2.Mean.T2']
)

df.meta$hedges.g <- (df.meta[, "pooled.sd"] / df.meta[, "mean.diff.T2"])

library('meta')
res.df_meta =  metacont(number.meditation, mean.meditation, sd.meditation, 
                        number.control, mean.conttrol, sd.control,
                        fixed = T, random = T, studlab = study.id,
                        data = df.meta, sm = "SMD")

# plot forest plot
forest(res.df_meta, leftcols = c('studlab'))

# plot funnel plot
funnel(res.df_meta)

# metabias: Test for funnel plot asymmetry, based on rank correlation or linear regression method.
metabias(res.df_meta, method.bias = 'linreg', k.min = , plotit = T)
# The p-value is 0.973 which implies no publication bias. However, this meta-analysis contains k=5 studies. Egger’s test may lack the statistical power to detect bias when the number of studies is small (i.e., k<10).

# conduct meta-regression, see: https://cjvanlissa.github.io/Doing-Meta-Analysis-in-R/plotting-regressions.html
library('metafor')
random.effects.model <- rma(
  df.meta[, "hedges.g"],
  df.meta[, "pooled.sd"]
)
  # second paramenter in rma-function = sampling variance = pooled standard deviation?

install.packages('ggplot2')
library(ggplot2)

df.meta$weights <- 1/sqrt(df.meta$pooled.sd)
# Specify basic plot, mapping med.frequency to the x-axis, effect size 'hedges.g' to the y-axis,
# and 'weights' to the weight parameter.
ggplot(df.meta, aes(x = med.frequency, y = hedges.g, size = weights)) +
  geom_point(shape = 1) + # Add scatter
  geom_abline(intercept = random.effects.model$b[1], slope = random.effects.model$b[2]) + # Add regression line
  theme_bw() + # Apply black and white theme
  theme(legend.position = "none") # Remove legend
  # see link for beatiful meta-regression with convidence intervals: https://bookdown.org/robcrystalornelas/meta-analysis_of_ecological_data/meta-regression.html#meta-regression-with-continuous-variable

# plot traffic light plot for RoB
## use {robvis} via the rob_traffic_light function
## see: https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/risk-of-bias-plots.html#traffic-light-plots

# plot Summary Plots
## use use {robvis} via the rob_summary function

# %% [markdown] id="c7M12ml6dtw6"
# # Notes

# %% [markdown] id="2-GuctVTVbPE"
# - Different Scales
#     - 2 Array-Dims with same values
#     - Results in Dim "Scale.2" only if 2nd scale is present, else: NA
#
# - Ignore 2nd scale entry for Sloan 2016 

# %% [markdown] id="HwVwkROXVbPF"
# # ToDo

# %% [markdown] id="iLpnmF67VbPF"
# - find solution for
#   - value: '3.79 during intervention, 2.04 during follow-up mean' in "Frequency.of.intervention.sessions.in.times.per.week.Intervention.1"
# - Function that plots forestplots by outcome and meditation type
#   - why does filtering by meditation types does not work?

# %% [markdown] id="tDtYYqNKVbPG"
# # Array Dims and Dimnames

# %% [markdown] id="rNRTnnTsVbPG"
# Dates of Measuring Time Points
# 	4 x 1
# 	c("Time.Point.0", "Time.Point.1", "Time.Point.2", "Time.Point.3")
# 	c("Date")
# 	Time.Point.0.Date:Time.Point.3.Date
# 	
# Between-Measuring Time Points' Duration
# 	3 x 1
# 	c("Time.Point.0...1", "Time.Point.0...2", "Time.Point.0...3")
# 	c("Duration.in.Days")
# 	Time.Point.0...1:Duration.in.Days
#
# Intervention and Comparisons
# 	7 x 4
# 	c("Name", "Short.Description", "Delivery.Mode", "Meditation.App", "Sessions.Duration.in.minutes", Frequency.in.times.per.week", "Total.Duration.in.Days")
# 	c("Intervention.1", "Intervention.2", "Intervention.3", "Control")
# 	Name.Intervention 1:Total.Duration.of.Intervention.in.Days.Control.or.Intervention.4
#
# Definition of Outcomes
# 	7 x 1
# 	c("Outcome.1", "Outcome.2", "Outcome.3", "Outcome.4", "Outcome.5", "Outcome.6", "Outcome.7")
# 	c("Definition")
# 	Outcome.1:Definition
#
# Measures of Outcomes
# 	7 x 3
# 	c("Outcome.1", "Outcome.2", "Outcome.3", "Outcome.4", "Outcome.5", "Outcome.6", "Outcome.7")
# 	c("Measures.Name", "Start.and.End.Point", "High.or.low.means.resilient")
# 	Outcome.1.Scale.s.or.other.Measure.s.Name:Outcome.7.High.or.low.means.resilient
# 	
# Means and SDs of Outcomes
# 	4 x 3 x 4 x 7
# 	c("Intervention.1", "Intervention.2", "Intervention.3", "Control"
# 	c("Mean", "SD", "n")
# 	c("T0", "T1", "T2", "T3")
# 	c("Outcome.1", "Outcome.2", "Outcome.3", "Outcome.4", "Outcome.5", "Outcome.6", "Outcome.7")
# 	Intervention.1.Mean.O1T0:Control.or.Intervention.4.n.in.case.of.period.O7T3
#
# Quantitative Results of Data Analyses
# 	7 x 3 x 3 x 6
# 	c("Outcome.1", "Outcome.2", "Outcome.3", "Outcome.4", "Outcome.5", "Outcome.6", "Outcome.7")
# 	c("P.Value", "Effect.Size", "Effect.Size.CI")
# 	c("T1", "T2", "T3")
# 	c("Int1.X.Int2", "Int1.X.Int3", "Int1.X.Int4", "Int2.X.Int3", "Int2.X.Int4", "Int3.X.Int4")
# 	Outcome.1.P.Value.1x2T1:Outcome.7.Confidence.Interval.of.Effect.Size.3x4T3
#
# Qualitative Results of Data Analyses
# 	7 x 3
# 	c("Key.Theme.1", "Key.Theme.2", "Key.Theme.4", "Key.Theme.5", "Key.Theme.6", "Key.Theme.7")
# 	c("Name.of.Key.Theme", "Description.of.Key.Theme", "Results")
# 	Key.Theme.1:Results
