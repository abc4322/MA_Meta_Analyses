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

# %% [markdown] heading_collapsed=true id="CN7cVG-zdtvz"
# # Preprocess data of raw covidence Export
#
#

# %% hidden=true id="L5OqXycudtv0"
raw.df <- read.csv("final_data_export_consensus.csv")

# %% [markdown] hidden=true id="TVlGoZT1dtv2"
#
# ## Install and load Dplyr
#

# %% colab={"base_uri": "https://localhost:8080/"} hidden=true id="noEhheyudtv3" outputId="73076355-5889-43e5-dcbf-6d96c36c6534"
install.packages("dplyr")
library("dplyr")
install.packages("sjmisc")
library("sjmisc")
# for data manipulation with dplyr see: https://www.youtube.com/watch?v=Gvhkp-Yw65U
# for splitting 2 values in 1 cell see: https://www.youtube.com/watch?v=DiY8EqZDwoI at 3:17 (e.g. if 2 scales for 1 outcome)
# for joining 2 data frames see:        https://www.youtube.com/watch?v=DiY8EqZDwoI at 11:57

# %% hidden=true id="fr3yuOGbdtv9"
raw.df <- raw.df %>%
  filter(Study.design == "Passive RCT")

# %% [markdown] hidden=true id="9IzRygfTdtv_"
# ## Drop unimportant columns by name

# %% [markdown] hidden=true id="YjOLv1chBdoB"
# ### Remove column rages (first columns)

# %% hidden=true id="fVJh-WRLdtwA"
my.df <- raw.df %>%
  select(-Reviewer.Name:-Further.Information.inserted.in.Extraction.Form.)  # "-" indicates deleting these columns

# %% [markdown] hidden=true id="mJ0I76q5dtwB"
# ### Remove single column names and repeating names with ascending numbers (table headlines)
#

# %% hidden=true id="fpJhMxcsdtwC"
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

# %% hidden=true id="nZ1K97-UVbN0"
# set options to print entire df (extend max. rows end cols)
options(repr.matrix.max.rows=30, repr.matrix.max.cols=1100)

# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} hidden=true id="yM0QVCjedDmJ" outputId="e54938d6-5342-41c0-a109-772cb57716a0"
my.df

# %% [markdown] hidden=true id="22dJtedvdtwE"
#
# ## Create arrays, data frame lists, and data frames from Covidence tables
#

# %% [markdown] hidden=true id="ZpNZyCCKdtwR"
#
# ### Set basic parameters
#

# %% hidden=true id="zd8iL_EwdtwU"
nm.placeholder <- -999  # placeholder for values marked as nm (not mentioned)

flag.x.s.r <- 2
  # x.s.r = exact value (0), mean s (1), or mean r (2)
  # flag.x.s.r = 2 --> include all
  # flag.x.s.r = 1 --> only include mean s and exact values
  # flag.x.s.r = 0 --> include only exact vaules

study.no <- length(my.df[,"Study.ID"])

# %% [markdown] hidden=true id="L5zVk_c8dtwV"
# ### Functions

# %% [markdown] hidden=true id="zMsGw6SrBoS1"
# #### Checking for digits and characters

# %% hidden=true id="-E3epanVdtwW"
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

# %% colab={"base_uri": "https://localhost:8080/", "height": 0} hidden=true id="clFW77sUVbOC" outputId="4ef0ee4e-778d-41c1-85d1-e8da4d33a9c3"
char.inside("mindfulness meditation")

# %% [markdown] hidden=true id="BhtFUMqudtwW"
#
# #### For mean values
#

# %% hidden=true id="YIjOsZEWdtwX"
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

# %% [markdown] hidden=true id="mLXTMZ4QdtwY"
#
# #### Convert value from nm, NA, digit, or char
#

 # %% colab={"base_uri": "https://localhost:8080/", "height": 0} hidden=true id="6LzWIl1YbheI" outputId="ed672427-bb3e-4559-e327-ffce3b257605"
 round(1.56565, digits = 3)

# %% hidden=true id="Q0dN9yZOdtwZ"
convert.value <- function(value, missing.value.placeholder = nm.placeholder, only.double = TRUE){
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

# %% [markdown] hidden=true id="1d-upnnG2Fup"
# #### Check if multiple commas are present in string

# %% hidden=true id="soDFylve2Fbm"
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

# %% [markdown] hidden=true id="HvqEIc9Ndtwb"
#
# #### For arrays
#

# %% hidden=true id="ln3DQzW-dtwb"
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


# %% [markdown] hidden=true id="DmwjedIRdtwc"
# #### For data frame lists
#

# %% hidden=true id="b-hKzmmDdtwd"
# create data frames out of 2D Tables (with rows and cols swapped)
clean.data.to.df.list.swap <- function(
  my.df, start, end, dims, study.names, dimname.list, check.multiple.commas = FALSE
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
          value, nm.placeholder, only.double = FALSE
          )
      }
    }
  }
  study.df.list
}


# %% hidden=true id="3boc2pzJdtwg"
# create data frames out of 2D Tables
clean.data.to.df.list <- function(
  my.df, start, end, dims, study.names, dimname.list, check.multiple.commas = FALSE
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


# %% [markdown] hidden=true id="IY3HznuXWgrY"
# #### Print arrays

# %% hidden=true id="RnZcGBSqWtKz"
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

# %% [markdown] hidden=true id="VBH-GtDNdtwk"
# ### Arrays
#

# %% [markdown] hidden=true id="p1aqcyGEhuWI"
# #### Population Characteristics

# %% colab={"base_uri": "https://localhost:8080/"} hidden=true id="sTgpJVklh1FW" outputId="661c628c-510a-4e9e-f5c0-aff23206a40d"
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

# %% [markdown] hidden=true id="NijSHYzz8hgo"
# ##### Correct input mistakes

# %% hidden=true id="vZJx4dJABh_n"
if (
  is.na(population.characteristics.array["No.Participants", "Intervention.4", 1, "Janowiak 1994"]) &
  population.characteristics.array["No.Participants", "Intervention.3", 1, "Janowiak 1994"] == 21
){
  population.characteristics.array["No.Participants", "Intervention.4", 1, "Janowiak 1994"] <- 21
  population.characteristics.array["No.Participants", "Intervention.3", 1, "Janowiak 1994"] <- NA

  population.characteristics.array[, "Intervention.4", 1, "Smith 2021"] <- population.characteristics.array[, "Intervention.2", 1, "Smith 2021"]
  population.characteristics.array[, "Intervention.2", 1, "Smith 2021"] <- NA
}



# %% colab={"base_uri": "https://localhost:8080/"} hidden=true id="SqV3Ew2FiQBQ" outputId="719343ce-c1a0-4175-8fd9-aed731af4992"
print.array.not.na(population.characteristics.array)

# %% [markdown] hidden=true id="xaAo8PmDSSbb"
# #### Results Descriptive

# %% hidden=true id="XlPWYl4tdtwl"
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

# %% hidden=true id="Nd4uXwuhdtwn"
# use my.pop.char.array[,,"T0","Johnson-Waddell 2018"] as test for "", NA, "nm", "double"
#      [,1]  [,2] [,3] [,4]  [,5]
# [1,] "197" NA   NA   "197" 394 
# [2,] ""    ""   NA   ""    "nm"
# [3,] ""    NA   NA   ""    "nm"
# [4,] ""    NA   NA   ""    "" 
# make flag.x.s.r work --> done


# %% [markdown] hidden=true id="7uGNLOh19Ixt"
# ##### Correct input mistakes

# %% hidden=true id="Q-BUcDKD9L38"
if (!is.na(results.descriptive.array["Intervention.1", "Mean", "T1", "Outcome.2", "Scale.1", "Ratanasiripong 2015"])){
  results.descriptive.array["Intervention.2",, "T1", "Outcome.2", "Scale.1", "Ratanasiripong 2015"] <- results.descriptive.array["Intervention.1",, "T1", "Outcome.2", "Scale.1", "Ratanasiripong 2015"]
  results.descriptive.array["Intervention.1",, "T1", "Outcome.2", "Scale.1", "Ratanasiripong 2015"] <- NA
}

# %% colab={"base_uri": "https://localhost:8080/"} hidden=true id="Bd_7r-Esgmvz" outputId="52265f08-f209-42c6-e4b1-da1d8defcac1"
print.array.not.na(results.descriptive.array)

# %% [markdown] hidden=true id="2QTPOlqldtwq"
#
# #### Results Quantitative
#

# %% hidden=true id="qmAGEv3Qdtws"
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

# %% [markdown] hidden=true id="HfyamaAkdtwt"
# ### Data frame lists

# %% [markdown] hidden=true id="QLuCkxIfVbOx"
# #### Intervention Comparison

# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} hidden=true id="6V5Rp0Drdtwu" outputId="73c4c435-ec27-4e14-928c-71b6b676c7d6"
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

my.df <- my.df %>%
  select(-Name.Intervention.1:-Total.Duration.of.Intervention.in.Days.Control.or.Intervention.4)

intervention.comparisons.df.list

# %% [markdown] hidden=true id="twxkiSZRdtw2"
#
# #### Measures of Outcomes
#

# %% colab={"base_uri": "https://localhost:8080/", "height": 0} hidden=true id="fwFdJjUIdtw3" outputId="3e49bd34-fdb9-41d6-95e4-3f916dce2751"
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


# %% [markdown] hidden=true id="5joO9oxVdtw4"
#
# #### Qualitative Results of Data Analyses
#

# %% colab={"base_uri": "https://localhost:8080/", "height": 0} hidden=true id="cpOokvFRdtw5" outputId="2925f4e1-6026-4f1b-a7cd-bd42cacec728"
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


# %% [markdown] hidden=true id="Rs8_xK2UXPTK"
# ### Data frames

# %% [markdown] hidden=true id="1ssi56Ffdtwv"
#
# #### Dates of Measuring Time Points
#

# %% colab={"base_uri": "https://localhost:8080/", "height": 0} hidden=true id="nF-WvDM0dtww" outputId="8feb49b0-4ccf-4b7e-bb98-3a1df2a1b500"
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

# %% [markdown] hidden=true id="wjV0e9Exdtwy"
#
# #### Between-Measuring Time Points Duration
#

# %% colab={"base_uri": "https://localhost:8080/", "height": 0} hidden=true id="cCsuJj6rdtwz" outputId="aee2400e-c103-4138-e14e-a464cf3fe54d"
between.T.duration.df <- my.df[
  , which(colnames(my.df)=="Time.Point.0...1.Duration.in.Days"):which(colnames(my.df)=="Time.Point.0...3.Duration.in.Days")
]

for (row in 1:nrow(between.T.duration.df)){
  for (col in 1:ncol(between.T.duration.df)){
    between.T.duration.df[row, col] <- convert.value(between.T.duration.df[row, col])
  }
}

rownames(between.T.duration.df) <- study.names

my.df <- my.df %>%
  select(-Time.Point.0...1.Duration.in.Days:-Time.Point.0...3.Duration.in.Days)

between.T.duration.df

# %% [markdown] hidden=true id="UAh7lkDLdtw0"
#
# #### Definition of Outcomes
#

# %% colab={"base_uri": "https://localhost:8080/", "height": 0} hidden=true id="L2vCxatldtw1" outputId="78dd9931-a8c9-49c3-bd6a-4c803a51e143"
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

# %% [markdown] hidden=true id="ZrqOKXPIZLGT"
# #### Outcome Names

# %% colab={"base_uri": "https://localhost:8080/", "height": 0} hidden=true id="AwYPRCAmVgC_" outputId="ebad07fa-9653-4466-da2b-4951427f2638"
outcome.names.df <- my.df[, which(colnames(my.df)=="Name.of.Outcome.1"):which(colnames(my.df)=="Name.of.Outcome.7")]

for (row in 1:nrow(outcome.names.df)){
  for (col in 1:ncol(outcome.names.df)){
    outcome.names.df[row, col] <- convert.value(outcome.names.df[row, col])
  }
}

my.df <- my.df %>%
  select(-Name.of.Outcome.1:-Name.of.Outcome.7)

rownames(outcome.names.df) <- study.names
outcome.names.df

# %% [markdown] hidden=true id="Igdgazs5Mgc8"
# #### Meditation Techniques

# %% colab={"base_uri": "https://localhost:8080/", "height": 0} hidden=true id="JVDPEdbOMfUD" outputId="492a8742-09a3-423e-e08c-fb8a91018857"
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

meditation.techniques.df

# %% [markdown] hidden=true id="d5b4N8P6izBC"
# ##### correct input mistakes

# %% hidden=true id="ecz49Zrai5QN"
if (
  is.na(meditation.techniques.df["Lee 2018", 1]) &
  is.na(meditation.techniques.df["Lee 2018", 2]) &
  is.na(meditation.techniques.df["Lee 2018", 3])
){
  meditation.techniques.df["Lee 2018", 1] <- "Only \"Mindfulness Meditation\" named"
}

# %% [markdown] hidden=true id="eZKlbjHMVbPB"
# #### Left 1D Data

# %% colab={"base_uri": "https://localhost:8080/", "height": 0} hidden=true id="UvagubmWVbPB" outputId="a2fe6f05-93ee-4340-9246-b3f63320d2bc"
my.df[my.df == 'None' | my.df == '' | my.df == NA] <- "NA"

one.D.info.df <- my.df

rownames(one.D.info.df) <- study.names

one.D.info.df

# %% colab={"base_uri": "https://localhost:8080/", "height": 0} hidden=true id="TOTcTcARK3cS" outputId="62458239-1d54-471a-e48b-8debc6c4ce37"
colnames(one.D.info.df)

# %% [markdown] hidden=true id="HGMi3f0FV2Jb"
# ## Make scale, delivery modes, and meditation type names unique

# %% [markdown] hidden=true id="qo7Bneh2BdI5"
# ### Meditation types

# %% colab={"base_uri": "https://localhost:8080/"} hidden=true id="nVKJjyISWEmI" outputId="21fe666d-844c-41e4-ff73-09f9a4f6fc99"
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

# %% hidden=true id="JUwRbuo6M_f1"
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

meditation.type.deconstructive <- c(
  "Vipassana"
)

meditation.type.attentional.and.constructive <- c(
  "Body Scan; Breathing Exercise (no further Info); Loving-Kindness Meditation"
)

meditation.type.attentional.constructive.and.deconstructive <- c(
  "Other: mindfulness-based exercises through audio video or text files, grounding visualization, gratitude, imagining the life you want, finding meaning",
  "Breathing Exercise (no further Info); Other: focusing on meta-awareness, \"Mindfulness-based exercises [...] about happiness in life as the result of a benevolent attitude towards life and a positive relationship to oneself and others\", \"Methods of introspection, reflection and self-care\"",
  "Prayer; Other: devotional readings to ponder, meditations taken from a widely used devotional book designed for Christian readers titled, forgiveness meditations"  # due to "forgiveness" --> strengthens cognitive and affective patterns (constructive), "reading to ponder" --> deconstructive
)


meditation.type.all <- c(
  meditation.type.attentional,
  meditation.type.constructive,
  meditation.type.attentional.and.constructive,
  meditation.type.attentional.constructive.and.deconstructive,
  meditation.type.deconstructive
)

# %% [markdown] hidden=true id="suhKn26IBgdG"
# ### Scales

# %% hidden=true id="DPsdl0wv7hXn"
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

# %% colab={"base_uri": "https://localhost:8080/", "height": 0} hidden=true id="7udcDROocc8L" outputId="d373ba3d-e58c-429f-971c-9b76ea83d515"
outcome.measures.df.list[["Gupta 2020"]]

# %% [markdown] hidden=true id="4tkp2rxMIrED"
# #### Resilience

# %% hidden=true id="AY42c7L3IW7S"
scale.CD.RISC.10.synonyms <- c(
  "Connor-Davidson Resilience Scale (CD-RISC-10) (Connor & Davidson, 2003)"
)

# %% [markdown] hidden=true id="iiYIyLWYIwAE"
# #### Mental health-related

# %% [markdown] hidden=true id="a0C2z3Xmzq9L"
# ##### Including Subscales

# %% hidden=true id="QWceMIVPH82r"
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

# %% [markdown] hidden=true id="yWVcnzO40j4y"
# ##### 2 Scales per Cell

# %% hidden=true id="HVY5_Eaj0oMP"
scales.DASS.PSS.synonyms <- c(
  "Depression, Anxiety\nand Stress Scale (DASS) 26, Perceived Stress Scale\n(PSS)27,28",
  "Depression, Anxiety, and Stress Scale – 21 (DASS-21),\nPerceived Stress Scale (PSS)"
)

scales.PSS.DASS.synonyms <- c(
  "Perceived Stress Scale (PSS), Depression Anxiety and Stress Scale (DASS) - Stress Subcale"
)

# %% [markdown] hidden=true id="csaEiELtzwRZ"
# ##### Anxiety

# %% hidden=true id="1eO77-XCzB20"
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

# %% [markdown] hidden=true id="_uIJGt86zzVJ"
# ##### Depression

# %% hidden=true id="rDCZR5aJIP-F"
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

# %% [markdown] hidden=true id="828kbyUbz1LN"
# ##### Stress

# %% hidden=true id="SkYmlm1FH0OD"
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

# %% [markdown] hidden=true id="zrlO4eS6z5ew"
# ##### Quality of life

# %% hidden=true id="WqC-XpLOJZSh"
scale.LSS.synonyms <- c(
  "Questionnaire for the Assessment of Happiness ( ger.: Lebensglückskala, LSS) (Ciccarello & Reinhard, 2014)"
)

scale.SWLS.synonyms <- c(
  "Satisfaction with Life Scale (SWLS [71])",
  "Satisfaction with Life Scale (SWLS)",
  "satisfaction with life scale (SWLS; Diener et al., 1985)"
)

scale.WHO.QOL.BREF.synonyms <- c(
  "World Health Organization Quality of Life-Brief (WHO-QOL-BREF)"
)

# %% [markdown] hidden=true id="xusmfMsOI3t2"
# #### Resilience Factors

# %% [markdown] hidden=true id="tNsa9WRlz-_F"
# ##### Coping, positve

# %% hidden=true id="k-og6O0yIfGl"
scale.not.common.positive.coping.synonyms <- c(
  "scales adapted from a\nwell-known set of brief measures developed by the Fetzer Institute (1999, pp. 86–87) - positve coping subscale"
)

# %% [markdown] hidden=true id="Em4Uu9Ay0BN8"
# ##### Empathy

# %% hidden=true id="EMSUl-nFInPg"
scale.BEA.synonyms <- c(
  "Batson Empathy Adjectives (BEA; Batson, 1986; Coke, Batson, & McDavis, 1978)"
)

# %% [markdown] hidden=true id="J1mZVAEI0DQb"
# ##### Mindfulness

# %% hidden=true id="ISVYIeknI_nw"
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
  "Mindfulness Attention Awareness Scale (MAAS) (Brown & Ryan, 2003)",
  "mindful attention awareness scale (MAAS; Brown and Ryan, 2003)"
)

scale.TMS.synonyms <- c(
  "The Toronto Mindfulness Scale (TMS)"
)

# %% [markdown] hidden=true id="9mUabHEA0NlP"
# ##### Psychological Capital

# %% hidden=true id="VjmrAnJ0KIH6"
scale.PCQ.synonyms <- c(
  "Psychological Capital\nQuestionnaire (PCQ) 19"
)

# %% [markdown] hidden=true id="y1sLz_A70Pge"
# ##### Self-acceptance

# %% hidden=true id="VkL_GWL8JGHd"
scale.CPI.synonyms <- c(
  "California Psychological Inventory French version (CPI) (Gough 1957) - Subscale Self-acceptance (Sa)"
)

# %% [markdown] hidden=true id="d3ftu1Yb0RrZ"
# ##### Self-compassion

# %% hidden=true id="KtfYnKU6zItB"
scale.SCS.synonyms <- c(
  "Self-Compassion Scale (SCS; Neff, 2003a)"
)

scale.SCS.SF.synonyms <- c(
  "Self-Compassion Survey Short-Form (SCS-SF)"
)

# %% [markdown] hidden=true id="L4lc2cVy0T8f"
# ##### Self-esteem

# %% hidden=true id="6UsBRYUOysPx"
scale.RSES.synonyms <- c(
  "Rosenberg Self-Esteem Scale (RSES; Rosenberg, 1965)"
)

# %% [markdown] hidden=true id="4whkx7Wm1atg"
# #### Set unique scale names

# %% colab={"base_uri": "https://localhost:8080/"} hidden=true id="WgVfk7ul1h51" outputId="24aae7e2-3418-485b-b7e4-12f9812f572c"
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
          "study: ", study.names[study], "\n\n"
          )
      }
    }
  }
}


# %% [markdown] hidden=true id="YY0xZxAgePtS"
# ## Fill empty n, mean age, and sex values

# %% [markdown] hidden=true id="BCW7blVJfAf3"
# ### n

# %% colab={"base_uri": "https://localhost:8080/", "height": 0} hidden=true id="G23yVcV4N1B6" outputId="ca36985e-4620-4044-d77d-8331e6467343"
colnames(one.D.info.df)

# %% colab={"base_uri": "https://localhost:8080/"} hidden=true id="iYJ3f1JIeNgc" outputId="49daa23d-c764-4901-8b54-0df8f9653d59"
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
              results.descriptive.array[intervention, "n", t, outcome, scale, study] <- n
            }
          }
        }
      }
    }
  }
}

# %% [markdown] hidden=true id="mK6iGfTCHYy-"
# --> Remove Messer 2016 from data for Meta-Analyses

# %% colab={"base_uri": "https://localhost:8080/"} hidden=true id="rZqyWSYE3ETC" outputId="b3176810-b8d0-4665-e569-5d067b82c7c0"
print.array.not.na(population.characteristics.array)

# %% [markdown] hidden=true id="R6gDeqegVbPC"
# ## Put total data into a list

# %% hidden=true id="tO7ZASXqVbPD"
m.data.list <- list(
    one.D.info.df,
    population.characteristics.array,
    intervention.comparisons.df.list,
    outcome.measures.df.list,
    results.descriptive.array,
    results.quantitative.array,
    results.qualitative.df.list,
    dates.measuring.time.points.df,
    between.T.duration.df,
    outcome.definitions.df,
    outcome.names.df,
    meditation.techniques.df
)

names(m.data.list) <- c(
    "one.D.info.df",
    "population.characteristics.array",
    "intervention.comparisons.df.list",
    "outcome.measures.df.list",
    "results.descriptive.array",
    "results.quantitative.array",
    "results.qualitative.df.list",
    "dates.measuring.time.points.df",
    "between.T.duration.df",
    "outcome.definitions.df",
    "outcome.names.df",
    "meditation.techniques.df"
)

# %% [markdown] id="JMb2giHBfawn"
# # Meta-Analyses =========================

# %% [markdown] id="wrlelJx6hEGy"
# ## Functions

# %% colab={"base_uri": "https://localhost:8080/"} id="EPvplRN3U9eS" outputId="698bc46d-db83-4e90-96e0-ba142862c992"
install.packages("esc")
install.packages("meta")
install.packages("metafor")
install.packages('ggplot2')
library("esc")
library("meta")
library("metafor")
library("ggplot2")

# %% [markdown] id="v08Vc3fG3ZE6"
# ### Meta results and plots by outcome and meditation type

# %% id="s2R6D_b2feWt"
meta.analyze <- function(
  outcome, meditation.types, m.data.list, time.point, preferred.scale = FALSE,
  print.forest = FALSE, print.funnel = FALSE, print.meta.results = FALSE,
  print.descriptive = FALSE, regression.factor = FALSE,
  return.data = FALSE, filter.vec = FALSE, sort.by = FALSE
    # meditation.types = vector of meditation types
    # preferred.scale = preferred scale in case of 2 scales for one outcome (Stress)
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
  sessions.duration <- c()
  sessions.frequency <- c()
  programs.duration <- c()
  delivery.mode <- c()

#___For other than Stress (Outcomes with one scale per outcome per study)___#
  if (preferred.scale == FALSE){
    for (study in 1:study.no){
      int.per.study <- 0
      for (outcome.no in 1:7){
        for (intervention.no in 1:4){
          results.descriptive.array <- m.data.list[["results.descriptive.array"]]
          if (
            m.data.list[["outcome.names.df"]][study, sprintf("Name.of.Outcome.%d", outcome.no)] == outcome &
            !(
              is.na(m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"]) |
              m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == "NA" |
              m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == nm.placeholder |
              m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == as.character(nm.placeholder)
            ) &
            (
              m.data.list[["meditation.techniques.df"]][study, intervention.no] %in% meditation.types |
              ((TRUE %in% (m.data.list[["meditation.techniques.df"]][study,] %in% meditation.types)) & intervention.no == 4)
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
# Descriptive Results
## for control group
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
            
## for intervention
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
# Results for meta-regression
            if (regression.factor != FALSE){
              if (
                !is.na(intervention.comparisons.df.list[[study]][intervention.no, "Sessions.Duration.in.minutes"]) &
                intervention.comparisons.df.list[[study]][intervention.no, "Sessions.Duration.in.minutes"] != "NA" &
                intervention.comparisons.df.list[[study]][intervention.no, "Sessions.Duration.in.minutes"] != nm.placeholder &
                intervention.comparisons.df.list[[study]][intervention.no, "Sessions.Duration.in.minutes"] != as.character(nm.placeholder)
              ){
                sessions.duration <- append(
                  sessions.duration,
                  intervention.comparisons.df.list[[study]][intervention.no, "Sessions.Duration.in.minutes"]
                )
              } else {
                sessions.duration <- append(
                  sessions.duration,
                  NA
                )
              }

              if (
                !is.na(intervention.comparisons.df.list[[study]][intervention.no, "Frequency.in.times.per.week"]) &
                intervention.comparisons.df.list[[study]][intervention.no, "Frequency.in.times.per.week"] != "NA" &
                intervention.comparisons.df.list[[study]][intervention.no, "Frequency.in.times.per.week"] != nm.placeholder &
                intervention.comparisons.df.list[[study]][intervention.no, "Frequency.in.times.per.week"] != as.character(nm.placeholder)
              ){
                sessions.frequency <- append(
                  sessions.frequency,
                  intervention.comparisons.df.list[[study]][intervention.no, "Frequency.in.times.per.week"]
                )
              } else {
                sessions.frequency <- append(
                  sessions.frequency,
                  NA
                )
              }

              if (
                !is.na(intervention.comparisons.df.list[[study]][intervention.no, "Total.Duration.in.Days"]) &
                intervention.comparisons.df.list[[study]][intervention.no, "Total.Duration.in.Days"] != "NA" &
                intervention.comparisons.df.list[[study]][intervention.no, "Total.Duration.in.Days"] != nm.placeholder &
                intervention.comparisons.df.list[[study]][intervention.no, "Total.Duration.in.Days"] != as.character(nm.placeholder)
              ){
                programs.duration <- append(
                  programs.duration,
                  intervention.comparisons.df.list[[study]][intervention.no, "Total.Duration.in.Days"]
                )
              } else {
                programs.duration <- append(
                  programs.duration,
                  NA
                )
              }

              if (
                !is.na(intervention.comparisons.df.list[[study]][intervention.no, "Delivery.Mode"]) &
                intervention.comparisons.df.list[[study]][intervention.no, "Delivery.Mode"] != "NA" &
                intervention.comparisons.df.list[[study]][intervention.no, "Delivery.Mode"] != nm.placeholder &
                intervention.comparisons.df.list[[study]][intervention.no, "Delivery.Mode"] != as.character(nm.placeholder)
              ){
                delivery.mode <- append(
                  delivery.mode,
                  intervention.comparisons.df.list[[study]][intervention.no, "Delivery.Mode"]
                )
              } else {
                delivery.mode <- append(
                  delivery.mode,
                  NA
                )
              }
            }
          }
        }
      }
    }

#___For Stress (Sometimes 2 scales per study per outcome)___#
  } else if (preferred.scale != FALSE){
    for (study in 1:study.no){
      int.per.study <- 0
      for (scale in 1:2){
        for (outcome.no in 1:7){
          for (intervention.no in 1:4){
            results.descriptive.array <- m.data.list[["results.descriptive.array"]]

            if (
              m.data.list[["outcome.names.df"]][study, sprintf("Name.of.Outcome.%d", outcome.no)] == outcome &
              !(
                is.na(m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"]) |
                m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == "NA" |
                m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == nm.placeholder |
                m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == as.character(nm.placeholder)
              ) &
              (
                m.data.list[["meditation.techniques.df"]][study, intervention.no] %in% meditation.types |
                ((TRUE %in% (m.data.list[["meditation.techniques.df"]][study,] %in% meditation.types)) & intervention.no == 4)
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

# skip loop if 2 scales per outcome are present and current scale is not the preferred one
              scale.name <- m.data.list[["outcome.measures.df.list"]][[study]][sprintf("Outcome.%d", outcome.no), "Measures.Name"]
              if (multiple.commas.present(scale.name)){
                cat("multiple commas in scale name, find solution:", scale.name, "\n\n")
                next
              } else if (grepl(",", scale.name)){  # comma = 2 scales per outcome
                if (scale == 1){
                  scale.name.substring <- sub(",.*", "", scale.name)
                    # extracts substring before comma
                  if (!grepl(gsub("([()])","\\\\\\1", preferred.scale), scale.name.substring)){
                    # grepl(gsub("([()])","\\\\\\1", preferred.scale) makes parenthesis in preferred.scale searchable
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

# Descriptive Results
## for control group
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
              
## for intervention
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
# Results for meta-regression

            }
          }
        }
      }
    }
  }

  pooled.sd <- sqrt((((n.int - 1) * sd.int^2) + ((n.control - 1) * sd.control^2)) / (n.int + n.control -2))
  mean.diff <- mean.int - mean.control
  hedges.g <- mean.diff / pooled.sd 
  
  meta.df <- data.frame(
    study.id =	study.id,
    n.control =	n.control,
    mean.control =	mean.control,
    sd.control =	sd.control,
    n.int =	n.int,
    mean.int	= mean.int,
    sd.int	= sd.int,
    pooled.sd = pooled.sd,
    mean.diff = mean.diff,
    hedges.g = hedges.g
  )

  # sort studies
  if (sort.by == "hedges g"){
    meta.df <- arrange(meta.df, hedges.g)
  } else if (sort.by == "name"){
    meta.df <- arrange(meta.df, study.id)
  } else if (sort.by != FALSE){
    cat('\n\nsort by "hedges g" or "name"\n\n')
  }

  # filter for studies for clustering
  if (!FALSE %in% filter.vec){
    meta.df <- meta.df[filter.vec,]
  }

  # get results df for forest plot
  results.forest.df <- metacont(
    n.int, mean.int, sd.int,
    n.control, mean.control, sd.control,
    fixed = T, random = T, studlab = study.id,
    data = meta.df, sm = "SMD"
  )

  # plot forest plot and results
  if (print.descriptive){
    print(meta.df)
  }

  if (print.meta.results){
    print(results.forest.df)
  }

  if (print.forest){
    forest(results.forest.df, leftcols = c('studlab'))
  }

  # get results for funnel and meta-regression plot
  results.yi.vi <- escalc(
    measure = "SMD",
    m1i = meta.df[,"mean.int"], sd1i = meta.df[,"sd.int"], n1i = meta.df[,"n.int"],
    m2i = meta.df[,"mean.control"], sd2i = meta.df[,"sd.control"], n2i = meta.df[,"n.control"]
  )

  results.funnel <- rma(
    yi = results.yi.vi[,"yi"], vi = results.yi.vi[,"vi"], measure="SMD"
  )
    # method argument is set default to: method="REML" = restricted maximum likelihood estimator (Viechtbauer, 2005; Raudenbush, 2009)
  
  if (print.funnel){
    # with {meta}
    # funnel(results.forest.df)

    # with {metafor}
    trim.and.fill <- trimfill(results.funnel)

    funnel(trim.and.fill, legend=TRUE)
  }

  if (regression.factor == "sessions.duration"){
    meta.df$weights <- 1/sqrt(meta.df$pooled.sd)
    # Specify basic plot, mapping regression.factor to the x-axis, effect size 'hedges.g' to the y-axis,
    # and 'weights' to the weight parameter.
    ggplot(meta.df, aes(x = regression.factor, y = hedges.g, size = weights)) +
      geom_point(shape = 1) + # Add scatter
      geom_abline(intercept = random.effects.model$b[1], slope = random.effects.model$b[2]) + # Add regression line
      theme_bw() + # Apply black and white theme
      theme(legend.position = "none") # Remove legend
      # see link for beatiful meta-regression with convidence intervals: https://bookdown.org/robcrystalornelas/meta-analysis_of_ecological_data/meta-regression.html#meta-regression-with-continuous-variable
  } else if (regression.factor == "sessions.frequency"){
    meta.df$weights <- 1/sqrt(meta.df$pooled.sd)
    ggplot(meta.df, aes(x = regression.factor, y = hedges.g, size = weights)) +
      geom_point(shape = 1) +
      geom_abline(intercept = random.effects.model$b[1], slope = random.effects.model$b[2]) + # Add regression line
      theme_bw() +
      theme(legend.position = "none")
  } else if (regression.factor == "programs.duration"){
    meta.df$weights <- 1/sqrt(meta.df$pooled.sd)
    ggplot(meta.df, aes(x = regression.factor, y = hedges.g, size = weights)) +
      geom_point(shape = 1) +
      geom_abline(intercept = random.effects.model$b[1], slope = random.effects.model$b[2]) + # Add regression line
      theme_bw() +
      theme(legend.position = "none")
  } else if (regression.factor == "delivery.mode"){
    meta.df$weights <- 1/sqrt(meta.df$pooled.sd)
    ggplot(meta.df, aes(x = regression.factor, y = hedges.g, size = weights)) +
      geom_point(shape = 1) +
      geom_abline(intercept = random.effects.model$b[1], slope = random.effects.model$b[2]) + # Add regression line
      theme_bw() +
      theme(legend.position = "none")
  }

  if (return.data == "descriptive"){
    return(meta.df)
  } else if (return.data == "meta results"){
    return(return.data == "results forest")
  } else if (return.data == "hedge's g"){
    return(meta.df[, "hedges.g"])
  }
}

# %% colab={"base_uri": "https://localhost:8080/", "height": 97} id="-A8_2oKhsMVZ" outputId="375f374f-a0f5-4d13-c156-2702e0c08b0c"
interventon.comparison.df.list

# %% [markdown] id="5Y7P_5jgXjRr"
# ## Get all present outcomes names

# %% colab={"base_uri": "https://localhost:8080/", "height": 725} id="tNOJnBHjXmX0" outputId="0856e22c-7bbb-4fb4-8e34-5dd67d65ade9"
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

outcomes.no.df <- data.frame(table(present.outcomes))
outcomes.no.10.plus <- as.vector(outcomes.no.df[
  outcomes.no.df$Freq >= 10, "present.outcomes"
])

present.outcomes <- unique(present.outcomes)
outcomes.no.df

# %% colab={"base_uri": "https://localhost:8080/", "height": 34} id="4bs8Fzm1QHxf" outputId="55aeca30-c528-41e8-cbd6-c52b72204c29"
outcomes.no.10.plus

# %% [markdown] id="QxzxWruAcvdB"
# ## Plots for posttest

# %% [markdown] id="4XiRpdn8l3vK"
# ### All Meditation types

# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} id="srR3cxj5rJux" outputId="cc3d6b0a-8822-42e4-e805-d08dbd5c2fb2"
print(1)
meta.results <- meta.analyze(
  "Stress", meditation.type.all, m.data.list, time.point = 2,
  double.scale = TRUE,
  preferred.scale = "Depression Anxiety and Stress Scale (DASS)",
  print.meta.results = TRUE, return.data = "meta results", print.forest = TRUE, print.funnel = TRUE
)
print(2)


# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} id="CwYMPzPdl6-R" outputId="5e16aa2c-5cde-454c-ae49-afa20f0861c0"
preferred.scales <- c("Depression Anxiety and Stress Scale (DASS)", "Perceived Stress Scale (PSS)")

meditation.type <- meditation.type.all

for (outcome in present.outcomes){
  if (outcome == "Stress"){
    print(outcome)
    print("Depression Anxiety and Stress Scale (DASS)")  
    meta.analyze(
      outcome, meditation.type, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE,
      preferred.scale = "Depression Anxiety and Stress Scale (DASS)"
    )
    cat("\n\n")
 
    meta.analyze(
      outcome, meditation.type, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE,
      preferred.scale = "Depression Anxiety and Stress Scale (DASS)"
    )
    cat("\n\n")

    print("Depression Anxiety and Stress Scale (DASS)")
    meta.analyze(
      outcome, meditation.type, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE,
      preferred.scale = "Perceived Stress Scale (PSS)"
    )
    cat("\n\n")
    print("Perceived Stress Scale (PSS)")
  } else {
    print(outcome)
    
    tryCatch(
      {
      meta.analyze(
        outcome, meditation.type, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
      )
      
          # try part
      },
      error=function(cond) {
        message(paste("Error in outome:", outcome))
        print(cond)
        return("")
        # except part for errors
        
      },
      warning=function(cond) {
        message(paste("Warning in outome:", outcome))
        print(cond)
        return("")
        # except part for warnings
        
      }
    )
    cat("\n\n")
  }
  print("#########################################")
}

# %% [markdown] id="ABPQ7D6Pakid"
# ### Attentional Family

# %% [markdown] id="pYjEsSGD0-70"
# #### Resilience Scale

# %% colab={"base_uri": "https://localhost:8080/", "height": 526} id="DFkAcg4d38O5" outputId="9094baad-9c7f-4c6d-ea3c-dee870db3936"
tryCatch(
  {
  meta.analyze(
    "Resilience Scale", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Resilience Scale"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Resilience Scale"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="e8v6rjOe1GhX"
# #### Anxiety (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="2T_B87wY4IUO" outputId="43d774e6-fe4f-4328-c70d-90773c7efb61"
tryCatch(
  {
  meta.analyze(
    "Anxiety (trait)", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (trait)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (trait)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="Q51xPYYe1KBH"
# #### Anxiety (state)

# %% colab={"base_uri": "https://localhost:8080/", "height": 506} id="Pmbhu6yD4aJ9" outputId="9fa04b21-b85a-4b18-f595-3bccd27e81a2"
tryCatch(
  {
  meta.analyze(
    "Anxiety (state)", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (state)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (state)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="H0wWgFFL1DzK"
# #### Depression (trait)

# %% colab={"base_uri": "https://localhost:8080/", "height": 857} id="YkVA1eS-4_fL" outputId="43e9fe2c-5299-4231-aa33-784a672b228d"
tryCatch(
  {
  meta.analyze(
    "Depression (trait)", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Depression (trait)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Depression (trait)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="kmtEx7Fh1O3r"
# #### Stress

# %% id="l4eaVJvtagRB"
preferred.scales <- c("Depression Anxiety and Stress Scale (DASS)", "Perceived Stress Scale (PSS)")

# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} id="5yfBZNLkp6te" outputId="a3f0c670-f9a4-413d-9b1f-422b88116d19"
meta.analyze(
  "Stress", meditation.type.all, m.data.list, time.point = 2,
  preferred.scale = preferred.scales[1],
  print.descriptive = TRUE, print.forest = TRUE, print.funnel = TRUE
)

# %% colab={"base_uri": "https://localhost:8080/", "height": 1000} id="KIMx0T3v5LT2" outputId="819ddfbc-8c22-4f63-a0ba-ce2e90f83292"
meta.analyze(
  "Stress", meditation.type.attentional, m.data.list, time.point = 2,
  preferred.scale = preferred.scales[1],
  print.descriptive = TRUE, print.forest = TRUE, print.funne = TRUE
)

# %% [markdown] id="IhttK7-b0uEC"
# #### Well-being or quality of life

# %% colab={"base_uri": "https://localhost:8080/", "height": 526} id="SDKNxmht7o02" outputId="c53f00d1-426f-4453-96fa-4625fd4667b4"
tryCatch(
  {
  meta.analyze(
    "Well-being or quality of life", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Well-being or quality of life"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Well-being or quality of life"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="0gU1K19i0qks"
# #### Acceptance

# %% colab={"base_uri": "https://localhost:8080/"} id="jTWmzeqH5yjd" outputId="b39ff08d-96aa-4d6c-a163-dc3e22699fd3"
tryCatch(
  {
  meta.analyze(
    "Acceptance", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Acceptance"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Acceptance"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="gqrJT0740brR"
# #### Empathy

# %% colab={"base_uri": "https://localhost:8080/"} id="z-Lq4ItO6Mto" outputId="0f4d1184-9344-4097-8cb9-261b57cc3af3"
tryCatch(
  {
  meta.analyze(
    "Empathy", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Empathy"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Empathy"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="PQRs58z607y7"
# #### Hope

# %% colab={"base_uri": "https://localhost:8080/"} id="wHbBmzpW6qlR" outputId="f8e1e291-1350-4900-9972-0bbade014f56"
tryCatch(
  {
  meta.analyze(
    "Hope", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Hope"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Hope"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="ioTzEW9i0iLX"
# #### Mindfulness (state)

# %% colab={"base_uri": "https://localhost:8080/"} id="CjnVuuWR64Li" outputId="da69e5fb-5a10-4e03-8416-54609be1f11a"
tryCatch(
  {
  meta.analyze(
    "Mindfulness (state)", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (state)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (state)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="-XnSButV0ywH"
# #### Mindfulness (trait)

# %% colab={"base_uri": "https://localhost:8080/"} id="K0e_hPpU6-V2" outputId="5754e691-1c05-41d7-d64c-e9d742262045"
tryCatch(
  {
  meta.analyze(
    "Mindfulness (trait)", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (trait)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (trait)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="Iz4cDns5042k"
# #### Optimism or positive attributional style

# %% colab={"base_uri": "https://localhost:8080/"} id="3dANn3JL7IAT" outputId="de7718fb-aa18-4480-ab17-916b1c1385cd"
tryCatch(
  {
  meta.analyze(
    "Optimism or positive attributional style", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Optimism or positive attributional style"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Optimism or positive attributional style"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="tie4axJh0ljV"
# #### Religiosity or spirituality or religious coping

# %% colab={"base_uri": "https://localhost:8080/"} id="BnBrb7_b7PJG" outputId="a12e6cdf-1c8b-4a31-86e3-0a0d7fda5254"
tryCatch(
  {
  meta.analyze(
    "Religiosity or spirituality or religious coping", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Religiosity or spirituality or religious coping"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Religiosity or spirituality or religious coping"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="CftTky4L1Bbg"
# #### Self-efficacy

# %% colab={"base_uri": "https://localhost:8080/"} id="ZtCxzKl57T-8" outputId="591c8d0b-7a93-4528-d6c9-db2b571707ff"
tryCatch(
  {
  meta.analyze(
    "Self-efficacy", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-efficacy"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-efficacy"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="0f_5cHOC0eqZ"
# #### Self-acceptance

# %% colab={"base_uri": "https://localhost:8080/"} id="4I3vtZgm7YHY" outputId="463dba28-2031-4b69-e96b-4b4745396be4"
tryCatch(
  {
  meta.analyze(
    "Self-acceptance", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-acceptance"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-acceptance"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="rU5h12XG0YkI"
# #### Self-esteem

# %% colab={"base_uri": "https://localhost:8080/"} id="ZncBs8NL7eIV" outputId="4f73d804-a362-4f92-b28b-34baa0e243bc"
tryCatch(
  {
  meta.analyze(
    "Self-esteem", meditation.type.attentional, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-esteem"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-esteem"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="DaoMK_039wkO"
# ### Constructive Family

# %% [markdown] id="lUI4sq51-_eL"
# #### Resilience Scale

# %% colab={"base_uri": "https://localhost:8080/"} id="I9Ml2t5B-Qci" outputId="61fd27ff-f4fd-4cde-8ef3-bba602d9a334"
tryCatch(
  {
  meta.analyze(
    "Resilience Scale", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Resilience Scale"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Resilience Scale"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="ZKrVDQ_k-Qcl"
# #### Anxiety (trait)

# %% colab={"base_uri": "https://localhost:8080/"} id="YTycod3n-Qco" outputId="c4b76753-8481-48ac-95e4-772abafb1eb3"
tryCatch(
  {
  meta.analyze(
    "Anxiety (trait)", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (trait)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (trait)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="O2Kuf8Iq-Qcq"
# #### Anxiety (state)

# %% colab={"base_uri": "https://localhost:8080/"} id="Gj6nBzF6-Qcs" outputId="c83b47cb-fdd5-460c-a900-b272f9c72444"
tryCatch(
  {
  meta.analyze(
    "Anxiety (state)", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (state)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (state)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="67WIqoUW-Qcu"
# #### Depression (trait)

# %% colab={"base_uri": "https://localhost:8080/"} id="Rcen3MPc-Qcx" outputId="2086c362-ab92-4ec6-d372-23eb8b10ebe9"
tryCatch(
  {
  meta.analyze(
    "Depression (trait)", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Depression (trait)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Depression (trait)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="I1KTzaPu-Qcz"
# #### Stress

# %% colab={"base_uri": "https://localhost:8080/", "height": 639} id="l_r60pw1-QdA" outputId="3b8aaf04-35b7-4377-ce0f-37eabff143cf"
preferred.scales <- c("Depression Anxiety and Stress Scale (DASS)", "Perceived Stress Scale (PSS)")

meta.analyze(
  "Stress", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE, preferred.scale = preferred.scales[1]
)

# %% colab={"base_uri": "https://localhost:8080/", "height": 639} id="CQS2TQ0O-QdE" outputId="f82edc7d-c523-48ec-ac75-3a098e53347c"
meta.analyze(
  "Stress", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE, preferred.scale = preferred.scales[2]
)

# %% [markdown] id="pP07uE6M-QdG"
# #### Well-being or quality of life

# %% colab={"base_uri": "https://localhost:8080/", "height": 506} id="kJ7I7aeP-QdO" outputId="6cbadb0d-90a1-4618-8e72-48208efa2fda"
tryCatch(
  {
  meta.analyze(
    "Well-being or quality of life", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Well-being or quality of life"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Well-being or quality of life"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="rZ4VaI4c-QdT"
# #### Acceptance

# %% colab={"base_uri": "https://localhost:8080/"} id="qrTr1hEb-QdU" outputId="9f8043b4-f23a-47bc-adaf-43feadc6dc2a"
tryCatch(
  {
  meta.analyze(
    "Acceptance", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Acceptance"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Acceptance"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="xeYo_d9N-QdZ"
# #### Empathy

# %% colab={"base_uri": "https://localhost:8080/"} id="naYvlsCR-Qda" outputId="e7a1f9d9-2413-4e71-cfbd-ac7d6a39feeb"
tryCatch(
  {
  meta.analyze(
    "Empathy", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Empathy"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Empathy"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="uxUFj9vM-Qdc"
# #### Hope

# %% colab={"base_uri": "https://localhost:8080/"} id="H75RkFLx-Qdd" outputId="7c558606-26c2-40d2-8bb7-a613fb1ac870"
tryCatch(
  {
  meta.analyze(
    "Hope", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Hope"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Hope"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="wATiNqep-Qde"
# #### Mindfulness (state)

# %% colab={"base_uri": "https://localhost:8080/"} id="IwsvqhUS-Qdf" outputId="357f417e-81d0-4615-9baf-021142e90ffb"
tryCatch(
  {
  meta.analyze(
    "Mindfulness (state)", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (state)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (state)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="wjMihDNn-Qdh"
# #### Mindfulness (trait)

# %% colab={"base_uri": "https://localhost:8080/"} id="00azfbdM-Qdj" outputId="7bae1e96-73ca-4861-c0fb-96c97d4de419"
tryCatch(
  {
  meta.analyze(
    "Mindfulness (trait)", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (trait)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (trait)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="YQE8M38H-Qdj"
# #### Optimism or positive attributional style

# %% colab={"base_uri": "https://localhost:8080/"} id="YQ6X8LVv-Qdl" outputId="ababb30c-1bd9-4132-8e1a-d917654ab606"
tryCatch(
  {
  meta.analyze(
    "Optimism or positive attributional style", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Optimism or positive attributional style"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Optimism or positive attributional style"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="8pTMirPp-Qdm"
# #### Religiosity or spirituality or religious coping

# %% colab={"base_uri": "https://localhost:8080/"} id="dwcj0s7F-Qdn" outputId="3bfb942a-a5fc-4d84-a58c-9f69fa95a6de"
tryCatch(
  {
  meta.analyze(
    "Religiosity or spirituality or religious coping", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Religiosity or spirituality or religious coping"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Religiosity or spirituality or religious coping"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="V6KJ62RK-Qdo"
# #### Self-efficacy

# %% colab={"base_uri": "https://localhost:8080/"} id="nfahMwfu-Qdp" outputId="4b5abde1-e439-4aa7-a48e-a43323127b05"
tryCatch(
  {
  meta.analyze(
    "Self-efficacy", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-efficacy"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-efficacy"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="kz0cZmlp-Qdq"
# #### Self-acceptance

# %% colab={"base_uri": "https://localhost:8080/"} id="EOCN9ubE-Qdr" outputId="82aec745-b933-4193-f54a-ae3ee9e672a6"
tryCatch(
  {
  meta.analyze(
    "Self-acceptance", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-acceptance"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-acceptance"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="Q9-Bfcb1-Qdr"
# #### Self-esteem

# %% colab={"base_uri": "https://localhost:8080/"} id="fM48ipLB-Qds" outputId="6b948441-a235-40c6-f3d8-e9207b62fb0a"
tryCatch(
  {
  meta.analyze(
    "Self-esteem", meditation.type.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-esteem"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-esteem"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="gtgg5E70-D_A"
# ### Attentional and Contructive Family mixed

# %% colab={"base_uri": "https://localhost:8080/", "height": 86} id="GwGAS37O-X3g" outputId="5f7a2212-6ad7-45d8-8d30-151c0bf8848e"
tryCatch(
  {
  meta.analyze(
    "Resilience Scale", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Resilience Scale"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Resilience Scale"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="IVHOVFIK-X3i"
# #### Anxiety (trait)

# %% colab={"base_uri": "https://localhost:8080/"} id="gGNHgf-4-X3j" outputId="2368c425-bd50-489d-debb-d26bb7127dff"
tryCatch(
  {
  meta.analyze(
    "Anxiety (trait)", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (trait)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (trait)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="mOqVR5ow-X3k"
# #### Anxiety (state)

# %% colab={"base_uri": "https://localhost:8080/"} id="TfuzFgIA-X3k" outputId="3b3be175-3c86-4584-af8e-5277dc5594c5"
tryCatch(
  {
  meta.analyze(
    "Anxiety (state)", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (state)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (state)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="MU3fNsal-X3l"
# #### Depression (trait)

# %% colab={"base_uri": "https://localhost:8080/"} id="htRpzPjJ-X3m" outputId="a5b8d1f3-26c9-4378-9389-9e3769af2b29"
tryCatch(
  {
  meta.analyze(
    "Depression (trait)", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Depression (trait)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Depression (trait)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="GvjvSOd--X3n"
# #### Stress

# %% colab={"base_uri": "https://localhost:8080/", "height": 639} id="Q_Qz-9Sf-X3n" outputId="fb600dee-66e4-4c98-e1f9-b41b5848db0c"
preferred.scales <- c("Depression Anxiety and Stress Scale (DASS)", "Perceived Stress Scale (PSS)")

meta.analyze(
  "Stress", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE, preferred.scale = preferred.scales[1]
)

# %% colab={"base_uri": "https://localhost:8080/", "height": 639} id="UMu7ilMy-X3o" outputId="0d7e61e1-084b-4cfd-b4f7-ac3d630b5ded"
meta.analyze(
  "Stress", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE, preferred.scale = preferred.scales[2]
)

# %% [markdown] id="oCuWmamp-X3p"
# #### Well-being or quality of life

# %% colab={"base_uri": "https://localhost:8080/"} id="5Fk8df4L-X3q" outputId="10649a44-30e1-4777-fd62-e4f3396a03e4"
tryCatch(
  {
  meta.analyze(
    "Well-being or quality of life", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Well-being or quality of life"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Well-being or quality of life"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="QPeBombl-X3r"
# #### Acceptance

# %% colab={"base_uri": "https://localhost:8080/"} id="lpy9rtrL-X3s" outputId="af4eb2fa-983f-46f1-9c69-9c1614cd2621"
tryCatch(
  {
  meta.analyze(
    "Acceptance", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Acceptance"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Acceptance"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="X1KA7PsI-X3s"
# #### Empathy

# %% colab={"base_uri": "https://localhost:8080/"} id="klBbXAQb-X3t" outputId="1d4290e1-a717-4c0a-ab83-d65f6fd08a43"
tryCatch(
  {
  meta.analyze(
    "Empathy", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Empathy"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Empathy"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="eL7iiA3L-X3u"
# #### Hope

# %% colab={"base_uri": "https://localhost:8080/", "height": 86} id="58ACb8rX-X3v" outputId="1b525570-4984-4687-a839-6b011af7298d"
tryCatch(
  {
  meta.analyze(
    "Hope", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Hope"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Hope"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="tscTfMDL-X3x"
# #### Mindfulness (state)

# %% colab={"base_uri": "https://localhost:8080/"} id="OHngZLrC-X3y" outputId="e3c3e16c-12e3-430b-c536-858fd699e233"
tryCatch(
  {
  meta.analyze(
    "Mindfulness (state)", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (state)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (state)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="GI3IimV_-X3z"
# #### Mindfulness (trait)

# %% colab={"base_uri": "https://localhost:8080/"} id="oFmpTRGh-X3z" outputId="eb68db57-da20-46eb-89fe-cb405f26f8ff"
tryCatch(
  {
  meta.analyze(
    "Mindfulness (trait)", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (trait)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (trait)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="fwC2No57-X30"
# #### Optimism or positive attributional style

# %% colab={"base_uri": "https://localhost:8080/"} id="UAOpQLX7-X31" outputId="682308ef-0e0d-40c1-8efe-2f78bfa7cef6"
tryCatch(
  {
  meta.analyze(
    "Optimism or positive attributional style", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Optimism or positive attributional style"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Optimism or positive attributional style"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="iIRaJOtn-X33"
# #### Religiosity or spirituality or religious coping

# %% colab={"base_uri": "https://localhost:8080/"} id="mA4NuofZ-X33" outputId="c02bc3f1-60f5-4dcc-a3b0-7a6043a82031"
tryCatch(
  {
  meta.analyze(
    "Religiosity or spirituality or religious coping", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Religiosity or spirituality or religious coping"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Religiosity or spirituality or religious coping"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="BhUv7mIW-X34"
# #### Self-efficacy

# %% colab={"base_uri": "https://localhost:8080/"} id="ydfQWvk_-X34" outputId="ec0c5359-37a4-4e37-e0e0-53738ece507a"
tryCatch(
  {
  meta.analyze(
    "Self-efficacy", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-efficacy"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-efficacy"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="12RJHoj5-X35"
# #### Self-acceptance

# %% colab={"base_uri": "https://localhost:8080/"} id="KrURF2sj-X36" outputId="a42ce59d-07b2-4661-c1d0-1113e0e48640"
tryCatch(
  {
  meta.analyze(
    "Self-acceptance", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-acceptance"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-acceptance"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="Sr2vERTw-X37"
# #### Self-esteem

# %% colab={"base_uri": "https://localhost:8080/"} id="n-peyxsK-X37" outputId="7eccc801-fbbf-4d79-baf0-332c2c75ffc3"
tryCatch(
  {
  meta.analyze(
    "Self-esteem", meditation.type.attentional.and.constructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-esteem"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-esteem"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="W7w_hj6WwyI4"
# ### All Families mixed 

# %% [markdown] id="iaSp_tKC8Q7v"
# #### Resilience Scale

# %% colab={"base_uri": "https://localhost:8080/"} id="nthCodWA8Q7y" outputId="111b44de-4b34-4c37-a156-f45e1ac14266"
tryCatch(
  {
  meta.analyze(
    "Resilience Scale", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Resilience Scale"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Resilience Scale"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="kuTh7rAK8Q72"
# #### Anxiety (trait)

# %% colab={"base_uri": "https://localhost:8080/"} id="8b9UOKZK8Q74" outputId="f8138b6a-af74-4c38-b351-b26d7115b992"
tryCatch(
  {
  meta.analyze(
    "Anxiety (trait)", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (trait)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (trait)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="6y-M6ogl8Q77"
# #### Anxiety (state)

# %% colab={"base_uri": "https://localhost:8080/"} id="uFtO0KBN8Q78" outputId="8dd9643f-3920-490f-8de5-6590358e8333"
tryCatch(
  {
  meta.analyze(
    "Anxiety (state)", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Anxiety (state)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Anxiety (state)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="qCfWpzc08Q8A"
# #### Depression (trait)

# %% colab={"base_uri": "https://localhost:8080/"} id="ai-IodCC8Q8B" outputId="99a42207-f65c-4066-8d93-ab6f5c5519d4"
tryCatch(
  {
  meta.analyze(
    "Depression (trait)", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Depression (trait)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Depression (trait)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="P4gp5Ebx8Q8C"
# #### Stress

# %% colab={"base_uri": "https://localhost:8080/", "height": 639} id="WIXqXudq8Q8E" outputId="b0b9bd58-adc9-4700-8b3b-47ae4467adcd"
preferred.scales <- c("Depression Anxiety and Stress Scale (DASS)", "Perceived Stress Scale (PSS)")

meta.analyze(
  "Stress", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE, preferred.scale = preferred.scales[1]
)

# %% id="ySNjAWCf8Q8H"
meta.analyze(
  "Stress", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE, preferred.scale = preferred.scales[2]
)

# %% [markdown] id="SdNxc6eD8Q8I"
# #### Well-being or quality of life

# %% id="bUPxm1Wu8Q8J"
tryCatch(
  {
  meta.analyze(
    "Well-being or quality of life", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Well-being or quality of life"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Well-being or quality of life"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="JMbgYwO58Q8M"
# #### Acceptance

# %% id="uWM4pioT8Q8N"
tryCatch(
  {
  meta.analyze(
    "Acceptance", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Acceptance"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Acceptance"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="VFe20pW48Q8O"
# #### Empathy

# %% id="vZMIoZlR8Q8P"
tryCatch(
  {
  meta.analyze(
    "Empathy", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
    # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Empathy"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Empathy"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="TXkuwh8k8Q8R"
# #### Hope

# %% id="PRpZDcfg8Q8S"
tryCatch(
  {
  meta.analyze(
    "Hope", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Hope"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Hope"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="HlhH4cdc8Q8U"
# #### Mindfulness (state)

# %% id="GyPnZc6R8Q8V"
tryCatch(
  {
  meta.analyze(
    "Mindfulness (state)", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (state)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (state)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="_EU8SuxA8Q8W"
# #### Mindfulness (trait)

# %% id="OPJprcZ78Q8X"
tryCatch(
  {
  meta.analyze(
    "Mindfulness (trait)", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Mindfulness (trait)"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Mindfulness (trait)"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="t8UuOziS8Q8Z"
# #### Optimism or positive attributional style

# %% id="Y9rna4qH8Q8a"
tryCatch(
  {
  meta.analyze(
    "Optimism or positive attributional style", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Optimism or positive attributional style"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Optimism or positive attributional style"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="-naiZped8Q8b"
# #### Religiosity or spirituality or religious coping

# %% id="i4h0pfKN8Q8d"
tryCatch(
  {
  meta.analyze(
    "Religiosity or spirituality or religious coping", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Religiosity or spirituality or religious coping"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Religiosity or spirituality or religious coping"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="-xv3bczh8Q8e"
# #### Self-efficacy

# %% id="WlH6QiCr8Q8f"
tryCatch(
  {
  meta.analyze(
    "Self-efficacy", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-efficacy"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-efficacy"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="cFjZAA3j8Q8h"
# #### Self-acceptance

# %% id="Rx2N7-kr8Q8h"
tryCatch(
  {
  meta.analyze(
    "Self-acceptance", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-acceptance"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-acceptance"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="TdS3a4Ki8Q8j"
# #### Self-esteem

# %% id="WoAZv33g8Q8k"
tryCatch(
  {
  meta.analyze(
    "Self-esteem", meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, print.forest = TRUE, print.funnel = TRUE
  )
      # try part
  },
  error=function(cond) {
    message(paste("Error in outome:", "Self-esteem"))
    print(cond)
    return("")
    # except part for errors
  },
  warning=function(cond) {
    message(paste("Warning in outome:", "Self-esteem"))
    print(cond)
    return("")
    # except part for warnings
  }
)

# %% [markdown] id="gZWt-bXKnkl0"
# ## Meta-Regression

# %% id="9Eu_3KoKSvJL"
sort(study.names)

# %% [markdown] id="5wHykeuINbe7"
# ### Session's Duration

# %% [markdown] id="aJjde5uNNfZ5"
# ### Sessions' Frequency

# %% [markdown] id="Psnjj8RROFVl"
# ### Meditation Program's Duration

# %% [markdown] id="SPc7r5wGOXvC"
# ### Delivery Mode

# %% [markdown] id="9b3oXIdXOwhd"
# ### Follow-Up Period's Duration

# %% id="haa3qZQnnjNV"

# %% [markdown] id="_H3LjqlU71Fn"
# # Old Code

# %% id="zbkRBPppUC9A"
# preferred.scales <- c("Depression Anxiety and Stress Scale (DASS)", "Perceived Stress Scale (PSS)")

# for (outcome in present.outcomes){
#   print(outcome)
#   if (outcome == "Stress"){
#     for (preferred.scale in preferred.scales){
#       print(preferred.scale)
#       meta.analyze(
#         outcome, meditation.type.attentional, m.data.list, time.point = 2, preferred.scale = preferred.scale
#       )
#       Sys.sleep(1)
#       cat("\n\n")
#     }
#   } else {
#     print(outcome)
#     tryCatch(
#       {
#       meta.analyze(
#         outcome, meditation.type.attentional, m.data.list, time.point = 2
#       )
#       Sys.sleep(1)
#           # try part
#       },
#       error=function(cond) {
#         message(paste("Error in outome:", outcome))
#         print(cond)
#         return("")
#         # except part for errors
#       },
#       warning=function(cond) {
#         message(paste("Warning in outome:", outcome))
#         print(cond)
#         return("")
#         # except part for warnings
#       }
#     )
#     cat("\n\n")
#   }
#   print("#########################################")
# }

# %% id="qf7M02WXwmIy"
# preferred.scales <- c("Depression Anxiety and Stress Scale (DASS)", "Perceived Stress Scale (PSS)")

# for (outcome in present.outcomes){
#   print(outcome)
#   if (outcome == "Stress"){
#     for (preferred.scale in preferred.scales){
#       cat(" preferred scale:", preferred.scale)
#       meta.analyze(
#         outcome, meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2, preferred.scale = preferred.scale
#       )
#       cat("\n")
#     }
#   } else {
#     print(outcome)
#     tryCatch(
#       {
#       meta.analyze(
#         outcome, meditation.type.attentional.constructive.and.deconstructive, m.data.list, time.point = 2
#       )
#           # try part
#       },
#       error=function(cond) {
#         message(paste("Error in outome:", outcome))
#         print(cond)
#         return("")
#         # except part for errors
#       },
#       warning=function(cond) {
#         message(paste("Warning in outome:", outcome))
#         print(cond)
#         return("")
#         # except part for warnings
#       }
#     )
#     cat("\n")
#   }
# }

# %% [markdown] id="lIpLLsZqPCHT"
# ### Debugging Meditation Type Filter & an preferred Scale Filter

# %% id="yzXgJwmbR-n4"
# # Get all studies with stress, their intervention names, and descriptive results to test the forest plot function
# stress.outcome.names.df <- outcome.names.df

# for (row in 1:nrow(stress.outcome.names.df)){
#   if (!("Stress" %in% stress.outcome.names.df[row,])){
#     stress.outcome.names.df <- stress.outcome.names.df[-c(row),]
#   }
# }

# # above loop does not work for every row for some reason --> repeating the loop works
# for (row in 1:nrow(stress.outcome.names.df)){
#   if (!("Stress" %in% stress.outcome.names.df[row,])){
#     stress.outcome.names.df <- stress.outcome.names.df[-c(row),]
#   }
# }

# stress.outcome.names.df

# %% id="kKWT7zwi51hn"
# outcome.measures.df.list[rownames(stress.outcome.names.df)]

# %% [markdown] id="VmrUnJPQ3s1c"
# ### Get heterogeneity of metacont() results

# %% id="lqcNfuGt3sea"
# get.heterogeneity <- function(meta.results){
#   I2.found <- FALSE
#   for (line in capture.output(meta.results)){
#     # capture.output() gets output as character vector
#     if (
#       grepl("I\\^2", line) &
#       !grepl("%", substring(sub(".*I\\^2 = ", "", line), 1, 4)) &
#       is.double(suppressWarnings(as.double(substring(sub(".*I\\^2 = ", "", line), 1, 4))))
#         # substring(sub(".*I\\^2 = ", "", line), 1, 4) returns 1st four characters after I^2
#     ){
#       # for values >= 10 % (e.g., "15.5" = 4 characters)
#       I2.found <- TRUE
#       return(
#         as.double(substring(sub(".*I\\^2 = ", "", line), 1, 4))
#       )
#     } else if (
#       grepl("I\\^2", line) &
#       is.double(suppressWarnings(as.double(substring(sub(".*I\\^2 = ", "", line), 1, 3))))
#     ){
#       # for falues < 10 %
#       I2.found <- TRUE
#       return(
#         as.double(substring(sub(".*I\\^2 = ", "", line), 1, 3))
#       )
#     } else if (
#       grepl("I\\^2", line) &
#       !is.double(suppressWarnings(as.double(substring(sub(".*I\\^2 = ", "", line), 1, 4)))) &
#       !is.double(suppressWarnings(as.double(substring(sub(".*I\\^2 = ", "", line), 1, 3))))
#     ){
#       cat(
#         "unknown heterogeneity values:", "\n",
#         substring(sub(".*I\\^2 = ", "", line), 1, 4), "\n\n"
#       )
#     }
#   }
#   if (!I2.found){
#     print("no heterogeneity found")
#   }
# }


# %% [markdown] id="-y9wk6MajJpz"
# ### Get Number of included studies from metacont() results

# %% id="o5T44xktjVVW"
# get.no.incl.studies <- function(meta.results){
#   if (is.double(as.double(substring(capture.output(meta.results)[1], 32)))){
#     return(as.double(substring(capture.output(meta.results)[1], 32)))
#   } else {
#     cat("unknown number of studies", substring(capture.output(meta.results)[1], 32))
#   }
# }


# %% [markdown] id="fz3jyxplpwFy"
# ### Get clustering combinations with lowest heterogeneity

# %% [markdown] id="WSkO_gnq_ROJ"
# #### Iterate over all possible combinations

# %% id="EzEH8LULqLgp"
# most.homogen.combs.all <- function(
#   outcome, meditation.types, m.data.list, time.point,
#   group.no, min.group.size = 2, max.hetero = 50,
#   double.scale = FALSE, preferred.scale = FALSE,
#   print.forest = FALSE, test.limit = FALSE
# ){
# # group.no is working for = 2 or 3
#   meta.results <- meta.analyze(
#     outcome, meditation.types, m.data.list, time.point,
#     return.data == "meta results"
#   )

#   no.incl.studies <- get.no.incl.studies(meta.results)
#   group.combs.df <- expand.grid(rep(
#     list(1:group.no),
#     no.incl.studies
#   ))
#     # returns data frame of all combinations
  
#   lowest.mean.hetero <- 100

#   no.combs <- nrow(group.combs.df)

#   if (test.limit != FALSE){
#     no.combs <- test.limit
#   }

#   best.group.1 <- FALSE
#   best.group.2 <- FALSE
#   best.group.3 <- FALSE

#   if (group.no == 3){
#     for (combination in 1:no.combs){
#       group.1 <- which(group.combs.df[combination,] == 1)
#       group.2 <- which(group.combs.df[combination,] == 2)
#       group.3 <- which(group.combs.df[combination,] == 3)

#       if (
#         length(group.1) >= min.group.size &
#         length(group.2) >= min.group.size &
#         length(group.3) >= min.group.size
#       ){
#         meta.results.1 <- meta.analyze(
#           outcome, meditation.types, m.data.list, time.point,
#           double.scale = double.scale, preferred.scale = preferred.scale,
#           return.data == "meta results",
#           filter.vec = group.1
#         )

#         meta.results.2 <- meta.analyze(
#           outcome, meditation.types, m.data.list, time.point,
#           double.scale = double.scale, preferred.scale = preferred.scale,
#           return.data == "meta results",
#           filter.vec = group.2
#         )

#         meta.results.3 <- meta.analyze(
#           outcome, meditation.types, m.data.list, time.point,
#           double.scale = double.scale, preferred.scale = preferred.scale,
#           return.data == "meta results",
#           filter.vec = group.3
#         )

#         hetero.1 <- get.heterogeneity(meta.results.1)
#         hetero.2 <- get.heterogeneity(meta.results.2)
#         hetero.3 <- get.heterogeneity(meta.results.3)
        
#         mean.hetero <- (hetero.1 + hetero.2 + hetero.3) / 3

#         if (
#           hetero.1 <= max.hetero &
#           hetero.2 <= max.hetero &
#           hetero.3 <= max.hetero &
#           mean.hetero < lowest.mean.hetero
#         ){
#           lowest.mean.hetero <- mean.hetero
#           best.group.1 <- group.1
#           best.group.2 <- group.2
#           best.group.3 <- group.3
#           cat(
#             "hetero.1:", hetero.1, "hetero.2:", hetero.2, "hetero.3:", hetero.3,
#             "mean:", lowest.mean.hetero, "comb.no:", combination, "\n\n"
#           )
#         }
#       }
#     }
#     if (print.forest & !FALSE %in% c(best.group.1, best.group.2, best.group.3)){
#       cat("Forest Plot Group 1")
#       meta.analyze(
#         outcome, meditation.types, m.data.list, time.point,
#         double.scale = double.scale, preferred.scale = preferred.scale,
#         print.forest = TRUE, filter.vec = best.group.1
#       )

#       cat("\n\nForest Plot Group 2")
#       meta.analyze(
#         outcome, meditation.types, m.data.list, time.point,
#         double.scale = double.scale, preferred.scale = preferred.scale,
#         print.forest = TRUE, filter.vec = best.group.2
#       )

#       cat("\n\nForest Plot Group 3")
#       meta.analyze(
#         outcome, meditation.types, m.data.list, time.point,
#         double.scale = double.scale, preferred.scale = preferred.scale,
#         print.forest = TRUE, filter.vec = best.group.3
#       )
#     }
    
#     if (FALSE %in% c(best.group.1, best.group.2, best.group.3)){
#       cat("no best combination found for I^2 <=", max.hetero)
#     }
#   } else if (group.no == 2){
#     for (combination in 1:no.combs){
#       group.1 <- which(group.combs.df[combination,] == 1)
#       group.2 <- which(group.combs.df[combination,] == 2)
      
#       if (
#         length(group.1) >= min.group.size &
#         length(group.2) >= min.group.size
#       ){
#         meta.results.1 <- meta.analyze(
#           outcome, meditation.types, m.data.list, time.point,
#           double.scale = double.scale, preferred.scale = preferred.scale,
#           return.data == "meta results",
#           filter.vec = group.1
#         )

#         meta.results.2 <- meta.analyze(
#           outcome, meditation.types, m.data.list, time.point,
#           double.scale = double.scale, preferred.scale = preferred.scale,
#           return.data == "meta results",
#           filter.vec = group.2
#         )

#         hetero.1 <- get.heterogeneity(meta.results.1)
#         hetero.2 <- get.heterogeneity(meta.results.2)

#         mean.hetero <- (hetero.1 + hetero.2) / 2

#         if (
#           hetero.1 <= max.hetero &
#           hetero.2 <= max.hetero &
#           mean.hetero < lowest.mean.hetero
#         ){
#           lowest.mean.hetero <- mean.hetero
#           best.group.1 <- group.1
#           best.group.2 <- group.2
#           cat(
#             "hetero.1:", hetero.1, "hetero.2:", hetero.2,
#             "mean:", lowest.mean.hetero, "comb.no:", combination, "\n\n"
#           )
#         }
#       }
#     }
#     if (print.forest & !FALSE %in% c(best.group.1, best.group.2)){
#       cat("Forest Plot Group 1")
#       meta.analyze(
#         outcome, meditation.types, m.data.list, time.point,
#         double.scale = double.scale, preferred.scale = preferred.scale,
#         print.forest = TRUE, filter.vec = best.group.1
#       )

#       cat("\n\nForest Plot Group 2")
#       meta.analyze(
#         outcome, meditation.types, m.data.list, time.point,
#         double.scale = double.scale, preferred.scale = preferred.scale,
#         print.forest = TRUE, filter.vec = best.group.2
#       )
#     }

#     if (FALSE %in% c(best.group.1, best.group.2)){
#       cat("no best combination found for I^2 <=", max.hetero)
#     }
#   } else {
#     print("wrong group.no; set it to 2 or 3")
#   }
# }


# %% id="-g1FfLmgxEPg"
# k <- 13
# l <- rep(list(1:3), k)
# df <- expand.grid(l)
# df <- df[1:((nrow(df) / 3)),]
# df  # <-- = all usefull combinations?

# # print(which(
# #   df[,1] == 1 &
# #   df[,2] == 1 &
# #   df[,3] == 1 &
# #   df[,4] == 1 &
# #   df[,5] == 1 &
# #   df[,6] == 1 &
# #   df[,7] == 1 &
# #   df[,8] == 1 &
# #   df[,9] == 1 &
# #   df[,10] == 1 &
# #   df[,11] == 1 &
# #   df[,12] == 1 &
# #   df[,13] == 1
# # ))

# %% [markdown] id="XKdzr1_a_XIF"
# #### Genetic algorithmys (currently only for Stress)

# %% id="3_9lI7VACi_Z"
# install.packages("genalg")
# library("genalg")

# %% [markdown] id="knQIqA6j_igw"
# ##### Funtion to optimize

# %% id="QiJ7qJOJ_aVw"
# GA.evaluate <- function(vec = c()){
#   min.group.size <- 2
#   outcome <- "Stress"
#   meditation.types <- meditation.type.all
#   m.data.list <- m.data.list
#   time.point <- 2
#   max.hetero <- 100
#   double.scale <- TRUE
#   preferred.scale <- "Depression Anxiety and Stress Scale (DASS)"
#   group.no <- 3

#   # round continuous data from input
#   vec <- round(vec, digits = 0)

#   hetero.measure <- 30.000
#     # 30.000 = max possible value (100^2 * 3)

#   group.1 <- which(vec == 1)
#   group.2 <- which(vec == 2)
#   group.3 <- which(vec == 3)

#   if (
#     length(group.1) >= min.group.size &
#     length(group.2) >= min.group.size &
#     length(group.3) >= min.group.size
#   ){
#     meta.results.1 <- meta.analyze(
#       outcome, meditation.types, m.data.list, time.point,
#       double.scale = double.scale, preferred.scale = preferred.scale,
#       return.data == "meta results",
#       filter.vec = group.1
#     )

#     meta.results.2 <- meta.analyze(
#       outcome, meditation.types, m.data.list, time.point,
#       double.scale = double.scale, preferred.scale = preferred.scale,
#       return.data == "meta results",
#       filter.vec = group.2
#     )

#     meta.results.3 <- meta.analyze(
#       outcome, meditation.types, m.data.list, time.point,
#       double.scale = double.scale, preferred.scale = preferred.scale,
#       return.data == "meta results",
#       filter.vec = group.3
#     )

#     hetero.1 <- get.heterogeneity(meta.results.1)
#     hetero.2 <- get.heterogeneity(meta.results.2)
#     hetero.3 <- get.heterogeneity(meta.results.3)
    
#     if (
#       hetero.1 <= max.hetero &
#       hetero.2 <= max.hetero &
#       hetero.3 <= max.hetero
#     ){
#       hetero.measure <- (hetero.1^2 + hetero.2^2 + hetero.3^2) / 3
#     }
#   }
#   hetero.measure
# }

# %% [markdown] id="Gb4AvgMbC4CU"
# ##### Monitoring for GA

# %% id="MlwQ2UBNC6RA"
GA.monitor <- function(obj){
  min.eval = min(obj$evaluations)
  i = obj$iters
  print(append("Lowest I^2", min.eval))
}

# %% [markdown] id="NQxS0V-JiJCv"
# ## Cluster Meditation types by stress

# %% [markdown] id="fE8a4X3lPTnF"
# ### With all poissible combinations

# %% id="96XsceHuiOwt"
# most.homogen.combs(
#   "Stress", meditation.type.all, m.data.list, time.point = 2, max.hetero = 90,
#   preferred.scale = "Depression Anxiety and Stress Scale (DASS)",
#   group.no = 3, print.forest = TRUE
# )

# %% id="_Bmkt0Jauv1z"
# most.homogen.combs(
#   "Stress", meditation.type.all, m.data.list, time.point = 2, max.hetero = 90,
#   preferred.scale = "Depression Anxiety and Stress Scale (DASS)",
#   group.no = 2, print.forest = TRUE
# )

# %% [markdown] id="qVcxWPjqPYzU"
# ### With Genetic Algorithms

# %% id="dzERmdWmDwE6"
# garesults <- rbga(
#   stringMin=c(1,1,1,1,1,1,1,1,1,1,1,1,1),
#   stringMax=c(3,3,3,3,3,3,3,3,3,3,3,3,3),
#   popSize = 10, iters = 10, evalFunc = GA.evaluate
# )

# %% id="isjClPBIjMNk"
# plot(garesults)

# %% id="yh8wTLepNuLr"
# best.index <- which.min(garesults$evaluations)
# best.genome <- garesults$population[best.index,]
# best.genome <- round(best.genome, digits = 0)
# best.genome

# %% id="Nrkk7TLWOrQW"
# best.index <- which.min(garesults$evaluations)
# best.genome <- garesults$population[best.index,]
# best.genome <- round(best.genome, digits = 0)

# min.group.size <- 2
# outcome <- "Stress"
# meditation.types <- meditation.type.all
# m.data.list <- m.data.list
# time.point <- 2
# max.hetero <- 90
# double.scale <- TRUE
# preferred.scale <- "Depression Anxiety and Stress Scale (DASS)"

# group.1 <- which(best.genome == 1)
# group.2 <- which(best.genome == 2)
# group.3 <- which(best.genome == 3)

# meta.analyze(
#   outcome, meditation.types, m.data.list, time.point,
#   double.scale = double.scale, preferred.scale = preferred.scale,
#   print.forest = TRUE,
#   filter.vec = group.1
# )

# meta.analyze(
#   outcome, meditation.types, m.data.list, time.point,
#   double.scale = double.scale, preferred.scale = preferred.scale,
#   print.forest = TRUE,
#   filter.vec = group.2
# )

# meta.analyze(
#   outcome, meditation.types, m.data.list, time.point,
#   double.scale = double.scale, preferred.scale = preferred.scale,
#   print.forest = TRUE,
#   filter.vec = group.3
# )

# %% [markdown] id="HFxhgFJrl9Pj"
# ### With K-Means Clustering

# %% id="hVIj5mC1nRuV"
# hedges.g.df <- data.frame(meta.analyze(
#   "Stress", meditation.type.all, m.data.list, time.point = 2,
#   double.scale = TRUE,
#   preferred.scale = "Depression Anxiety and Stress Scale (DASS)",
#   return.hedges.g = TRUE
# ))

# %% id="_03k1XFCn1tD"
# library(lattice)

# %% id="PCjqnm6IlKCe"
# plot(hedges.g.df)

# %% id="4vtBBkAawtIr"
# hedges.g.df$cluster = factor( kmeans(hedges.g.df, centers=3)$clust )
# mycomb <- combn(1:ncol(hedges.g.df), 2)
# for (xy in 1:45 ) {
#   plot(x=hedges.g.df[, mycomb[1,xy]], 
#        y=hedges.g.df[, mycomb[2,xy]], 
#        col=as.numeric(hedges.g.df$clust), 
#        xlab=names(hedges.g.df)[mycomb[1,xy]],
#        ylab=names(hedges.g.df)[mycomb[2,xy]])
# }

# %% id="ug-Ca0tWuwG-"
# hedges.g.df$squared <- hedges.g.df[,1]^2

# %% id="vEbtbKc-v5XF"
# plot(hedges.g.df)

# %% id="Yr1ZquXtwKup"
# hedges.g.df$cluster = factor( kmeans(hedges.g.df, centers=3)$clust )
# mycomb <- combn(1:ncol(hedges.g.df), 2)
# for (xy in 1:45 ) {
#   plot(x=hedges.g.df[, mycomb[1,xy]], 
#        y=hedges.g.df[, mycomb[2,xy]], 
#        col=as.numeric(hedges.g.df$clust), 
#        xlab=names(hedges.g.df)[mycomb[1,xy]],
#        ylab=names(hedges.g.df)[mycomb[2,xy]])
# }

# %% [markdown] id="oX7ODLX95c4Y"
# not fitting study names for stress (no suficient descriptive results (prefering PSS)):
# - Nidich 2009
# - Plummer 2018
# - Janowiak 1994
# - Messer 2016
# - Nolan 2020 (other scales res are present)
#
# fitting:
# - Ratanasiripong 2015
# - Sloan 2016
# - Barry 2019
# - Warnecke 2011
# - Weytens 2014	
# - Smith 2021
# - Siembor 2018
# - Lee 2018
# - Bultas 2021
# - Huberty 2019
# - Dorais 2021
# - Schulte-Frankenfeld 2021
#
# not in forestplot:
# None

# %% id="aoiNcDFF1mFw"
# print.array.not.na(results.descriptive.array[,,"T1",,,rownames(stress.outcome.names.df)])

# %% id="cLmynRnXcqkH"
# intervention.comparisons.df.list

# %% id="rHsMzG5Taki6"
# meta.analyze(
#   "Stress", meditation.type.all, m.data.list, time.point = "T1",
#   preferred.scale = "Perceived Stress Scale (PSS)", print.descriptive = TRUE
# )

# %% id="nB6nCBn6HKp2"
# outcome <- "Stress"
# meditation.types <- meditation.type.all
# time.point <- 2
# outcome.no <- 1
# intervention.no <- 1
# study <- which(study.names == "Lee 2018")
# scale <- 1

# %% id="94NmtYmQYh1o"
# # Check data of studys not included in forest plot in function's if statement

# if (
#   m.data.list[["outcome.names.df"]][study, sprintf("Name.of.Outcome.%d", outcome.no)] == outcome &
#   !(
#     is.na(m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"]) |
#     m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == "NA" |
#     m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == nm.placeholder
#   ) &
#   (
#     m.data.list[["meditation.techniques.df"]][study, intervention.no] %in% meditation.types |
#     ((TRUE %in% (m.data.list[["meditation.techniques.df"]][study,] %in% meditation.types)) & intervention.no == 4)
#   ) &
#   !(
#     NA %in% results.descriptive.array[intervention.no,,time.point, outcome.no, scale, study] |
#     nm.placeholder %in% results.descriptive.array[intervention.no,,time.point, outcome.no, scale, study]
#   ) &
#   !(
#     NA %in% results.descriptive.array[4,,time.point, outcome.no, scale, study] |
#     nm.placeholder %in% results.descriptive.array[4,, time.point, outcome.no, scale, study]
#   )
# ){
#   print("TRUE")
# } else {
#   print("FALSE")
# }

# %% id="Gmnf4Iw4lXRS"
# m.data.list[["outcome.names.df"]][study, sprintf("Name.of.Outcome.%d", outcome.no)] == outcome

# %% id="1WTQ-LHDVESI"
# !(
#   is.na(m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"]) |
#   m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == "NA" |
#   m.data.list[["intervention.comparisons.df.list"]][[study]][intervention.no,"Name"] == nm.placeholder
# )

# %% id="m3bZsSOaejwH"
# (
#   m.data.list[["meditation.techniques.df"]][study, intervention.no] %in% meditation.types |
#   ((TRUE %in% (m.data.list[["meditation.techniques.df"]][study,] %in% meditation.types)) & intervention.no == 4)
# )

# %% id="SPsj3wEWhJSW"
# m.data.list[["meditation.techniques.df"]][study, intervention.no] %in% meditation.types

# %% id="SSzdQ4vEhWmc"
# m.data.list[["meditation.techniques.df"]][study, intervention.no]

# %% id="7Am5Oe_Xhp4h"
# intervention.comparisons.df.list[study]

# %% id="6TsidYdShhqP"
# m.data.list[["meditation.techniques.df"]]

# %% id="Z3ag5dviey2B"
# !(
#   NA %in% results.descriptive.array[intervention.no,,time.point, outcome.no, scale, study] |
#   nm.placeholder %in% results.descriptive.array[intervention.no,,time.point, outcome.no, scale, study]
# )

  # %% id="Zm1USXRNWqjb"
  # !(
  #   NA %in% results.descriptive.array[4,,time.point, outcome.no, scale, study] |
  #   nm.placeholder %in% results.descriptive.array[4,, time.point, outcome.no, scale, study]
  # )

# %% [markdown] id="wbNsWX_iLMOP"
# # Code of Test-Meta-Analyses

# %% id="APL7Y8ojLLoR"
# library('readxl')
# # import csv file into R
# # data <- read_excel('review_47966_20220515233722.xlsx')
# # write.csv(data, file = 'review_47966_20220515233722.csv')
# data <- read.csv('review_47966_20220515233722.csv')

# # create df of all Meditation 1 studies
# data.med1 =  data[data$Practiced.Techniques.in.Intervention.1 == 'Meditation 1', ] # select only rows with meditation 1 as intervetion 1


# # pooled.sd <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n1-2))
# # mean.diff <- mean.1 - mean.2
# # cohens'd <- mean.diff/pooled.sd
# # calculate hedges g out of cohens d with hedges_g()

# # create df for meta analysis with Meditation 1
# library('esc')
# df.meta <- data.frame(
#   study.id =	data.med1[, 'Study.ID'],
#   number.meditation =	data.med1[, 'Sample.size.Intervention.1.T1'],  # number of participants in meditation condition
#   mean.meditation =	data.med1[, 'Intervention.1.Mean.T2'],
#   sd.meditation =	data.med1[, 'Intervention.1.SD.T2'],
#   number.control =	data.med1[, 'Sample.size.Intervention.2.T1'],
#   mean.conttrol	= data.med1[, 'Intervention.2.Mean.T2'],
#   sd.control	= data.med1[, 'Intervention.2.SD.T2'],
#   med.frequency = data.med1[, 'Frequency'],
#   pooled.sd = sqrt((data.med1[, 'Sample.size.Intervention.1.T1']-1)*data.med1[, 'Intervention.1.SD.T2']^2 + (data.med1[, 'Sample.size.Intervention.2.T1']-1)*data.med1[, 'Intervention.2.SD.T2']^2) / (data.med1[, 'Sample.size.Intervention.1.T1']+data.med1[, 'Sample.size.Intervention.1.T1']-2),
#   mean.diff.T2 = data.med1[, 'Intervention.1.Mean.T2']-data.med1[, 'Intervention.2.Mean.T2']
# )

# df.meta$hedges.g <- (df.meta[, "pooled.sd"] / df.meta[, "mean.diff.T2"])

# library('meta')
# res.df_meta =  metacont(number.meditation, mean.meditation, sd.meditation, 
#                         number.control, mean.conttrol, sd.control,
#                         fixed = T, random = T, studlab = study.id,
#                         data = df.meta, sm = "SMD")

# # plot forest plot
# forest(res.df_meta, leftcols = c('studlab'))

# # plot funnel plot
# funnel(res.df_meta)

# # metabias: Test for funnel plot asymmetry, based on rank correlation or linear regression method.
# metabias(res.df_meta, method.bias = 'linreg', k.min = , plotit = T)
# # The p-value is 0.973 which implies no publication bias. However, this meta-analysis contains k=5 studies. Egger’s test may lack the statistical power to detect bias when the number of studies is small (i.e., k<10).

# # conduct meta-regression, see: https://cjvanlissa.github.io/Doing-Meta-Analysis-in-R/plotting-regressions.html
# library('metafor')
# random.effects.model <- rma(
#   df.meta[, "hedges.g"],
#   df.meta[, "pooled.sd"]
# )
#   # second paramenter in rma-function = sampling variance = pooled standard deviation?

# install.packages('ggplot2')
# library(ggplot2)

# df.meta$weights <- 1/sqrt(df.meta$pooled.sd)
# # Specify basic plot, mapping med.frequency to the x-axis, effect size 'hedges.g' to the y-axis,
# # and 'weights' to the weight parameter.
# ggplot(df.meta, aes(x = med.frequency, y = hedges.g, size = weights)) +
#   geom_point(shape = 1) + # Add scatter
#   geom_abline(intercept = random.effects.model$b[1], slope = random.effects.model$b[2]) + # Add regression line
#   theme_bw() + # Apply black and white theme
#   theme(legend.position = "none") # Remove legend
#   # see link for beatiful meta-regression with convidence intervals: https://bookdown.org/robcrystalornelas/meta-analysis_of_ecological_data/meta-regression.html#meta-regression-with-continuous-variable

# # plot traffic light plot for RoB
# ## use {robvis} via the rob_traffic_light function
# ## see: https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/risk-of-bias-plots.html#traffic-light-plots

# # plot Summary Plots
# ## use use {robvis} via the rob_summary function

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
#
# - Why does forest plt function prints later / in the wrong order?
#
# - fix error for funnel plot "Number of parameters to be estimated is larger than the number of observations"?
#   - maybe due to study number (k) = 3?
#
# - check if theresa has corrected my errors I have noticed
#
# - calculate meta-regression for outcomes k <=10
#   - Set unique delivery modes
#   - make meta.analyze catch up regression factors
#
# ## Set unique delivery modes & make meta.analyze catch up regression factors

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
