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
# # Preprocess Data
#

# %% vscode={"languageId": "r"}
raw.df <- read.csv("2023_12_10_Data Extraction All Data Final Export4.csv")



# %% vscode={"languageId": "r"}
# install.packages("sjmisc")
# install.packages("tidyverse")
# install.packages("abind")
library(conflicted)  # solve tidyverse package conflicts with conflict_prefer()
library(tidyverse)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library("sjmisc")
library("abind")
# for data manipulation with dplyr see: https://www.youtube.com/watch?v=Gvhkp-Yw65U
# for splitting 2 values in 1 cell see: https://www.youtube.com/watch?v=DiY8EqZDwoI at 3:17 (e.g. if 2 scales for 1 outcome)
# for joining 2 data frames see:        https://www.youtube.com/watch?v=DiY8EqZDwoI at 11:57


# %% vscode={"languageId": "r"}
raw.df

# %% [markdown]
# ## Drop unimportant columns by name

# %% [markdown]
# ### Remove column rages (first columns)

# %% vscode={"languageId": "r"}
my.df <- raw.df %>%
  select(-Lead.Author.Email.Adress:-Further.Information.inserted.in.Extraction.Form.)  # "-" indicates deleting these columns


# %% [markdown] heading_collapsed=true
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


# %% [markdown] hidden=true
#
#

# %% [markdown]
# ### assign raters df

# %% vscode={"languageId": "r"}
# my.df.RJ <- my.df %>% filter(Reviewer.Name == "Robin Jacob")
# my.df.TD <- my.df %>% filter(Reviewer.Name == "Theresa Dicks")
# my.df.PK <- my.df %>% filter(Reviewer.Name == "Pascal Kemmerer")
# my.df.cons <- my.df %>% filter(Reviewer.Name == "Consensus")

# %% [markdown]
# # Calculate inter-rater reliability

# %% [markdown]
# ### Functions from main script

# %% [markdown] heading_collapsed=true
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


# %% [markdown] heading_collapsed=true
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


# %% [markdown] heading_collapsed=true
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


# %% vscode={"languageId": "r"}
conv_val_vec <- function(vec, missing.value.placeholder = nm.placeholder, only.double = TRUE){
  
  if (!is.vector(vec)){
    stop(paste0("this function is only for vectors. got ", class(vec), "instead."))
  }

  output <- c()

  for (i in seq_along(vec)){
    out_i <- convert.value(
      vec[i], missing.value.placeholder = missing.value.placeholder, only.double = only.double
    )

    output[i] <- out_i
  }
  output
}

# %% [markdown]
# ## Operations

# %% vscode={"languageId": "r"}
# genrate df with column names of data to compare for inter-rater reliablity
ex.df <- data.frame(
  start = c(
    "Number.of.Participants.Intervention.1.T0",'Numer.of.Measuring.Time.Points', "Time.Point.0.Date", "Time.Point.0...1.Duration.in.Days",
    "Number.of.Experimental.Groups", "Name.Intervention.1", "Practiced.Techniques.in.Intervention.1", "Name.of.Outcome.1",
    "Intervention.1.Mean.O1T0", "ITT.or.Per.Protocol.Analysis.present.", "Imputation.of.missing.Data"
  ),
  end = c(
    "Number.of.Males.Over.All.T3",'Numer.of.Measuring.Time.Points', "Time.Point.3.Date", "Time.Point.0...3.Duration.in.Days",
    "Number.of.Experimental.Groups", "Total.Duration.of.Intervention.in.Days.Control.or.Intervention.4", "Practiced.Techniques.in.Control.or.Intervention.4", "Name.of.Outcome.7",
    "Control.or.Intervention.4.n.in.case.of.period.O7T3", "ITT.or.Per.Protocol.Analysis.present.", "Imputation.of.missing.Data"
  ),
  row.names = c(
    "Pop.Char", "n.t", "dates", "between.t", "n.groups", 
    "intervention.comparison", "medittation.tech", "outcomes",
    "descriptive results", "ITT or PPA", "Imputation"
  )
)
ex.df <- ex.df |>
  mutate(
    swap = case_when(rownames(ex.df) %in% c("Pop.Char", "medittation.tech", "intervention.comparison", "descriptive results") ~ T, T ~ F),
    store_type = case_when(
      rownames(ex.df) %in% c("Pop.Char", "descriptive results") ~ "array",
      rownames(ex.df) %in% c("intervention.comparison") ~ "df_list",
      T ~ "df",
    )
  )

ex.df

# %% vscode={"languageId": "r"}
# get all columns of interest in one df
df.of.interest <- my.df |> select(Covidence..:Reviewer.Name, -Title)

for (i in 1:nrow(ex.df)){
  df.of.interest <- cbind(df.of.interest, my.df |> select(!!sym(ex.df[i, "start"]):!!sym(ex.df[i, "end"])))
}
# df.of.interest <- df.of.interest |>
#   select(
#     -Short.Description.Intervention.1:-Short.Description.Control.or.Intervention.4,
#     # -Name.Intervention.1:-Name.Control.or.Intervention.4
#   )
colnames(df.of.interest |> select(-Covidence..: -Reviewer.Name))

# %% jupyter={"outputs_hidden": true} vscode={"languageId": "r"}
# generate separate df for every extractor
my.df.RJ <- df.of.interest %>% filter(Reviewer.Name == "Robin Jacob")
my.df.TD <- df.of.interest %>% filter(Reviewer.Name == "Theresa Dicks")
my.df.PK <- df.of.interest %>% filter(Reviewer.Name == "Pascal Kemmerer")
my.df.RJ; my.df.TD; my.df.PK

# %% [markdown] heading_collapsed=true
# ## RJ vs. PK

# %% vscode={"languageId": "r"}
# get an inner join of my.df.RJ and my.df.PK in wide format
join_df_wide <- inner_join(my.df.RJ, my.df.PK, by = c("Covidence..", "Study.ID")) |>
  select(Study.ID, contains("Reviewer.Name."), everything())
join_df_wide

# %% vscode={"languageId": "r"}
# get data from RJ and PK of this inner join
join_df_RJ <- join_df_wide |>
  select(Study.ID, ends_with(".x"), -Reviewer.Name.x) |>
  rename_at(vars(ends_with(".x")), ~  sub("\\.x$", ".RJ", .)) |>  # .x are RJ columns
  mutate(across(everything(), ~ if_else(str_trim(.) == "", NA, .)))

join_df_PK <- join_df_wide |>
  select(Study.ID, ends_with(".y"), -Reviewer.Name.y) |>
  rename_at(vars(ends_with(".y")), ~ sub("\\.y$", ".PK", .)) |>  # .y are PK columns
  mutate(across(everything(), ~ if_else(str_trim(.) == "", NA, .)))

join_df_RJ; join_df_PK

# %% [markdown] vscode={"languageId": "r"}
# ## Swap values in columns of interest

# %% [markdown] vscode={"languageId": "r"}
# ### Functions

# %% vscode={"languageId": "r"}
# function to reoder the rows' values based on orders_vec
swap_vals_row_iter <- function(df, orders_vec, start, end){

  df_sel <- df |> select(start:end)

  # check equal df_sel and orders_vec shape
  orders_vec_col_num <- length(str_split(orders_vec[1], "")[[1]])
  if (length(orders_vec) != nrow(df_sel) | orders_vec_col_num != ncol(df_sel))
    stop(paste0("Shape of df_sel and orders_vec is not equal; df_sel shape: ", nrow(df_sel), "/", ncol(df_sel), "; orders_vec shape: ", length(orders_vec), "/", orders_vec_col_num))

  # iterate over rows of df_sel and reorder values within each row
  for (row in seq_row(df_sel)){
    new_order <- as.numeric(str_split(orders_vec[row], "")[[1]])
    df_sel[row, ] <- df_sel[row, new_order]
  }

  # replace the reordered values with the original ones
  df |> mutate(across(all_of(start:end), ~ df_sel[[cur_column()]]))
}

# %% vscode={"languageId": "r"}
# # old
# # function to reoder the rows' values based on orders_vec
# swap_vals_row_iter <- function(df, orders_vec){

#   # check equal df and orders_vec shape
#   orders_vec_col_num <- length(str_split(orders_vec[1], "")[[1]])
#   if (length(orders_vec) != nrow(df) | orders_vec_col_num != ncol(df))
#     stop(paste0("Shape of df and orders_vec is not equal; df shape: ", nrow(df), "/", ncol(df), "; orders_vec shape: ", length(orders_vec), "/", orders_vec_col_num))

#   # iterate over rows of df and reorder values within each row
#   for (row in seq_row(df)){
#     new_order <- as.numeric(str_split(orders_vec[row], "")[[1]])
#     df[row, ] <- df[row, new_order]
#   }
#   df
# }

# %% vscode={"languageId": "r"}
# function using swap_vals_row_iter iterating over a range of columns
swap_vals_col_iter <- function(df, global_start, global_end, orders_vec, int_num = 4, inter = 1){

  # column number of one interation (number of interventions + intermediate non-intervention columns)
  rep_len <- (int_num + inter)

  # n of iterations
  n_cols <- df |> select(!!sym(global_start):!!sym(global_end)) |> ncol()
  n_iter <- n_cols / rep_len

  # iterate over columns in batches and reorder values of each row
  for (batch in 0:(n_iter - 1)){

    # column range (start to end) in which values are swapped
    start <- which(colnames(df) == global_start) + (int_num + inter) * batch
    end <- which(colnames(df) == global_start) + (int_num + inter) * batch + int_num - 1
    
    # reorder values in each row
    df <- df |>
      # select(start:end) |>
      swap_vals_row_iter(orders_vec, start, end)

    # # add intermediate column
    # if (inter != 0)
    #   df_swap <- cbind(df_swap, df_sel |> select(end + inter))

    # if (batch == 0)
    #   df_out <- df_swap
    # else
    #   df_out <- cbind(df_out, df_swap)
  }
  df
}

# %% vscode={"languageId": "r"}
# # old
# # function using swap_vals_row_iter iterating over a range of columns
# swap_vals_col_iter <- function(df, global_start, global_end, orders_vec, int_num = 4, inter = 1, offs = 0){

#   # select columns of interest
#   df_sel <- df |> select(!!sym(global_start):!!sym(global_end))

#   # column number of one interation (number of interventions + intermediate non-intervention columns)
#   rep_len <- (int_num + inter)

#   # n of iterations
#   n_cols <- (ncol(df_sel) - offs)  # n of columns - excluded columns from beginning / offset
#   n_iter <- n_cols / rep_len

#   # iterate over columns in batches and reorder values of each row
#   for (batch in 0:(n_iter - 1)){

#     # column range (start to end) in which values are swapped
#     start <- offs + (int_num + inter) * batch + 1
#     end <- offs + (int_num + inter) * batch + int_num

#     # cat(start, end, "\n")
#     # print(df_sel |>
#     #   select(start:end))
    
#     # reorder values in each row
#     df_swap <- df_sel |>
#       select(start:end) |>
#       swap_vals_row_iter(orders_vec)

#     # add intermediate column
#     if (inter != 0)
#       df_swap <- cbind(df_swap, df_sel |> select(end + inter))

#     if (batch == 0)
#       df_out <- df_swap
#     else
#       df_out <- cbind(df_out, df_swap)
#   }
#   df_out
# }

# %% vscode={"languageId": "r"}
# function using swap_vals_col_iter for ranges of columns defined in ex.df
swap_vals_all <- function(df, ex.df, orders_vec, reviewer = ".PK"){

  # iterate over rows in ex.df
  for (row in seq_row(ex.df)){

    # reoder values
    start <- paste0(ex.df[row, "start"], reviewer)
    end <- paste0(ex.df[row, "end"], reviewer)

    df <- df |> swap_vals_col_iter(start, end, orders_vec, inter = ifelse(rownames(ex.df)[row] == "Pop.Char", 1, 0))
    
    # # define output
    # if (row == 1)
    #   df_out <- df_swap

    # # add new columns to ouput
    # else
    #   df_out <- cbind(df_out, df_swap)
  }

  # # add Study.ID column
  # df_out <- df_out |>
  #   mutate(Study.ID = df$Study.ID) |>
  #   relocate(Study.ID)

  df
}

# %% vscode={"languageId": "r"}
# # old
# # function using swap_vals_col_iter for ranges of columns defined in ex.df
# swap_vals_all <- function(df, ex.df, orders_vec, reviewer = ".PK"){
  
#   # iterate over rows in ex.df
#   for (row in seq_row(ex.df)){

#     # reoder values
#     starts <- paste0(ex.df[row, "start"], reviewer)
#     ends <- paste0(ex.df[row, "end"], reviewer)

#     df_swap <- swap_vals_col_iter(df, starts, ends, orders_vec, inter = ifelse(rownames(ex.df)[row] == "Pop.Char", 1, 0))
    
#     # define output
#     if (row == 1)
#       df_out <- df_swap

#     # add new columns to ouput
#     else
#       df_out <- cbind(df_out, df_swap)
#   }

#   # add Study.ID column
#   df_out <- df_out |>
#     mutate(Study.ID = df$Study.ID) |>
#     relocate(Study.ID)

#   df_out
# }

# %% [markdown] vscode={"languageId": "r"}
# ### Operations

# %% vscode={"languageId": "r"}
orders_vec <- c(
  "1234",
  "1243",
  "1243",
  "4123",
  "1243",
  "1423",
  "2143"
)


# %% vscode={"languageId": "r"}
i <- 1
join_df_PK |> select(
  !!sym(paste0((ex.df |> filter(swap))[i,"start"], ".PK")):
  !!sym(paste0((ex.df |> filter(swap))[i,"end"], ".PK"))
)

# %% vscode={"languageId": "r"}
join_df_PK_swap <- join_df_PK |> swap_vals_all(ex.df |> filter(swap), orders_vec)

# %% [markdown] heading_collapsed=true hidden=true
# #### Clean / convert values 

# %% hidden=true vscode={"languageId": "r"}
# convert values with convert_values function according to store_type of ex.df
nm.placeholder <- -999
reviewer <- ".PK"

for (row in seq_row(ex.df)){
  start <- paste0(ex.df[row, "start"], reviewer)
  start_num <- which(colnames(join_df_PK_swap) == start)
  end <- paste0(ex.df[row, "end"], reviewer)
  end_num <- which(colnames(join_df_PK_swap) == end)

  if (ex.df[row, "store_type"] %in% c("array", "df"))
    only.double <- T
    
  else if (ex.df[row, "store_type"] == "df_list")
    only.double <- F
  else
    print("unknown case")

  join_df_PK_conv <- join_df_PK_swap |>
    mutate(across(all_of(start_num:end_num), ~ conv_val_vec(
      ., missing.value.placeholder = nm.placeholder, only.double = only.double
    )))

  join_df_RJ_conv <- join_df_RJ |>
    mutate(across(all_of(start_num:end_num), ~ conv_val_vec(
      ., missing.value.placeholder = nm.placeholder, only.double = only.double
    )))
}

# %% vscode={"languageId": "r"}
join_df_PK_conv

# %% vscode={"languageId": "r"}
join_df_RJ_conv

# %% vscode={"languageId": "r"}
sum(unlist(join_df_RJ_conv) == unlist(join_df_PK_conv), na.rm = T)

# %% vscode={"languageId": "r"}
# check quality percentage of join_df_PK and join_df_RJ
(sum(unlist(join_df_RJ_conv) == unlist(join_df_PK_conv), na.rm = T) / length(unlist(join_df_RJ_conv))) * 100

# %% [markdown]
# # Find out if groups are swapped in data set (yes, they are)

# %% vscode={"languageId": "r"}
my.df.RJ.all <- my.df %>% filter(Reviewer.Name == "Robin Jacob" & Study.ID %in% common.ID.RJ.PK)
my.df.PK.all <- my.df %>% filter(Reviewer.Name == "Pascal Kemmerer" & Study.ID %in% common.ID.RJ.PK)

# %% vscode={"languageId": "r"}
dims <- c(4, 7)

study.names <- my.df.PK.all[,"Study.ID"]
study.no <- length(study.names)

print(study.names)

dimname.list <- list(c("Intervention.1", "Intervention.2", "Intervention.3", "Control"),
                     c("Name", "Short.Description", "Delivery.Mode", "Meditation.App", "Sessions.Duration.in.minutes",
                       "Frequency.in.times.per.week", "Total.Duration.in.Days")
                     )
start <- "Name.Intervention.1"
end <- "Total.Duration.of.Intervention.in.Days.Control.or.Intervention.4"

intervention.comparisons.df.list.PK <- clean.data.to.df.list.swap(
 my.df.PK.all, start, end, dims, study.names, dimname.list
)

intervention.comparisons.df.list.PK


# %% vscode={"languageId": "r"}
dims <- c(4, 7)

study.names <- my.df.RJ.all[,"Study.ID"]
study.no <- length(study.names)

dimname.list <- list(c("Intervention.1", "Intervention.2", "Intervention.3", "Control"),
                     c("Name", "Short.Description", "Delivery.Mode", "Meditation.App", "Sessions.Duration.in.minutes",
                       "Frequency.in.times.per.week", "Total.Duration.in.Days")
                     )
start <- "Name.Intervention.1"
end <- "Total.Duration.of.Intervention.in.Days.Control.or.Intervention.4"

intervention.comparisons.df.list.RJ <- clean.data.to.df.list.swap(
 my.df.RJ.all, start, end, dims, study.names, dimname.list
)

intervention.comparisons.df.list.RJ


# %% vscode={"languageId": "r"}
comp.list <- list()
i <- 1
for (study.name in common.ID.RJ.PK){
  RJ.df.comp <- intervention.comparisons.df.list.RJ[[study.name]]$Name
  PK.df.comp <- intervention.comparisons.df.list.PK[[study.name]]$Name
  comp.list[[i]] <- cbind(RJ.df.comp, PK.df.comp)
  i <- i + 1
}
names(comp.list) <- common.ID.RJ.PK
comp.list
