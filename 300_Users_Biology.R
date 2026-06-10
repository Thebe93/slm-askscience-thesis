
rm(list=ls())

# ====== CONFIG ======
INPUT_CSV <- "C:/Users/thebe/OneDrive/Desktop/Corsi Trento/Tesi/Python/questions_with_answers_and_metadata_with_connections.csv"
OUT_DIR   <- "C:/Users/thebe/OneDrive/Desktop/Corsi Trento/Tesi/Python"
N         <- 300
SEED      <- 123

# ====== LIBRARIES======
library(readr)
library(dplyr)
library(stringr)


set.seed(SEED)

# ====== LOAD ======
dat <- read_csv(INPUT_CSV, show_col_types = FALSE)

# rename categories
dat <- dat %>% rename(
  comment_id   = coalesce(names(.)[names(.)=="id"],           "id"),
  comment_text = coalesce(names(.)[names(.)=="comment"],      "comment"),
  user         = coalesce(names(.)[names(.)=="dummy_name"],   "dummy_name")
)

# ====== FILTER only for deleted/removed ======
to_lower <- function(x) { if(is.character(x)) tolower(trimws(x)) else x }

dat <- dat %>%
  mutate(
    comment_text = if_else(is.na(comment_text), "", comment_text),
    user         = if_else(is.na(user), "", user),
    user_l       = to_lower(user),
    text_l       = to_lower(comment_text)
  ) %>%
  filter(!(text_l %in% c("[deleted]","[removed]"))) %>%   
  filter(!(user_l %in% c("[deleted]","[removed]"))) %>%  
  select(-user_l, -text_l)

dat <- dat %>% distinct(comment_id, user, .keep_all = TRUE) # 1 label per user-thread

# ====== Sampling ======
if(nrow(dat) < N){
  warning("Avaiable rows below N")
  samp <- dat
} else {
  samp <- dat %>% sample_n(N)
}

# ====== Selecting columns ======
keep_cols <- intersect(c("comment_id","user","comment_text","upvotes","karma"), names(samp))
out <- samp %>% select(all_of(keep_cols)) %>% mutate(label = "")

csv_out <- file.path(OUT_DIR, "manual_label_sample_CLEAN.csv")
readr::write_excel_csv2(out, csv_out)  


cat("nrow sample:", nrow(out), "\n")
cat("It contains '[deleted]' withing the text? ",
    any(tolower(out$comment_text) %in% c("[deleted]","[removed]")), "\n")
cat("It contains '[deleted]' within user? ",
    any(tolower(out$user) %in% c("[deleted]","[removed]")), "\n")
