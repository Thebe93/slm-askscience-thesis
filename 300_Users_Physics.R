
rm(list=ls())

# ====== CONFIG ======
INPUT_CSV <- "C:/Users/thebe/OneDrive/Desktop/Corsi Trento/Tesi/Python/questions_with_answers_and_metadata_with_connections_physics.csv"
OUT_DIR   <- "C:/Users/thebe/OneDrive/Desktop/Corsi Trento/Tesi/Python"
N         <- 300
SEED      <- 123

# ====== LIBRARIES ======
library(readr)     
library(dplyr)
library(stringr)


set.seed(SEED)

# ====== LOAD ======
dat <- readr::read_csv(INPUT_CSV, show_col_types = FALSE)

# rename categories
dat <- dat %>% rename(
  comment_id   = dplyr::coalesce(names(.)[names(.)=="id"],           "id"),
  comment_text = dplyr::coalesce(names(.)[names(.)=="comment"],      "comment"),
  user         = dplyr::coalesce(names(.)[names(.)=="dummy_name"],   "dummy_name")
)

# ====== FILTER solo deleted/removed in testo o autore ======
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

csv_out <- file.path(OUT_DIR, "manual_label_sample_CLEAN_physics.csv")
readr::write_excel_csv2(out, csv_out)  


cat("\nControlli veloci:\n")
cat("nrow campione:", nrow(out), "\n")
cat("Contiene ancora '[deleted]' nel testo? ",
    any(tolower(out$comment_text) %in% c("[deleted]","[removed]")), "\n")
cat("Contiene ancora '[deleted]' nell'user? ",
    any(tolower(out$user) %in% c("[deleted]","[removed]")), "\n")
