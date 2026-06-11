# ============================
# Reddit AskScience — BIOLOGY
# Task: User-in-thread classification (expert vs non-expert)
# Approach: Supervised + semi-supervised (self-training)
# Anti-leakage: user-level split (no user appears in both train and test)
# ============================

rm(list=ls())
# ---- Datasets abd configurations ----
setwd("C:\\Users\\thebe\\OneDrive\\Desktop\\Corsi Trento\\Tesi\\Python")
FLAIR_DATA <- "C:/Users/thebe/OneDrive/Desktop/Corsi Trento/Tesi/Python/Data/Flair_Data.csv"
INPUT_BIOLOGY_CSV <- "C:/Users/thebe/OneDrive/Desktop/Corsi Trento/Tesi/Python/questions_with_answers_and_metadata_with_connections_biology.csv"
DICT_BIOLOGY      <- "C:/Users/thebe/OneDrive/Desktop/Corsi Trento/Tesi/Python/Data/biology_terms.csv"
LABEL_XLSX        <- "C:/Users/thebe/OneDrive/Desktop/Corsi Trento/Tesi/Python/manual_label_sample_CLEAN_biology.xlsx"
OUT_DIR           <- "C:\\Users\\thebe\\OneDrive\\Desktop\\Corsi Trento\\Tesi\\Python\\Data\\output_models_bio"
SVD_COMPONENTS    <- 40
SEED              <- 123

# ---- Libraries ----
suppressPackageStartupMessages({
  library(tidymodels)
  library(stopwords)
  library(quanteda)
  library(quanteda.textstats)
  library(text2vec)
  library(sentimentr)
  library(igraph)
  library(readxl)
  library(readr)
  library(lubridate)
  library(scales)
  library(irlba)
  library(entropy)
  library(stringr)
  library(gt)
  library(webshot2)
  library(doParallel)
})

set.seed(SEED)
if(!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# Select two cores for running the models
cl <- makeCluster(3)
registerDoParallel(cl)

# ---- Shortcuts and utility functions ----
safe_read_csv <- function(path){
  if(!file.exists(path)) stop("File not found: ", path)
  readr::read_csv(path, show_col_types = FALSE)
}

load_dict <- function(path){
  if(is.null(path) || !file.exists(path)) return(character(0))
  d <- readr::read_csv(path, col_names = FALSE, show_col_types = FALSE) %>% pull(1)
  d <- d[!is.na(d)]
  unique(tolower(d))
}

char_entropy <- function(txt){
  if(is.na(txt) || txt == "") return(0)
  chars <- strsplit(txt, "")[[1]]
  probs <- table(chars)/length(chars)
  entropy::entropy(probs, unit = "log2")
}

has_formula <- function(txt){
  if(is.na(txt) || txt == "") return(0)
  as.integer(str_detect(txt, "\\\\(|\\\\)|\\\\frac|\\\\sum|\\^|[=<>]|\\$"))
}

has_reference <- function(txt){
  if(is.na(txt) || txt == "") return(0)
  as.integer(str_detect(txt, "doi\\.|doi.org|arxiv.org|pmid|pubmed|nature.com|science.org"))
}

nzchar_safe <- function(x) !is.na(x) & x != ""

# ============ 1) Load & basic cleaning ============

# Specified all the variables in the columns in order to avoid error of parsing,
# especially for "distinguished"
data_raw_biology <- readr::read_csv(
  INPUT_BIOLOGY_CSV,
  col_types = cols(
    id                = col_character(),
    comment           = col_character(),
    upvotes           = col_double(),
    dummy_name        = col_character(),
    karma             = col_double(),
    account_age_days  = col_double(),
    time              = col_datetime(format = ""),
    distinguished     = col_character(),
    edited            = col_character(),
    replied_to        = col_character()
  )
)

# Add the text of each question to biology dataset
ids_raw <- data_raw_biology %>%
  distinct(id) %>%
  pull()

flair <- safe_read_csv(FLAIR_DATA)

flair_filtered <- flair %>%
  filter(id %in% ids_raw)

flair_questions <- flair_filtered %>%
  dplyr::select(id, question)

data_raw_with_question <- data_raw_biology %>%
  left_join(flair_questions, by = "id")

# Rearrange the structure of the dataset
data_cleaned_biology <- data_raw_with_question %>%
  rename(
    question_id      = id,
    comment_text     = comment,
    user             = dummy_name,
    comment_time     = time
  ) %>%
  filter(!(comment_text %in% c("[removed]", "[deleted]"))) %>%
  mutate(
    comment_time = gsub("(\\+|\\-)(\\d{2}):(\\d{2})", "\\1\\2\\3", comment_time),
    comment_time = suppressWarnings(lubridate::ymd_hms(comment_time, tz = "UTC", quiet = TRUE))
  )


ANON <- c("", "anonymous", "[deleted]", "[removed]")

data_cleaned_biology <- data_cleaned_biology %>%
  mutate(user = tolower(trimws(coalesce(user, ""))),
         replied_to = tolower(trimws(coalesce(replied_to, "")))) %>%
  mutate(user = if_else(user %in% ANON, "anonymous_user", user),
         replied_to = if_else(replied_to %in% c(ANON, "thread"), NA_character_, replied_to))


# 1.1 identify AMA questions
data_cleaned_biology <- data_cleaned_biology %>%
  mutate(
    is_AMA_thread = ifelse(
      str_detect(question, stringr::fixed("AskScience AMA Series:", ignore_case = TRUE)),
      1, 0
    )
  )

# threads list 
ama_threads <- data_cleaned_biology %>%
  filter(is_AMA_thread == 1) %>%
  distinct(question_id) %>%
  pull()

# AMA expert function
identify_AMA_expert <- function(df_thread) {
  
  df_thread <- df_thread %>%
    mutate(
      is_top_level = ifelse(
        is.na(replied_to) | replied_to %in% c("", "thread"),
        1, 0
      ),
      comment_time = lubridate::as_datetime(comment_time)
    )
  
  activity <- df_thread %>%
    group_by(user_norm = tolower(trimws(user))) %>%
    summarise(
      answers = n(),
      top_level_answers = sum(is_top_level),
      first_answer_time = min(comment_time, na.rm = TRUE),
      .groups = "drop"
    )
  
  ama_expert <- activity %>%
    arrange(desc(top_level_answers), desc(answers), first_answer_time) %>%
    slice(1) %>%
    pull(user_norm)
  
  return(ama_expert)
}

# Identify AMA experts in each thread
AMA_experts <- lapply(ama_threads, function(qid) {
  df_thread <- data_cleaned_biology %>% filter(question_id == qid)
  expert_user <- identify_AMA_expert(df_thread)
  tibble(
    question_id = qid,
    ama_expert_user = expert_user
  )
}) %>%
  bind_rows()

# Add AMA experts in the dataset
data_cleaned_biology <- data_cleaned_biology %>%
  mutate(user_norm = tolower(trimws(user))) %>%
  left_join(AMA_experts, by = "question_id") %>%
  mutate(
    is_AMA_expert_user = ifelse(user_norm == ama_expert_user, 1, 0)
  )

# 1.2 upload labeled sample

labels_raw <- readxl::read_excel(LABEL_XLSX, sheet = 1)

need_cols <- c("question_id","user","label")
if (!all(need_cols %in% names(labels_raw))) {
  stop("Labeled file must contain: question_id, user, label")
}

labels_bio <- labels_raw %>%
  transmute(
    question_id = as.character(question_id),
    user_norm   = tolower(trimws(as.character(user))),
    label_raw   = trimws(as.character(label))
  ) %>%
  mutate(
    label = case_when(
      label_raw %in% c("1","expert","Expert") ~ "expert",
      label_raw %in% c("0","non-expert","nonexpert","Non-expert","Non expert") ~ "non-expert",
      TRUE ~ NA_character_
    )
  ) %>%
  distinct(question_id, user_norm, .keep_all = TRUE)

# All the labeled users
labeled_users <- labels_bio %>% distinct(user_norm) %>% pull()

# Split 70/30 only for labeled users
set.seed(123)
labeled_split <- initial_split(
  tibble(user_norm = labeled_users),
  prop = 0.7
)

labeled_train_users <- training(labeled_split)$user_norm
labeled_test_users  <- testing(labeled_split)$user_norm

# ============ 2) User split (anti-leakage) ============

# 2.1) All users in dataset
users <- unique(data_cleaned_biology$user)
users_norm <- tolower(trimws(users))

# 2.2) Detect which dataset users correspond to labeled users
matched_labeled_users <- users[match(labeled_users, users_norm)]

# 2.3) Unlabeled users (to be split 80/20)
unlabeled_users <- setdiff(users, matched_labeled_users)

# 2.4) Split ONLY unlabeled users 80/20
set.seed(123)
unlabeled_split <- initial_split(
  tibble(user = unlabeled_users),
  prop = 0.8
)

unl_train_users <- training(unlabeled_split)$user
unl_test_users  <- testing(unlabeled_split)$user

# 2.5) Combine:
train_users <- union(unl_train_users, matched_labeled_users[match(labeled_train_users, labeled_users)])
test_users  <- union(unl_test_users,  matched_labeled_users[match(labeled_test_users, labeled_users)])

# 2.6) Derive train/test datasets
bio_train0 <- data_cleaned_biology %>% filter(user %in% train_users)
bio_test0  <- data_cleaned_biology %>% filter(user %in% test_users)

# ============ 3) Feature engineering (comment-level) ============
fe_comment <- function(df){
  
  # --- 3.1) Cleaning each comment ---
  df <- df %>%
    mutate(
      comment_text_raw = coalesce(comment_text, ""),
      comment_text_clean = comment_text_raw %>%
        str_remove_all("http\\S+") %>%               
        str_replace_all("[[:punct:]]", " ") %>%      
        str_squish() %>%
        tolower()
    )
  
  # --- 3.2) Basic features to retrieve from comments ---
  out <- df %>%
    mutate(
      n_chars_clean   = nchar(comment_text_clean),
      n_words         = if_else(n_chars_clean > 0, str_count(comment_text_clean, "\\S+"), 0L),
      avg_word_length = if_else(n_words > 0, n_chars_clean / n_words, 0),
      has_link        = as.integer(str_detect(comment_text_raw, "http[s]?://")),
      has_any_number  = as.integer(str_detect(comment_text_raw, "\\d")),
      has_scientific_number = as.integer(str_detect(comment_text_raw, "\\d+\\.\\d+|\\d+e[+-]?\\d+")),
      
      # scientific cues
      has_formula   = vapply(comment_text_raw, has_formula,   integer(1)),
      has_reference = vapply(comment_text_raw, has_reference, integer(1)),
      entropy       = vapply(comment_text_clean, char_entropy, numeric(1))
    )
  
  # --- 3.3) Readability  ---
  corp <- quanteda::corpus(out, text_field = "comment_text_clean")
  rb <- quanteda.textstats::textstat_readability(
    corp, measure = c("Flesch.Kincaid","SMOG","Flesch")
  ) %>% as_tibble()
  
  out <- bind_cols(out, rb)
  
  # --- 3.4) Sentiment (one value per comment row) ---
  # sentiment() returns sentence-level with element_id = index of original vector element
  s_df <- sentimentr::sentiment(out$comment_text_clean)
  
  s_agg <- s_df %>%
    dplyr::group_by(element_id) %>%
    dplyr::summarise(sentiment = mean(sentiment, na.rm = TRUE), .groups = "drop")
  
  out$sentiment <- s_agg$sentiment[match(seq_len(nrow(out)), s_agg$element_id)]
  out$sentiment <- tidyr::replace_na(out$sentiment, 0)
  
  return(out)
}

bio_train1 <- fe_comment(bio_train0)
bio_test1  <- fe_comment(bio_test0)

# ============ 4) Dictionary Biology ============
bio_terms <- load_dict(DICT_BIOLOGY)
dict_bio  <- quanteda::dictionary(list(biology = bio_terms))

add_dict_counts_bio <- function(df){
  
  # Tokens on cleaned text
  toks <- quanteda::tokens(df$comment_text_clean, remove_punct = TRUE, remove_symbols = TRUE)
  dfm  <- quanteda::dfm(toks)
  
  # Check if dictionary is empty
  if (length(bio_terms) == 0 || quanteda::ndoc(dfm) == 0) {
    out <- df %>%
      mutate(
        dict_biology    = 0L,
        biology_density = 0
      )
    return(out)
  }
  
  dfmL <- quanteda::dfm_lookup(dfm, dict_bio, valuetype = "fixed", case_insensitive = TRUE)
  dmat <- quanteda::convert(dfmL, to = "data.frame") %>% dplyr::select(-doc_id)
  
  # Check if quanteda find words within the dictionary
  if (ncol(dmat) == 0) {
    out <- df %>%
      mutate(
        dict_biology    = 0L,
        biology_density = 0
      )
    return(out)
  }
  
  # Measuring density of biology words
  colnames(dmat) <- "dict_biology"
  out <- dplyr::bind_cols(df, dmat) %>%
    mutate(
      dict_biology    = tidyr::replace_na(dict_biology, 0L),
      biology_density = dplyr::if_else(n_words > 0, dict_biology / n_words, 0)
    )
  return(out)
}

# Add to datasets
bio_train2 <- add_dict_counts_bio(bio_train1)
bio_test2  <- add_dict_counts_bio(bio_test1)

# ============ 5) TF-IDF + SVD (Latent Semantic Analysis) ============

# Apply to the Train sample
it_train <- itoken(
  bio_train2$comment_text_clean,
  preprocessor = identity,
  tokenizer = word_tokenizer,
  progressbar = FALSE
)

vocab <- create_vocabulary(
  it_train,
  stopwords = stopwords::stopwords("en")
)

vocab <- prune_vocabulary(
  vocab,
  term_count_min = 5,
  doc_proportion_max = 0.7
)

vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
tfidf_tr  <- TfIdf$new()
dtm_train_tfidf <- tfidf_tr$fit_transform(dtm_train)

svd_k <- min(SVD_COMPONENTS, max(2, ncol(dtm_train_tfidf) - 1))
if (ncol(dtm_train_tfidf) >= 3) {
  svd_res <- irlba::irlba(dtm_train_tfidf, nv = svd_k)
  doc_emb_train <- dtm_train_tfidf %*% svd_res$v
  colnames(doc_emb_train) <- paste0("svd_", seq_len(ncol(doc_emb_train)))
  bio_train3 <- bind_cols(bio_train2, as_tibble(as.matrix(doc_emb_train)))
} else {
  warning("Small vocabolary for SVD: pass SVD")
  svd_res <- NULL
  bio_train3 <- bio_train2
}

# Apply to the Test sample
embed_new <- function(text_vec_clean){
  it  <- itoken(text_vec_clean, preprocessor = identity, tokenizer = word_tokenizer, progressbar = FALSE)
  dtm <- create_dtm(it, vectorizer)
  dtm_tfidf <- tfidf_tr$transform(dtm)
  if (!is.null(svd_res)) {
    emb <- dtm_tfidf %*% svd_res$v
    colnames(emb) <- paste0("svd_", seq_len(ncol(emb)))
    as_tibble(as.matrix(emb))
  } else {
    tibble()
  }
}

bio_test3 <- bind_cols(bio_test2, embed_new(bio_test2$comment_text_clean))

# --- 6) SET UP THE TRAIN/TEST SAMPLES
bio_train3_all <- bio_train3 %>%
  mutate(
    question_id = as.character(question_id),
    user_norm   = tolower(trimws(coalesce(as.character(user), "")))
  ) %>%
  group_by(question_id, user_norm) %>%
  arrange(comment_time, .by_group = TRUE) %>%
  mutate(
    q_user_rank = row_number(),
    is_early_responder = as.integer(q_user_rank == 1)
  ) %>%
  ungroup()

bio_test3_all <- bio_test3 %>%
  mutate(
    question_id = as.character(question_id),
    user_norm   = tolower(trimws(coalesce(as.character(user), "")))
  ) %>%
  group_by(question_id, user_norm) %>%
  arrange(comment_time, .by_group = TRUE) %>%
  mutate(
    q_user_rank = row_number(),
    is_early_responder = as.integer(q_user_rank == 1)
  ) %>%
  ungroup()

# I assign each user's label only to their first comment in each thread (q_user_rank==1)
# This avoids multiple correlated labels per user-in-thread and prevents leakage within thread

# TRAIN
bio_train4_all <- bio_train3_all %>%
  mutate(
    question_id = as.character(question_id),
    user_norm   = tolower(trimws(coalesce(as.character(user), ""))),
    anonymous = as.integer(user == "anonymous_user")
  ) %>%
  dplyr::select(-any_of("label")) %>%
  left_join(labels_bio, by = c("question_id","user_norm")) %>%
  mutate(
    label = ifelse(q_user_rank == 1, as.character(label), NA_character_),
    label = factor(label, levels = c("non-expert","expert")),
    label_raw = ifelse(q_user_rank == 1, label_raw, NA_character_)
  )

# TEST 
bio_test4_all <- bio_test3_all %>%
  mutate(
    question_id = as.character(question_id),
    user_norm   = tolower(trimws(coalesce(as.character(user), ""))),
    anonymous = as.integer(user == "anonymous_user")
  ) %>%
  dplyr::select(-any_of("label")) %>%
  left_join(labels_bio, by = c("question_id","user_norm")) %>%
  mutate(
    label = ifelse(q_user_rank == 1, as.character(label), NA_character_),
    label = factor(label, levels = c("non-expert","expert")),
    label_raw = ifelse(q_user_rank == 1, label_raw, NA_character_)
    
  )

# Subset labeled sample
bio_test4_labeled <- bio_test4_all %>% filter(!is.na(label))

# ============================================ #
#            NETWORK FEATURES                  #
# ============================================ #

# Network metrics (indegree, pagerank, betweenness, embeddedness)
# are computed separately for TRAIN and TEST to prevent information leakage.
# This implies that absolute scales differ across splits.

# ============================================
# 7. REPLY GRAPH FEATURES: pagerank / indegree / outdegree
# ============================================

# 7.1) Set up the edges
prep_reply_edges <- function(df) {
  df %>%
    transmute(
      from = user,
      to   = replied_to
    ) %>%
    filter(!is.na(from), nzchar(from)) %>%
    filter(from != "anonymous_user") %>%
    filter(is.na(to) | to != "anonymous_user")
}


# 7.2) Outdegree, Indegree/Pagerank

# Measuring outdegree including anonymous
compute_reply_metrics <- function(edges_df) {
  edges_clean <- edges_df %>%
    filter(!is.na(to) & nzchar(to) & from != to) %>%
    filter(from != "anonymous_user", to != "anonymous_user")
  
  # Outdegree only with valid observations
  outdeg <- edges_clean %>%
    count(user = from, name = "outdegree")
  
  if (nrow(edges_clean) == 0) {
    return(list(
      outdeg = tibble(user = character(), outdegree = integer()),
      indeg  = tibble(user = character(), indegree = integer()),
      pr     = tibble(user = character(), pagerank = numeric())
    ))
  }
  
  # Measuring indegree/pagerank selecting only valid observations
  g <- igraph::graph_from_data_frame(d = dplyr::select(edges_clean, from, to), directed = TRUE)
  
  indeg_tbl <- tibble(
    user     = igraph::V(g)$name,
    indegree = as.integer(igraph::degree(g, mode = "in"))
  )
  
  pr_vec <- igraph::page_rank(g, directed = TRUE)$vector
  pr_tbl <- tibble(user = names(pr_vec), pagerank = as.numeric(pr_vec))
  
  list(outdeg = outdeg, indeg = indeg_tbl, pr = pr_tbl)
}

# Join the metrics with data sets
attach_reply_metrics <- function(df, metrics) {
  df %>%
    mutate(user_norm = tolower(trimws(coalesce(as.character(user), "")))) %>%
    left_join(metrics$outdeg, by = c("user_norm" = "user")) %>%
    left_join(metrics$indeg,  by = c("user_norm" = "user")) %>%
    left_join(metrics$pr,     by = c("user_norm" = "user")) %>%
    mutate(
      outdegree = tidyr::replace_na(outdegree, 0L),
      indegree  = tidyr::replace_na(indegree,  0L),
      pagerank  = tidyr::replace_na(pagerank,  0)
    )
}

# --- Executing the prepared function for both TRAIN AND TEST ---
edges_train <- prep_reply_edges(bio_train4_all)
edges_test  <- prep_reply_edges(bio_test4_all)

m_train <- compute_reply_metrics(edges_train)
m_test  <- compute_reply_metrics(edges_test)

bio_train4 <- attach_reply_metrics(bio_train4_all, m_train)
bio_test4  <- attach_reply_metrics(bio_test4_all,  m_test)

# 7.3) USER FEATURES WITHIN EACH THREAD/NETWORK

augment_with_user_net <- function(bio_train4_all, bio_test4_all) {
  
  # --- Normalize
  prep <- function(df){
    df %>%
      dplyr::mutate(
        user         = tolower(trimws(dplyr::coalesce(as.character(user), ""))),
        question_id  = as.character(question_id),
        upvotes      = suppressWarnings(as.numeric(upvotes)),
        comment_time = suppressWarnings(lubridate::as_datetime(comment_time))
      )
  }
  
  bio_train4_all <- prep(bio_train4_all)
  bio_test4_all  <- prep(bio_test4_all)
  
  # ===========================
  # 7.3.1) USER AGG (train / test)
  # ===========================
  compute_user_agg <- function(df){
    df %>%
      dplyr::group_by(user) %>%
      dplyr::summarise(
        n_comments  = dplyr::n(),
        sum_upvotes = sum(upvotes, na.rm = TRUE),
        avg_upvotes = mean(upvotes, na.rm = TRUE),
        first_seen  = suppressWarnings(min(comment_time, na.rm = TRUE)),
        last_seen   = suppressWarnings(max(comment_time, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        active_span_days = as.numeric(difftime(last_seen, first_seen, units = "days"))
      )
  }
  
  user_agg_train <- compute_user_agg(bio_train4_all)
  user_agg_test  <- compute_user_agg(bio_test4_all)
  
  # ==============================
  # 7.3.2) SOCIAL PARTICIPATION (train/test)
  # ==============================
  compute_user_social <- function(df){
    df %>%
      dplyr::group_by(user) %>%
      dplyr::summarise(
        n_threads = dplyr::n_distinct(question_id),
        n_comments_user = dplyr::n(),
        avg_comments_per_thread = n_comments_user / n_threads,
        threads_multi = sum(table(question_id) > 1),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        n_threads_log = log1p(n_threads),
        avg_comments_per_thread_log = log1p(avg_comments_per_thread)
      )
  }
  
  user_social_train <- compute_user_social(bio_train4_all)
  user_social_test  <- compute_user_social(bio_test4_all)
  
  # ==============================
  # 7.3.3) U–Q NETWORK (train/test)
  # ==============================
  compute_uq_net <- function(df){
    inc <- df %>%
      dplyr::filter(nzchar(question_id), nzchar(user)) %>%
      dplyr::distinct(question_id, user)
    inc <- inc %>% filter(user != "anonymous_user")
    
    
    if (nrow(inc) == 0)
      return(tibble::tibble(user = unique(df$user), uq_pagerank = 0, uq_degree = 0L))
    
    qs_keep <- inc %>% dplyr::count(question_id) %>% dplyr::filter(n >= 2)
    inc2 <- inc %>% dplyr::semi_join(qs_keep, by = "question_id")
    if (nrow(inc2) == 0)
      return(tibble::tibble(user = unique(df$user), uq_pagerank = 0, uq_degree = 0L))
    
    U <- sort(unique(inc2$user))
    Q <- sort(unique(inc2$question_id))
    edges_bip <- tibble::tibble(from = paste0("u:", inc2$user),
                                to   = paste0("q:", inc2$question_id))
    verts <- tibble::tibble(
      name = c(paste0("u:", U), paste0("q:", Q)),
      type = c(rep(TRUE, length(U)), rep(FALSE, length(Q)))
    )
    
    g_bip <- igraph::graph_from_data_frame(edges_bip, directed = FALSE, vertices = verts)
    g_u   <- igraph::bipartite_projection(g_bip, which = "true")
    if (igraph::vcount(g_u) == 0)
      return(tibble::tibble(user = unique(df$user), uq_pagerank = 0, uq_degree = 0L))
    
    pr  <- igraph::page_rank(g_u, directed = FALSE)$vector
    deg <- igraph::degree(g_u, mode = "all")
    uq <- tibble::tibble(
      user        = sub("^u:", "", names(pr)),
      uq_pagerank = as.numeric(pr),
      uq_degree   = as.integer(deg[names(pr)])
    )
    missing_users <- setdiff(unique(df$user), uq$user)
    
    if (length(missing_users) > 0) {
      missing_tbl <- tibble::tibble(
        user        = missing_users,
        uq_pagerank = 0,
        uq_degree   = 0L
      )
      uq <- dplyr::bind_rows(uq, missing_tbl)
    }
    
    uq
    
  }
  
  uq_train <- compute_uq_net(bio_train4_all)
  uq_test  <- compute_uq_net(bio_test4_all)
  
  # ==============================
  # 7.3.4) REPLY GRAPH
  # ==============================
  prep_edges <- function(df){
    df %>%
      transmute(from = user, to = replied_to) %>%
      filter(!is.na(from) & nzchar(from)) %>%
      filter(from != "anonymous_user") %>%
      filter(is.na(to) | to != "anonymous_user")
  }
  
  
  reply_metrics <- function(edges_df){
    edges_clean <- edges_df %>%
      filter(!is.na(to) & nzchar(to) & from != to) %>%
      filter(from != "anonymous_user", to != "anonymous_user")
    
    
    
    outdeg <- edges_clean %>%
      dplyr::count(user = from, name = "rp_outdegree")
    
    if (nrow(edges_clean) == 0) {
      return(list(
        outdeg = tibble::tibble(user = character(), rp_outdegree = integer()),
        indeg  = tibble::tibble(user = character(), rp_indegree = integer()),
        pr     = tibble::tibble(user = character(), rp_pagerank = numeric())
      ))
    }
    
    g <- igraph::graph_from_data_frame(dplyr::select(edges_clean, from, to), directed = TRUE)
    indeg_tbl <- tibble::tibble(
      user        = igraph::V(g)$name,
      rp_indegree = as.integer(igraph::degree(g, mode = "in"))
    )
    pr_vec <- igraph::page_rank(g, directed = TRUE)$vector
    pr_tbl <- tibble::tibble(user = names(pr_vec), rp_pagerank = as.numeric(pr_vec))
    list(outdeg = outdeg, indeg = indeg_tbl, pr = pr_tbl)
  }
  
  edges_train <- prep_edges(bio_train4_all)
  edges_test  <- prep_edges(bio_test4_all)
  rp_train <- reply_metrics(edges_train)
  rp_test  <- reply_metrics(edges_test)
  
  # ==============================
  # 7.4) ENRICH (train/test)
  # ==============================
  enrich_split <- function(df, rp_metrics, user_agg_split, user_social_split, uq_split){
    df %>%
      dplyr::left_join(user_agg_split,   by = "user") %>%
      dplyr::left_join(user_social_split,by = "user") %>% 
      dplyr::left_join(uq_split,         by = "user") %>%
      dplyr::left_join(rp_metrics$outdeg, by = dplyr::join_by(user == user)) %>%
      dplyr::left_join(rp_metrics$indeg,  by = dplyr::join_by(user == user)) %>%
      dplyr::left_join(rp_metrics$pr,     by = dplyr::join_by(user == user)) %>%
      dplyr::mutate(
        n_comments   = ifelse(is.na(n_comments), 1L, n_comments),
        sum_upvotes  = ifelse(is.na(sum_upvotes), upvotes, sum_upvotes),
        avg_upvotes  = ifelse(is.na(avg_upvotes), upvotes, avg_upvotes),
        uq_pagerank  = tidyr::replace_na(uq_pagerank,  0),
        uq_degree    = tidyr::replace_na(uq_degree,    0L),
        rp_outdegree = tidyr::replace_na(rp_outdegree, 0L),
        rp_indegree  = tidyr::replace_na(rp_indegree,  0L),
        rp_pagerank  = tidyr::replace_na(rp_pagerank,  0),
        n_threads = tidyr::replace_na(n_threads, 0L),
        avg_comments_per_thread = tidyr::replace_na(avg_comments_per_thread, 0),
        n_threads_log = tidyr::replace_na(n_threads_log, 0),
        avg_comments_per_thread_log = tidyr::replace_na(avg_comments_per_thread_log, 0),
        uq_pagerank_log = log1p(uq_pagerank * 1e6),
        uq_degree_log   = log1p(uq_degree)
      )
  }
  
  list(
    train4 = enrich_split(bio_train4_all, rp_train, user_agg_train, user_social_train, uq_train),
    test4  = enrich_split(bio_test4_all,  rp_test,  user_agg_test,  user_social_test,  uq_test)
  )
}

# --- Executing all the prepared functions and attach results to TRAIN and TEST---
aug_bio <- augment_with_user_net(bio_train4_all, bio_test4_all)
bio_train4 <- aug_bio$train4
bio_test4  <- aug_bio$test4

# --- Introduce a context predictor that stabilizes upvotes for each threads
add_thread_context <- function(df) {
  
  df <- df %>%
    dplyr::mutate(
      question_id = as.character(question_id),
      user_norm   = tolower(trimws(dplyr::coalesce(as.character(user), "")))
    )
  
  # (A) Thread-level context
  df <- df %>%
    dplyr::group_by(question_id) %>%
    dplyr::mutate(
      n_comments_total_thread = dplyr::n(),
      mean_upvotes_thread     = mean(upvotes, na.rm = TRUE),
      rel_upvotes_raw         = dplyr::if_else(
        mean_upvotes_thread > 0,
        upvotes / mean_upvotes_thread,
        upvotes
      )
    ) %>%
    dplyr::ungroup()
  
  df <- df %>%
    dplyr::mutate(
      rel_upvotes_raw = dplyr::if_else(
        is.na(rel_upvotes_raw) | is.infinite(rel_upvotes_raw),
        0,
        rel_upvotes_raw
      )
    )
  
  # (B) User-within-thread participation (this is what your label suggests)
  df <- df %>%
    dplyr::group_by(question_id, user_norm) %>%
    dplyr::mutate(
      n_comments_user_thread = dplyr::n()
    ) %>%
    dplyr::ungroup()
  
  # Shift for log on possibly negative rel_upvotes_raw
  min_val <- min(df$rel_upvotes_raw, na.rm = TRUE)
  shift   <- abs(min_val) + 1e-9
  
  df %>%
    dplyr::mutate(
      log_n_comments_total_thread = log1p(n_comments_total_thread),
      log_n_comments_user_thread  = log1p(n_comments_user_thread),
      rel_upvotes_log             = log1p(rel_upvotes_raw + shift)
    )
}


# --- Add to TRAIN and TEST  ---
bio_train4 <- add_thread_context(bio_train4)
bio_test4  <- add_thread_context(bio_test4)

# ============================================
# 7.5) THREAD-LEVEL REPLY CENTRALITY (status in conversation)
# ============================================

compute_thread_reply_centrality <- function(df){
  
  df <- df %>%
    mutate(
      question_id = as.character(question_id),
      user_norm   = tolower(trimws(coalesce(as.character(user), ""))),
      replied_to  = tolower(trimws(coalesce(as.character(replied_to), "")))
    )
  
  edges <- df %>%
    transmute(
      question_id,
      from = user_norm,
      to   = replied_to
    ) %>%
    filter(from != "anonymous_user") %>%
    filter(!is.na(to) & nzchar(to) & to != "anonymous_user") %>%
    filter(from != to)
  
  out <- edges %>%
    group_by(question_id) %>%
    group_modify(~{
      e <- .x
      if (nrow(e) == 0) {
        return(tibble(
          user_norm = character(),
          rp_indegree_thread = integer(),
          rp_betweenness_thread = numeric(),
          rp_embeddedness_thread = numeric()
        ))
      }
      
      g <- igraph::graph_from_data_frame(e %>% dplyr::select(from, to), directed = TRUE)
      
      indeg <- igraph::degree(g, mode = "in")
      btw   <- igraph::betweenness(g, directed = TRUE, normalized = TRUE)
      
      g_und <- igraph::as_undirected(g, mode = "collapse")
      emb   <- igraph::transitivity(g_und, type = "local", isolates = "zero")
      
      tibble(
        user_norm = names(indeg),
        rp_indegree_thread     = as.integer(indeg),
        rp_betweenness_thread  = as.numeric(btw[names(indeg)]),
        rp_embeddedness_thread = as.numeric(emb[names(indeg)])
      )
    }) %>%
    ungroup()
  
  out
}

# Computed separately for train/test (consistent with anti-leakage)
thr_train <- compute_thread_reply_centrality(bio_train4_all)
thr_test  <- compute_thread_reply_centrality(bio_test4_all)

# Join on (question_id, user_norm) on bio_train4 / bio_test4 already enriched
bio_train4 <- bio_train4 %>%
  mutate(
    question_id = as.character(question_id),
    user_norm   = tolower(trimws(coalesce(as.character(user), "")))
  ) %>%
  left_join(thr_train, by = c("question_id","user_norm")) %>%
  mutate(
    rp_indegree_thread     = tidyr::replace_na(rp_indegree_thread, 0L),
    rp_betweenness_thread  = tidyr::replace_na(rp_betweenness_thread, 0),
    rp_embeddedness_thread = tidyr::replace_na(rp_embeddedness_thread, 0),
  )

bio_test4 <- bio_test4 %>%
  mutate(
    question_id = as.character(question_id),
    user_norm   = tolower(trimws(coalesce(as.character(user), "")))
  ) %>%
  left_join(thr_test, by = c("question_id","user_norm")) %>%
  mutate(
    rp_indegree_thread     = tidyr::replace_na(rp_indegree_thread, 0L),
    rp_betweenness_thread  = tidyr::replace_na(rp_betweenness_thread, 0),
    rp_embeddedness_thread = tidyr::replace_na(rp_embeddedness_thread, 0),
  )



# ============================
# 8) SEMI-SUPERVISED (Self-Training)
# ============================

bio_train5_all <- bio_train4
bio_test5_all  <- bio_test4

# adjust upvotes variables with weights due to negative values
min_sum = min(bio_train5_all$sum_upvotes, na.rm = TRUE)
shift_sum = abs(min_sum) + 1e-9

min_avg = min(bio_train5_all$avg_upvotes, na.rm = TRUE)
shift_avg = abs(min_avg) + 1e-9

# --- 8.1) Remove "names" in vectors and mutate network predictors into log-
bio_train5_all <- bio_train5_all %>%
  dplyr::mutate(
    has_reference = as.integer(unname(has_reference)),
    has_formula   = as.integer(unname(has_formula)),
    entropy       = as.numeric(unname(entropy)),
    uq_pagerank_log            = log1p(uq_pagerank * 1e6),
    uq_degree_log              = log1p(uq_degree),
    rp_pagerank_log            = log1p(rp_pagerank * 1e6),
    rp_outdegree_log           = log1p(rp_outdegree),
    rp_indegree_thread_log     = log1p(rp_indegree_thread),
    rp_betweenness_thread_log  = log1p(rp_betweenness_thread * 1e6),
    rp_embeddedness_thread_log = log1p(rp_embeddedness_thread * 1e6),
    sum_upvotes_log = log1p(sum_upvotes + shift_sum),
    avg_upvotes_log = log1p(avg_upvotes + shift_avg)
  )

bio_test5_all <- bio_test5_all %>%
  dplyr::mutate(
    has_reference = as.integer(unname(has_reference)),
    has_formula   = as.integer(unname(has_formula)),
    entropy       = as.numeric(unname(entropy)),
    uq_pagerank_log            = log1p(uq_pagerank * 1e6),
    uq_degree_log              = log1p(uq_degree),
    rp_pagerank_log            = log1p(rp_pagerank * 1e6),
    rp_outdegree_log           = log1p(rp_outdegree),
    rp_indegree_thread_log     = log1p(rp_indegree_thread),
    rp_betweenness_thread_log  = log1p(rp_betweenness_thread * 1e6),
    rp_embeddedness_thread_log = log1p(rp_embeddedness_thread * 1e6),
    sum_upvotes_log = log1p(sum_upvotes + shift_sum),
    avg_upvotes_log = log1p(avg_upvotes + shift_avg)
  )

message("Rows TRAIN: ", nrow(bio_train5_all))
message("Rows TEST : ", nrow(bio_test5_all))
message("Labeled in TRAIN: ", sum(!is.na(bio_train5_all$label)))
message("Labeled in TEST : ", sum(!is.na(bio_test5_all$label)))

saveRDS(bio_test5_all,  file.path(OUT_DIR, "bio_test_all_new_2.rds"))
saveRDS(bio_train5_all, file.path(OUT_DIR, "bio_train_all_new_2.rds"))

# ============================
# 8.2) Predictors — grouped into:
# 1) Social Participation
# 2) Social Recognition
# 3) Expertise (textual / cognitive / disciplinary)
# ============================

# --- 8.2.1) SOCIAL PARTICIPATION ---
pred_social_participation <- c(
  # Activity of the user
  "n_comments",
  "active_span_days",
  
  # Participation across threads
  "n_threads",
  "avg_comments_per_thread",
  "n_threads_log",
  "avg_comments_per_thread_log",
  "threads_multi",
  "is_early_responder",
  
  # Participation inside specific thread
  "n_comments_user_thread"
)

# --- 8.2.2) SOCIAL RECOGNITION ---
pred_social_recognition <- c(
  
  # Upvotes-based recognition
  "sum_upvotes_log",
  "avg_upvotes_log",
  "mean_upvotes_thread",
  "rel_upvotes_log",
  
  # User–Question network centrality
  "uq_pagerank_log",
  "uq_degree_log",
  "rp_indegree_thread_log",
  
  # Reply-graph centrality
  "rp_betweenness_thread_log",
  "rp_embeddedness_thread_log",
  
  # AMA experts
  "is_AMA_expert_user"
  
  # Visibility / identity signal
  # "anonymous" removed because it increases noise
)

# --- 8.2.3) EXPERTISE (textual / semantic / disciplinary) ---
pred_expertise <- c(
  # Text length and structure
  # "n_chars_clean", removed because it increases noise
  "n_words",
  "avg_word_length",
  
  # Stylistic / scientific cues
  "has_link", "has_any_number", "has_scientific_number", "has_reference", "has_formula",
  
  # Cognitive complexity
  "entropy",
  "Flesch.Kincaid", "SMOG", "Flesch",
  
  # Semantic polarity
  "sentiment",
  
  # Domain-knowledge density
  "biology_density"
)

# --- Combine all three group predictors ---
pred_cols <- c(
  pred_social_participation,
  pred_social_recognition,
  pred_expertise
)

# --- Add SVD latent semantic dimensions ---
svd_cols <- grep("^svd_", names(bio_train5_all), value = TRUE)

# --- Final predictor list ---
Xcols <- intersect(c(pred_cols, svd_cols), names(bio_train5_all))

# ===============================================
# 8.3–8.7  SELF-TRAINING (SSL) —
# ===============================================

# ---- 8.3) Split Labeled / Unlabeled----
L_train <- bio_train5_all %>%
  dplyr::filter(!is.na(label)) %>%
  dplyr::mutate(label = factor(label, levels = c("non-expert","expert"))) %>%
  dplyr::select(label, all_of(Xcols))

U_train <- bio_train5_all %>%
  dplyr::filter(is.na(label)) %>%
  dplyr::select(all_of(Xcols))

stopifnot(nrow(bio_train5_all) == nrow(L_train) + nrow(U_train))

# ---- 8.4) Retaining only features that vary within label samples ----
keep_cols <- L_train %>%
  dplyr::select(-label) %>%
  dplyr::summarise(dplyr::across(where(is.numeric), n_distinct)) %>%
  tidyr::pivot_longer(everything(), names_to = "var", values_to = "k") %>%
  dplyr::filter(k > 1) %>%
  dplyr::pull(var)

L_train <- L_train %>% dplyr::select(label, all_of(keep_cols))
U_train <- U_train %>% dplyr::select(all_of(keep_cols))

cat("Features active for GLM:", length(keep_cols), "\n")

# ---- 8.5) Recipe ----
rec_ssl <- recipes::recipe(label ~ ., data = L_train) %>%
  recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
  recipes::step_normalize(recipes::all_numeric_predictors())

glm_spec_ssl <- logistic_reg(
  penalty = tune(), 
  mixture = 0.5 
) %>%
  set_engine(
    "glmnet",
    nlambda = 80,
    lambda.min.ratio = 1e-3,
    maxit = 2e5,
    thresh = 1e-7,
    standardize = FALSE # Already normalize within the recipe
  ) %>%
  set_mode("classification")


# ---- 8.6) Grid GLM  ----
glm_grid_ssl <- dials::grid_regular(
  dials::penalty(range = c(-5, 0), trans = scales::log10_trans()),
  levels = 30
)
cat("penalty combinations GLM:", nrow(glm_grid_ssl), "\n")

# ---- 8.7) Helper self-training
self_train_generic <- function(
    model_spec,
    model_name,
    grid,
    L_df,
    U_df,
    rec_obj  = rec_ssl,
    metrics  = yardstick::metric_set(pr_auc, roc_auc),
    th_pos   = 0.80,
    th_neg   = 0.20,
    max_iter = 5,
    seed     = SEED
){
  set.seed(seed)
  
  # numerical with and without "names"
  L_df <- L_df %>%
    dplyr::select(label, where(is.numeric)) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~unname(.)))
  
  # workflow with recipe 
  wf <- workflows::workflow() %>%
    workflows::add_recipe(rec_obj) %>%
    workflows::add_model(model_spec)
  
  v <- min(5, max(3, floor(nrow(L_df) / 20)))
  folds <- vfold_cv(L_df, v = v)
  
  tune0 <- tune::tune_grid(
    wf, resamples = folds, grid = grid,
    metrics = metrics,
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE)
  )
  best0  <- tune::select_best(tune0, metric = "pr_auc")
  wf_fit <- tune::finalize_workflow(wf, best0) %>% parsnip::fit(L_df)
  
  lower_once <- FALSE
  for (iter in seq_len(max_iter)) {
    if (nrow(U_df) == 0) break
    pr <- predict(wf_fit, U_df, type = "prob")
    pr <- dplyr::bind_cols(U_df, pr)
    
    pos <- pr %>% dplyr::filter(.pred_expert >= th_pos) %>%
      dplyr::mutate(label = factor("expert", levels = levels(L_df$label))) %>%
      dplyr::select(label, where(is.numeric))
    neg <- pr %>% dplyr::filter(.pred_expert <= th_neg) %>%
      dplyr::mutate(label = factor("non-expert", levels = levels(L_df$label))) %>%
      dplyr::select(label, where(is.numeric))
    
    pseudo <- dplyr::bind_rows(pos, neg)
    
    if (nrow(pseudo) == 0) {
      cat(model_name, ": no pseudo-labels (iter=", iter, ", soglie ",
          th_neg, "/", th_pos, ").\n", sep = "")
      if (!lower_once && th_pos > 0.65) {
        th_pos <- th_pos - 0.1; th_neg <- th_neg + 0.1
        lower_once <- TRUE
        cat("→ thresholds made more tolerant:", th_neg, "/", th_pos, "\n")
        next
      }
      break
    }
    
    cat(model_name, ": added ", nrow(pseudo), " pseudo-labels (iter=",
        iter, ")\n", sep = "")
    L_df <- dplyr::bind_rows(L_df, pseudo)
    used <- dplyr::bind_rows(
      dplyr::select(pos, -label),
      dplyr::select(neg, -label)
    )
    U_df <- dplyr::anti_join(U_df, used, by = names(U_df))
    
    v <- min(5, max(3, floor(nrow(L_df) / 20)))
    folds <- vfold_cv(L_df, v = v)
    
    tune_i <- tune::tune_grid(
      wf, resamples = folds, grid = grid, metrics = metrics,
      control = tune::control_grid(save_pred = TRUE, verbose = TRUE)
    )
    best_i <- tune::select_best(tune_i, metric = "pr_auc")
    wf_fit <- tune::finalize_workflow(wf, best_i) %>% parsnip::fit(L_df)
  }
  
  wf_fit
}


saveRDS(rec_ssl,              file.path(OUT_DIR, "recipe_preprocessing_bio_ssl_04.rds"))


# ============================
# 9) MODELS SPECIFICATIONS
# ============================

# Decision Tree specifications
tree_spec_ssl <- decision_tree(
  cost_complexity = tune(),
  min_n = tune(),
  tree_depth = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_grid_ssl <- grid_regular(
  cost_complexity(range = c(-4, -1)),
  min_n(range = c(10, 50)),
  tree_depth(range = c(2, 10)),
  levels = c(5, 5, 5)
)

# Random Forest specifications
p <- length(Xcols)
mtry_vals <- unique(pmax(1, floor(c(0.1, 0.2, 0.35, 0.5) * p)))

rf_spec_ssl <- rand_forest(
  mtry  = tune(),
  min_n = tune(),
  trees = 1500
) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_grid_ssl <- expand.grid(
  mtry  = mtry_vals,
  min_n = c(2L, 5L, 10L, 20L),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# ============================
# 9.1) SELF-TRAINING MODELS
# ============================

fit_glm_ssl <- self_train_generic(
  model_spec = glm_spec_ssl,
  model_name = "GLM (SSL)",
  grid       = glm_grid_ssl,
  L_df       = L_train,
  U_df       = U_train,
  th_pos     = 0.80,
  th_neg     = 0.20,
  max_iter   = 5
)

saveRDS(fit_glm_ssl, file.path(OUT_DIR, "fit_glm_ssl_bio_04.rds"))


final_tree_ssl_fit <- self_train_generic(
  model_spec = tree_spec_ssl,
  model_name = "Decision Tree (SSL)",
  grid       = tree_grid_ssl,
  L_df       = L_train,
  U_df       = U_train,
  th_pos     = 0.80,
  th_neg     = 0.20,
  max_iter   = 5
)

saveRDS(final_tree_ssl_fit, file.path(OUT_DIR, "final_tree_ssl_fit_bio_04.rds"))


final_rf_ssl_fit <- self_train_generic(
  model_spec = rf_spec_ssl,
  model_name = "Random Forest (SSL)",
  grid       = rf_grid_ssl,
  L_df       = L_train,
  U_df       = U_train,
  th_pos     = 0.75,  
  th_neg     = 0.25,  
  max_iter   = 5
)

saveRDS(final_rf_ssl_fit,     file.path(OUT_DIR, "final_rf_ssl_fit_bio_04.rds"))



# ============================
# 9.2) PREPARED THE TEST LABELED SAMPLE
# ============================

# ---- TRAIN LABELED
train_labeled <- bio_train5_all %>%
  filter(user_norm %in% labeled_train_users, q_user_rank == 1) %>%
  filter(!is.na(label))

# ---- TEST LABELED
test_labeled <- bio_test5_all %>%  
  filter(user_norm %in% labeled_test_users, q_user_rank == 1) %>%
  filter(!is.na(label))

# ============================
# 9.3) EVALUATION ON TEST
# ============================

eval_ssl_model <- function(fit, test_df, name = "MODEL SSL", fixed_threshold = 0.4) {
  
  if (nrow(test_df) == 0) {
    warning(name, ": no labeled example in TEST.")
    return(invisible(NULL))
  }
  
  if (!is.factor(test_df$label))
    test_df$label <- factor(test_df$label, levels = c("non-expert", "expert"))
  
  # === Predictions ===
  Xtest    <- test_df %>% dplyr::select(all_of(keep_cols))
  prob_df  <- predict(fit, Xtest, type = "prob")
  class_df <- predict(fit, Xtest, type = "class") %>%
    dplyr::rename(pred_class = .pred_class)
  
  preds <- dplyr::bind_cols(
    test_df %>% dplyr::select(label),
    prob_df,
    class_df
  )
  
  # === Probability column ===
  prob_col <- if (".pred_expert" %in% names(preds)) ".pred_expert" else
    setdiff(grep("^\\.pred_", names(preds), value = TRUE), "pred_class")[1]
  
  # === Print metrics ===
  options(yardstick.event_first = FALSE)
  cat("\n==== ", name, " (TEST labeled) ====\n", sep = "")
  print(dplyr::bind_rows(
    yardstick::accuracy(preds,  truth = label, estimate = pred_class),
    yardstick::f_meas(preds,    truth = label, estimate = pred_class, event_level = "second"),
    yardstick::precision(preds, truth = label, estimate = pred_class, event_level = "second"),
    yardstick::recall(preds,    truth = label, estimate = pred_class, event_level = "second")
  ))
  print(dplyr::bind_rows(
    yardstick::roc_auc(preds, truth = label, !!rlang::sym(prob_col), event_level = "second"),
    yardstick::pr_auc(preds,  truth = label, !!rlang::sym(prob_col), event_level = "second")
  ))
  
  cat(sprintf("\nFixed threshold (%s): %.3f\n", name, fixed_threshold))
  
  invisible(list(
    preds     = preds,
    prob_col  = prob_col,
    fixed_thr = fixed_threshold
  ))
}

# Models' evaluation
glm_eval  <- eval_ssl_model(fit_glm_ssl, test_labeled, "GLM (SSL)", fixed_threshold = 0.4)
saveRDS(fit_glm_ssl,        file.path(OUT_DIR, "final_glm_ssl_fit_bio_04.rds"))

tree_eval <- eval_ssl_model(final_tree_ssl_fit, test_labeled, "Decision Tree (SSL)", fixed_threshold = 0.4)
saveRDS(final_tree_ssl_fit, file.path(OUT_DIR, "final_tree_ssl_fit_bio_04.rds"))

rf_eval   <- eval_ssl_model(final_rf_ssl_fit, test_labeled, "Random Forest (SSL)", fixed_threshold = 0.4)
saveRDS(rf_eval, file.path(OUT_DIR, "rf_ssl_eval_bio_04.rds"))



# Create a table as image for model comparison
metrics_from_eval <- function(eval_obj, model_name){
  preds    <- eval_obj$preds
  prob_col <- eval_obj$prob_col
  thr      <- eval_obj$fixed_thr
  
  preds_thr <- preds %>%
    mutate(pred_thr = factor(ifelse(.data[[prob_col]] >= thr, "expert", "non-expert"),
                             levels = levels(label)))
  
  tibble(
    model      = model_name,
    threshold  = thr,
    accuracy   = yardstick::accuracy(preds_thr, truth = label, estimate = pred_thr)$.estimate,
    f1         = yardstick::f_meas(preds_thr, truth = label, estimate = pred_thr, event_level = "second")$.estimate,
    precision  = yardstick::precision(preds_thr, truth = label, estimate = pred_thr, event_level = "second")$.estimate,
    recall     = yardstick::recall(preds_thr, truth = label, estimate = pred_thr, event_level = "second")$.estimate,
    roc_auc    = yardstick::roc_auc(preds, truth = label, !!rlang::sym(prob_col), event_level = "second")$.estimate,
    pr_auc     = yardstick::pr_auc(preds, truth = label, !!rlang::sym(prob_col), event_level = "second")$.estimate
  )
}

table_models <- bind_rows(
  metrics_from_eval(glm_eval,  "GLM (SSL)"),
  metrics_from_eval(tree_eval, "Decision Tree (SSL)"),
  metrics_from_eval(rf_eval,   "Random Forest (SSL)")
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

gt_models <- table_models %>%
  gt() %>%
  tab_header(
    title = "Model comparison (Biology) — SSL",
    subtitle = "Metrics computed with a fixed F1 threshold 0.4"
  ) %>%
  fmt_number(columns = c(threshold, accuracy, f1, precision, recall, roc_auc, pr_auc), decimals = 3)

out_models_png <- file.path(OUT_DIR, "table_models_comparison_bio_04.png")
gtsave(gt_models, out_models_png)
out_models_png

# ============================================
# 10 ) BOOTSTRAP ON THE RF MODEL
# ============================================

fixed_thr <- rf_eval$fixed_thr

bootstrap_test <- function(model, test_df, Xcols, thr = fixed_thr,
                           B = 300, size = NULL) {
  
  if (is.null(size)) size <- nrow(test_df)
  metrics_list <- vector("list", B)
  
  for (b in seq_len(B)) {
    
    idx     <- sample(seq_len(nrow(test_df)), size = size, replace = TRUE)
    boot_df <- test_df[idx, ]
    
    boot_df$label <- factor(boot_df$label, levels = c("non-expert","expert"))
    
    # Probabilisitc predictions
    X_boot <- boot_df %>% dplyr::select(all_of(Xcols))
    prob   <- predict(model, X_boot, type = "prob")
    
    # Threshold
    pred_thr <- ifelse(prob$.pred_expert >= thr, "expert", "non-expert")
    
    boot_preds <- tibble::tibble(
      label = boot_df$label,
      .pred_expert = prob$.pred_expert,
      pred_class = factor(pred_thr, levels = c("non-expert","expert"))
    )
  
    
    boot_df <- test_df[idx, ]
    boot_df$label <- factor(boot_df$label, levels = c("non-expert","expert"))
    
    
    # Confusion matrix 
    cm <- yardstick::conf_mat(
      boot_preds,
      truth = label,
      estimate = pred_class
    )
    
    tp <- cm$table["expert","expert"]
    fp <- cm$table["non-expert","expert"]
    fn <- cm$table["expert","non-expert"]
    
    precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
    recall    <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
    f1        <- if ((precision + recall) == 0) 0 else 
      2 * precision * recall / (precision + recall)
    
    # Accuracy, AUC, PR-AUC
    acc <- yardstick::accuracy(boot_preds,
                               truth = label,
                               estimate = pred_class)$.estimate
    
    auc <- yardstick::roc_auc(
      boot_preds,
      truth = label,
      .pred_expert,
      event_level = "second"
    )$.estimate
    
    prc <- yardstick::pr_auc(
      boot_preds,
      truth = label,
      .pred_expert,
      event_level = "second"
    )$.estimate
    
    metrics_list[[b]] <- tibble::tibble(
      acc = acc,
      f1  = f1,
      prec = precision,
      rec = recall,
      auc = auc,
      prc = prc
    )
  }
  
  dplyr::bind_rows(metrics_list)
}

# Run the bootstrap
set.seed(123)

boot_rf <- bootstrap_test(
  model  = final_rf_ssl_fit,
  test_df = test_labeled,
  Xcols  = Xcols,
  thr    = fixed_thr, 
  B      = 300,
  size   = nrow(test_labeled)
)

boot_summary <- summary(boot_rf)
boot_summary

saveRDS(boot_rf, file.path(OUT_DIR, "bootstrap_rf_bio_04.rds"))

### 11) --- ABLATION + BOOTSTRAP --- ###
# Comparison between each constructed dimension within RF model

part_vars <- intersect(pred_social_participation, names(L_train))
recg_vars <- intersect(pred_social_recognition,   names(L_train))
exp_vars  <- intersect(pred_expertise,           names(L_train))

# For each model
eval_ssl_model_group <- function(fit, test_df, Xcols_group, name = "MODEL SSL"){
  if (nrow(test_df) == 0) {
    warning(name, ": no labeled example in TEST.")
    return(invisible(NULL))
  }
  
  if (!is.factor(test_df$label))
    test_df$label <- factor(test_df$label, levels = c("non-expert","expert"))
  
  # === Predictions ===
  Xtest   <- test_df %>% dplyr::select(all_of(Xcols_group))
  prob_df <- predict(fit, Xtest, type = "prob")
  class_df <- predict(fit, Xtest, type = "class") %>%
    dplyr::rename(pred_class = .pred_class)
  
  preds <- dplyr::bind_cols(test_df %>% dplyr::select(label), prob_df, class_df)
  
  # === Identify probability columns ===
  prob_cols <- grep("^\\.pred_", names(preds), value = TRUE)
  prob_cols <- setdiff(prob_cols, c(".pred_class","pred_class"))
  prob_col <- if (".pred_expert" %in% prob_cols) ".pred_expert" else prob_cols[1]
  
  # === Metrics===
  options(yardstick.event_first = FALSE)
  cat("\n==== ", name, " (TEST labeled) ====\n", sep = "")
  print(dplyr::bind_rows(
    yardstick::accuracy(preds, truth = label, estimate = pred_class),
    yardstick::f_meas(preds, truth = label, estimate = pred_class, event_level = "second"),
    yardstick::precision(preds, truth = label, estimate = pred_class, event_level = "second"),
    yardstick::recall(preds, truth = label, estimate = pred_class, event_level = "second")
  ))
  
  print(dplyr::bind_rows(
    yardstick::roc_auc(preds, truth = label, !!rlang::sym(prob_col), event_level = "second"),
    yardstick::pr_auc(preds,  truth = label, !!rlang::sym(prob_col), event_level = "second")
  ))
  
  # === BEST THRESHOLD (fixed at 0.4) ===
  fixed_thr <- 0.4
  cat(sprintf("\nFixed threshold (%s): %.3f\n", name, fixed_thr))
  
  invisible(list(
    preds = preds,
    prob_col = prob_col,
    fixed_thr = fixed_thr
  ))
}


### 12) --- ABLATION ---

run_rf_ablation_boot <- function(drop_vars, model_name, 
                                 B = 300, fixed_thr = 0.5) {
  
  # ====== 12.1) Predictor selections ======
  all_feats <- setdiff(names(L_train), "label")
  keep_feats <- setdiff(all_feats, drop_vars)
  
  cat(model_name, " — number of predictors:", length(keep_feats), "\n")
  
  L_sub <- L_train %>% dplyr::select(label, all_of(keep_feats))
  U_sub <- U_train %>% dplyr::select(all_of(keep_feats))
  
  # --- Ensure label is factor everywhere ---
  L_sub <- L_sub %>%
    dplyr::mutate(label = factor(label, levels = c("non-expert","expert")))
  
  test_labeled <- test_labeled %>%
    dplyr::mutate(label = factor(label, levels = c("non-expert","expert")))
  
  
  rec_sub <- recipes::recipe(label ~ ., data = L_sub) %>%
    recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
    recipes::step_normalize(recipes::all_numeric_predictors())
  
  # ====== 12.2) Self-training RF ======
  fit_sub <- self_train_generic(
    model_spec = rf_spec_ssl,
    model_name = model_name,
    grid       = rf_grid_ssl,
    L_df       = L_sub,
    U_df       = U_sub,
    rec_obj    = rec_sub,
    th_pos     = 0.75,
    th_neg     = 0.25,
    max_iter   = 5
  )
  
  # ====== 12.3) Evaluation ======
  Xtest <- test_labeled %>% dplyr::select(all_of(keep_feats))
  prob <- predict(fit_sub, Xtest, type = "prob")
  
  preds <- dplyr::bind_cols(
    test_labeled %>% dplyr::select(label),
    prob
  ) %>%
    dplyr::mutate(
      label = factor(label, levels = c("non-expert","expert")),
      pred_class = factor(
        ifelse(.pred_expert >= fixed_thr, "expert", "non-expert"),
        levels = c("non-expert", "expert")
      )
    )
  
  
  cat(sprintf("\n[%s] Using FIXED threshold: %.3f\n", model_name, fixed_thr))
  
  metrics <- tibble::tibble(
    accuracy  = yardstick::accuracy(preds, truth = label, estimate = pred_class)$.estimate,
    f1        = yardstick::f_meas(preds, truth = label, estimate = pred_class, event_level = "second")$.estimate,
    precision = yardstick::precision(preds, truth = label, estimate = pred_class, event_level = "second")$.estimate,
    recall    = yardstick::recall(preds, truth = label, estimate = pred_class, event_level = "second")$.estimate,
    auc       = yardstick::roc_auc(preds, truth = label, .pred_expert, event_level = "second")$.estimate,
    pr_auc    = yardstick::pr_auc(preds, truth = label, .pred_expert, event_level = "second")$.estimate
  )
  
  print(metrics)
  
  # ====== 12.4) Bootstrap ======
  boot_res <- bootstrap_test(
    model = fit_sub,
    test_df = test_labeled,
    Xcols = keep_feats,
    thr = fixed_thr,
    B = 300,
    size = nrow(test_labeled)
  )
  
  boot_sum <- boot_res %>% 
    summarise(
      acc_mean = mean(acc),
      acc_med  = median(acc),
      f1_mean  = mean(f1),
      f1_med   = median(f1),
      prec_mean = mean(prec),
      rec_mean  = mean(rec),
      auc_mean  = mean(auc),
      prc_mean  = mean(prc)
    )
  
  return(list(
    fit = fit_sub,
    metrics = metrics,
    feats = keep_feats,
    bootstrap = boot_sum
  ))
}

# Run each model with ablation + bootstrap
abl_no_part <- run_rf_ablation_boot(
  drop_vars = part_vars,
  model_name = "RF (SSL) — NO Social Participation",
  fixed_thr = 0.4
)

saveRDS(abl_no_part, file.path(OUT_DIR, "abl_no_part.rds"))

abl_no_recg <- run_rf_ablation_boot(
  drop_vars = recg_vars,
  model_name = "RF (SSL) — NO Social Recognition",
  fixed_thr = 0.4
)

saveRDS(abl_no_recg, file.path(OUT_DIR, "abl_no_recg.rds"))


abl_no_exp <- run_rf_ablation_boot(
  drop_vars = exp_vars,
  model_name = "RF (SSL) — NO Expertise",
  fixed_thr = 0.4
)

saveRDS(abl_no_exp, file.path(OUT_DIR, "abl_no_exp.rds"))



extract_metrics_boot <- function(res){

    metrics_obj <- res$metrics
  boot_obj <- res$bootstrap
  
  tibble(
    accuracy = metrics_obj$accuracy,
    f1 = metrics_obj$f1,
    precision = metrics_obj$precision,
    recall = metrics_obj$recall,
    auc = metrics_obj$auc,
    pr_auc = metrics_obj$pr_auc,
    
    # Bootstrap means
    acc_boot = boot_obj$acc_mean,
    f1_boot = boot_obj$f1_mean,
    prec_boot = boot_obj$prec_mean,
    rec_boot = boot_obj$rec_mean,
    auc_boot = boot_obj$auc_mean,
    prc_boot = boot_obj$prc_mean
  )
}
    


# full model — pred_class at fixed threshold 0.4 for consistency with ablation models
rf_preds_thr <- rf_eval$preds %>%
  dplyr::mutate(
    pred_class = factor(
      ifelse(.data[[rf_eval$prob_col]] >= fixed_thr, "expert", "non-expert"),
      levels = c("non-expert", "expert")
    )
  )

full_model_metrics <- tibble(
  accuracy  = yardstick::accuracy(rf_preds_thr, label, pred_class)$.estimate,
  f1        = yardstick::f_meas(rf_preds_thr, label, pred_class, event_level = "second")$.estimate,
  precision = yardstick::precision(rf_preds_thr, label, pred_class, event_level = "second")$.estimate,
  recall    = yardstick::recall(rf_preds_thr, label, pred_class, event_level = "second")$.estimate,
  auc       = yardstick::roc_auc(rf_eval$preds, label, !!rlang::sym(rf_eval$prob_col), event_level = "second")$.estimate,
  pr_auc    = yardstick::pr_auc(rf_eval$preds, label, !!rlang::sym(rf_eval$prob_col), event_level = "second")$.estimate
)

boot_sum_full <- boot_rf %>%
  summarise(
    acc_mean  = mean(acc),
    f1_mean   = mean(f1),
    prec_mean = mean(prec),
    rec_mean  = mean(rec),
    auc_mean  = mean(auc),
    prc_mean  = mean(prc)
  )

full_model_obj <- list(
  metrics = full_model_metrics,
  bootstrap = boot_sum_full
)

# Final Table
table_ablation <- tibble(
  model = c(
    "Full Model",
    "No SP",
    "No SR",
    "No Exp."
  )
) %>%
  bind_cols(
    bind_rows(
      extract_metrics_boot(full_model_obj),
      extract_metrics_boot(abl_no_part),
      extract_metrics_boot(abl_no_recg),
      extract_metrics_boot(abl_no_exp)
    )
  ) %>%
  mutate(
    delta_f1 = f1 - first(f1),
    delta_auc = auc - first(auc)
  )

print(table_ablation)

saveRDS(table_ablation, 
        file = "table_ablation_biology_04.rds")



# Create a table to represent ablation
gt_ablation <- table_ablation %>%
  gt() %>%
  tab_header(
    title = "Ablation comparison (Biology) — RF (SSL)",
    subtitle = "Full model vs removing each construct (with bootstrap means)"
  ) %>%
  cols_label(
    model = "Model",
    accuracy = "Acc", precision = "Prec", recall = "Rec",
    pr_auc = "PR-AUC", auc = "ROC-AUC",
    delta_f1 = "ΔF1", delta_auc = "ΔAUC"
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 3) %>%
  tab_options(
    table.font.size = px(20),
    column_labels.font.size = px(18),
    heading.title.font.size = px(22),
    heading.subtitle.font.size = px(14),
    data_row.padding = px(5)         
  ) %>%
  tab_options(table.width = px(1200))


out_ablation_png <- file.path(OUT_DIR, "table_ablation_bio_04.png")
gtsave(gt_ablation, out_ablation_png, vwidth = 1400, vheight = 600)


# ============================
# 13) FINAL MODEL AND PREDICTIONS ON UNLABELED TRAIN IF NEEDED
# ============================

thr <- 0.4
idx_u_train <- is.na(bio_train5_all$label)

pr_trainU <- predict(
  final_rf_ssl_fit,
  new_data = bio_train5_all[idx_u_train, keep_cols, drop = FALSE],
  type = "prob"
)

cl_trainU <- ifelse(pr_trainU$.pred_expert >= thr, "expert", "non-expert")

bio_train5_pred <- bio_train5_all %>% 
  mutate(
    pred_prob_expert = NA_real_,
    pred_class       = NA_character_
  )

bio_train5_pred$pred_prob_expert[idx_u_train] <- pr_trainU$.pred_expert
bio_train5_pred$pred_class[idx_u_train]       <- cl_trainU

table(bio_train5_pred$pred_class[idx_u_train])

# Return to default cores usage
stopCluster(cl)
registerDoSEQ()


# Shortcut for fast model execution 
bio_test5_all <- readRDS(file.path(OUT_DIR, "bio_test_all_new_2.rds"))
bio_train5_all <- readRDS(file.path(OUT_DIR, "bio_train_all_new_2.rds"))
rec_ssl <- readRDS(file.path(OUT_DIR, "recipe_preprocessing_bio_ssl_04.rds"))
glm_eval <- readRDS(file.path(OUT_DIR,"fit_glm_ssl_bio_04.rds"))
tree_eval <- readRDS(file.path(OUT_DIR, "final_tree_ssl_fit_bio_04.rds"))
rf_eval <- readRDS(file.path(OUT_DIR, "rf_ssl_eval_bio_04.rds"))
final_rf_ssl_fit <- readRDS(file.path(OUT_DIR, "final_rf_ssl_fit_bio_04.rds"))
fit_glm_ssl <- readRDS(file.path(OUT_DIR, "fit_glm_ssl_bio_04.rds"))
final_tree_ssl_fit <- readRDS(file.path(OUT_DIR, "final_tree_ssl_fit_bio_04.rds"))
bootstrap_rf_bio_04 <- readRDS(file.path(OUT_DIR, "bootstrap_rf_bio_04.rds"))
table_ablation <- readRDS(file.path("C:\\Users\\thebe\\OneDrive\\Desktop\\Corsi Trento\\Tesi\\Python", "table_ablation_biology_04.rds"))
