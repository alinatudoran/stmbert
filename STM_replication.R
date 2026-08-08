# =============================================================================
# Companion code for: "Measuring Organizational Constructs in Large-Scale Text:
# An interpretive framework combining topic modeling and BERT",
# *Organization Studies*, Two-stage framework paper (OS-24-0908).
# =============================================================================
# This script covers Stage 1 (STM operationalization):
# STM topic modeling of Glassdoor employee reviews. 
# preprocessing, threshold/K selection, the competitive models, the final 
# K = 30 model, topic outputs, stm proportions, stability analysis reported in 
# Appendix 6 and the temporal prevalence analysis reported in Appendix 7.
# Stratified data splitting into train and test (80/20)
#
# Reproducibility notes:
#   - Set the data path in the "Data" block below before running.
#   - Seeds are fixed throughout; sessionInfo() is printed at the end.
#   - Inline "# [1] ..." comments record the expected output for each step.
# =============================================================================


# Packages required (install once, then leave commented) -----------------------
# install.packages("caret")
# install.packages("clue") 
# install.packages("dplyr")
# install.packages("future")
# install.packages("future.apply")
# install.packages("ggplot2")
# install.packages("ggrepel")
# install.packages("gridExtra")
# install.packages("hunspell")
# install.packages("lubridate")
# install.packages("openxlsx")
# install.packages("parallel")
# install.packages("purrr")
# install.packages("rlang")
# install.packages("scales")
# install.packages("stm")
# install.packages("stringi")
# install.packages("stringr")
# install.packages("tibble")
# install.packages("tidyr")
# install.packages("tm")
# install.packages("readxl")


# Load libraries ---------------------------------------------------------------
library(caret)        # for createDataPartition() train/test split
library(clue)         # solve_LSAP topic matching
library(dplyr)        # data wrangling
library(future)       # parallel back-end
library(future.apply) # parallel apply
library(ggplot2)      # plots
library(ggrepel)      # non-overlapping labels
library(grid)         # textGrob / gpar
library(gridExtra)    # grid.arrange
library(hunspell)     # spell correction
library(lubridate)    # date parsing (year bins, Appendix 7)
library(openxlsx)     # Excel output
library(parallel)     # detect cores / base parallel processing
library(purrr)        # map/list_rbind
library(rlang)        # a collection of frameworks for programming with R
library(scales)       # axis scales
library(stm)          # structural topic model + diagnostics
library(stringi)      # word counts
library(stringr)      # regex helpers (spell-check, repetition)
library(tibble)       # tibbles for result tables
library(tidyr)        # used by replace_na
library(tm)           # removeWords / stripWhitespace
library(readxl)       # for reading the labeled dataset (Excel) - Step 6




# Step 1 — Data collection, description, and cleaning (paper Section 3.1) ------

  # Set this to your local copy of the Glassdoor review file before running.
  # The raw data is NOT distributed with this code (Glassdoor ToS + privacy);
  # supply your own semicolon-separated file. See the replication README.
  data_path <- "data/comments.csv"    # relative to the project root
  comments  <- read.csv(data_path, sep = ";", comment.char = "#")
  dim(comments)
  # [1] 45875   22  (initial dataset; paper: n = 45,875, 22 columns)
  
  
  # Remove duplicates
  df_unique <- comments %>%
    distinct(reviewId, .keep_all = TRUE)

  # Review date and job type are the metadata of interest. Job type is used for the
  # office-role filter below; review date is retained and preprocessed for the
  # metadata (year) diagnostic — see Step 5 / Appendix 7 near the end of the script.
  
  # Filter rows where the jobTitle.Text contains any of the specified keywords.
  # Built from a vector (not a hand-wrapped string literal) so line breaks can't
  # embed stray characters into the pattern, and wrapped with \\b so short
  # keywords (e.g. "IT", "PR") only match whole words, not substrings.
  office_role_keywords <- c(
    "manager", "engineer", "software", "analyst", "recruiter",
    "specialist", "data", "HR", "lead", "scientist",
    "human", "resource", "digital", "representative", "assistant",
    "executive", "IT", "expert", "reviewer", "director",
    "account", "business", "intelligence", "investigator", "marketing",
    "consultant", "developer", "finance", "learning", "training",
    "architect", "designer", "coordinator", "project", "brand",
    "economist", "editor", "copywriter", "PR", "administrator",
    "planner", "machine learning", "researcher", "clerk", "advisor"
  )
  office_role_pattern <- paste0("\\b(", paste(office_role_keywords, collapse = "|"), ")\\b")

  filtered_df <- df_unique %>%
   filter(grepl(office_role_pattern, jobTitle.text, ignore.case = TRUE))
  nrow(filtered_df)
  # [1] 20188  (after removing duplicate IDs + office-role filter; paper: 20,188)
  
 
  # Select id and pros
  data <- filtered_df[, c("reviewId", "pros")]   # focus on the 'pros' field (positive aspects)
  colnames(data) <- c("id", "pros") 


  
# Step 3 — STM text preprocessing (paper Section 3.3) --------------------------
  
  # Average review length on raw text (before preprocessing)—paper: 14.78 words (median 8)
  pros_chr  <- as.character(data$pros)
  pros_trim <- trimws(pros_chr)
  wc <- stri_count_words(trimws(as.character(data$pros)))
  cat(sprintf("Average length: %.2f words | Median: %d | N: %d\n",
              mean(wc), median(wc), length(wc)))
  # Average length: 14.78 words | Median: 8 | N: 20188
  
  
  # Spell correction 
    # NOTE: spell correction depends on the installed hunspell dictionary version;
    # results may vary slightly across systems.
  
  # Plan for parallel processing
  plan(multisession, workers = parallel::detectCores() - 1)

  # Spell corrector that preserves original formatting
  correct_spelling_clean <- function(text) {
    # Match word-like tokens including hyphens, ampersands, etc.
    words <- str_extract_all(text, "\\b\\w[\\w\\-&']*\\b")[[1]]

    if (length(words) == 0) return(text)

    # Identify misspelled words
    misspelled <- words[!hunspell_check(words)]
    if (length(misspelled) == 0) return(text)

    # Suggest corrections
    suggestions <- sapply(misspelled, function(w) {
      s <- hunspell_suggest(w)[[1]]
      if (length(s) > 0) s[1] else w
    }, USE.NAMES = FALSE)

    # Map corrections
    correction_map <- setNames(suggestions, misspelled)

    # Use regex to replace only whole word matches (case-sensitive)
    corrected_text <- text
    for (miss in names(correction_map)) {
      corrected_text <- gsub(paste0("\\b", miss, "\\b"), correction_map[[miss]], corrected_text, perl = TRUE)
    }

    return(corrected_text)
  }

  # Ensure data$pros is character
  data$pros <- as.character(data$pros)

  # Apply in parallel
  data$pros <- future_sapply(data$pros, correct_spelling_clean, 
                             USE.NAMES = FALSE, future.seed = TRUE)

  
  # Normalize quotes, punctuation, and contractions  
    clean_text <- function(x) {
      x <- tolower(x)
      x <- gsub("’", "'", x)                     # Convert curly quotes to straight
      x <- gsub("[^a-z0-9\\s'-]", " ", x)        # Keep only basic chars, hyphens, apostrophes
      x <- gsub("\\s+", " ", x)                  # Collapse multiple spaces
      x <- gsub("\\b'[a-z]{1,2}\\b", " ", x)     # Remove contractions like 've, 'll, 're
      x <- gsub("\\b[a-z]{1,2}'\\b", " ", x)     # Rare reversed contractions like t'
      trimws(x)
    }

    data$pros <- sapply(data$pros, clean_text)


 # Detect 2+ repeated words, even with punctuation (e.g. "Good, good, good")
    has_repetition <- function(text) {
      if (is.na(text) || !nzchar(text)) return(FALSE)

      # Look for repeated word patterns using regex
      # \b(word)[\s[:punct:]]+\1\b = match same word repeated with space/punct in between
      pattern <- "\\b(\\w{2,})[\\s[:punct:]]+\\1\\b"

      # Case-insensitive matching
      return(str_detect(text, regex(pattern, ignore_case = TRUE)))
    }

    # Ensure column is character
    data$pros <- as.character(data$pros)

    # Apply and count
    repetition_flags <- sapply(data$pros, has_repetition)
    cat("Comments with repetition (2+ with punctuation):", sum(repetition_flags), "\n")
    # Comments with repetition (2+ with punctuation): 251 
    
    
    # Filter out repeated rows
    data <- data[!repetition_flags, ]
    nrow(data) 
    # [1] 19937
    

 # Define custom stopword list
    custom_stopwords <- c(
      # Modal verbs
      "can", "could", "may", "might", "must", "shall", "should", "will", "would",

      # Pronouns and possessives
      "I", "i", "me", "my", "mine", "you", "your", "yours", "yourself",
      "he", "him", "his", "himself",
      "she", "her", "hers", "herself",
      "it", "its", "itself",
      "we", "our", "ours", "ourselves",
      "they", "them", "their", "theirs", "themselves", "one",

      # Common verbs
      "do", "does", "did", "done",
      "have", "has", "had",
      "am", "is", "are", "was", "were", "be", "been", "being",

      # Common adverbs, articles, etc.
      "a", "an", "the",
      "this", "that", "these", "those",
      "also", "just", "really", "very", "some", "any",

      # Contractions and artifacts
      "don't", "youll", "like", "make", "I've", "i've",
      "ive", "im", "youre", "theyre", "weve", "whats", "cant", "wont",
      "didnt", "doesnt", "isnt", "wasnt", "werent", "arent",
      "hadnt", "hasnt", "havent", "wouldnt", "shouldnt", "couldnt",
      "lets", "thats", "theres",

      # clean_text() strips the "'nt/'s/'ve/'ll/'re/'d/'m" suffix off contractions
      # (e.g. "don't" -> "don", "doesn't" -> "doesn") before stopwords are removed,
      # so the stems below - not the forms above - are what actually appear in the text.
      "don", "doesn", "didn", "isn", "wasn", "weren", "aren",
      "hadn", "hasn", "haven", "wouldn", "shouldn", "couldn",
      "won", "let", "what", "there"
    )

  # Remove stop words (apply the custom list defined above)
    data$pros <- tolower(data$pros)
    data$pros <- removeWords(data$pros, custom_stopwords)
    data$pros <- stripWhitespace(data$pros)
    
    
 # Process with textProcessor. Besides the custom stopwords passed here, it applies
    # its defaults: lowercasing, punctuation and number removal, and removal of the
    # standard English stopword list. stem = FALSE keeps original word forms (no stemming).
  processed <- textProcessor(
    documents = data$pros,
    metadata = data,
    stem = FALSE,
    customstopwords = custom_stopwords)

    processed$docs.removed
    # [1] 4731 11625   (empty docs dropped by textProcessor)
    
    
    # Check output
    head(processed$documents, 3)     # Tokenized and cleaned documents
    head(processed$meta, 3)          # Data
    

 # # Assess minimum document-frequency threshold (Table 3)
    out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 1)
    out_5 <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 5)
    out_10 <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)
    out_15 <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 15)

    out    # "Removing 3139 of 7511 terms" → 41.8% removed, 4372 kept
    out_5  # "Removing 5288 of 7511 terms" → 70.4% removed, 2223 kept
    out_10 # "Removing 5970 of 7511 terms" → 79.5% removed, 1541 kept
    out_15 # "Removing 6325 of 7511 terms" → 84.2% removed, 1186 kept
    
 # Number of words kept
  print(length(out$vocab)) # 4372   (thresh 1)
  print(length(out_5$vocab)) # 2223 (thresh 5)
  print(length(out_10$vocab)) # 1541 (thresh 10)
  print(length(out_15$vocab)) # 1186 (thresh 15)
 
 # Select threshold = 1 and save the output object meta, documents, and vocab into variables
  docs <- out$documents
  vocab <- out$vocab
  meta <- out$meta

 # Calculate average and median length after preprocessing
  doc_lengths <- sapply(out$documents, function(doc) sum(doc[2, ]))
  cat("Effective doc length — mean:", round(mean(doc_lengths), 2),
      "| median:", median(doc_lengths), "\n")
  # mean: 8.61 | median: 6
  
  

# Step 4 — STM topic model estimation (paper Section 3.4) ----------------------

  # Search for k 
    set.seed(12345)
    options(mc.cores = detectCores() - 1)

    
  # Run model K = 5-100 (increments of 5 to K = 50, then 10) 
    model1_stm <- searchK(out$documents, out$vocab,
                          K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100),
                          data = meta,
                          max.em.its = 75,
                          verbose = FALSE)
    

    # Figure 2a: Trends in residuals, held-out likelihood, semantic coherence, and exclusivity
            # Graph metrics
            df_fine_4metrics_5_100 <- model1_stm$results[, c("K", "heldout", "residual", "semcoh", "exclus")]
            df_fine_4metrics_5_100$K <- as.numeric(df_fine_4metrics_5_100$K)
            df_fine_4metrics_5_100$semcoh <- as.numeric(df_fine_4metrics_5_100$semcoh)
            df_fine_4metrics_5_100$exclus <- as.numeric(df_fine_4metrics_5_100$exclus)
            df_fine_4metrics_5_100$heldout <- as.numeric(df_fine_4metrics_5_100$heldout)
            df_fine_4metrics_5_100$residual <- as.numeric(df_fine_4metrics_5_100$residual)

            # Shared plot theme (minimal, black)
            black_theme <- theme_minimal(base_size = 12) +
              theme(
                text = element_text(color = "black"),
                axis.text = element_text(color = "black"),
                axis.title = element_text(color = "black"),
                plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                panel.grid = element_blank(),  # Remove all grid lines
                panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
              )
    
            # Helper: line plot for one diagnostic
            
            # Set once so all panels share the same ticks
            k_min <- min(df_fine_4metrics_5_100$K, na.rm = TRUE)
            k_max <- max(df_fine_4metrics_5_100$K, na.rm = TRUE)
    
            plot_metric_5_100 <- function(metric_col, ylab, title) {
              ggplot(df_fine_4metrics_5_100, aes(x = K, y = {{ metric_col }})) +
                geom_line(color = "black") +
                geom_point(color = "black", size = 1.8) +
                labs(title = title, x = "Number of Topics (K)", y = ylab) +
                scale_x_continuous(
                  breaks = seq(k_min, k_max, by = 5),  
                  minor_breaks = NULL,                  
                  expand = c(0, 0)
                ) +
                black_theme
            }
    
    
            # Generate individual plots
            p1_5_100 <- plot_metric_5_100(residual, "Residual Dispersion", "Residuals")
            p2_5_100 <- plot_metric_5_100(heldout, "Held-out Likelihood", "Held-out Likelihood")
            p3_5_100 <- plot_metric_5_100(semcoh, "Semantic Coherence", "Semantic Coherence")
            p4_5_100 <- plot_metric_5_100(exclus, "Exclusivity", "Exclusivity")
    
    
            # Save and display combined plot
            pdf("Figure_2a_diagnostics_K5-100.pdf", width = 12, height = 10)
            grid.arrange(
              arrangeGrob(p1_5_100, p2_5_100, p3_5_100, p4_5_100, ncol = 2, widths = c(0.45, 0.45), 
                          heights = unit(c(10, 10), "cm"), padding = unit(0.4, "cm"))
              # top = textGrob("Diagnostic Values by Number of Topics", gp = gpar(fontsize = 18, fontface = "bold"))
            )
            dev.off()

        
        
    # Figure 2b: Coherence–exclusivity trade-off, K = 5–100 
          # Graph metrics  
          df <- model1_stm$results[, c("K", "exclus", "semcoh")]
          df$K <- as.numeric(df$K)
          df$K_label <- paste0("K", df$K)
          df$semcoh <- as.numeric(df$semcoh)
          df$exclus <- as.numeric(df$exclus)
          
          pdf("Figure_2b_coherence_exclusivity_K5-100.pdf", width = 10, height = 6)
          ggplot(df, aes(x = semcoh, y = exclus)) +
            geom_point(size = 4, color = "black") +
            geom_text_repel(
              aes(label = K_label),
              size = 4,
              box.padding = 0.2,
              point.padding = 0.2,
              max.overlaps = Inf,
              segment.color = "gray50"
            ) +
            
            labs(
              #  title = "Structural Topic Model Semantic Coherence-Exclusivity Plot (K: 5-100)",
              x = "Semantic Coherence",
              y = "Exclusivity"
            ) +
            theme_minimal(base_size = 14) +
            theme(
              panel.grid = element_blank(),
              panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
              plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
              axis.text = element_text(color = "black"),
              axis.title = element_text(color = "black")
            )
          
          dev.off()
          
        



  # Run model K = 10–40, increments of 1 
    model2_stm <- searchK(out$documents, out$vocab,
                          K = 10:40,
                          data = meta,
                          max.em.its = 75,
                          verbose = FALSE,
                          seed=2345678)
   
    
    # Figure 3a: Trends in residuals, held-out likelihood, semantic coherence, and exclusivity
    # Graph metrics
          df_fine_4metrics <- model2_stm$results[, c("K", "heldout", "residual", "semcoh", "exclus")]
          df_fine_4metrics$K <- as.numeric(df_fine_4metrics$K)
          df_fine_4metrics$semcoh <- as.numeric(df_fine_4metrics$semcoh)
          df_fine_4metrics$exclus <- as.numeric(df_fine_4metrics$exclus)
          df_fine_4metrics$heldout <- as.numeric(df_fine_4metrics$heldout)
          df_fine_4metrics$residual <- as.numeric(df_fine_4metrics$residual)

    # Shared plot theme (minimal, black)
          black_theme <- theme_minimal(base_size = 12) +
            theme(
              text = element_text(color = "black"),
              axis.text = element_text(color = "black"),
              axis.title = element_text(color = "black"),
              plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
              panel.grid = element_blank(),  # Remove all grid lines
              panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
            )

    # Helper: line plot for one diagnostic
          
          # Set once so all panels share the same ticks
          k_min <- min(df_fine_4metrics$K, na.rm = TRUE)
          k_max <- max(df_fine_4metrics$K, na.rm = TRUE)
  
          plot_metric <- function(metric_col, ylab, title) {
            ggplot(df_fine_4metrics, aes(x = K, y = {{ metric_col }})) +
              geom_line(color = "black") +
              geom_point(color = "black", size = 1.8) +
              labs(title = title, x = "Number of Topics (K)", y = ylab) +
              scale_x_continuous(
                breaks = seq(k_min, k_max, by = 5),  
                minor_breaks = NULL,                  
                expand = c(0, 0)
              ) +
              black_theme
          }


          # Generate individual plots
          p1 <- plot_metric(residual, "Residual Dispersion", "Residuals")
          p2 <- plot_metric(heldout, "Held-out Likelihood", "Held-out Likelihood")
          p3 <- plot_metric(semcoh, "Semantic Coherence", "Semantic Coherence")
          p4 <- plot_metric(exclus, "Exclusivity", "Exclusivity")


          # Save and display combined plot
          pdf("Figure_3a_diagnostics_K10-40.pdf", width = 12, height = 10)
          grid.arrange(
            arrangeGrob(p1, p2, p3, p4, ncol = 2, widths = c(0.45, 0.45), heights = unit(c(10, 10), "cm"), padding = unit(0.4, "cm"))
           # top = textGrob("Diagnostic Values by Number of Topics", gp = gpar(fontsize = 18, fontface = "bold"))
          )
          dev.off()
  

        
        
    # Figure 3b: Coherence–exclusivity trade-off, K = 10-40 
    # Graph metrics
          df_fine <- model2_stm$results[, c("K", "exclus", "semcoh")]
          df_fine$K_label <- paste0("K", df_fine$K)
          df_fine$semcoh <- as.numeric(df_fine$semcoh)
          df_fine$exclus <- as.numeric(df_fine$exclus)
          
          pdf("Figure_3b_coherence_exclusivity_K10-40.pdf", width = 10, height = 6)
          
          ggplot(df_fine, aes(x = semcoh, y = exclus)) +
            geom_point(size = 4, color = "black") +
            geom_text_repel(
              aes(label = K_label),
              size = 4,
              box.padding = 0.4,
              point.padding = 0.3,
              max.overlaps = Inf,
              segment.color = "gray50"
            ) +
            labs(
              #title = "STM Semantic Coherence–Exclusivity Plot (K: 10–40)",
              x = "Semantic Coherence",
              y = "Exclusivity"
            ) +
            theme_minimal(base_size = 14) +
            theme(
              panel.grid = element_blank(),
              panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
              plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
              axis.text = element_text(color = "black"),
              axis.title = element_text(color = "black")
            )
          
          dev.off()
          
     
               
          
  # Fit candidate models: K = 24, 27, 30 (10 independent runs each) 
          # The three candidates carried forward from Step 4. selectModel runs the model
          # 10 times because STM estimation is sensitive to initialization;
          # we take the first returned run (runout[[1]]). 
          model_24 <- selectModel(out$documents, out$vocab, K = 24,
                                  max.em.its = 300, data = meta, runs = 10, seed = 8458159)
          model_27 <- selectModel(out$documents, out$vocab, K = 27,
                                  max.em.its = 300, data = meta, runs = 10, seed = 8458159)
          model_30 <- selectModel(out$documents, out$vocab, K = 30,
                                  max.em.its = 300, data = meta, runs = 10, seed = 8458159)
          
          # Extract the selected run for each candidate
          selected_model_24 <- model_24$runout[[1]]
          selected_model_27 <- model_27$runout[[1]]
          selected_model_30 <- model_30$runout[[1]]       
          
          
          
      
# Step 5 — Qualitative interpretation and final coding scheme development (paper Section 3.5) -----------------------------
          
  # Top words of the three candidate models (Step 5 comparison) 
  # All four label types (prob/FREX/lift/score) per model. K=30 is the final model
  # reported in Appendix 1; topics are X#-labelled in Appendices 5/6.
        
        # Function to create a workbook and save topic labels for one model
        save_model_labels_to_excel <- function(model, K, file_name) {
          labels <- labelTopics(model, topics = 1:K, n = 20)
          topic_labels <- paste0("Topic ", 1:K)

          # Create a new workbook
          wb <- createWorkbook()

          # Loop over each label type and add a sheet
          for (type in c("prob", "frex", "lift", "score")) {
            df <- as.data.frame(labels[[type]])
            df <- cbind(Topic = topic_labels, df)
            sheet_name <- switch(type,
                                 prob = "Probability",
                                 frex = "FREX",
                                 lift = "Lift",
                                 score = "Score")

            addWorksheet(wb, sheet_name)
            writeData(wb, sheet = sheet_name, df)
          }

          # Save workbook
          saveWorkbook(wb, file_name, overwrite = TRUE)
          cat("Saved:", file_name, "\n")
        }
          
        # Generate all three Excel files
        save_model_labels_to_excel(selected_model_24, 24, "stm_labels_K24.xlsx")
        save_model_labels_to_excel(selected_model_27, 27, "stm_labels_K27.xlsx")
        save_model_labels_to_excel(selected_model_30, 30, "stm_labels_K30.xlsx") # Appendix 1


        

  # Save top 10 most representative comments for each topic
  # NOTE: exports verbatim review text. Requires the raw review file.

        # Named vector: reviewId -> original (pre-cleaning) pros text.
        # Defined here (rather than only later, near doc_topic_proportions_30)
        # so it's available for the verbatim quote export below.
        original_pros_map <- setNames(as.character(filtered_df$pros), filtered_df$reviewId)

        # Helper function to save quotes with IDs to Excel
        save_topic_quotes_with_ids <- function(model, K, meta, original_pros_map) {
          wb <- createWorkbook()

          for (topic_num in 1:K) {
            # texts = meta$pros only supplies what findThoughts ranks/selects on
            # (meta$pros is what the model was fit on); the actual exported quote
            # is looked up from original_pros_map so it reads as a real sentence,
            # not the cleaned/stopword-stripped training text.
            thoughts_result <- findThoughts(model, texts = meta$pros, topics = topic_num, n = 10)
            indices <- thoughts_result$index[[1]]

            # Replace 'id' if metadata uses a different column name
            doc_ids <- meta$id[indices]
            quotes <- original_pros_map[as.character(doc_ids)]

            df <- data.frame(ID = doc_ids, Quote = quotes, stringsAsFactors = FALSE)
            sheet_name <- paste0("Topic_", topic_num)

            addWorksheet(wb, sheet_name)
            writeData(wb, sheet = sheet_name, df)
          }

          # Save the workbook
          file_name <- paste0("stm_topic_quotes_with_ids_k", K, ".xlsx")
          saveWorkbook(wb, file_name, overwrite = TRUE)
          cat("Saved:", file_name, "\n")
        }

        # Run for all three models
        save_topic_quotes_with_ids(selected_model_24, 24, meta, original_pros_map)
        save_topic_quotes_with_ids(selected_model_27, 27, meta, original_pros_map)
        save_topic_quotes_with_ids(selected_model_30, 30, meta, original_pros_map)


        
  # Document–topic (theta) matrix for K=30
        # Per-document topic proportions + IDs + text. Feeds Step 6: construct-level
        # scores are formed by summing the proportions of STM topics mapped to each
        # construct, to identify candidate documents for the labeled dataset.
        # NOTE: includes original verbatim reviews (original_pros) — requires the raw
        # file.
        
        
        # Recover the original ID/text for each row of theta.
        # STM drops documents at TWO stages, so theta has 19,932 rows, not 20,188.
        # To realign IDs with theta, replay the SAME removals, IN ORDER, on the
        # id-bearing data frame:
        #   funnel: 20,188 (filtered) -> 19,937 (repeated-word removal) -> 19,932 (STM)
        #   removed_docs1 = dropped by textProcessor   (indices into the 19,937 set)
        #   removed_docs2 = dropped by prepDocuments    (indices into the POST-textProcessor set)
        # Order matters: removed_docs2 is relative to the set AFTER docs1 are gone,
        # so remove docs1 first, THEN docs2 — never combine them in one c() step.
        
        # Sanity check — document counts through the pipeline
        nrow(filtered_df) # [1] 20188  (after dedup + office-role filter)
        nrow(data) # [1] 19937  (after repeated-word removal)
        length(meta$pros) # 19932   (after STM: textProcessor + prepDocuments)
       
         # Make a copy
        data_with_id <- data
        
        # # Document–topic proportions (K = 30)
        theta30 <- selected_model_30$theta
        dim(theta30) # 19932 x 30  (one row per surviving document — matches meta)

        # Identify the removed documents in the pre-analysis step
        removed_docs1 <- processed$docs.removed # these were the first documents removed with preprocess function
        removed_docs2 <- out$docs.removed  # indices removed based on lower.threshold

        # Step 1: Remove documents filtered out in the first preprocessing step
        data_fil <- data_with_id[-removed_docs1, ]

        # Step 2: Remove documents filtered out in `prepDocuments()` (lower.thresh filtering)
        data_fil2 <- data_fil[-removed_docs2, ]  # Final filtered dataset
        nrow(data_fil2)
        # [1] 19932   (matches meta and theta30)


        # Create a dataset with ID, pros and proportions
        doc_topic_proportions_30 <- data.frame(
          doc_id = data_fil2$id,
          doc_text = meta$pros,
          theta30,
          stringsAsFactors = FALSE
          )


      # Get the original reviews without grammar correction

      # Lookup the original pros for each doc_id (original_pros_map defined above,
      # near save_topic_quotes_with_ids)
      doc_topic_proportions_30$original_pros <- original_pros_map[as.character(doc_topic_proportions_30$doc_id)]

      # Reorder columns: place original_pros right after doc_text
      doc_topic_proportions_30 <- doc_topic_proportions_30 %>%
        relocate(original_pros, .after = doc_text)

      # Save the full data to an Excel file
      write.xlsx(
        doc_topic_proportions_30,
        file = "doc_topic_proportions_k30_with_original_pros.xlsx",
        overwrite = TRUE
      )

      cat("Excel file saved: doc_topic_proportions_k30_with_original_pros.xlsx\n")


      
      
      
# Validating stability (Appendix 6 / Panels A & B, paper Section 3.5.2.)---------------  
      
      # Appendix 6 — Test 1: sensitivity to document length (Panel A)-----------------
      # Re-estimate STM at minimum-token thresholds (3, 5, 8), K fixed at 30, so any
      # change reflects the length filter, not a change in K.
      # Settings and helper functions
      K_star <- 30
      prev_formula    <- NULL
      content_formula <- NULL
      seed <- 123
      min_tokens_set <- c(3,5,8)
      
      
      # PRE-filter at textProcessor() stage, THEN prepDocuments (rebuilds IDs to 1..V)
      prep_minlen_from_processed <- function(processed, lower_thresh_vocab = 1, minlen = 0) {
        # raw token counts from textProcessor output (sum of row 2 = per-type counts;
        # length() alone would count matrix cells = 2x unique word TYPES, not tokens)
        raw_lengths <- vapply(processed$documents,
                              function(x) if (length(x)) sum(x[2, ]) else 0L, integer(1))
        keep_idx <- which(raw_lengths >= minlen & raw_lengths > 0)
        
        # subset BEFORE prep
        proc_sub <- list(
          documents = processed$documents[keep_idx],
          vocab     = processed$vocab,
          meta      = processed$meta[keep_idx, , drop = FALSE]
        )
        
        # rebuild indices after filtering
        pr <- prepDocuments(proc_sub$documents, proc_sub$vocab, proc_sub$meta,
                            lower.thresh = lower_thresh_vocab)
        
        # lengths after prep (for reporting)
        doc_lengths <- vapply(pr$documents, function(x) if (length(x)) sum(x[2,]) else 0L, integer(1))
        
        list(
          documents   = pr$documents,
          vocab       = pr$vocab,
          meta        = pr$meta,
          doc_lengths = doc_lengths
        )
      }
      
      check_sequential <- function(docs, V) {
        idxs <- unique(unlist(lapply(docs, function(x) if (length(x)) x[1,] else integer(0))))
        length(idxs) == 0 || (min(idxs) == 1 && max(idxs) <= V)
      }
      
      # --- Helper: fit STM with optional prevalence/content --- 
      fit_stm_fixedK <- function(docs, vocab, meta, K_star, prev_formula = NULL, 
                                 content_formula = NULL, seed = 123)
      { 
        args <- list( documents = docs, vocab = vocab, K = K_star, data = meta, 
                      init.type = "Spectral", seed = seed ) 
        if (!is.null(prev_formula)) args$prevalence <- prev_formula 
        if (!is.null(content_formula)) args$content <- content_formula 
        do.call(stm, args) 
        
      }
      
      
      # --- Diagnostics ---
      model_diagnostics <- function(model, documents) {
        ho <- make.heldout(documents = documents, vocab = model$vocab)
        ev <- eval.heldout(model, ho$missing)
        sc <- semanticCoherence(model, documents)
        ex <- exclusivity(model)
        list(heldout = ev$expected.heldout, mean_sc = mean(sc), mean_ex = mean(ex))
      }
      
      # --- Baseline (minlen = 0) from processed ---
      base <- prep_minlen_from_processed(processed, lower_thresh_vocab = 1, minlen = 0)
      stopifnot(check_sequential(base$documents, length(base$vocab)))
      set.seed(seed)
      mod_base  <- fit_stm_fixedK(base$documents, base$vocab, base$meta,
                                  K_star, prev_formula, content_formula, seed)
      diag_base <- model_diagnostics(mod_base, base$documents)
      
      
      # --- Robustness loop: min-tokens thresholds ---
      fits <- list(base = mod_base)
      diags <- list(base = diag_base)
      retention <- tibble(min_tokens = 0,
                          n_docs = length(base$documents),
                          median_tokens = median(base$doc_lengths),
                          retained_share = 1)
      
      for (mt in min_tokens_set) {
        dat <- prep_minlen_from_processed(processed, lower_thresh_vocab = 1, minlen = mt)
        stopifnot(check_sequential(dat$documents, length(dat$vocab)))
        set.seed(seed)
        mod <- fit_stm_fixedK(dat$documents, dat$vocab, dat$meta,
                              K_star, prev_formula, content_formula, seed)
        fits[[paste0("min", mt)]]  <- mod
        diags[[paste0("min", mt)]] <- model_diagnostics(mod, dat$documents)
        retention <- bind_rows(
          retention,
          tibble(min_tokens = mt,
                 n_docs = length(dat$documents),
                 median_tokens = median(dat$doc_lengths),
                 retained_share = length(dat$documents) / length(base$documents))
        )
      }
      
      # Retention: documents kept at each min-token threshold (Panel A)
      retention
      # min_tokens 0 / 3 / 5 / 8  ->  
      # 19932 / 19893 / 19387 / 17320 docs  -> 
      # (100 / 99.8 / 97.3 / 86.9)
      
      
      
      # Diagnostics: held-out likelihood, mean coherence, mean exclusivity (Panel A)
      diag_tbl <- purrr::imap(diags, ~ tibble(
        spec             = .y,
        heldout          = .x$heldout,
        mean_coherence   = .x$mean_sc,
        mean_exclusivity = .x$mean_ex
      )) |> purrr::list_rbind()
      diag_tbl
      # base/min3/min5/min8 -> 
      # heldout -5.94/-5.29/-10.1/-9.63 | 
      # coh -193/-202/-193/-190 | 
      # excl 9.90/9.90/9.90/9.91
      
      
      
      # Stability vs. baseline — three dimensions (Panel A) 
      # lexical: RBO (rank-biased overlap) | distributional: cosine(β) | prevalence: cor(θ)
      # RBO (Webber et al., 2010): top-heavy similarity of ranked top-word lists.
      
      # RBO for two ranked top-k lists (finite version)
      rbo_score <- function(a, b, p = 0.9) {
        a <- as.character(a); b <- as.character(b)
        k <- max(length(a), length(b))
        if (k == 0) return(0)
        seen_a <- character(0); seen_b <- character(0)
        overlap <- numeric(k)
        for (i in seq_len(k)) {
          if (i <= length(a)) seen_a <- c(seen_a, a[i])
          if (i <= length(b)) seen_b <- c(seen_b, b[i])
          overlap[i] <- length(intersect(seen_a, seen_b))
        }
        weights <- (1 - p) * p^(seq_len(k) - 1)
        sum(weights * (overlap / seq_len(k)))
      }
      
      
      # Top-n terms per topic, as a list of character vectors (prob or frex ranking)
      top_terms_matrix <- function(model, n, which = c("prob", "frex")) {
        which <- match.arg(which)
        labs <- labelTopics(model, n = n)
        mat  <- if (which == "prob") labs$prob else labs$frex
        split(mat, seq_len(nrow(mat))) |> lapply(as.character)
      }
      
      # Align topics of two models by RBO, then compute RBO, prevalence cor, cosine(β)
      align_and_metrics <- function(modelA, modelB,
                                    exclude = integer(), n_top = 30, which = "frex",
                                    p = 0.9, min_common_vocab = 50) {
        
        # Total topics and keep-set after exclusion (original indices)
        K_tot <- nrow(labelTopics(modelA, n = 1)$frex)
        stopifnot(K_tot == nrow(labelTopics(modelB, n = 1)$frex))
        keep <- setdiff(seq_len(K_tot), exclude)
        if (length(keep) == 0) stop("After exclusion no topics remain.")
        
        # Top terms per topic
        A_terms_all <- top_terms_matrix(modelA, n_top, which)
        B_terms_all <- top_terms_matrix(modelB, n_top, which)
        A_terms <- A_terms_all[keep]; B_terms <- B_terms_all[keep]
        
        # Build RBO similarity matrix on the kept topics
        K <- length(keep)
        R <- matrix(0, K, K)
        for (i in seq_len(K)) for (j in seq_len(K)) {      # <-- RESTORED loop
          R[i, j] <- rbo_score(A_terms[[i]], B_terms[[j]], p = p)
        }
        
        # Optimal assignment using RBO (top-heavy, robust for short texts)
        assign_red <- solve_LSAP(R, maximum = TRUE)
        
        # Map reduced indices back to ORIGINAL topic indices
        mapA <- keep
        mapB <- keep[assign_red]
        
        # Mean RBO across matched topics
        mean_rbo <- mean(R[cbind(seq_len(K), assign_red)])
        
        # Global prevalence correlation across matched topics
        prevA <- colMeans(modelA$theta)[mapA]
        prevB <- colMeans(modelB$theta)[mapB]
        prev_cor <- suppressWarnings(cor(prevA, prevB, use = "pairwise.complete.obs"))
        
        # Mean cosine on β over COMMON vocabulary only
        vocabA <- modelA$vocab; vocabB <- modelB$vocab
        common <- intersect(vocabA, vocabB)
        mean_cos <- NA_real_
        if (length(common) >= min_common_vocab) {
          idxA <- match(common, vocabA)
          idxB <- match(common, vocabB)
          betaA <- exp(modelA$beta$logbeta[[1]])[, idxA, drop = FALSE]
          betaB <- exp(modelB$beta$logbeta[[1]])[, idxB, drop = FALSE]
          cos_vals <- mapply(function(iA, iB) {
            v1 <- betaA[iA, ]; v2 <- betaB[iB, ]
            denom <- sqrt(sum(v1 * v1)) * sqrt(sum(v2 * v2))
            if (denom == 0) NA_real_ else sum(v1 * v2) / denom
          }, mapA, mapB)
          mean_cos <- mean(cos_vals, na.rm = TRUE)
        }
        
        tibble(
          mean_rbo            = mean_rbo,
          prevalence_cor      = as.numeric(prev_cor),
          mean_cosine_beta    = mean_cos,
          matched_topics_kept = K
        )
      }
      
    
      # Stability of matched topics vs. baseline (Panel A) 
      # Match each trimmed model's topics to baseline by RBO, then score RBO,
      # prevalence correlation (θ) and cosine(β). Ranking = prob, top-30 terms.
      
      exclude_topics <- c()   # no topics excluded
      
      stability_tbl <- purrr::map(names(fits)[-1], function(nm) {
        align_and_metrics(fits$base, fits[[nm]],
                          exclude = exclude_topics,
                          n_top = 30, which = "prob",
                          p = 0.6, min_common_vocab = 50) |>
          tibble::add_column(spec = nm, .before = 1)
      }) |> purrr::list_rbind()
      
      stability_tbl
      # spec  mean_rbo  prevalence_cor  mean_cosine_beta  matched_topics
      # min3    0.544       0.926           0.630              30
      # min5    0.344       0.731           0.383              30
      # min8    0.332       0.725           0.419              30
      
    
      
      

 # Appendix 6 — Test 2: aggregation-based stability (Panel B) --------------------
 # Pool ultra-short (<= 8-token) reviews within the same day into ~15- and ~20-token
 # pseudo-documents, refit STM (K = 30), and compare to the baseline model.
 # Depends on Test 1: mod_base, base, align_and_metrics, seed, K_star, *_formula.
      
      # Attach review date to the preprocessed corpus, restrict to processed IDs
      filtered_df_clean <- filtered_df %>% distinct(reviewId, .keep_all = TRUE)
      data_with_date <- data %>%
        left_join(filtered_df_clean %>% select(reviewId, reviewDateTime),
                  by = c("id" = "reviewId")) %>%
        semi_join(processed$meta %>% select(id), by = "id")
      
      # Parse timestamp
      data_parsed <- data_with_date %>%
        mutate(
          reviewDateTime = gsub("Z$", "", reviewDateTime),
          reviewDateTime = parse_date_time(reviewDateTime,
                                           orders = c("ymd_HMS","ymd_HMSOS","ymd_HM","ymd"),
                                           exact = FALSE),
          has_time = !is.na(reviewDateTime)
        )
      
      # Fixed settings
      TIME_GRAIN       <- "day"
      SHORT_MAX_TOKENS <- 8
      TEXT_COL         <- "pros"
      
      # Target-independent pieces: annotate, keep long docs 1:1, hold aside undated docs
      with_time <- data_parsed %>%
        filter(has_time) %>%
        mutate(
          time_bucket = floor_date(reviewDateTime, unit = TIME_GRAIN),
          txt         = as.character(.data[[TEXT_COL]]),
          n_tokens    = stri_count_words(replace_na(txt, "")),
          is_short    = n_tokens <= SHORT_MAX_TOKENS
        ) %>%
        arrange(reviewDateTime)
      
      no_time_docs <- data_parsed %>%
        filter(!has_time) %>%
        transmute(time_bucket = as.POSIXct(NA), text = .data[[TEXT_COL]], n_reviews = 1L)
      
      long_docs <- with_time %>%
        filter(!is_short) %>%
        transmute(time_bucket, text = txt, n_reviews = 1L)
      
      # Aggregate + refit + stability metrics for each target batch size
      panel_b <- purrr::map(c(15, 20), function(target) {
        
        short_docs_batched <- with_time %>%
          filter(is_short) %>%
          group_by(time_bucket) %>%
          mutate(cum_tokens = cumsum(n_tokens),
                 batch_id   = floor((cum_tokens - 1) / target)) %>%
          ungroup() %>%
          group_by(time_bucket, batch_id) %>%
          summarise(text = paste(txt, collapse = " "), n_reviews = n(), .groups = "drop") %>%
          select(time_bucket, text, n_reviews)
        
        aggregated_data <- bind_rows(long_docs, short_docs_batched, no_time_docs) %>%
          arrange(is.na(time_bucket), time_bucket) %>%
          mutate(doc_id = row_number(),
                 n_tokens = stri_count_words(replace_na(text, "")))
        
        # STM on aggregated corpus (cleaning off — text already processed)
        tp_agg <- textProcessor(aggregated_data$text, metadata = aggregated_data,
                                lowercase = FALSE, removestopwords = FALSE,
                                removenumbers = FALSE, removepunctuation = FALSE,
                                stem = FALSE, verbose = FALSE)
        prep_agg <- prepDocuments(tp_agg$documents, tp_agg$vocab, tp_agg$meta)
        stopifnot(check_sequential(prep_agg$documents, length(prep_agg$vocab)))
        
        set.seed(seed)
        fit_agg <- stm(prep_agg$documents, prep_agg$vocab, K = K_star,
                       prevalence = prev_formula, content = content_formula,
                       init.type = "Spectral", verbose = FALSE)
        
        # Stability vs baseline
        res_agg <- align_and_metrics(mod_base, fit_agg, exclude = integer(),
                                     n_top = 30, which = "prob", p = 0.7,
                                     min_common_vocab = 50)
        
        # Quality diagnostics
        mean_coh_agg  <- mean(semanticCoherence(fit_agg, prep_agg$documents), na.rm = TRUE)
        mean_excl_agg <- mean(as.numeric(exclusivity(fit_agg)), na.rm = TRUE)
        
        held_agg <- make.heldout(prep_agg$documents, vocab = prep_agg$vocab)
        set.seed(seed)
        fit_agg_held <- stm(held_agg$documents, held_agg$vocab, K = K_star,
                            prevalence = prev_formula, content = content_formula,
                            init.type = "Spectral", verbose = FALSE)
        heldout_ll_agg <- eval.heldout(fit_agg_held, held_agg$missing)$expected.heldout
        
        tibble::tibble(
          target_tokens       = target,
          docs_after          = nrow(aggregated_data),
          retained_share      = nrow(aggregated_data) / length(base$documents),
          heldout_likelihood  = heldout_ll_agg,
          mean_coherence      = mean_coh_agg,
          mean_exclusivity    = mean_excl_agg,
          mean_rbo            = res_agg$mean_rbo,
          prevalence_cor      = res_agg$prevalence_cor,
          mean_cosine_beta    = res_agg$mean_cosine_beta,
          matched_topics_kept = res_agg$matched_topics_kept
        )
      }) |> purrr::list_rbind()
      
      print(panel_b, width = Inf)
      # target 15 -> docs 12709 (63.8%) | coh -154 | excl 9.82 | RBO 0.33 | cos 0.45 | θ 0.81
      # target 20 -> docs 11545 (57.9%) | coh -142 | excl 9.78 | RBO 0.33 | cos 0.48 | θ 0.80
      
      
      
      
      
      
# Appendix 6 — Test 3: export corpus for BERTopic triangulation (Panel C) ------

  # BERTopic is run separately in Python (bertopic_triangulation.ipynb). 
  # We export data$pros — the review text after Step 3 preprocessing
  # (spell correction, lowercasing, punctuation/contraction normalization,
  # repeated-word removal, custom stop-word removal): the exact strings fed to
  # textProcessor(). BERTopic then applies its own vectorizer on top.

      # Docs STM actually used, in STM's order
      ids_stm <- out$meta$id
      
      base_export <- data %>%
        semi_join(tibble(id = ids_stm), by = "id") %>%   # same document set
        mutate(pros = as.character(pros)) %>%
        slice(match(ids_stm, id)) %>%                    # identical order to STM
        transmute(doc_id = row_number(), id, text = pros)
 
      # Write for the Python/BERTopic notebook (path relative to the project root)
      write.csv(base_export, "texts_for_bertopic_base.csv", row.names = FALSE)
      saveRDS(base_export, "texts_for_bertopic_base.rds")   # for mapping back later
      
      
      
  
      
      
# Metadata associations (Appendix 7 / Figure A7, paper Section 3.5.4)-----------
# Two-class comparison: 2023 (incl. early-Jan 2024 spillover) vs. years <= 2022.

      # Create the data set by adding the variable reviewDateTime to the datafile
      # First, get one row per reviewId in comments
      comments_unique <- comments %>%
        select(reviewId, reviewDateTime) %>%
        distinct(reviewId, .keep_all = TRUE)

      # Rename reviewId to id to match meta
      comments_unique <- comments_unique %>%
        rename(id = reviewId)

      # Now perform a safe left join
      meta_new <- meta %>%
        left_join(comments_unique, by = "id")

      # Binning reviewDateTime
      # Step 1: Convert reviewDateTime to Date or Date-Time
      meta_new$reviewDateTime <- as.POSIXct(meta_new$reviewDateTime)

      #Step 2: Create Time-Based Bins e.g. by year
      meta_new$year <- year(meta_new$reviewDateTime)
      table(meta_new$year)

      meta_new$year_bin <- dplyr::case_when(
        meta_new$year <= 2022 ~ "Earlier time period",
        meta_new$year == 2023 ~ "2023",
        meta_new$year == 2024 ~ "2023" # small spillover from early Jan 2024 (from 1 January - 17 January 2024)
      )

      meta_new$year_ <- factor(meta_new$year_bin, levels = c("Earlier time period", "2023"))

      #Step 3: Use in estimateEffect - year as categorical
      # (it is evaluating statistical differences between years (regression))
      K = 30
      prep <- estimateEffect(1:K ~ year_, stmobj = selected_model_30,
                             metadata = meta_new, documents = out$documents)
      summary(prep)

      # Create Excel workbook
      wb <- createWorkbook()

      # Extract coefficients for each topic
      summary_prep <- summary(prep)
      coef_list <- lapply(summary_prep$tables, as.data.frame)


      # Loop through each topic
      for (i in seq_along(coef_list)) {
        topic_name <- paste("Topic", i)

        # Format the data
        topic_data <- data.frame(
          Covariate = rownames(coef_list[[i]]),
          coef_list[[i]],
          row.names = NULL
        )
        colnames(topic_data) <- c("Covariate", "Estimate", "Std. Error", "t-value", "p-value")

        # Add significance stars
        topic_data$Significance <- ifelse(topic_data$`p-value` < 0.01, "***",
                                          ifelse(topic_data$`p-value` < 0.05, "**",
                                                 ifelse(topic_data$`p-value` < 0.1, "*", "")))

        # Add sheet to Excel
        addWorksheet(wb, topic_name)
        writeData(wb, topic_name, topic_data)
      }   

        # Save Excel workbook AFTER the loop
        saveWorkbook(wb, "coefficients_with_significance_2_classes.xlsx", overwrite = TRUE)


        # Assemble significant temporal effects (one row per significant topic)
        all_sig_data <- data.frame()

        for (i in seq_along(coef_list)) {
          topic_data <- data.frame(
            Topic = paste0("Topic_", i),
            Covariate = rownames(coef_list[[i]]),
            coef_list[[i]],
            row.names = NULL
          )

        colnames(topic_data) <- c("Topic", "Covariate", "Estimate", "Std. Error", "t-value", "p-value")
        topic_data$CI_lower <- topic_data$Estimate - 1.96 * topic_data$`Std. Error`
        topic_data$CI_upper <- topic_data$Estimate + 1.96 * topic_data$`Std. Error`

        # Keep only significant, non-intercept results
        sig_data <- subset(topic_data, Covariate != "(Intercept)" & `p-value` < 0.05)

        if (nrow(sig_data) > 0) {
          sig_data$TopicNum <- i
          all_sig_data <- rbind(all_sig_data, sig_data)
          }
        } 

        all_sig_data$Direction <- ifelse(all_sig_data$Estimate > 0, "Positive", "Negative")
        
        # X# labels, ordered numerically (significant topics only)
        lev <- paste0("X", sort(unique(all_sig_data$TopicNum)))
        all_sig_data$TopicLabel <- factor(paste0("X", all_sig_data$TopicNum), levels = lev)
        
        write.csv(all_sig_data, file = "all_significant_coefficients_2CLASSES.csv", 
                  row.names = FALSE)
      

 # Figure A7 — topic prevalence, 2023 vs. <= 2022 -------------------------
      lab_dir <- c("Positive" = "Higher in 2023", "Negative" = "Lower in 2023")
      gg_combined <- ggplot(all_sig_data, aes(x = Estimate, y = TopicLabel)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
        geom_point(aes(color = Direction, fill = Direction, shape = Direction, size = abs(Estimate))) +
        geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.3, alpha = 0.4) +
        scale_color_manual(name = "Change in 2023",
                           values = c("Positive" = "steelblue", "Negative" = "firebrick"),
                           labels = lab_dir) +
        scale_fill_manual (name = "Change in 2023",
                           values = c("Positive" = "steelblue", "Negative" = "firebrick"),
                           labels = lab_dir) +
        scale_shape_manual(name = "Change in 2023",
                           values = c("Positive" = 24, "Negative" = 25),   
                           labels = lab_dir) +
        scale_size_continuous(range = c(2, 6), guide = "none") +
        labs(
          x = "Estimated difference, 2023 vs. ≤ 2022",
          y = "Topic"
        ) +
        theme_classic(base_size = 14) +
        theme(
          axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 11),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        )
      
      ggsave("Figure_A7_topic_prevalence_by_year.png",
             plot = gg_combined, width = 14, height = 10, dpi = 300, bg = "white")
      
    
      
      
      

# Step 6 - Labeled dataset split------------------------------------------------
# In the construct-guided approach, the manually coded subsample from 
# Step 2—now organized at the construct level via the coding scheme 
# finalized in Step 5 and enlarged by purposive sampling, is split 80/20 into the
# training and test sets used by the BERT notebook (Stage 2).
      
    df <- read_excel("data/labeled_dataset.xlsx")  # user-supplied; columns: doc_id, pros, 13 binary construct labels
    str(df)                                        # 657 obs. of 15 variables
      
    
    # Create stratified split based on dominant_label
      df$dominant_label <- apply(df[, 3:15], 1, function(x) {
        if (sum(x) == 0) {
          return("None")  # in case there are unlabeled rows
        } else {
          return(names(df[, 3:15])[which.max(x)])
        }
      })
      
      set.seed(42)
      train_index <- createDataPartition(df$dominant_label, p = 0.8, list = FALSE)
      
      # Split the data
      train_data <- df[train_index, ]
      test_data <- df[-train_index, ]
      
      # Drop the helper column 
      train_data$dominant_label <- NULL
      test_data$dominant_label <- NULL
      
      # Check the distributions
      # Function to compute proportion of 1s per label column
      label_proportion <- function(data, label_cols) {
        colSums(data[, label_cols]) / nrow(data)
      }
      
      # Define label columns
      label_cols <- c("Control", "Skill", "Career", "Externally generated goals",
                      "Work life balance", "Variety", "Clarity", "Money",
                      "Physical setting and security", "People", "Supervision",
                      "Significance", "Others")
      
      # Note: the label column Externally generated goals corresponds to the 
      # construct Job-Specific Demands in the paper — the column retains the 
      # name of Warr's umbrella category, whose two components 
      # (Job-Specific Demands and Work–Life Balance) are treated as separate 
      # constructs (see Appendix 3).
      
      # Get proportions
      original_prop <- label_proportion(df, label_cols)
      train_prop <- label_proportion(train_data, label_cols)
      test_prop <- label_proportion(test_data, label_cols)
      
      # Combine into one data frame (reported as Table 4)
      proportion_df <- data.frame(
        Label = label_cols,
        Original = round(original_prop, 3),
        Train = round(train_prop, 3),
        Test = round(test_prop, 3)
      )
      print(proportion_df)
      
      # Positive counts for the constructs enlarged by purposive sampling
      # (Section 3.6: 40-positive operational minimum -> ~32 expected in training)
      table(train_data$Control)
      table(train_data$Variety)
      table(train_data$Clarity)
      
      # Save train and test sets to CSV files (inputs to the BERT notebook)
      write.csv(train_data, "train_data.csv", row.names = FALSE)
      write.csv(test_data, "test_data.csv", row.names = FALSE)
      
      
      
# Session information ----------------------------------------------------------
# Record package versions for reproducibility. 
# The output is in the README file.
  sessionInfo()
