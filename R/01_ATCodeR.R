#' Transform free-text substances into ATC Codes
#'
#' @param input_df A data frame containing the column specified by `column_name`.
#' @param column_name The name of the column in `input_df` that contains free-text substances.
#' @return A data frame with the transformed data(focus on antineoplastic medication) and additional columns.
#' @export

ATCtransform <- function(input.df, column_name) {
  # Load necessary libraries (assuming they are already installed)
  library(tidyr)
  library(tidyverse)
  library(quanteda)

  # Replace Umlaute function
  repl_chars <- c("ä", "ö", "ü", "ß", "Ä", "Ö", "Ü")
  replacements <- c("ae", "oe", "ue", "ss", "AE", "OE", "UE")

  # Apply replacements
  for (i in 1:length(repl_chars)) {
    input.df[[column_name]] <- stringr::str_replace_all(input.df[[column_name]], repl_chars[i], replacements[i])
  }

  # Create tokens while removing punctuation and symbols, and splitting hyphens
  toks <- quanteda::tokens(input.df[[column_name]], remove_punct = TRUE, remove_symbols = TRUE, split_hyphens = TRUE)

  # Define the vectors for digits, stingle letters, and bad tokens
  four_digit_numbers <- as.character(00:9999)
  single_letters <- c(letters, LETTERS)
  bad_tokens <- c(single_letters, four_digit_numbers, "antimetabolit", "bolus", "und", "wait", "see",
                  "auc5", "watch", "wee", "auc6", "liposomales", "immunsuppressiva",
                  "immunsuppressivum", "proteinkinaseinhibitor", "endokrine", "antimetabolit",
                  "d", "immunsuppressivum", "unbekannte", "und", "mit", "arm", "i", "ii",  "null",
                  "nach", "weekly", "dosis", "dosisdicht", "low", "n.n.bez", "antikoerper",
                  "taegl", "bez", "targeted", "teil", "inhibitoren", "nul", "elderly",
                  "haut", "mono", "kg", "mg", "in", "iv", "armb", "o.n.a", "zusatzmedikation", "and", "Sonstige")

  # Replace bad tokens
  toks <- quanteda::tokens_remove(toks, pattern = bad_tokens)

  # Convert to upper case
  toks <- quanteda::tokens_toupper(toks)

  ######################################
  # Load regime and abbreviations dictionary
  dictionary_path <- system.file("extdata", "RA_dictionary_v2.cat", package = "ATCodeR")
  dict_RA <- readRDS(dictionary_path)

  # Apply regime and abbreviations dictionary to token object
  dict_toks <- quanteda::tokens_lookup(toks, dictionary = dict_RA, levels = 2, exclusive = FALSE, capkeys = TRUE)

  # Function to remove duplicates
  remove_duplicate_tokens <- function( dict_toks) {
    tokens(lapply( dict_toks, function(x) {
      unique(x)
    }))
  }

  # Apply the function to remove duplicates
  dict_toks <- remove_duplicate_tokens(dict_toks)

  # Convert into a vector
  subs1 <- as.character(unlist(lapply(dict_toks, paste, collapse = " ")))

  # Replace hyphens with underscores
  subs1 <- gsub("-", "_", subs1)

  # Transfer back to token
  ##changes
  toks <- quanteda::tokens(subs1)

  # Run the dictionary look up a second time
  toks <- quanteda::tokens_lookup(toks, dictionary = dict_RA, levels = 2, exclusive = FALSE, capkeys = TRUE)

  # For counting translations later on
  toks_forcount <- toks
  ##  regime and abbreviations dictionary completed ##

  ######################################

  # Load single substance dictionary
  dictionary_path2 <- system.file("extdata", "atc_substance_dictionary_v2.cat", package = "ATCodeR")
  quanteda_dict  <- readRDS(dictionary_path2)

  # Apply single substance dictionary so each medication gets an ATC code
  subs2 <- quanteda::tokens_lookup(toks, dictionary = quanteda_dict, levels = 5, exclusive = FALSE, case_insensitive = TRUE, capkeys = TRUE)

  # Alphabetical order
  toks <- lapply(quanteda::tokens(subs2), sort)

  # Unlist and add ATC substance column
  subs3 <- lapply(toks, function(x) paste(x, collapse = " "))

  input.df[[paste0(column_name, "_processed")]] <- unlist(subs3)

  # Check if all columns are in the dataframe
  print(names(input.df))

  ######################################
  # Counting words per row
  word_counts <- str_count(input.df[[paste0(column_name, "_processed")]], "\\S+")

  # Find maximum word count and separate each entry into a separate column
  max_word_count <- max(word_counts, na.rm = TRUE)

  input.df <- input.df %>%
    tidyr::separate(
            col = paste0(column_name, "_processed"),
             into = paste0("word", 1:max_word_count),
             sep = "\\s+",
             extra = "merge",
             fill = "right")

  # Function to replace multiple translation for the same word. Keeping the one that starts with L, or if not available the first translation
  process_row <- function(row) {
    # Extracting the part after the "_"
    parts <- sapply(strsplit(row, "-"), function(x) x[2])

    # Finding the identical part
    unique_parts <- unique(parts)

    for (part in unique_parts) {
      indices <- which(parts == part)

      if (length(indices) > 1) {
        l_index <- indices[startsWith(row[indices], "L")]
        if (length(l_index) > 0) {
          keep_index <- l_index[1]
        } else {
          keep_index <- indices[1]
        }

        # Setze alle anderen Indizes auf NA
        indices <- setdiff(indices, keep_index)
        row[indices] <- NA
      }
    }
    return(row)
  }

  # Apply Function for every row so every medication is only translated once, if available with a substance code from section "L"
  input.df <- as.data.frame(t(apply(input.df, 1, process_row)))

  # Choose only one translation for ATG / L04AA04 / L04AA03
  process_column <- function(column) {
    sapply(column, function(x) {
      if (grepl("L04AA04-ANTITHYMOZYTAERES", x) && grepl("L04AA03-ANTILYMPHOZYTAERES", x)) {
        return(gsub("L04AA04-ANTITHYMOZYTAERES", "NA", x))  # Replace with NA
      } else {
        return(x)
      }
    })
  }

  # Apply the function to every column in the DataFrame
  input.df[] <- lapply(input.df, process_column)

  # Sort all Antineoplastic substances in one column
  # Function to check if a string starts with "L" and contains "-"
  starts_with_L_and_contains_dash <- function(x) {
    grepl("^L.*-.*", x)
  }

  # Function to check if a string does not start with "L" and contains "-"
  not_starts_with_L_and_contains_dash <- function(x) {
    grepl("^[^L].*-.*", x)
  }

  # Combine values based on the content criteria, skipping the first column
  input.df <- input.df %>%
    rowwise() %>%
    mutate(
      `AntineoplasticMedication` = sort(paste(c_across(2:ncol(input.df))[sapply(c_across(2:ncol(input.df)), starts_with_L_and_contains_dash)], collapse = ", ")),
      `AdditionalMedication` = paste(c_across(2:ncol(input.df))[sapply(c_across(2:ncol(input.df)), not_starts_with_L_and_contains_dash)], collapse = ", ")
    ) %>%
    ungroup()

  # Reorder the columns
  input.df <-  input.df %>%
    select(1, AntineoplasticMedication, AdditionalMedication)

  # Create Vector that identifies a study
  Study <- c("s_mas", "FORUM", "AMLSG", "studie", "gmall", "protokoll", " block", "aml_aza", "cws",
             "studienmedikation", "sor_aml", "euro_b.o.s.s", "gm_jung_konsolidation",
             "gnrh_analogon", "s.mas", "siop", "coss", "freiburger", "Schema",
             "gm_bnhl_block", "ewing", "gmall_elderly", "ielsg", "nhl_bfm",
             "ovar", "primain", "aml_bfm", "euramos", "euramos_1",
             "euro", "guidance_protokoll", "register", "vai ", "aida",
             "aieop_bfm", "amlsg_16_10", "euronet_phl_c1", "ewall", "flamsa_ric",
             "gm_alt_induktion", "gm_alt_konsolidation", "gm_jung_induktion",
             "gmall_b_nhl", "iaws", "iii_studie", "lch", "matrix", "mk_7339_010",
             "o_tie", "piqur_studie", "pkc412_stud", "pkc412_studie", "pkc_studie",
             "registry", "resgex_studie")

  # Words that identifies substances
  Medication <- c("aprepitant", "gimeracil", "oteracil", "antilymphozytaeres",  "tipiracil", "Aromatasehemmer",
                  "cjm112", "emtansin", "emtansine", "entospletinib", "hormontherapie",
                  "lhrh", "mafodotin", "mamac", "trenatone", "yescarta")

  Info <- c("szt", "konditionierung", "induktion", "allogen", "konsolidierung",
            "therapie", "chemotherapie", "tace", "hd", "kons", "vorphase",
            "autolog", "nhl", "all", "ind ", "aml", "b_all", "analog",
            "hyperthermie", "iii", "kmt", "neuroblastom", "autologe",
            "erhaltungstherapie", "esk", "hipec", "monotherapie",
            "stammzellen", "stammzelltransplantation", "supportive",
            "allogene", "hit", "i.th", "il_2", "induktionstherapie",
            "knochenmark", "lichttherapie","optune", "lokale", "mcl", "periphere",
            "photodynamische", "puva", "autogen", "cht", "cpt",
            "decider", "ernaehrungstherapie", "eskaliert",
            "iace", "inatallation", "inst", "kombi", "konsolidation",
            "ml_ds", "palliativ", "pdt", "phase", "reinduktion",
            "relapse", "schmerz", "stamm", "symptomlindernde", "tacp", "tor_aml", "triple", "zns", "zns_prophylaxe",
            "BEST", "SUPPORTIVE", "CARE")

  # Function to check if any word from a vector exists in a string
  contains_word <- function(x, word_list) {
    pattern <- paste(word_list, collapse = "|")
    matches <- regmatches(x, gregexpr(pattern, x, ignore.case = TRUE))
    unique(unlist(matches))
  }

  # Add new columns `study`, `medication`, and `info` based on the presence of words in the respective vectors in any cell of the row
  input.df <- input.df %>%
    rowwise() %>%
    mutate(
      Medication = paste(unique(unlist(lapply(c_across(cols = 1), contains_word, Medication))), collapse = ", "),
      Study = paste(unique(unlist(lapply(c_across(cols = 1), contains_word, Study))), collapse = ", "),
      Info = paste(unique(unlist(lapply(c_across(cols = 1), contains_word, Info))), collapse = ", ")
    ) %>%
    ungroup()

  # Count translations
  # Creating a new DataFrame with selected columns
  new_df <- input.df %>%
    select(c(`AntineoplasticMedication`, `AdditionalMedication`))

  new_df$combinedColumn <- paste0(as.character(input.df$AntineoplasticMedication), as.character(input.df$AdditionalMedication))

  # Convert into a token
  tokstranslated_forcount <- tokens(new_df$combinedColumn, , remove_punct = TRUE)

  # Compare token count
  counts_translated<- sum(ntoken(tokstranslated_forcount))

  cat("Number of ATC Code transformed:", counts_translated)

  return(input.df)
}
