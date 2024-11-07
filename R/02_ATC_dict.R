pacman::p_load(dplyr, stringr, quanteda, readxl)

# Download newest version https://www.wido.de/publikationen-produkte/analytik/arzneimittel-klassifikation/ and extract atc codes in sperate file, load
excel_path <- system.file("extdata", "ATC_excel.xlsx", package = "ATCodeR")

atc <- read_excel(excel_path)

# Rename into Code and Meaning
names(atc) <- c("Code", "Meaning")

# Delete rows with NA
atc <- atc[complete.cases(atc), ]

# Change - for _
atc$Meaning <- gsub("-", "_",atc$Meaning)

# Change Umlaute
replace_umlauts <- function(text) {
  text <- gsub("ä", "ae", text)
  text <- gsub("ö", "oe", text)
  text <- gsub("ü", "ue", text)
  text <- gsub("Ä", "AE", text)
  text <- gsub("Ö", "OE", text)
  text <- gsub("Ü", "UE", text)
  return(text)
}

atc$Meaning <- sapply(atc$Meaning, replace_umlauts)

# Get a character vector based on the structure of the Code variable. There are 5 different Levels
newline <- character()
for (i in 1:length(atc$Code)) {
  if (nchar(atc$Code[i]) == 1) {
    newline[i] <- paste0(paste0(rep("    ", 0), collapse = ""), atc$Code[i], "-",
                         atc$Meaning[i])
  }
  if (nchar(atc$Code[i]) == 3) {
    newline[i] <- paste0(paste0(rep("    ", 1), collapse = ""), atc$Code[i], "-",
                         atc$Meaning[i])
  }
  if (nchar(atc$Code[i]) == 4) {
    newline[i] <- paste0(paste0(rep("    ", 2), collapse = ""), atc$Code[i], "-",
                         atc$Meaning[i])
  }
  if (nchar(atc$Code[i]) == 5) {
    newline[i] <- paste0(paste0(rep("    ", 3), collapse = ""), atc$Code[i], "-",
                         atc$Meaning[i])
  }
  if (nchar(atc$Code[i]) == 7) {
    newline[i] <- paste0(paste0(rep("    ", 4), collapse = ""), atc$Code[i], "-",
                         atc$Meaning[i])
  }
}

# Create function to create a nested list that is the base for the dictionary
create_nested_dict <- function(lines) {
  dict <- list() #Initialize an empty list to store the nested dictionary
  # Split the line into code and meaning based on the hyphen
  for (line in lines) {
    parts <- strsplit(line, "-", fixed = TRUE)[[1]]
    Code <- parts[1]# The first part is the code
    Meaning <- parts[2]# The second part is the meaning

    # Calculate the indent level based on the number of leading spaces in the code
    indent_level <- (nchar(Code) - nchar(gsub(" ", "", Code))) / 4

    # Remove spaces from the code to clean it up
    Code <- gsub(" ", "", Code)  # Remove spaces for Code

    # Create a header combining the code and the meaning with a hyphen
    header <- paste(Code, Meaning, sep = "-") # Ensure hyphen is used as separator

    if (indent_level == 0) {
      # Top-level entry: directly add the header to the dictionary
      dict[[header]] <- list()
      # Second-level entry
      # Create a first-level key
    } else if (indent_level == 1) {
      level1 <- paste(substr(Code, 1, 1), atc$Meaning[match(substr(Code, 1, 1), atc$Code)], sep = "-")
      if (!is.list(dict[[level1]])) dict[[level1]] <- list()
      dict[[level1]][[header]] <- list()
      # Third-level entry
      # Create first and second-level keys
    } else if (indent_level == 2) {
      level1 <- paste(substr(Code, 1, 1), atc$Meaning[match(substr(Code, 1, 1), atc$Code)], sep = "-")
      level2 <- paste(substr(Code, 1, 3), atc$Meaning[match(substr(Code, 1, 3), atc$Code)], sep = "-")
      if (!is.list(dict[[level1]][[level2]])) dict[[level1]][[level2]] <- list()
      dict[[level1]][[level2]][[header]] <- list()
      # Fourth-level entry
      # Create first, second, and third-level keys
    } else if (indent_level == 3) {
      level1 <- paste(substr(Code, 1, 1), atc$Meaning[match(substr(Code, 1, 1), atc$Code)], sep = "-")
      level2 <- paste(substr(Code, 1, 3), atc$Meaning[match(substr(Code, 1, 3), atc$Code)], sep = "-")
      level3 <- paste(substr(Code, 1, 4), atc$Meaning[match(substr(Code, 1, 4), atc$Code)], sep = "-")
      if (!is.list(dict[[level1]][[level2]][[level3]])) dict[[level1]][[level2]][[level3]] <- list()
      dict[[level1]][[level2]][[level3]][[header]] <- list()
      # Fifth-level entry
      # Create first, second, third, and fourth-level keys
    } else if (indent_level == 4) {
      level1 <- paste(substr(Code, 1, 1), atc$Meaning[match(substr(Code, 1, 1), atc$Code)], sep = "-")
      level2 <- paste(substr(Code, 1, 3), atc$Meaning[match(substr(Code, 1, 3), atc$Code)], sep = "-")
      level3 <- paste(substr(Code, 1, 4), atc$Meaning[match(substr(Code, 1, 4), atc$Code)], sep = "-")
      level4 <- paste(substr(Code, 1, 5), atc$Meaning[match(substr(Code, 1, 5), atc$Code)], sep = "-")
      if (!is.list(dict[[level1]][[level2]][[level3]][[level4]])) dict[[level1]][[level2]][[level3]][[level4]] <- list()
      dict[[level1]][[level2]][[level3]][[level4]][[header]] <- Meaning # Add the meaning to the header key in the fourth-level list
    }
  }

  return(dict)# Return the constructed nested dictionary
}

# Apply function to the character vector
nested_dict <- create_nested_dict(newline)

# Create function to delete empty lists
remove_empty_lists <- function(lst) {
  lst <- lst[sapply(lst, function(x) !is.null(x) && (is.list(x) && length(x) > 0 || !is.list(x)))]
  lst <- lapply(lst, function(x) if (is.list(x)) remove_empty_lists(x) else x)
  return(lst)
}

# Apply function to delete empty lists
nested_dict <- remove_empty_lists(nested_dict)

nested_dict[["L-ANTINEOPLASTISCHE UND IMMUNMODULIERENDE MITTEL"]][["L04-IMMUNSUPPRESSIVA"]][["L04A-IMMUNSUPPRESSIVA"]][["L04AA-Selektive Immunsuppressiva"]][["L04AA03-Antilymphozytaeres Immunglobulin (Pferd)"]] <- c("ATG", "Antilymphozytaeres Immunglobulin (Pferd)")
nested_dict[["L-ANTINEOPLASTISCHE UND IMMUNMODULIERENDE MITTEL"]][["L04-IMMUNSUPPRESSIVA"]][["L04A-IMMUNSUPPRESSIVA"]][["L04AA-Selektive Immunsuppressiva"]][["L04AA04-Antithymozytaeres Immunglobulin (Kaninchen)"]] <- c("Antithymozytaeres","Antithymozytaeres Immunglobulin (Kaninchen)")
nested_dict[["L-ANTINEOPLASTISCHE UND IMMUNMODULIERENDE MITTEL"]][["L01-ANTINEOPLASTISCHE MITTEL"]][["L01F-MONOKLONALE ANTIKOERPER UND ANTIKOERPER_WIRKSTOFF_KONJUGATE"]][["L01FX-Andere monoklonale Antikoerper und Antikoerper_Wirkstoff_Konjugate"]][["L01FX02-Gemtuzumab ozogamicin"]] <- c("Gemtuzumab ozogamicin","Gemtuzumab")
nested_dict[["L-ANTINEOPLASTISCHE UND IMMUNMODULIERENDE MITTEL"]][["L01-ANTINEOPLASTISCHE MITTEL"]][["L01F-MONOKLONALE ANTIKOERPER UND ANTIKOERPER_WIRKSTOFF_KONJUGATE"]][["L01FX-Andere monoklonale Antikoerper und Antikoerper_Wirkstoff_Konjugate"]][["L01FX14-Polatuzumab vedotin"]] <- c("Polatuzumab vedotin", "Polatuzumab ")

# Convert nested list to quanteda dictionary
quanteda_dict <- quanteda::dictionary(nested_dict)

#Save the substance dictionary
saveRDS(quanteda_dict, "C:\\Users\\ischn\\Desktop\\RoteListe\\atc_substance_dictionary_v2.cat")
quanteda_dict <- readRDS("C:\\Users\\ischn\\Desktop\\RoteListe\\atc_substance_dictionary_v2.cat")
