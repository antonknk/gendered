#' any_gender
#'
#' Calculates and stores occurance of gendered words
#' @param text  a character vector
#' @return A list
#' @export
any_gender <- function(text){
    neutral_forms <- gendered::neutral %>%
        dplyr::pull("gendergerechte_alternativen") %>%
        paste0("\\b", ., "\\b") %>%
        paste0(collapse = "|")

    # pair gendering
    anygender_pair <- text %>%
        stringr::str_remove_all("-|\\bliebe\\b|\\bdie\\b|\\bden\\b|\\bder\\b|\\bauch\\b|\\bwerte\\b") %>%
        stringr::str_squish() %>%
        stringr::str_detect("(\\w+innen\\s+(und|&)\\s+\\w+|\\w+\\s+(und|&)\\s+\\w+innen)")

    matches_pair <- text %>%
        stringr::str_remove_all("-|\\bliebe\\b|\\bdie\\b|\\bden\\b|\\bder\\b|\\bauch\\b|\\bwerte\\b") %>%
        stringr::str_squish() %>%
        stringr::str_extract_all("(\\w+innen\\s+(und|&)\\s+\\w+|\\w+\\s+(und|&)\\s+\\w+innen)")

    n_gendered_pair <- lengths(matches_pair)

    n_words <- text %>%
        str_count(boundary("word"))

    percent_gendered_pair <- n_gendered_pair / n_words

    # symbol gendering
    anygender_symbol <- text %>%
        stringr::str_squish() %>%
        stringr::str_extract_all("\\S+([:punct:]i|I)nnen\\b")

    matches_symbol <- text %>%
        stringr::str_squish() %>%
        stringr::str_extract_all("\\S+([:punct:]i|I)nnen\\b")

    n_gendered_symbol <- lengths(matches_symbol)

    percent_gendered_symbol <- n_gendered_symbol / n_words

    # neutrals gendering
    anygender_neutral <- text %>%
        stringr::str_squish() %>%
        stringr::str_detect(neutral_forms)

    matches_neutral <- text %>%
        stringr::str_squish() %>%
        stringr::str_extract_all(neutral_forms)

    n_gendered_neutral <- lengths(matches_neutral)

    percent_gendered_neutral <- n_gendered_neutral / n_words

    # TOTAL STAT
    total_gendered_n = n_gendered_symbol + n_gendered_pair + n_gendered_neutral
    total_gendered_percent = total_gendered_n / n_words


    return(tibble::tibble(
        "matches_pair" = matches_pair,
        "matches_symbol" = matches_symbol,
        "matches_neutral" = matches_neutral,
        "anygender_pair" = anygender_pair,
        "anygender_symbol" = anygender_symbol,
        "anygender_neutral" = anygender_neutral,
        "n_gendered_pair" = n_gendered_pair,
        "n_gendered_symbol" = n_gendered_symbol,
        "n_gendered_neutral" = n_gendered_neutral,
        "n_words" = n_words,
        "percent_gendered_pair" = percent_gendered_pair,
        "percent_gendered_symbol" = percent_gendered_symbol,
        "percent_gendered_neutral" = percent_gendered_neutral,
        "total_gendered_n" = total_gendered_n,
        "total_gendered_percent" = total_gendered_percent
    )
    )
}



