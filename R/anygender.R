# simple function to give number of matches across all forms of gendering
test <- "Die Wählerinnen und Wähler, aber auch Gründer und Gründerinnen, Aber wie ist es
        mit Jurist*innen und Zahnärzt:innen? An die Fleischer_innen und BauerInnen, wer denk an die?"

any_gender <- function(text){
    # pair gendering

    anygender_pair <- text %>%
        stringr::str_remove_all("-|\\bliebe\\b|\\bdie\\b|\\bden\\b|\\bder\\b|\\bauch\\b|\\bwerte\\b") %>%
        stringr::str_squish() %>%
        stringr::str_detect("(\\w+innen\\s+(und|&)\\s+\\w+|\\w+\\s+(und|&)\\s+\\w+innen)")

    matches_pair <- text %>%
        stringr::str_remove_all("-|\\bliebe\\b|\\bdie\\b|\\bden\\b|\\bder\\b|\\bauch\\b|\\bwerte\\b") %>%
        stringr::str_squish() %>%
        stringr::str_extract_all("(\\w+innen\\s+(und|&)\\s+\\w+|\\w+\\s+(und|&)\\s+\\w+innen)")

    n_gendered_pair <- length(purrr::pluck(matches_pair, 1))

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

    n_gendered_symbol <- length(purrr::pluck(matches_symbol, 1))

    percent_gendered_symbol <- n_gendered_symbol / n_words

    # TOTAL STAT
    total_gendered_n = sum(c(n_gendered_symbol, n_gendered_pair), na.rm = T)
    total_gendered_percent = total_gendered_n / n_words


    return(
        list(
            "matches_pair" = matches_pair,
                        "anygender_pair" = anygender_pair,
                        "n_gendered_pair" = n_gendered_pair,
                        "n_words" = n_words,
                        "percent_gendered_pair" = percent_gendered_pair,
                        "matches_symbol" = matches_symbol,
                        "anygender_symbol" = anygender_symbol,
                        "n_gendered_symbol" = n_gendered_symbol,
            "percent_gendered_symbol" = percent_gendered_symbol,
            "total_gendered_n" = total_gendered_n,
            "total_gendered_percent" = total_gendered_percent
        )
    )
}

any_gender(test)

