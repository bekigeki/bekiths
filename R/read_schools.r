#' Read and parse school Excel files
#'
#' Reads all `.xlsx` files in a directory, extracts metadata (e.g. Stichtag,
#' Erstellungsdatum) from the header area, and returns a combined tibble with
#' selected school columns and file‑level metadata.
#'
#' @param path Character scalar. Directory containing the Excel files to read. 
#' @param sheet Sheet index or name to read from each workbook (passed to
#'   \code{readxl::read_excel()}). 
#' @param skip Number of rows to skip before reading the main data table
#'   (applied only to the data read, not to the metadata scan). 
#' @param cols Optional named vector specifying which columns to read and how
#'   to name them.
#'   If \code{use_names = FALSE}, values are integer positions in the sheet
#'   and names are the output column names.
#'   If \code{use_names = TRUE}, names are the output column names and values
#'   are the original column names in the Excel file.
#'   Defaults to a built‑in positional mapping suitable for standard school
#'   exports.
#' @param use_names Logical. If \code{FALSE}, \code{cols} is interpreted as
#'   position‑based mapping; if \code{TRUE}, \code{cols} is interpreted as
#'   name‑based mapping and columns are renamed accordingly.
#' @param meta_specs Optional named list of metadata extraction rules.
#'   Each element must itself be a list with components \code{col} (integer
#'   column index), \code{pattern} (regular expression used with
#'   \code{stringr::str_match()}), and optional \code{group} (capture‑group
#'   index to extract). If \code{NULL}, defaults look for \code{Stichtag}
#'   and \code{erstellt am} in the first column.
#'
#' @return A tibble with one row per school row across all files, including
#'   the selected school variables, a \code{source_file} column, and one
#'   column per extracted metadata field (e.g. \code{stichtag},
#'   \code{erstellt_am}). 
#'
#' @examples
#' \dontrun{
#' # Read all standard school export files in a directory using defaults
#' df_schulen <- read_schools("data/schulen")
#'
#' # Use name-based column mapping (e.g. when headers are stable)
#' df_schulen2 <- read_schools(
#'   path      = "data/schulen",
#'   use_names = TRUE,
#'   cols      = c(
#'     Schuljahr    = "Jahr",
#'     Schulname    = "Schule",
#'     Klassen_3    = "Klassen_3",
#'     Schueler_3   = "Schueler_3"
#'   )
#' )
#'
#' # Override a single metadata spec (e.g. different Stichtag pattern)
#' custom_meta <- list(
#'   stichtag = list(
#'     col     = 1,
#'     pattern = "Stichtag\\s*[:=]\\s*([^,]+)",
#'     group   = 1
#'   )
#' )
#' df_schulen3 <- read_schools("data/schulen", meta_specs = custom_meta)
#' }
#'
#' @export
read_schools <- function(
                         path,
                         sheet = 1,
                         skip = 0,              # wird NACH dem Metaparsen angewendet
                         cols  = NULL,
                         use_names = FALSE,
                         meta_specs = NULL      # Liste von Extraktionsregeln
                         ){

  files <- list.files(
    path    = path,
    pattern = "^[a-zA-Z].*\\.xlsx$",
    full.names = TRUE
  )

  
  
  if (length(files) == 0) {
    return(tibble())
  }
  
  # Default-Spaltenmapping (Positionsbasiert)
  default_pos <- c(
    Schuljahr        = 1,
    Element          = 8,
    Schulname        = 9,
    Schulamt         = 10,
    Kreis            = 11,
    Schultraeger     = 12,
    Schulart         = 5,
    PLZ              = 14,
    Ort              = 15,
    Strasse          = 16,
    AGS              = 17,
    Schulteil_PLZ    = 19,
    Schulteil_Ort    = 20,
    Schulteil_Strasse = 21,
    Klassen_3        = 26,
    Schueler_3       = 27
  )

  if (is.null(cols)) {
    cols <- default_pos
  }

  

  # Default-Metaspezifikationen:
  # 1) Stichtag irgendwo in Spalte 1
  # 2) "erstellt am" irgendwo in Spalte 1
  if (is.null(meta_specs)) {
    meta_specs <- list(
      stichtag = list(
        col     = 1,
        pattern = "Stichtag:\\\\s*([^,]+)",  # 1. Capture-Gruppe
        group   = 1
      ),
      erstellt_am = list(
        col     = 1,
        pattern = "erstellt am\\\\s*([^/]+)", # bis zum nächsten "/"
        group   = 1
      )
    )
  }

  dat_list <- purrr::map(files, function(f) {
    # 1) Ganzes Sheet als Character lesen, ohne skip
    raw_full <- readxl::read_excel(
      path  = f,
      sheet = sheet,
      col_types = "text",
      .name_repair = "unique_quiet"
    )

    # 2) Metadaten extrahieren: Regex über ganze Spalte
    meta_vals <- list()

    for (nm in names(meta_specs)) {
      spec <- meta_specs[[nm]]
      col_vec <- raw_full[[spec$col]]

      # alle Matches in der Spalte
      m <- stringr::str_match(col_vec, spec$pattern)
      # m ist Matrix: Spalten = vollständiger Match + Gruppen
      # wir wollen die erste nicht-NA Zeile der gewünschten Gruppe
      if (all(is.na(m))) {
        value <- NA_character_
      } else {
        grp_col <- if (!is.null(spec$group)) spec$group + 1 else 1
        cand <- m[, grp_col]
        idx  <- which(!is.na(cand))[1]
        value <- if (length(idx)) cand[idx] else NA_character_
      }

      meta_vals[[nm]] <- value
    }

    # 3) Eigentliche Daten mit skip einlesen
    raw <- readxl::read_excel(
      path  = f,
      sheet = sheet,
      skip  = skip,
      col_types = "text",
      .name_repair = "unique_quiet"
    )

    if (!use_names) {
      max_pos <- max(cols)
      if (ncol(raw) < max_pos) {
        rlang::abort(
          paste0(
            "File '", f, "' has only ", ncol(raw),
            " columns; need at least ", max_pos, "."
          )
        )
      }
      out <- dplyr::select(raw, !!!cols)
    } else {
      missing_src <- setdiff(unname(cols), names(raw))
      if (length(missing_src) > 0) {
        rlang::abort(
          paste0(
            "File '", f, "' is missing columns: ",
            paste(missing_src, collapse = ", ")
          )
        )
      }
      out <- dplyr::rename(raw, !!!cols) |>
        dplyr::select(dplyr::all_of(names(cols)))
    }

    # 4) Metadaten und source_file anhängen
    out <- dplyr::mutate(
      out,
      source_file = basename(f),
      !!!meta_vals
    )

    out
  })

  df_schulen <- dplyr::bind_rows(dat_list)
  df_schulen
}

