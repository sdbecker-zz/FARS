#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read in data from a file
#'
#'\code{fars_read} returns a data structure with data read from a given file.
#'
#' The function takes an atomic character vector holding the name of the file
#' and given that it exists will return the data from the file in a tbl_df
#' data structure. If the file does not exist the function will exit and an
#' error will be thrown.
#'
#'@section Required functions to import from other packages:
#'
#' readr::read_csv();
#' dplyr::tbl_df()
#'
#'@param filename An atomic character vector holding the path name of the file.
#'
#'@return A tbl_df data structure with contents of the given file.
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a filename
#'
#'\code{make_filename} completes a file name with an input year.
#'
#' The function uses the argument given to the function to augment to the
#' predefined template, and outputs a filename.
#'
#'@section Required functions to import from other packages:
#' None
#'
#'@param year An atomic character or numeric vector representing the year to
#' augment to the file name.
#'
#'@return An atomic character vector representing the file name.
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Return a list of data frames for each year.
#'
#'\code{fars_read_years} cycles through a vector of years and outputs a list
#' containing filtered data frames for each year.
#'
#' The function creates a file name from an element of the argument, if the
#' file exists it reads the data. The data is then filtered into two columns
#' and returned into a list. This is done for each element in the argument -
#' which in this case would be a character/numeric vector/list of years.
#'
#'@section Required functions to import from other packages:
#' dplyr::mutate
#'
#'
#'@param years A character/numeric vector of years that can be converted
#' into integers.
#'
#'@return A list of data frames containing the filtered data frames
#' for each year in the argument vector/list.
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                        dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Return a table of the number of accidents per month in each year.
#'
#'\code{fars_summarize_years} cycles through a vector of years and reads data
#' for each year and collates, summarizes and formats the data per year
#'
#' The function creates a list of data frames that is combines into one
#' data frame The data frame is then grouped by year and month and the number
#' of accidents calculate per month. The result is then formatted into a table.
#'
#'@section Required functions to import from other packages:
#' dplyr::bind; dplyr::group_by; dplyr::summarize; tidyr:: spread
#'
#'@param years A character/numeric vector of years that can be converted
#' into integers.
#'
#'@return A data frame with the number of accidents per month
#'over the years.
#'@export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plot state and points where accidents occurred.
#'
#'\code{fars_map_state} plots the outline of a state given as an integer
#'argument and plots the points where all the accidents occured in that state
#'for a given year.
#'
#' The function filters out the relevant points of longitude and latitude
#' where accidents occured and plots them within an outline of the state
#' in which they occurred. Further, the points represent where the accidents
#' occured within a given state for a given year.
#'
#'@section Required functions to import from other packages:
#' maps::map; note that graphics::points is part of the base graphics
#' and does not need to be imported.
#'
#'@param state.num An atomic numeric vector representing the state in the
#'USA of interest.
#'
#'@param year An atomic character or numeric vector representing the year of
#'interest.
#'
#'@return Outputs an outline of the state with points representing
#' the accidents that occured during the given year in that state.
#'@export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
