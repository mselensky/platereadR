#' Plot standard curve
#'
#' This function serves as a wrapper to quickly visualize an analytical standard curve from a Gen5 software output
#' @param assay.data.raw An excel table exported from the Gen5 software.
#' @param standard.IDs A 2-column dataframe consisting of 'well_ID' (e.g., A12) and 'standard' (a standard concentration, generally in parts per million, mapped to each well_ID)
#' @keywords plot standard curve
#' @export
#' @examples
#' plot_stdCurve
#'
plot_stdCurve <- function(assay.data.raw, standard.IDs, title) {
  suppressWarnings(
    assay_data <- assay.data.raw %>%
      slice(-c(1:33)) %>%
      row_to_names(1) %>%
      clean_names() %>%
      filter(!is.na(.[,2]),
             !.[,2] == "Curve Name",
             !.[,2] == "StdCurve") %>%
      rename("well_row" = names(.[,2])) %>%
      select(-1, -15)
  )

  assay_data_long <- assay_data %>%
    pivot_longer(cols = 2:ncol(.),
                 values_to = "absorbance",
                 names_to = "well_column") %>%
    mutate(well_ID = str_c(well_row, well_column),
           well_ID = str_remove(well_ID, "x"),
           absorbance = as.numeric(absorbance)) %>%
    select(well_ID, absorbance)

  suppressMessages(
    standard_data <- assay_data_long %>%
      left_join(., standardIDs) %>%
      filter(!is.na(standard))
  )

  standard_data %>%
    ggplot(aes(absorbance, standard)) +
    geom_point() +
    geom_smooth(method = "lm") +
    ggtitle(title)
}
