#' Obtaining a Flat Table from Pivot Tables
#'
#' Transformations that allow obtaining a flat table from reports in excel or csv
#' format that contain data in the form of pivot tables. They can be defined for
#' a single report and applied to a set of reports.
#'
#' A pivot table should only contain label rows and columns, and an array of
#' values, usually numeric data.
#'
#' flattabler package provides four categories of functions: Pivot table
#' import, pivot table definition, pivot table transformation, and flat table
#' generation.
#'
#' @section Pivot table import:
#' Functions to import pivot tables from a text or Excel file or set of files,
#' or to generate it from a data frame.
#' - [read_text_file()]
#' - [read_text_folder()]
#' - [read_excel_sheet()]
#' - [read_excel_file()]
#' - [read_excel_folder()]
#' - [pivot_table()]
#'
#' @section Pivot table definition: Functions to view the object, to split a
#'   table into several, to define its characteristics, and to remove the rows
#'   and columns that are not part of the pivot table.
#' - [view_table_attr()]
#' - [divide()]
#' - [get_page()]
#' - [set_page()]
#' - [define_labels()]
#' - [remove_empty()]
#' - [remove_rows()]
#' - [remove_cols()]
#' - [remove_top()]
#' - [remove_bottom()]
#' - [remove_left()]
#' - [remove_right()]
#'
#' @section Pivot table transformation:
#' Functions to transform the rows and columns of labels, or the array of values
#' of the pivot table.
#' - [fill_labels()]
#' - [remove_agg()]
#' - [extract_labels()]
#' - [get_col_values()]
#' - [fill_values()]
#' - [remove_k()]
#' - [replace_dec()]
#'
#' @section Flat table generation:
#' Functions to generate a flat table from a pivot table and to apply a set of
#' transformations to a list of tables.
#' - [unpivot()]
#' - [flatten_table_list()]
#'
#' @docType package
#' @name flattabler
NULL
