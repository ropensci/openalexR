#' Get coverage of OpenAlex fields in openalexR
#'
#' @param entity The OA entity to inspect field coverage for. Returns
#'   information for all fields if `NULL` (default).
#'
#' @return Data frame of field coverage information
#' @export
#'
#' @seealso oa_entities()
#' @examples
#' oa_entities()
#' head(get_coverage(entity = "works"))
get_coverage <- function(entity = NULL) {
  oa2df_coverage <- getExportedValue("openalexR", "oa2df_coverage")

  if (!is.null(entity)) {
    entity <- rlang::arg_match(entity, oa_entities())
    oa2df_coverage[[entity]]
  } else {
    oa2df_coverage
  }
}
