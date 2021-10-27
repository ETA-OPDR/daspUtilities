onet_crosswalk <- function(df, onet_occupation_columnn, onet_soc_schema = 'ONET_SOC_2019_CODE', keep_details = FALSE, include_soc = FALSE) {

  onet_occupation_columnn <- enquo(onet_occupation_columnn)

  #This block sets the target schema for subsequent crosswalking
  if (onet_soc_schema == 'ONET_SOC_2019_CODE') {
    onet_2010_to_2019_crosswalk <- onet_2010_to_2019_crosswalk %>%
      mutate(crosswalked_code = ONET_SOC_2019_CODE,
             crosswalked_title = ONET_SOC_2019_TITLE,
             target_schema = onet_soc_schema)
  } else if (onet_soc_schema == 'ONET_SOC_2010_CODE') {
    onet_2010_to_2019_crosswalk <- onet_2010_to_2019_crosswalk %>%
      mutate(crosswalked_code = ONET_SOC_2010_CODE,
             crosswalked_title = ONET_SOC_2010_TITLE,
             target_schema = onet_soc_schema)
  } else {
    stop('onet_soc_schema must be one of the following options:
         ONET_SOC_2019_CODE
         ONET_SOC_2010_CODE',
         call. = FALSE)
  }

  if (include_soc & onet_soc_schema != 'ONET_SOC_2019_CODE') {
    stop('include_soc may only be TRUE if onet_soc_Schema is set to ONET_SOC_2019_CODE')
  }

  df_cols <- c(names(df),'crosswalked_code', 'crosswalked_title', 'target_schema', 'source_schema')

  # Test whether a SOC code in the use input occupation column is a valid OES_2019_Estimates_Code.
  # If it is the script will join to the crosswalk on the first valid match.
  by <- setNames('ONET_SOC_2019_CODE', quo_name(onet_occupation_columnn))

  df_2019 <- df %>%
    filter(!!onet_occupation_columnn %in% onet_2010_to_2019_crosswalk$ONET_SOC_2019_CODE) %>%
    left_join(onet_2010_to_2019_crosswalk %>% distinct(ONET_SOC_2019_CODE, .keep_all = TRUE),
              by=by) %>%
    mutate(source_schema = 'ONET_SOC_2019_CODE') %>%
    select(df_cols)

  by <- setNames('ONET_SOC_2010_CODE', quo_name(onet_occupation_columnn))

  df_2010 <- df %>%
    filter(!(!!onet_occupation_columnn %in% onet_2010_to_2019_crosswalk$ONET_SOC_2019_CODE),
           !!onet_occupation_columnn %in% onet_2010_to_2019_crosswalk$ONET_SOC_2010_CODE) %>%
    left_join(onet_2010_to_2019_crosswalk %>% distinct(ONET_SOC_2010_CODE, .keep_all = TRUE),
              by=by) %>%
    mutate(source_schema = 'ONET_SOC_2010_CODE') %>%
    select(df_cols)

  df_no_valid_code <- df %>%
    filter(!(!!onet_occupation_columnn %in% onet_2010_to_2019_crosswalk$ONET_SOC_2019_CODE),
           !(!!onet_occupation_columnn %in% onet_2010_to_2019_crosswalk$ONET_SOC_2010_CODE)) %>%
    mutate(crosswalked_code = NA_character_,
           crosswalked_title = NA_character_,
           target_schema = onet_soc_schema,
           source_schema = 'not_valid_detailed_soc') %>%
    select(df_cols)

  df <- bind_rows(df_2019,
                  df_2010,
                  df_no_valid_code)

  df <- df %>%
    mutate(onet_transformed = ifelse(!!onet_occupation_columnn == crosswalked_code, FALSE, TRUE))

  if (!keep_details) {
    df <- df %>% select(-c('target_schema','source_schema','onet_transformed'))
  }


  if(include_soc) {
    df <- df %>%
      left_join(onet_2010_to_2019_crosswalk, by = c('crosswalked_code' = 'ONET_SOC_2019_CODE')) %>%
      select(-ONET_SOC_2019_TITLE)
  }

  message('This function will return the onet occupation code for the first match in the target schema when a onet SOC code in a source schema is split into more granular target schema onet occupation codes')

  return(df)

}
