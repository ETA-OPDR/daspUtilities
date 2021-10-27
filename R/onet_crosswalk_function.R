soc_crosswalk <- function(df, occupation_column, soc_schema = 'OES_2019_Estimates_Code', keep_details = TRUE) {

  occupation_column <- enquo(occupation_column)

  #This block sets the target schema for subsequent crosswalking
  if (soc_schema == 'OES_2019_Estimates_Code') {
    SOC_Crosswalk <- SOC_Crosswalk %>%
      mutate(crosswalked_code = OES_2019_Estimates_Code,
             crosswalked_title = OES_2019_Estimates_Title,
             target_schema = soc_schema)
  } else if (soc_schema == 'SOC_2018_Code') {
    SOC_Crosswalk <- SOC_Crosswalk %>%
      mutate(crosswalked_code = SOC_2018_Code,
             crosswalked_title = SOC_2018_Title,
             target_schema = soc_schema)
  } else if (soc_schema == 'OES_2018_Estimates_Code') {
    SOC_Crosswalk <- SOC_Crosswalk %>%
      mutate(crosswalked_code = OES_2018_Estimates_Code,
             crosswalked_title = OES_2018_Estimates_Title,
             target_schema = soc_schema)
  } else if (soc_schema == 'SOC_2010_Code') {
    SOC_Crosswalk <- SOC_Crosswalk %>%
      mutate(crosswalked_code = SOC_2010_Code,
             crosswalked_title = SOC_2010_Title,
             target_schema = soc_schema)
  } else {
    stop('soc_schema must be one of the following options:
         OES_2019_Estimates_Code
         SOC_2018_Code
         OES_2018_Estimates_Code
         SOC_2010_Code', call. = FALSE)
  }

  df_cols <- c(names(df),'crosswalked_code', 'crosswalked_title', 'target_schema', 'source_schema')

  # Test whether a SOC code in the use input occupation column is a valid OES_2019_Estimates_Code.
  # If it is the script will join to the crosswalk on the first valid match.
  by <- setNames('OES_2019_Estimates_Code', quo_name(occupation_column))

  df_2019_estimate <- df %>%
    filter(!!occupation_column %in% SOC_Crosswalk$OES_2019_Estimates_Code) %>%
    left_join(SOC_Crosswalk %>% distinct(OES_2019_Estimates_Code, .keep_all = TRUE),
              by=by) %>%
    mutate(source_schema = 'OES_2019_Estimates_Code') %>%
    select(df_cols)

  by <- setNames('SOC_2018_Code', quo_name(occupation_column))

  df_2018 <- df %>%
    filter(!(!!occupation_column %in% SOC_Crosswalk$OES_2019_Estimates_Code),
           !!occupation_column %in% SOC_Crosswalk$SOC_2018_Code) %>%
    left_join(SOC_Crosswalk %>% distinct(SOC_2018_Code, .keep_all = TRUE),
              by=by) %>%
    mutate(source_schema = 'SOC_2018_Code') %>%
    select(df_cols)

  by <- setNames('OES_2018_Estimates_Code', quo_name(occupation_column))

  df_2018_estimate <- df %>%
    filter(!(!!occupation_column %in% SOC_Crosswalk$OES_2019_Estimates_Code),
           !(!!occupation_column %in% SOC_Crosswalk$SOC_2018_Code),
           !!occupation_column %in% SOC_Crosswalk$OES_2018_Estimates_Code) %>%
    left_join(SOC_Crosswalk %>% distinct(OES_2018_Estimates_Code, .keep_all = TRUE),
              by=by) %>%
    mutate(source_schema = 'OES_2018_Estimates_Code') %>%
    select(df_cols)

  by <- setNames('SOC_2010_Code', quo_name(occupation_column))

  df_2010 <- df %>%
    filter(!(!!occupation_column %in% SOC_Crosswalk$OES_2019_Estimates_Code),
           !(!!occupation_column %in% SOC_Crosswalk$SOC_2018_Code),
           !(!!occupation_column %in% SOC_Crosswalk$OES_2018_Estimates_Code),
           !!occupation_column %in% SOC_Crosswalk$SOC_2010_Code) %>%
    left_join(SOC_Crosswalk %>% distinct(SOC_2010_Code, .keep_all = TRUE),
              by=by) %>%
    mutate(source_schema = 'SOC_2010_Code') %>%
    select(df_cols)

  df_no_valid_code <- df %>%
    filter(!(!!occupation_column %in% SOC_Crosswalk$OES_2019_Estimates_Code),
           !(!!occupation_column %in% SOC_Crosswalk$SOC_2018_Code),
           !(!!occupation_column %in% SOC_Crosswalk$OES_2018_Estimates_Code),
           !(!!occupation_column %in% SOC_Crosswalk$SOC_2010_Code)) %>%
    mutate(crosswalked_code = NA_character_,
           crosswalked_title = NA_character_,
           target_schema = soc_schema,
           source_schema = 'not_valid_detailed_soc') %>%
    select(df_cols)

  df <- bind_rows(df_2019_estimate,
                  df_2018,
                  df_2018_estimate,
                  df_2010,
                  df_no_valid_code)

  df <- df %>%
    mutate(soc_transformed = ifelse(!!occupation_column == crosswalked_code, FALSE, TRUE))

  if (!keep_details) {
    df <- df %>% select(-c('target_schema','source_schema','soc_transformed'))
  }

  message('This function will return the occupation code for the first match in the target schema when a SOC code in a source schema is split into more granular target schema occupation codes')

  return(df)

}


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
