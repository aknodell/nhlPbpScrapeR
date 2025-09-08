.preclean_shifts_from_html_report <- function(html_shifts, scrape_results, verbose = F) {
  if (verbose) {
    message("Pre-cleaning shifts (HTML)")
  }

  s <-
    scrape_results$api_results$meta |>
    dplyr::pull(session)

  session <- scrape_results$api_results$meta$session
  game <-
    scrape_results$api_results$meta$game_id |>
    as.character() |>
    stringr::str_sub(start = 7)
  round <- game |> stringr::str_sub(start = 2, end = 2)
  series <- game |> stringr::str_sub(start = 3, end = 3)
  series_game <- game |> stringr::str_sub(start = 4)

  has_shootout <-
    session != 3 |
    (session == 3 & round == "0" & series == "0")

  if (nrow(html_shifts) > 0) {
    html_shifts |>
      dplyr::mutate(
        shift_start = shift_start_time,
        shift_end =
          purrr::pmap_int(
            list(
              start = shift_start_time,
              dur = duration,
              p = game_period
            ),
            function(start, dur, p) {
              min(
                start + dur,
                ifelse(
                  p == 4 & has_shootout,
                  3900,
                  p * 1200
                )
              )
            }
          )
      ) |>
      # remove all zero-length shifts
      dplyr::filter(!(shift_start_time == game_period * 1200)) |>
      # combine shifts with no breaks into single shifts
      dplyr::group_by(venue, sweater_number, game_period) |>
      dplyr::mutate(
        rest = tidyr::replace_na(shift_start - dplyr::lag(shift_end), 1),
        group = cumsum(rest != 0)
      ) |>
      dplyr::group_by(game_id, game_period, venue, sweater_number, group) |>
      dplyr::summarise(
        # game_period = min(game_period),
        shift_start_time = min(shift_start_time),
        shift_start_clock = min(shift_start_clock),
        shift_end_time = max(shift_end_time),
        shift_end_clock = max(shift_end_clock),
        duration = sum(duration),
        shift_start = min(shift_start),
        shift_end = max(shift_end),
        .groups = "drop"
      ) |>
      dplyr::select(-group)
  } else {
    tibble::tibble()
  }
}

.extract_details_from_html_pbp <- function(html_pbp, scrape_results, verbose = F) {
  if (verbose) {
    message("Cleaning play-by-play (HTML)")
  }

  session <- scrape_results$api_results$meta$session
  game <-
    scrape_results$api_results$meta$game_id |>
    as.character() |>
    stringr::str_sub(start = 7)
  round <- game |> stringr::str_sub(start = 2, end = 2)
  series <- game |> stringr::str_sub(start = 3, end = 3)
  series_game <- game |> stringr::str_sub(start = 4)

  has_shootout <- session != 3 | (session == 3 & round == "0" & series == "0")

  sweater_id_lookup <-
    scrape_results$api_results$rosters |>
    dplyr::select(team, sweater_number, api_id) |>
    tidyr::unite("sweater_number", team, sweater_number, sep = "#")

  html_pbp <-
    html_pbp |>
    .manually_add_html_events(scrape_results$api_results$meta$game_id) |>
    .manually_change_html_events(scrape_results$api_results$meta$game_id) |>
    .manually_clean_html_events(scrape_results$api_results$meta$game_id) |>
    # dplyr::group_by(game_period) |>
    dplyr::mutate(
      game_seconds = ifelse(game_period == 5 & has_shootout, 3900, game_seconds),
      game_seconds =
        dplyr::case_when(
          event_type == "PEND" ~
            dplyr::case_when(
              game_period == 4 & max(game_period, na.rm = T) > 4 & has_shootout  ~
                3900,
              game_period == max(game_period, na.rm = T) ~ max(game_seconds, na.rm = T),
              T ~ game_period * 1200
            ),
          event_type == "GEND" ~ max(game_seconds, na.rm = T),
          T ~ game_seconds
        ),
      desc =
        event_description |>
        stringr::str_replace_all(
          c(
            "(?<=\\b)L\\.A(?=[\\s\\#])" = "LAK",
            "(?<=\\b)N\\.J(?=[\\s\\#])" = "NJD",
            "(?<=\\b)S\\.J(?=[\\s\\#])" = "SJS",
            "(?<=\\b)T\\.B(?=[\\s\\#])" = "TBL"
          )
        ),
      event_team_html =
        ifelse(
          event_type %in%
            c(
              "BLOCK", "MISS", "SHOT", "GOAL",
              "HIT", "GIVE", "TAKE", "FAC", "PENL",
              "DELPEN"
            ) |
            (event_type == "CHL" & desc |>
               stringr::str_to_lower() |>
               stringr::str_detect("home|away")),
          stringr::str_sub(
            desc |>
              stringr::str_remove("^[^A-Z]*"),
            end = 3
          ),
          NA_character_
        ),
      event_team_zone_html =
        desc |>
        stringr::str_to_upper() |>
        stringr::str_extract("(OFF|DEF|NEU)\\.? ZONE") |>
        stringr::str_sub(end = 1),
      event_player_1_sweater_html =
        dplyr::case_when(
          event_type == "FAC" ~
            desc |>
            stringr::str_extract(stringr::str_c(event_team_html, '\\s*#\\d+')),
          event_type %in% c("MISS", "BLOCK", "PENL", "HIT") ~
            desc |>
            stringr::str_extract('[A-Z\\.]{3}\\s*#\\d+'),
          event_type %in% c("SHOT", "GOAL", "GIVE", "TAKE") ~
            stringr::str_c(
              event_team_html |>
                stringr::str_c(
                  desc |>
                    stringr::str_extract('#\\d+')
                )
            ),
          T ~ NA_character_
        ) |>
        stringr::str_squish(),
      event_player_2_sweater_html =
        dplyr::case_when(
          event_type %in% c("FAC", "PENL", "HIT", "BLOCK") ~
            desc |>
            stringr::str_remove(event_player_1_sweater_html) |>
            stringr::str_extract('[A-Z\\.]{3}\\s?#\\d+'),
          event_type == "GOAL" ~
            stringr::str_c(
              event_team_html,
              desc |>
                stringr::str_remove(
                  event_player_1_sweater_html |>
                    stringr::str_extract("#\\d+")
                ) |>
                stringr::str_extract('#\\d+')
            ),
          T ~ NA_character_
        ) |>
        stringr::str_squish(),
      event_player_3_sweater_html =
        dplyr::case_when(
          event_type == "GOAL" ~
            stringr::str_c(
              event_team_html,
              desc |>
                stringr::str_remove(
                  event_player_1_sweater_html |>
                    stringr::str_extract("#\\d+")
                ) |>
                stringr::str_remove(
                  event_player_2_sweater_html |>
                    stringr::str_extract("#\\d+")
                ) |>
                stringr::str_extract('#\\d+')
            ),
          T ~ NA_character_
        ) |>
        stringr::str_squish(),
      dplyr::across(
        c(event_player_1_sweater_html, event_player_2_sweater_html, event_player_3_sweater_html),
        .fns = (\(x) stringr::str_remove_all(x, "\\s"))
      ),
      event_detail_1_html =
        purrr::map2_chr(
          event_type,
          desc,
          function(t, d) {
            if (t %in% c("BLOCK", "MISS", "SHOT", "GOAL")) {
              if (d |> stringr::str_to_lower() |> stringr::str_detect("failed attempt")) {
                "Failed Attempt"
              } else {
                ret <-
                  d |>
                  stringr::str_split(",") |>
                  purrr::flatten_chr() |>
                  purrr::pluck(
                    ifelse(
                      d |> stringr::str_to_lower() |> stringr::str_detect("penalty shot"),
                      3,
                      2
                    )
                  ) |>
                  stringr::str_to_lower() |>
                  stringr::str_squish()

                if (stringr::str_detect(ret, "((off|def|neu)\\.? zone)|(\\d+\\s*ft\\.)")) {
                  NA_character_
                } else {
                  ret |>
                    stringr::str_replace_all("-+", " ") |>
                    stringr::str_to_title()
                }
              }
            } else if (t == "PENL") {
              d |>
                stringr::str_extract("^.*\\(") |>
                stringr::str_remove("^.*[A-Z]{2}") |>
                stringr::str_remove("\\(") |>
                stringr::str_to_upper() |>
                stringr::str_remove_all("[^A-Z\\s-]") |>
                stringr::str_squish()
            } else {
              NA_character_
            }
          }
        ) |>
        stringr::str_replace_all("-+", " ") |>
        stringr::str_squish() |>
        stringr::str_to_title(),
      event_detail_2_html =
        purrr::pmap_chr(
          list(
            t = event_type,
            d = desc,
            detail = event_detail_1_html
          ),
          function(t, d, detail) {
            if (is.na(detail)) {
              if (is.na(d)) {
                NA_character_
              } else if (d |> stringr::str_to_lower() |> stringr::str_detect("own goal")) {
                "Own Goal"
              } else if (d |> stringr::str_to_lower() |> stringr::str_detect("failed attempt")) {
                NA_character_
              } else {
                NA_character_
              }
            } else if (t %in% c("MISS", "SHOT", "GOAL", "BLOCK")) {
              if (d |> stringr::str_to_lower() |> stringr::str_detect("own goal")) {
                "Own Goal"
              } else if (d |> stringr::str_to_lower() |> stringr::str_detect("blocked by teammate")) {
                "Teammate Blocked"
              } else if (d |> stringr::str_to_lower() |> stringr::str_detect("failed attempt")) {
                NA_character_
              } else {
                ret <-
                  d |>
                  stringr::str_to_lower() |>
                  stringr::str_split(",") |>
                  purrr::flatten_chr() |>
                  purrr::pluck(
                    ifelse(
                      d |> stringr::str_to_lower() |> stringr::str_detect("penalty shot"), #|own goal"),
                      2,
                      3
                    ),
                    .default = NA_character_
                  ) |>
                  stringr::str_to_lower() |>
                  stringr::str_squish()

                if (
                  any(
                    stringr::str_detect(ret, "((off|def|neu)\\.? zone)|(penalty shot)|(ft\\.)"),
                    ret == "",
                    is.null(ret),
                    length(ret) == 0,
                    is.na(ret)
                  )
                ) {
                  NA_character_
                } else {
                  ret |>
                    stringr::str_replace_all("-+", " ") |>
                    stringr::str_to_title()
                }
              }
            } else if (t == "PENL") {
              d |>
                stringr::str_to_lower() |>
                stringr::str_extract("\\(\\d+\\s+min\\)") |>
                stringr::str_remove_all("[\\(\\)]") |>
                stringr::str_squish()
            } else {
              NA_character_
            }
          }
        ) |>
        stringr::str_replace_all("-+", " ") |>
        stringr::str_squish() |>
        stringr::str_to_title(),
      event_detail_3_html =
        purrr::map2_chr(
          event_type,
          desc,
          function(t, d) {
            if (t %in% c("MISS", "SHOT", "GOAL")) {
              splits <-
                d |>
                stringr::str_to_lower() |>
                stringr::str_count("(\\d+\\s*ft\\.?)|(\\(\\d+\\))|(failed attempt)")

              ret <-
                d |>
                stringr::str_to_lower() |>
                stringr::str_split("(\\d+\\s*ft\\.?)|(\\(\\d+\\))|(failed attempt)") |>
                purrr::flatten_chr() |>
                purrr::pluck(
                  splits + 1,
                  .default = NA_character_
                ) |>
                stringr::str_squish()

              if (
                any(
                  ret == "",
                  is.null(ret),
                  length(ret) == 0,
                  is.na(ret)
                )
              ) {
                NA_character_
              } else {
                ret |>
                  stringr::str_replace_all("-+", " ") |>
                  stringr::str_to_title()
              }
            } else if (t == "BLOCK") {
              ret <-
                d |>
                stringr::str_to_lower() |>
                stringr::str_split("(off|def|neu)\\.?\\s*zone") |>
                purrr::flatten_chr() |>
                purrr::pluck(
                  2,
                  .default = NA_character_
                ) |>
                stringr::str_squish()

              if (
                any(
                  ret == "",
                  is.null(ret),
                  length(ret) == 0,
                  is.na(ret)
                )
              ) {
                NA_character_
              } else {
                ret
              }
            } else {
              NA_character_
            }
          }
        ) |>
        stringr::str_replace_all("-+", " ") |>
        stringr::str_squish() |>
        stringr::str_to_title(),
      next_faceoff_team = ifelse(event_type == "FAC", event_team_html, NA_character_),
      next_faceoff_zone = ifelse(event_type == "FAC", event_team_zone_html, NA_character_)
    ) |>
    tidyr::fill(c(next_faceoff_team, next_faceoff_zone), .direction = "up") |>
    dplyr::left_join(
      sweater_id_lookup |>
        dplyr::rename(event_player_1_html = api_id),
      by = dplyr::join_by(event_player_1_sweater_html == sweater_number)
    ) |>
    dplyr::left_join(
      sweater_id_lookup |>
        dplyr::rename(event_player_2_html = api_id),
      by = dplyr::join_by(event_player_2_sweater_html == sweater_number)
    ) |>
    dplyr::left_join(
      sweater_id_lookup |>
        dplyr::rename(event_player_3_html = api_id),
      by = dplyr::join_by(event_player_3_sweater_html == sweater_number)
    )

  teams <-
    html_pbp |>
    dplyr::filter(!is.na(event_team_html)) |>
    dplyr::group_by(event_team_html) |>
    dplyr::tally(sort = T) |>
    head(2) |>
    dplyr::pull(event_team_html)

  html_pbp |>
    dplyr::mutate(
      event_team_html =
        ifelse(
          event_type == "STOP",
          purrr::pmap_chr(
            list(
              description = desc,
              next_team = next_faceoff_team,
              next_zone = next_faceoff_zone
            ),
            function(description, next_team, next_zone) {
              if (
                description |>
                stringr::str_to_upper() |>
                tidyr::replace_na("") |>
                stringr::str_detect(
                  "(ICING)|(GOALIE STOPPED)|(PUCK FROZEN - GOALIE)|(OFF-?SIDE)"
                )
              ) {
                if (is.na(next_zone)) {
                  NA_character_
                } else if (next_zone == "D") {
                  next_team
                } else if (next_zone == "O") {
                  teams |>
                    purrr::discard(magrittr::equals, e2 = next_team)
                } else {
                  NA_character_
                }
              } else if (
                description |>
                stringr::str_to_upper() |>
                tidyr::replace_na("") |>
                stringr::str_detect("TIMEOUT\\s*-?\\s*(HOME|AWAY)")
              ) {
                if (description |> stringr::str_to_upper() |> stringr::str_detect("HOME")) {
                  scrape_results$api_results$meta$home_team
                } else if (description |> stringr::str_to_upper() |> stringr::str_detect("AWAY")) {
                  scrape_results$api_results$meta$away_team
                } else {
                  NA_character_
                }
              } else {
                NA_character_
              }
            }
          ),
          event_team_html
        ),
      event_detail_3_html =
        ifelse(
          event_type == "STOP" &
            stringr::str_detect(desc |> stringr::str_to_upper(), "OFF-?SIDE") &
            !is.na(event_team_html) &
            next_faceoff_zone != "N",
          "INTENTIONAL",
          event_detail_3_html
        ) |>
        stringr::str_replace_all("-+", " ") |>
        stringr::str_squish() |>
        stringr::str_to_title(),
      event_distance_html =
        ifelse(
          event_type %in% c("MISS", "SHOT", "GOAL"),
          desc |>
            stringr::str_to_upper() |>
            stringr::str_extract("\\d+\\s*FT\\.?") |>
            stringr::str_extract("\\d+") |>
            as.integer(),
          NA_integer_
        ) |>
        as.double()
    ) |>
    dplyr::select(
      -c(
        desc,
        event_player_1_sweater_html:event_player_3_sweater_html,
        next_faceoff_team,
        next_faceoff_zone
      )
    )
}

.extract_details_from_api_pbp <- function(api_pbp, scrape_results, verbose = F) {
  if (verbose) {
    message("Cleaning play-by-play (API)")
  }

  session <- scrape_results$api_results$meta$session
  game <-
    scrape_results$api_results$meta$game_id |>
    as.character() |>
    stringr::str_sub(start = 7)
  round <- game |> stringr::str_sub(start = 2, end = 2)
  series <- game |> stringr::str_sub(start = 3, end = 3)
  series_game <- game |> stringr::str_sub(start = 4)

  has_shootout <- session != 3 | (session == 3 & round == "0" & series == "0")

  home_def_side_lookup <-
    api_pbp |>
    dplyr::filter(
      (event_team == scrape_results$api_results$meta$home_team_id &
         zone_code == "D") |
        (event_team == scrape_results$api_results$meta$away_team_id &
           zone_code == "O")
    ) |>
    dplyr::group_by(game_period) |>
    dplyr::summarise(coords_x = median(coords_x)) |>
    dplyr::mutate(
      home_team_def_zone_imp =
        dplyr::case_when(
          sign(coords_x) == -1 ~ "left",
          sign(coords_x) == 1 ~ "right",
          T ~ NA_character_
        )
    ) |>
    dplyr::select(-coords_x)

  home_team_abb <-
    tibble::tibble(
      event_team =
        c(
          scrape_results$api_results$meta$away_team_id,
          scrape_results$api_results$meta$home_team_id
        ),
      event_team_api =
        c(
          scrape_results$api_results$meta$away_team,
          scrape_results$api_results$meta$home_team
        )
    )

  api_pbp |>
    .manually_add_api_events(scrape_results$api_results$meta$game_id) |>
    .manually_change_api_events(scrape_results$api_results$meta$game_id) |>
    .manually_clean_api_events(scrape_results$api_results$meta$game_id) |>
    dplyr::left_join(
      home_team_abb,
      by = dplyr::join_by(event_team)
    ) |>
    dplyr::left_join(
      home_def_side_lookup,
      by = dplyr::join_by(game_period)
    ) |>
    dplyr::mutate(
      home_team_def_zone =
        purrr::map2_chr(
          home_team_def_zone,
          home_team_def_zone_imp,
          function(x, y) {
            tidyr::replace_na(x, y)
          }
        ),
      next_faceoff_x = ifelse(event_type == "faceoff", coords_x, NA_integer_),
      next_faceoff_team = ifelse(event_type == "faceoff", event_team_api, NA_character_),
      next_faceoff_zone = ifelse(event_type == "faceoff", zone_code, NA_character_)
    ) |>
    tidyr::fill(c(next_faceoff_x, next_faceoff_team, next_faceoff_zone), .direction = "up") |>
    dplyr::mutate(
      game_seconds = ifelse(game_period == 5 & has_shootout, 3900, game_seconds),
      game_seconds =
        dplyr::case_when(
          event_type == "period-end" ~
            dplyr::case_when(
              game_period == 4 & max(game_period, na.rm = T) > 4 & has_shootout  ~
                3900,
              game_period == max(game_period, na.rm = T) ~ max(game_seconds, na.rm = T),
              T ~ game_period * 1200
            ),
          event_type == "game-end" ~ max(game_seconds, na.rm = T),
          T ~ game_seconds
        ),
      event_team_api =
        purrr::pmap_chr(
          list(
            t = event_type,
            team = event_team_api,
            description = event_reason_1,
            def_side = home_team_def_zone,
            next_x = next_faceoff_x,
            next_team = next_faceoff_team,
            next_zone = next_faceoff_zone
          ),
          function(t, team, description, def_side, next_x, next_team, next_zone) {
            next_zone <- tidyr::replace_na("N")
            next_x <- tidyr::replace_na(0)

            if (t == "stoppage") {
              if (
                description |>
                stringr::str_to_lower() |>
                stringr::str_detect("(icing)|(goalie-stopped-after-sog)|(puck-frozen)")
              ) {
                if (is.na(next_zone)) {
                  NA_character_
                } else if (next_zone == "D") {
                  next_team
                } else if (next_zone == "O") {
                  home_team_abb$event_team_api |>
                    purrr::discard(magrittr::equals, e2 = next_team)
                } else {
                  NA_character_
                }
              } else if (description |> stringr::str_to_lower() |> stringr::str_detect("offside")) {
                if (next_zone == "D") {
                  next_team
                } else if (next_zone == "O") {
                  home_team_abb$event_team_api |>
                    purrr::discard(magrittr::equals, e2 = next_team)
                } else {
                  if (tidyr::replace_na(def_side == "right", F)) {
                    if (next_x > 0) {
                      scrape_results$api_results$meta$away_team
                    } else if (next_x < 0) {
                      scrape_results$api_results$meta$home_team
                    } else {
                      NA_character_
                    }
                  } else if (tidyr::replace_na(def_side == "left", F)) {
                    if (next_x > 0) {
                      scrape_results$api_results$meta$home_team
                    } else if (next_x < 0) {
                      scrape_results$api_results$meta$away_team
                    } else {
                      NA_character_
                    }
                  } else {
                    NA_character_
                  }
                }
              } else {
                NA_character_
              }
            } else {
              team
            }
          }
        ),
      event_reason_3 =
        ifelse(
          event_type == "stoppage" &
            stringr::str_detect(event_reason_1 |> stringr::str_to_lower(), "offside") &
            !is.na(event_team_api) &
            next_faceoff_zone != "N",
          "intentional",
          NA_character_
        )
    ) |>
    dplyr::select(
      -c(home_team_def_zone_imp, next_faceoff_x, next_faceoff_team, next_faceoff_zone)
    )
}

.join_api_pbp_to_html_pbp <- function(html_pbp, api_pbp, verbose = F) {
  if (verbose) {
    message("Joining HTML and API play-by-plays")
  }

  starting_rows <- nrow(html_pbp)

  ret <-
    html_pbp |>
    dplyr::group_by(game_period, game_seconds, event_type) |>
    dplyr::mutate(event_type_index = cumsum(!is.na(event_type))) |>
    dplyr::full_join(
      api_pbp |>
        dplyr::mutate(
          event_type =
            event_type |>
            stringr::str_replace_all(
              c(
                "blocked-shot" = "BLOCK",
                "delayed-penalty" = "DELPEN",
                "faceoff" = "FAC",
                "game-end" = "GEND",
                "giveaway" = "GIVE",
                "shot-on-goal" = "SHOT",
                "goal" = "GOAL",
                "hit" = "HIT",
                "missed-shot" = "MISS",
                "failed-shot-attempt" = "MISS",
                "penalty" = "PENL",
                "period-end" = "PEND",
                "period-start" = "PSTR",
                "stoppage" = "STOP",
                "takeaway" = "TAKE",
                "shootout-complete" = "SOC"
              )
            )
        ) |>
        dplyr::mutate(
          dplyr::across(
            .cols = c(event_type_detail, tidyselect::starts_with("event_reason_")),
            .fns = function(x) {
              x |>
                stringr::str_replace_all("-+", " ") |>
                stringr::str_to_title()
            }
          )
        ) |>
        dplyr::group_by(game_period, game_seconds, event_type) |>
        dplyr::mutate(event_type_index = cumsum(!is.na(event_type))),
      by = dplyr::join_by(game_id, game_period, game_seconds, event_type, event_type_index)
    ) |>
    dplyr::ungroup() |>
    tibble::rowid_to_column() |>
    dplyr::mutate(
      event_team = ifelse(is.na(event_team_api), event_team_html, event_team_api),
      event_team_zone = ifelse(is.na(zone_code), event_team_zone_html, zone_code),
      event_player_1 = ifelse(is.na(event_player_1), event_player_1_html, event_player_1),
      event_player_2 = ifelse(is.na(event_player_2), event_player_2_html, event_player_2),
      event_player_3 = ifelse(is.na(event_player_3), event_player_3_html, event_player_3),
      event_detail_1 =
        dplyr::case_when(
          event_type %in% c("BLOCK", "MISS", "SHOT", "GOAL", "PENL") ~
            ifelse(
              is.na(event_type_detail),
              ifelse(
                event_detail_1_html != tidyr::replace_na(event_reason_1, ""),
                event_detail_1_html,
                NA_character_
              ),
              event_type_detail
            ),
          event_type == "STOP" ~
            event_reason_1,
          T ~ NA_character_
        ) |>
        stringr::str_to_title(),
      event_detail_2 =
        dplyr::case_when(
          event_type %in% c("MISS", "SHOT", "GOAL") ~
            ifelse(is.na(event_reason_1), event_detail_2_html, event_reason_1),
          event_type == "BLOCK" ~
            ifelse(
              is.na(event_reason_1),
              event_detail_2_html,
              ifelse(event_reason_1 == "Blocked", NA_character_, event_reason_1)
            ),
          event_type == "PENL" ~ penalty_class,
          event_type == "STOP" ~
            event_reason_2,
          T ~ NA_character_
        ) |>
        stringr::str_to_title(),
      event_detail_3 =
        dplyr::case_when(
          event_type %in% c("BLOCK", "MISS", "SHOT", "GOAL") ~
            ifelse(is.na(event_reason_2), event_detail_3_html, event_reason_2),
          event_type == "PENL" ~
            ifelse(
              is.na(penalty_in_minutes),
              stringr::str_extract("\\d+", event_detail_2_html),
              as.character(penalty_in_minutes)
            ),
          event_type == "STOP" ~
            event_reason_3,
          T ~ NA_character_
        ) |>
        stringr::str_to_title()
    ) |>
    dplyr::select(
      game_id, game_period, game_seconds, event_id, event_type, event_team,
      event_team_strength = event_strength, game_strength_state, event_team_zone,
      event_description, event_player_1, event_player_2, event_player_3,
      event_detail_1, event_detail_2, event_detail_3, coords_x, coords_y,
      event_distance_pbp = event_distance_html, home_team_def_zone,
      tidyselect::starts_with("away_on"),
      tidyselect::starts_with("home_on"),
      away_goalie_api,
      home_goalie_api
    )

  if (nrow(ret) > starting_rows) {
    warning(
      "{unique(api_pbp$game_id)} API contains plays not in HTML play-by-play" |>
        glue::glue()
    )
  }

  ret
}

.split_on_ice_ids <- function(df) {
  df |>
    dplyr::mutate(
      away_on =
        away |>
        purrr::map(
          function(on) {
            tibble::tibble(
              value =
                on |>
                stringr::str_split(", ") |>
                purrr::flatten_chr() |>
                as.integer()
            ) |>
              tibble::rowid_to_column() |>
              dplyr::mutate(
                name =
                  "away_on_{rowid}" |>
                  glue::glue()
              ) |>
              dplyr::select(-rowid) |>
              tidyr::pivot_wider(names_from = name, values_from = value)
          }
        ),
      home_on =
        home |>
        purrr::map(
          function(on) {
            tibble::tibble(
              value =
                on |>
                stringr::str_split(", ") |>
                purrr::flatten_chr() |>
                as.integer()
            ) |>
              tibble::rowid_to_column() |>
              dplyr::mutate(
                name =
                  "home_on_{rowid}" |>
                  glue::glue()
              ) |>
              dplyr::select(-rowid) |>
              tidyr::pivot_wider(names_from = name, values_from = value)
          }
        )
    ) |>
    tidyr::unnest(c(away_on, home_on)) |>
    dplyr::select(-c(away, home))
}

.get_shift_events <- function(scrape_results, verbose = F) {
  if (verbose) {
    message("Cleaning shift events for play-by-play")
  }

  scrape_results$html_results$shifts |>
    dplyr::inner_join(
      scrape_results$api_results$rosters |>
        dplyr::select(venue, sweater_number, api_id, position),
      by = dplyr::join_by(venue, sweater_number)
    ) |>
    tidyr::pivot_longer(
      c(shift_start, shift_end),
      names_to = "event_type",
      values_to = "game_seconds"
    ) |>
    dplyr::arrange(game_period, game_seconds, venue, event_type, api_id) |>
    (\(df) {
      away_on <- c()
      away_goalie <- NA_integer_
      home_on <- c()
      home_goalie <- NA_integer_

      for (r in seq(nrow(df))) {
        if (df[[r, "event_type"]] == "shift_start") {
          if (df[[r, "venue"]] == "away") {
            if (df[[r, "position"]] == "G") {
              away_goalie <- df[[r, "api_id"]]
            } else {
              away_on <- sort(c(away_on, df[[r, "api_id"]]))
            }
          } else {
            if (df[[r, "position"]] == "G") {
              home_goalie <- df[[r, "api_id"]]
            } else {
              home_on <- sort(c(home_on, df[[r, "api_id"]]))
            }
          }
        } else {
          if (df[[r, "venue"]] == "away") {
            if (df[[r, "position"]] == "G") {
              away_goalie <- NA_integer_
            } else {
              away_on <- purrr::discard(away_on, magrittr::equals, df[[r, "api_id"]])
            }
          } else {
            if (df[[r, "position"]] == "G") {
              home_goalie <- NA_integer_
            } else {
              home_on <- purrr::discard(home_on, magrittr::equals, df[[r, "api_id"]])
            }
          }
        }

        df[[r, "away"]] <- stringr::str_c(away_on, collapse = ", ")
        df[[r, "away_skaters_on"]] <- length(away_on)
        df[[r, "away_goalie"]] <- away_goalie
        df[[r, "home"]] <- stringr::str_c(home_on, collapse = ", ")
        df[[r, "home_skaters_on"]] <- length(home_on)
        df[[r, "home_goalie"]] <- home_goalie
      }

      df
    })() |>
    dplyr::group_by(
      game_id, game_period, game_seconds, event_type = "CHANGE", venue
    ) |>
    dplyr::summarise(
      away = dplyr::last(away),
      away_skaters_on = dplyr::last(away_skaters_on),
      away_goalie = dplyr::last(away_goalie),
      home = dplyr::last(home),
      home_skaters_on = dplyr::last(home_skaters_on),
      home_goalie = dplyr::last(home_goalie),
      .groups = "drop"
    ) |>
    tibble::rowid_to_column(var = "shift_id") |>
    .split_on_ice_ids() |>
    dplyr::arrange(shift_id) |>
    dplyr::mutate(
      event_team_html =
        ifelse(
          venue == "home",
          scrape_results$api_results$meta$home_team,
          scrape_results$api_results$meta$away_team
        )
    )
}

.replace_sweater_numbers_with_player_ids <- function(html_pbp, scrape_results, verbose = F) {
  if (verbose) {
    message("Matching on-ice sweater numbers to API IDs")
  }

  html_pbp |>
    dplyr::select(
      event_id, game_period, game_seconds,
      tidyselect::starts_with("home_on"),
      tidyselect::starts_with("away_on")
    ) |>
    tidyr::pivot_longer(
      c(tidyselect::starts_with("home_on"), tidyselect::starts_with("away_on")),
      names_to = "venue",
      values_to = "sweater_number"
    ) |>
    dplyr::mutate(venue = venue |> stringr::str_sub(end = 4)) |>
    dplyr::left_join(
      scrape_results$api_results$rosters |>
        dplyr::select(venue, sweater_number, api_id, position),
      by = dplyr::join_by(venue, sweater_number)
    ) |>
    dplyr::mutate(
      away_goalie = ifelse(stringr::str_detect(venue, "away") & position == "G", api_id, NA_integer_),
      home_goalie = ifelse(stringr::str_detect(venue, "home") & position == "G", api_id, NA_integer_)
    ) |>
    dplyr::group_by(event_id) |>
    tidyr::fill(c(away_goalie, home_goalie), .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::filter(
      !(
        tidyr::replace_na(api_id, -2) == tidyr::replace_na(away_goalie, -1) |
          tidyr::replace_na(api_id, -2) == tidyr::replace_na(home_goalie, -1)
      )
    ) |>
    tidyr::pivot_wider(
      id_cols = c(event_id, game_period, game_seconds, away_goalie, home_goalie),
      names_from = venue,
      values_from = api_id,
      values_fn = \(x) stringr::str_c(sort(x), collapse = ", ")
    ) |>
    .split_on_ice_ids()
}

.get_event_ids_with_shifts <- function(html_pbp, scrape_results, shift_events, verbose = F) {
  html_pbp <-
    html_pbp |>
    .replace_sweater_numbers_with_player_ids(scrape_results, verbose)

  shift_events <-
    shift_events |>
    dplyr::mutate(end = dplyr::lead(game_seconds)) |>
    dplyr::filter(!is.na(end)) |>
    dplyr::mutate(
      game_seconds =
        purrr::map2(
          game_seconds, end,
          function(s, e) {
            tibble::tibble(
              game_seconds = seq(s, e)
            )
          }
        )
    ) |>
    tidyr::unnest(game_seconds) |>
    dplyr::select(-c(game_id, event_type, venue, end))

  dplyr::left_join(
    html_pbp,
    shift_events,
    by = c(
      "game_period",
      "game_seconds",
      "away_goalie",
      "home_goalie",
      dplyr::intersect(
        html_pbp |>
          colnames() |>
          purrr::keep(
            .p = stringr::str_detect,
            pattern = "(home|away)_on_"
          ),
        shift_events |>
          colnames() |>
          purrr::keep(
            .p = stringr::str_detect,
            pattern = "(home|away)_on_"
          )
      )
    )
  )
}

.add_shifts_to_html_pbp <- function(html_pbp, scrape_results, verbose = F) {
  if (verbose) {
    message("Adding shift events to play-by-play")
  }

  shift_events <- .get_shift_events(scrape_results, verbose)

  event_ids_with_shifts <- .get_event_ids_with_shifts(html_pbp, scrape_results, shift_events, verbose)

  session <- scrape_results$api_results$meta$session
  game <-
    scrape_results$api_results$meta$game_id |>
    as.character() |>
    stringr::str_sub(start = 7)
  round <- game |> stringr::str_sub(start = 2, end = 2)
  series <- game |> stringr::str_sub(start = 3, end = 3)
  series_game <- game |> stringr::str_sub(start = 4)

  has_shootout <-
    session != 3 |
    (session == 3 & round == "0" & series == "0")

  html_pbp |>
    dplyr::select(
      -c(
        tidyselect::starts_with("away_on"),
        tidyselect::starts_with("home_on")
      )
    ) |>
    dplyr::left_join(
      event_ids_with_shifts |>
        dplyr::select(-game_seconds),
      by = dplyr::join_by(game_period, event_id)
    ) |>
    dplyr::bind_rows(
      shift_events |>
        dplyr::mutate(event_id = 0) |>
        dplyr::group_by(game_period, venue) |>
        dplyr::filter(!(event_type == "CHANGE" & shift_id == max(shift_id))) |>
        dplyr::ungroup()
    ) |>
    dplyr::group_by(game_seconds, game_period) |>
    dplyr::mutate(
      fac_event_id = c(event_id[event_type == "FAC"], NA) |> head(1),
      is_penalty_shot =
        stringr::str_to_lower(event_description) |>
        stringr::str_detect("penalty shot"),
      event_sort_order =
        ifelse(
          has_shootout & game_period == 5,
          dplyr::case_when(
            event_type == "PSTR" ~ 1,
            event_type == "PEND" ~ 3,
            event_type == "GEND" ~ 4,
            T ~ 2
          ),
          dplyr::case_when(
            event_type %in% c("SHOT", "MISS", "BLOCK", "HIT", "GIVE", "TAKE") ~
              ifelse(
                event_id < fac_event_id | is.na(fac_event_id),
                ifelse(is_penalty_shot, 2, 1),
                7
              ) |>
              tidyr::replace_na(1),
            event_type %in% c("STOP", "PENL", "GOAL") ~ 2,
            event_type %in% c("PEND", "EISTR") ~ 3,
            event_type == "GEND" ~ 8,
            event_type %in% c("PSTR", "EIEND") ~ 5,
            event_type == "FAC" ~ 6,
            T ~ 4
          )
        )
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      game_seconds,
      game_period,
      event_sort_order,
      event_id
    ) |>
    dplyr::mutate(
      max_shift = shift_id |> tidyr::replace_na(-1) |> cummax(),
      shift_id = ifelse(shift_id < max_shift, max_shift, shift_id)
    ) |>
    tidyr::fill(shift_id, .direction = "downup") |>
    dplyr::group_by(game_period) |>
    tidyr::fill(home_team_def_zone, .direction = "downup") |>
    dplyr::ungroup() |>
    dplyr::group_by(shift_id) |>
    dplyr::mutate(
      dplyr::across(
        .cols =
          c(
            tidyselect::starts_with("away_on"),
            away_skaters_on,
            away_goalie,
            tidyselect::starts_with("home_on"),
            home_skaters_on,
            home_goalie
          ),
        .fns = (\(x) dplyr::first(x))
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      scrape_results$api_results$meta |>
        dplyr::select(game_id, away_team, home_team),
      by = dplyr::join_by(game_id)
    ) |>
    dplyr::mutate(
      event_length = tidyr::replace_na(dplyr::lead(game_seconds) - game_seconds, 0),
      home_skater_strength_state =
        dplyr::case_when(
          is_penalty_shot ~
            "Penalty Shot",
          game_period == 5 & has_shootout ~
            "Shootout",
          T ~
            "{
              ifelse(is.na(home_goalie), 'E', '')
            }{
              tidyr::replace_na(home_skaters_on, 0)
            }v{
              tidyr::replace_na(away_skaters_on, 0)
            }{
              ifelse(is.na(away_goalie), 'E', '')
            }" |>
            glue::glue()
        ),
      empty_net_team =
        dplyr::case_when(
          is.na(away_goalie) & is.na(home_goalie) ~ "Both",
          is.na(away_goalie) ~ away_team,
          is.na(home_goalie) ~ home_team,
          T ~ NA_character_
        ),
      event_team = ifelse(event_type == "CHANGE", event_team_html, event_team),
      shift_id = dplyr::dense_rank(shift_id),
      cumulative_penalty_shots = cumsum(home_skater_strength_state == "Penalty Shot"),
      shift_id =
        shift_id +
        cumulative_penalty_shots,
        # (home_skater_strength_state == "Penalty Shot"),
      shift_id = ifelse(home_skater_strength_state == "Shootout", max(shift_id) + 1, shift_id),
      # shift_id = dplyr::dense_rank(shift_id),
      dplyr::across(
        .cols =
          c(
            tidyselect::starts_with("away_on"),
            tidyselect::starts_with("home_on"),
          ),
        .fns =
          (\(api_id, strength_state)
            ifelse(
              strength_state %in% c("Penalty Shot", "Shootout"),
              NA_integer_,
              api_id)
          ),
        strength_state = home_skater_strength_state
      ),
      home_skaters_on =
        ifelse(
          home_skater_strength_state %in% c("Penalty Shot", "Shootout"),
          0,
          home_skaters_on
        ),
      away_skaters_on =
        ifelse(
          home_skater_strength_state %in% c("Penalty Shot", "Shootout"),
          0,
          away_skaters_on
        )
    ) |>
    dplyr::select(-c(event_id)) |>
    tibble::rowid_to_column(var = "event_id") |>
    dplyr::select(
      game_id, game_period, game_seconds, shift_id, event_id, event_type,
      event_length, event_team, event_team_strength, home_skater_strength_state,
      empty_net_team, event_team_zone, event_description, event_player_1,
      event_player_2, event_player_3, event_detail_1, event_detail_2,
      event_detail_3, coords_x, coords_y, event_distance_pbp, home_team_def_zone,
      away_team, tidyselect::starts_with("away_on"), away_skaters_on, away_goalie,
      home_team, tidyselect::starts_with("home_on"), home_skaters_on, home_goalie
    )
}

.clean_metadata <- function(scrape_results, verbose = F) {
  if (verbose) {
    message("Cleaning metadata")
  }

  scrape_results$api_results$meta |>
    dplyr::full_join(
      scrape_results$html_results$meta,
      by = dplyr::join_by(game_id)
    )
}

.clean_game_rosters <- function(scrape_results, verbose = F) {
  if (verbose) {
    message("Cleaning game rosters")
  }

  scrape_results$api_results$rosters |>
    dplyr::full_join(
      scrape_results$html_results$rosters |>
        dplyr::select(-team),
      by = dplyr::join_by(game_id, venue, sweater_number, position_category, position)
    ) |>
    dplyr::select(
      game_id, api_id, name, venue, team, sweater_number, position_category, position, letter
    )
}

.clean_scratches <- function(scrape_results, verbose = F) {
  if (verbose) {
    message("Cleaning scratches")
  }

  api_scratches <- scrape_results$api_results$scratches
  html_scratches <- scrape_results$html_results$scratches

  if (nrow(api_scratches) > 0) {
    if (nrow(html_scratches) > 0) {
      api_scratches |>
        dplyr::mutate(
          name_caps = name |> stringr::str_to_upper()
        ) |>
        dplyr::left_join(
          html_scratches |>
            dplyr::select(
              name_caps = name,
              venue,
              sweater_number,,
              letter,
              position_category,
              position
            ),
          by = dplyr::join_by(venue, name_caps)
        ) |>
        dplyr::select(
          game_id, api_id, name, venue, team, sweater_number, position_category, position, letter
        )
    } else {
      api_scratches
    }
  } else if (nrow(html_scratches) > 0) {
    html_scratches |>
      dplyr::select(-team) |>
      dplyr::left_join(
        tibble::tibble(
          venue = c("away", "home"),
          team =
            c(
              scrape_results$api_results$meta$away_team,
              scrape_results$api_results$meta$home_team
            )
        ),
        by = dplyr::join_by(venue)
      ) |>
      dplyr::select(
        game_id, name, venue, team, sweater_number, position_category, position, letter
      )
  } else {
    tibble::tibble(game_id = integer(0))
  }
}

.clean_coaches <- function(scrape_results, verbose = F) {
  if (verbose) {
    message("Cleaning coaches")
  }

  api_coaches <- scrape_results$api_results$coaches
  html_coaches <- scrape_results$html_results$coaches

  if (nrow(api_coaches) == 2 | nrow(html_coaches) == 0) {
    api_coaches
  } else {
    if (nrow(api_coaches) != 0) {
      api_coaches |>
        dplyr::select(-team) |>
        dplyr::bind_rows(
          html_coaches |>
            dplyr::select(-team) |>
            dplyr::filter(!venue %in% api_coaches$venue)
        ) |>
        dplyr::left_join(
          tibble::tibble(
            venue = c("away", "home"),
            team =
              c(
                scrape_results$api_results$meta$away_team,
                scrape_results$api_results$meta$home_team
              )
          ),
          by = dplyr::join_by(venue)
        )
    } else {
      html_coaches |>
        dplyr::select(-team) |>
        dplyr::left_join(
          tibble::tibble(
            venue = c("away", "home"),
            team =
              c(
                scrape_results$api_results$meta$away_team,
                scrape_results$api_results$meta$home_team
              )
          ),
          by = dplyr::join_by(venue)
        )
    }
  }
}

.clean_officials <- function(scrape_results, verbose = F) {
  if (verbose) {
    message("Cleaning officials")
  }

  referees <- {
    if(nrow(scrape_results$api_results$referees) > 0) {
      scrape_results$api_results$referees
    } else {
      scrape_results$html_results$referees
    }
  }

  linesmen <- {
    if(nrow(scrape_results$api_results$linesmen) > 0) {
      scrape_results$api_results$linesmen
    } else {
      scrape_results$html_results$linesmen
    }
  }

  dplyr::bind_rows(
    referees |>
      dplyr::mutate(role = "R"),
    linesmen |>
      dplyr::mutate(role = "L")
  )
}

.clean_shifts <- function(scrape_results, clean_pbp, verbose = F) {
  if (verbose) {
    message("Cleaning shifts")
  }

  if (nrow(scrape_results$html_results$shifts) > 0) {
    faceoffs <-
      clean_pbp |>
      dplyr::filter(event_type == "FAC") |>
      dplyr::select(game_period, game_seconds, event_id, event_team_strength, event_team, event_team_zone, home_team, away_team) |>
      dplyr::group_by(game_period, game_seconds) |>
      dplyr::filter(event_id == min(event_id)) |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(c(home_team, away_team), values_to = "team", names_to = "venue") |>
      dplyr::mutate(
        venue = venue |> stringr::str_sub(end = 4),
        zone =
          ifelse(
            team == event_team,
            event_team_zone,
            dplyr::case_when(
              event_team_zone == "O" ~ "D",
              event_team_zone == "D" ~ "O",
              T ~ event_team_zone
            )
          ) |>
          stringr::str_c("ZS")
      )

    scrape_results$html_results$shifts |>
      dplyr::left_join(
        tibble::tibble(
          venue = c("away", "home"),
          team = c(
            scrape_results$api_results$meta$away_team,
            scrape_results$api_results$meta$home_team
          )
        ),
        by = dplyr::join_by(venue)
      ) |>
      dplyr::left_join(
        scrape_results$api_results$rosters |>
          dplyr::select(team, sweater_number, api_id),
        by = dplyr::join_by(team, sweater_number)
      ) |>
      dplyr::left_join(
        faceoffs |>
          dplyr::transmute(
            game_period,
            shift_start = game_seconds,
            team,
            shift_start_zone = zone
          ),
        by = dplyr::join_by(team, game_period, shift_start)
      ) |>
      dplyr::left_join(
        faceoffs |>
          dplyr::transmute(
            game_period,
            shift_end = game_seconds,
            team,
            shift_end_zone = zone
          ),
        by = dplyr::join_by(team, game_period, shift_end)
      ) |>
      dplyr::mutate(
        shift_start_zone = tidyr::replace_na(shift_start_zone, "OTF"),
        shift_end_zone = tidyr::replace_na(shift_end_zone, "OTF")
      ) |>
      dplyr::ungroup() |>
      dplyr::select(
        game_id,
        api_id,
        team,
        game_period,
        shift_start,
        shift_start_zone,
        shift_end,
        shift_end_zone
      )
  } else {
    tibble::tibble(game_id = integer(0))
  }
}

.clean_pbp <- function(scrape_results, verbose = F) {
  if (verbose) {
    message("Cleaning play-by-play")
  }

  clean_html_pbp <- .extract_details_from_html_pbp(scrape_results$html_results$pbp, scrape_results, verbose)

  clean_api_pbp <- .extract_details_from_api_pbp(scrape_results$api_results$pbp, scrape_results, verbose)

  combined_pbp <- .join_api_pbp_to_html_pbp(clean_html_pbp, clean_api_pbp, verbose)

  .add_shifts_to_html_pbp(combined_pbp, scrape_results, verbose) |>
    dplyr::select(
      tidyselect::where(
        fn = (\(x) {!all(is.na(x))})
      )
    )
}

clean_game_details_all_sources <- function(scrape_results, verbose = F) {
  if (verbose) {
    message("Cleaning scrape results")
  }

  if (!identical(names(scrape_results), c("api_results", "html_results"))) {
    stop("scrape_results must be the output of the get_game_details_all_sources() function")
  }

  scrape_results$html_results$shifts <-
    scrape_results$html_results$shifts |>
    .manually_change_shift_events(scrape_results$api_results$meta$game_id) |>
    .manually_add_shift_events(scrape_results$api_results$meta$game_id) |>
    .manually_clean_shifts(scrape_results$api_results$meta$game_id) |>
    .preclean_shifts_from_html_report(scrape_results, verbose)

  clean_meta <- .clean_metadata(scrape_results, verbose)
  clean_rosters <- .clean_game_rosters(scrape_results, verbose)
  clean_scratches <- .clean_scratches(scrape_results, verbose)
  clean_coaches <- .clean_coaches(scrape_results, verbose)
  clean_officials <- .clean_officials(scrape_results, verbose)
  clean_pbp <- .clean_pbp(scrape_results, verbose)
  clean_shifts <- .clean_shifts(scrape_results, clean_pbp, verbose)

  list(
    game_metadata_clean = clean_meta,
    game_rosters_clean = clean_rosters,
    scratches_clean = clean_scratches,
    coaches_clean = clean_coaches,
    officials_clean = clean_officials,
    shifts_clean = clean_shifts,
    pbp_clean = clean_pbp
  )
}
