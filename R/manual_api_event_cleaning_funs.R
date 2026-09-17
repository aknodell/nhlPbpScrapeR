.manually_added_api_events <-
  readr::read_csv(
    "data/manually_added_api_events.csv",
    col_types = readr::cols(
      .default = readr::col_integer(),
      event_type = readr::col_character(),
      zone_code = readr::col_character()
    )
  )

.manually_changed_api_events <-
  readr::read_csv(
    "data/manually_changed_api_events.csv",
    col_types = readr::cols(
      .default = readr::col_integer(),
      event_type = readr::col_character()
    )
  )

.manually_add_api_events <- function(api_events, g_id) {
  api_events |>
    dplyr::bind_rows(
      .manually_added_api_events |>
        dplyr::filter(game_id == g_id)
    )
}

.manually_change_api_events <- function(api_events, g_id) {
  api_events |>
    dplyr::left_join(
      .manually_changed_api_events |>
        dplyr::filter(game_id == g_id),
      by = dplyr::join_by(game_id, game_period, game_seconds, event_type)
    ) |>
    dplyr::mutate(
      game_seconds = ifelse(is.na(new_game_seconds), game_seconds, new_game_seconds)
    ) |>
    dplyr::select(-new_game_seconds)
}

.manually_clean_api_events <- function(api_events, g_id) {
  if (g_id == 2019030012) {
    api_events <-
      api_events |>
      dplyr::filter(
        !(game_seconds == 951 & event_type == "faceoff")
      )
  }
  if (g_id == 2019021053) {
    api_events <-
      api_events |>
      dplyr::filter(
        !(game_seconds == 2334 & event_type == "stoppage")
      )
  }
  if (g_id == 2019021041) {
    api_events <-
      api_events |>
      dplyr::filter(
        !(game_seconds == 1420 & event_type == "stoppage")
      )
  }
  if (g_id == 2019020971) {
    api_events <-
      api_events |>
      dplyr::filter(
        !(game_seconds == 3135 & event_type == "stoppage")
      )
  }
  if (g_id == 2019020395) {
    api_events <-
      api_events |>
      dplyr::filter(
        !(game_seconds == 1403 & event_type == "stoppage")
      )
  }
  if (g_id == 2019020318) {
    api_events <-
      api_events |>
      dplyr::filter(
        !(game_seconds == 2358 & event_type == "stoppage")
      )
  }
  if (g_id == 2019020179) {
    api_events <-
      api_events |>
      dplyr::filter(
        !(game_seconds == 326 & event_type == "stoppage")
      )
  }
  if (g_id == 2019020019) {
    api_events <-
      api_events |>
      dplyr::filter(
        !(game_seconds == 1786 & event_type == "stoppage")
      )
  }
  if (g_id == 2015020207) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            game_seconds == 3581 ~ 3580,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2014021127) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 754 ~ 3524,
            sort_order == 755 ~ 3525,
            sort_order == 756 ~ 3527,
            T ~ game_seconds
          ),
        event_player_1 =
          dplyr::case_when(
            sort_order == 755 ~ 8467875,
            sort_order == 756 ~ 8474009,
            T ~ event_player_1
          ),
        coords_y =
          dplyr::case_when(
            sort_order == 756 ~ 3,
            sort_order == 757 ~ -11,
            T ~ coords_y
          ),
        coords_x =
          dplyr::case_when(
            sort_order == 756 ~ 83,
            T ~ coords_x
          )
      )
  }
  if (g_id == 2014020945) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 585 ~ 3469,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2014020859) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 697 ~ 3581,
            T ~ game_seconds
          ),
        event_player_1 =
          dplyr::case_when(
            sort_order == 697 ~ 8471707,
            T ~ event_player_1
          )
      )
  }
  if (g_id == 2009030414) {
    api_events <-
      api_events |>
      dplyr::filter(sort_order != 797)
  }
  if (g_id == 2009030313) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 701 ~ 3541,
            sort_order == 705 ~ 3569,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030311) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 667 ~ 3555,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030246) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 579 ~ 3195,
            sort_order == 582 ~ 3220,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030241) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 554 ~ 2923,
            sort_order == 570 ~ 3011,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030234) {
    api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 701 ~ 3551,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030214) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 562 ~ 3441,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030185) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 548 ~ 3139,
            sort_order == 585 ~ 3334,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030184) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 674 ~ 3548,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030175) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 674 ~ 3520,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030165) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 707 ~ 3751,
            sort_order == 724 ~ 3847,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030154) {
    api_events <-
      api_events |>
      dplyr::filter(sort_order != 837)
  }
  if (g_id == 2009030153) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 688 ~ 3554,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030121) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 579 ~ 3588,
            sort_order == 580 ~ 3589,
            sort_order == 581 ~ 3589,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009030114) {
    api_events <-
      api_events |>
      dplyr::mutate(
        game_seconds =
          dplyr::case_when(
            sort_order == 6 ~ 0,
            sort_order == 674 ~ 3391,
            T ~ game_seconds
          )
      )
  }
  if (g_id == 2009021210) {
    api_events <-
      api_events |>
      dplyr::filter(
        sort_order != 610,
        sort_order != 612
      )
  }
  if (g_id == 2009021191) {
    api_events <-
      api_events |>
      dplyr::mutate(
        event_type =
          dplyr::case_when(
            sort_order == 636 ~ "shot-on-goal",
            sort_order == 638 ~ "shot-on-goal",
            T ~ event_type
          )
      )
  }
  if (g_id == 2009021094) {
    api_events <-
      api_events |>
      dplyr::filter(
        sort_order != 420,
        sort_order != 421
      )
  }
  if (g_id == 2009021059) {
    api_events <-
      api_events |>
      dplyr::mutate(
        event_type =
          dplyr::case_when(
            sort_order == 712 ~ "missed-shot",
            T ~ event_type
          )
      )
  }

  api_events
}

.manually_remove_api_events <- function(api_events, g_id) {

}
