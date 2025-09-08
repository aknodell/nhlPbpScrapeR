.manually_added_shift_events <-
  readr::read_csv(
    "data/manually_added_shift_events.csv",
    col_types = readr::cols(
      .default = readr::col_integer(),
      venue = readr::col_character()
    )
  )

.manually_changed_shift_events <-
  readr::read_csv(
    "data/manually_changed_shift_events.csv",
    col_types = readr::cols(
      .default = readr::col_integer(),
      venue = readr::col_character()
    )
  )

.manually_add_shift_events <- function(shift_events, g_id) {
  shift_events |>
    dplyr::bind_rows(
      .manually_added_shift_events |>
        dplyr::filter(game_id == g_id)
    )
}

.manually_change_shift_events <- function(shift_events, g_id) {
  shift_events |>
    dplyr::left_join(
      .manually_changed_shift_events |>
        dplyr::filter(game_id == g_id),
      by =
        dplyr::join_by(
          game_id,
          venue,
          sweater_number,
          game_period,
          shift_start_time,
          shift_end_time,
          duration
        )
    ) |>
    dplyr::mutate(
      shift_start_time =
        ifelse(
          is.na(new_shift_start_time),
          shift_start_time,
          new_shift_start_time
        ),
      shift_end_time =
        ifelse(is.na(new_shift_end_time), shift_end_time, new_shift_end_time),
      duration = ifelse(is.na(new_duration), duration, new_duration)
    ) |>
    dplyr::select(-c(new_shift_start_time, new_shift_end_time, new_duration))
}

.manually_clean_shifts <- function(s, g_id) {
  if (g_id == 2021020452) {
    s |>
      dplyr::mutate(
        duration = ifelse(duration != shift_end_time - shift_start_time, duration + 1, duration)
      )
  } else if (g_id == 2021020427) {
    s |>
      dplyr::mutate(
        duration = ifelse(duration != shift_end_time - shift_start_time, duration + 1, duration)
      )
  } else if (g_id == 2021020416) {
    s |>
      dplyr::mutate(
        duration = ifelse(duration != shift_end_time - shift_start_time, duration + 1, duration)
      )
  } else if (g_id == 2020020865) {
    s |>
      dplyr::mutate(
        ## terry's last shift
        duration =
          ifelse(venue == "away" & sweater_number == 61 & game_period == 4, 55, duration)
      )
  } else if (g_id == 2020020762) {
    s |>
      dplyr::filter(
        !(shift_start_time == 1200 & shift_end_time == 0)
      )
  } else if (g_id == 2020020367) {
    s |>
      dplyr::filter(
        !(shift_start_time == 2400 & shift_end_time == 1200)
      )
  } else if (g_id == 2020020124) {
    s |>
      dplyr::filter(
        !(shift_start_time == 3600 & shift_end_time == 2477)
      ) |>
      dplyr::mutate(
        duration =
          ifelse(venue == "home" & sweater_number == 53 & shift_start_time == 3430, 170, duration)
      )
  } else if (g_id == 2019030012) {
    s |>
    dplyr::mutate(
      duration =
        dplyr::case_when(
          venue == "away" & shift_start_time == 897 & sweater_number == 4 ~ 34,
          T ~ duration
        )
    )
  } else if (g_id == 2019021076) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 1880 & sweater_number == 4),
        !(venue == "home" & shift_start_time == 1880 & sweater_number == 28),
        !(venue == "away" & shift_start_time == 2180 & sweater_number == 24),
        !(venue == "away" & shift_start_time == 2204 & sweater_number == 21)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 1845 & sweater_number == 4) ~ 51,
            (venue == "home" & shift_start_time == 1845 & sweater_number == 28) ~ 51,
            (venue == "away" & shift_start_time == 2136 & sweater_number == 21) ~ 83,
            T ~ duration
          )
      )
  } else if (g_id == 2019021053) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 2332 & sweater_number == 14),
        !(venue == "away" & shift_start_time == 2332 & sweater_number == 22),
        !(venue == "away" & shift_start_time == 2332 & sweater_number == 55),
        !(venue == "away" & shift_start_time == 2332 & sweater_number == 81)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2317 & sweater_number == 88) ~ 67,
            (venue == "away" & shift_start_time == 2317 & sweater_number == 24) ~ 76,
            (venue == "away" & shift_start_time == 2317 & sweater_number == 98) ~ 76,
            (venue == "away" & shift_start_time == 1200 & sweater_number == 88) ~ 1200,
            T ~ duration
          )
      )
  } else if (g_id == 2019021047) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 2913 & sweater_number == 8),
        !(venue == "away" & shift_start_time == 2175 & sweater_number == 13),
        !(venue == "away" & shift_start_time == 2616 & sweater_number == 6)
      )
  } else if (g_id == 2019021019) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 0 & sweater_number == 86),
        !(venue == "home" & shift_start_time == 37 & sweater_number == 17),
        !(venue == "home" & shift_start_time == 37 & sweater_number == 20),
        !(venue == "home" & shift_start_time == 2414 & sweater_number == 77)
      ) |>
      dplyr::mutate(
        shift_start_time =
          ifelse(venue == "away" & shift_start_time == 268 & sweater_number == 48, 227, shift_start_time),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 208 & sweater_number == 21) ~ 60,
            (venue == "away" & shift_start_time == 208 & sweater_number == 46) ~ 60,
            (venue == "away" & shift_start_time == 227 & sweater_number == 48) ~ 64,
            T ~ duration
          )
      )
  } else if (g_id == 2019020963) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 1200 & sweater_number == 45)
      )
  } else if (g_id == 2019020842) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 263 & sweater_number == 46),
        !(venue == "away" & shift_start_time == 270 & sweater_number == 24)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 251 & sweater_number == 24) ~ 52,
            T ~ duration
          )
      )
  } else if (g_id == 2019020726) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2400 & sweater_number == 50) ~ 1200,
            (venue == "home" & shift_start_time == 3535 & sweater_number == 27) ~ 65,
            T ~ duration
          )
      )
  } else if (g_id == 2019020722) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2400 & sweater_number == 30) ~ 1200,
            T ~ duration
          )
      )
  } else if (g_id == 2019020710) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 132 & sweater_number == 21),
        !(venue == "home" & shift_start_time == 2462 & sweater_number == 23),
        !(venue == "home" & shift_start_time == 2462 & sweater_number == 55),
        !(venue == "home" & shift_start_time == 3027 & sweater_number == 17),
        !(venue == "home" & shift_start_time == 3168 & sweater_number == 18),
        !(venue == "home" & shift_start_time == 3424 & sweater_number == 18),
        !(venue == "away" & shift_start_time == 3494 & sweater_number == 29),
        !(venue == "away" & shift_start_time == 3494 & sweater_number == 32)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            venue == "home" & shift_start_time == 3446 & sweater_number == 72 ~ 3424,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 126 & sweater_number == 38) ~ 7,
            (venue == "home" & shift_start_time == 2430 & sweater_number == 23) ~ 43,
            (venue == "home" & shift_start_time == 2430 & sweater_number == 55) ~ 46,
            (venue == "home" & shift_start_time == 3015 & sweater_number == 17) ~ 89,
            venue == "home" & shift_start_time == 3424 & sweater_number == 72 ~ 70,
            T ~ duration
          )
      )
  } else if (g_id == 2019020708) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 2628 & sweater_number == 5),
        !(venue == "away" & shift_start_time == 2628 & sweater_number == 37)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_end_time == 2628) ~ duration + 2,
            (venue == "away" & shift_end_time == 2628) ~ duration + 15,
            T ~ duration
          )
      )
  } else if (g_id == 2019020674) {
    s |>
      dplyr::mutate(
        game_period =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 1200 & sweater_number == 30) ~ 3,
            T ~ game_period
          ),
        shift_start_time =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 1200 & sweater_number == 30) ~ 2400,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2019020580) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 2046 & sweater_number == 88),
        !(venue == "home" & shift_start_time == 2145),
        !(venue == "home" & shift_start_time == 2153)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2118 & sweater_number == 3) ~ 50,
            (venue == "home" & shift_start_time == 2118 & sweater_number == 88) ~ 50,
            (venue == "home" & shift_start_time == 2118 & sweater_number == 26) ~ 68,
            (venue == "home" & shift_start_time == 2118 & sweater_number == 89) ~ 68,
            (venue == "home" & shift_start_time == 2118 & sweater_number == 28) ~ 68,
            T ~ duration
          )
      )
  } else if (g_id == 2019020549) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 3487 & sweater_number == 44)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 3286 & sweater_number == 63) ~ 3265,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2427 & sweater_number == 21) ~ 15,
            (venue == "home" & shift_start_time == 3265 & sweater_number == 63) ~ duration + 21,
            T ~ duration
          )
      )
  } else if (g_id == 2019020535) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 1816 & sweater_number == 15),
        !(venue == "away" & shift_start_time == 1816 & sweater_number == 24),
        !(venue == "away" & shift_start_time == 1827 & sweater_number == 48),
        !(venue == "away" & shift_start_time == 1889 & sweater_number == 44)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 1484 & sweater_number == 42) ~ 1479,
            (venue == "away" & shift_start_time == 2210 & sweater_number == 5) ~ 2208,
            (venue == "away" & shift_start_time == 2842 & sweater_number == 67) ~ 2833,
            (venue == "home" & shift_start_time == 2844 & sweater_number == 8) ~ 2847,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 1479 & sweater_number == 42) ~ duration + 5,
            (venue == "away" & shift_end_time == 1806 & sweater_number == 5) ~ duration - 24,
            (venue == "away" & shift_end_time == 2208 & sweater_number == 5) ~ duration + 2,
            (venue == "away" & shift_end_time == 2629) ~ duration + 8,
            (venue == "home" & shift_end_time == 2833 & sweater_number == 67) ~ duration + 9,
            (venue == "home" & shift_end_time == 2847 & sweater_number == 8) ~ duration - 3,
            (venue == "away" & shift_end_time == 3552 & sweater_number == 33) ~ duration - 30,
            T ~ duration
          )
      )
  } else if (g_id == 2019020477) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 472),
        !(venue == "away" & shift_end_time == 553),
        !(venue == "away" & shift_start_time == 3486 & sweater_number == 73)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 780 & sweater_number == 29) ~ 778,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 423 & sweater_number != 71) ~ 130,
            (venue == "away" & shift_start_time == 435 & sweater_number == 81) ~ 118,
            (venue == "away" & shift_start_time == 778 & sweater_number == 29) ~ duration + 2,
            (venue == "away" & shift_start_time == 857 & sweater_number == 73) ~ 33,
            (venue == "away" & shift_end_time == 1761 & sweater_number == 81) ~ duration - 10,
            T ~ duration
          )
      )
  } else if (g_id == 2019020475) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 2515 & sweater_number == 19)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2461 & sweater_number == 19) ~ 61,
            (venue == "away" & shift_start_time == 3299 & sweater_number == 73) ~ 11,
            T ~ duration
          )
      )
  } else if (g_id == 2019020457) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 1200 & sweater_number == 2),
        !(venue == "away" & shift_start_time == 1200 & sweater_number == 71),
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 659 & sweater_number == 3) ~ 649,
            (venue == "away" & shift_start_time == 659 & sweater_number == 38) ~ 649,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 649 & sweater_number == 3) ~ duration + 10,
            (venue == "away" & shift_start_time == 649 & sweater_number == 39) ~ duration + 10,
            (venue == "away" & shift_end_time == 611 & sweater_number == 20) ~ duration - 26,
            (venue == "home" & shift_end_time == 963 & sweater_number == 3) ~ duration + 1,
            T ~ duration
          )
      )
  } else if (g_id == 2019020447) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 689),
        !(venue == "away" & shift_start_time == 709),
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 666 & sweater_number == 56) ~ 63,
            (venue == "away" & shift_start_time == 666 & sweater_number == 68) ~ 80,
            (venue == "away" & shift_start_time == 679 & sweater_number == 12) ~ 50,
            (venue == "away" & shift_start_time == 679 & sweater_number == 88) ~ 50,
            (venue == "away" & shift_start_time == 679 & sweater_number == 17) ~ 67,
            T ~ duration
          )
      )
  } else if (g_id == 2019020410) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 3506 & sweater_number == 72)
      )
  } else if (g_id == 2019020331) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2400 & sweater_number == 41) ~ 1200,
            T ~ duration
          )
      )
  } else if (g_id == 2019020318) {
    s |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            shift_start_time == 2358 ~ 2359,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (shift_end_time == 2358) ~ duration + 1,
            (shift_start_time == 2359) ~ duration - 1,
            T ~ duration
          )
      )
  } else if (g_id == 2019020316) {
    s |>
      dplyr::filter(
        !(venue == "away" & shift_start_time == 2489 & sweater_number != 14),
        !(venue == "away" & shift_start_time == 2666),
        !(venue == "away" & shift_start_time == 2671),
        !(venue == "away" & shift_start_time == 2973 & sweater_number != 44)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2578 & sweater_number == 44) ~ 2534,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2458 & sweater_number == 4) ~ 76,
            (venue == "away" & shift_start_time == 2473 & sweater_number == 15) ~ 61,
            (venue == "away" & shift_start_time == 2473 & sweater_number == 33) ~ 61,
            (venue == "away" & shift_start_time == 2483 & sweater_number == 67) ~ 51,
            (venue == "away" & shift_start_time == 2534 & sweater_number == 29) ~ 60,
            (venue == "away" & shift_start_time == 2534 & sweater_number == 44) ~ 60,
            (venue == "away" & shift_start_time == 2633 & sweater_number == 49) ~ 56,
            (venue == "away" & shift_start_time == 2633 & sweater_number == 34) ~ 84,
            (venue == "away" & shift_start_time == 2664 & sweater_number == 5) ~ 34,
            (venue == "away" & shift_start_time == 2664 & sweater_number == 32) ~ 34,
            (venue == "away" & shift_start_time == 2664 & sweater_number == 15) ~ 53,
            (venue == "away" & shift_start_time == 2952 & sweater_number == 5) ~ 21,
            (venue == "away" & shift_start_time == 2952 & sweater_number == 2) ~ 43,
            (venue == "away" & shift_start_time == 2952 & sweater_number == 20) ~ 43,
            (venue == "away" & shift_start_time == 2952 & sweater_number == 38) ~ 43,
            (venue == "away" & shift_start_time == 2952 & sweater_number == 24) ~ 43,
            T ~ duration
          )
      )
  } else if (g_id == 2019020221) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 1170 & sweater_number == 40),
        !(venue == "home" & shift_start_time == 1123 & sweater_number == 48),
        !(venue == "away" & shift_start_time == 1123 & sweater_number == 10),
        !(venue == "away" & shift_start_time == 1127)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 279 & sweater_number == 40) ~ 921,
            (venue == "home" & shift_start_time == 1099 & sweater_number == 55) ~ 71,
            (venue == "home" & shift_start_time == 1111 & sweater_number == 25) ~ 46,
            (venue == "home" & shift_start_time == 1111 & sweater_number == 16) ~ 59,
            (venue == "home" & shift_start_time == 1111 & sweater_number == 17) ~ 59,
            (venue == "away" & shift_start_time == 0 & sweater_number == 31) ~ 1200,
            (venue == "away" & shift_start_time == 1075 & sweater_number == 90) ~ 95,
            (venue == "away" & shift_start_time == 1111 & sweater_number == 13) ~ 69,
            (venue == "away" & shift_start_time == 1111 & sweater_number == 26) ~ 69,
            T ~ duration
          )
      )
  } else if (g_id == 2019020201) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 2283 & sweater_number == 37),
        !(venue == "home" & shift_start_time == 2283 & sweater_number == 49)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2291 & sweater_number == 72) ~ 2283,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2283 & sweater_number == 72) ~ duration + 8,
            T ~ duration
          )
      )
  } else if (g_id == 2019020178) {
    s |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2260 & sweater_number == 8) ~ 2242,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2242 & sweater_number == 8) ~ duration + 18,
            T ~ duration
          )
      )
  } else if (g_id == 2019020169) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 1200 & sweater_number == 39) ~ 1200,
            T ~ duration
          )
      )
  } else if (g_id == 2019020072) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 0 & sweater_number == 39) ~ 1200,
            T ~ duration
          )
      )
  } else if (g_id == 2019020030) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 1342 & sweater_number == 30) ~ 1058,
            T ~ duration
          )
      )
  } else if (g_id == 2019020019) {
    s |>
      dplyr::mutate(
        sweater_number =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 3520 & sweater_number == 15) ~ 94,
            T ~ sweater_number
          )
      )
  } else if (g_id == 2019020011) {
    s |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "home" & shift_start_time == 2588) ~ 2587,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "home" & shift_end_time == 2562) ~ duration + 2,
            (venue == "home" & shift_end_time == 2588) ~ duration - 1,
            (venue == "home" & shift_start_time == 2587) ~ duration + 1,
            T ~ duration
          )
      )
  } else if (g_id == 2018020890) {
    s |>
      dplyr::filter(
        !(shift_start_time == 2400 & game_period == 2)
      )
  } else if (g_id == 2018020397) {
    s |>
      dplyr::filter(
        !(shift_start_time == 2400 & game_period == 2),
        !(venue == "away" & game_period == 2 & sweater_number == 35 & shift_start_time != 1200),
        !(venue == "away" & sweater_number == 8 & shift_start_time == 2363)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2399 & sweater_number == 59) ~ 2396,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 1200 & sweater_number == 35) ~ 1200,
            (venue == "away" & shift_start_time == 2396 & sweater_number == 59) ~ 4,
            T ~ duration
          )
      )
  } else if (g_id == 2018020072) {
    s |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2548 & sweater_number == 23) ~ 2563,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            (venue == "away" & shift_start_time == 2563 & sweater_number == 23) ~ duration - 15,
            T ~ duration
          )
      )
  } else if (g_id == 2017021267) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(23, 33, 15, 36, 25, 32),
          game_period = c(3),
          shift_start_time = c(3491, 3493, 3592, 3592, 3592, 3596),
          shift_start_clock = c(3491, 3493, 3592, 3592, 3592, 3596),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(109, 107, 8, 8, 8, 4)
        )
      )
  } else if (g_id == 2017021083) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(35),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400),
          shift_end_clock = c(2400),
          duration = c(1200)
        )
      )
  } else if (g_id == 2017020820) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(18, 25, 6, 23, 36),
          game_period = c(3),
          shift_start_time = c(3570, 3570, 3570, 3592, 3592),
          shift_start_clock = c(3570, 3570, 3570, 3592, 3592),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(30, 30, 30, 8, 8)
        )
      )
  } else if (g_id == 2017020666) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(11, 93),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(2434),
          shift_end_clock = c(2434),
          duration = c(34)
        )
      )
  } else if (g_id == 2017020434) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(7, 9, 17, 44, 1),
          game_period = c(3),
          shift_start_time = c(3589, 3589, 3589, 3589, 3597),
          shift_start_clock = c(3589, 3589, 3589, 3589, 3597),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(11, 11, 11, 11, 3)
        )
      )
  } else if (g_id == 2016021194) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & shift_end_time == 3895) ~ duration + 5,
            T ~ duration
          )
      )
  } else if (g_id == 2016021163) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(1, 14, 23, 24, 33, 44),
          game_period = c(3),
          shift_start_time = c(3575),
          shift_start_clock = c(3575),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(25)
        )
      )
  } else if (g_id == 2016021088) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & shift_end_time == 1883 ~ duration + 1,
            venue == "home" & shift_start_time == 1883 ~ duration - 1,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & shift_start_time == 1883 ~ 1884,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2016020963) {
    s |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            venue == "away" & sweater_number == 89 & shift_start_time == 206 ~ 215,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 89 & shift_start_time == 215 ~ duration - 9,
            T ~ duration
          )
      )
  } else if (g_id == 2016020954) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & shift_end_time == 805 ~ duration + 1,
            venue == "home" & shift_start_time == 805 ~ duration - 1,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & shift_start_time == 805 ~ 806,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2016020936) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(40, 42),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400),
          shift_end_clock = c(2400),
          duration = c(1200, 55)
        )
      )
  } else if (g_id == 2016020933) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & shift_end_time == 3516 ~ duration + 1,
            venue == "away" & shift_start_time == 3516 ~ duration - 1,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "away" & shift_start_time == 3516 ~ 3517,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2016020915) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(35),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200)
        )
      )
  } else if (g_id == 2016020856) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(31, 96, 9, 14),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400, 1274, 1244, 1244),
          shift_end_clock = c(2400, 1274, 1244, 1244),
          duration = c(1200, 74, 44, 44)
        )
      )
  } else if (g_id == 2016020693) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & shift_end_time == 2820 ~ duration + 1,
            venue == "home" & shift_start_time == 2820 ~ duration - 1,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & shift_start_time == 2820 ~ 2821,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2016020609) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & shift_end_time == 3539 ~ duration + 1,
            venue == "home" & shift_start_time == 3539 ~ duration - 1,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & shift_start_time == 3539 ~ 3540,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2016020536) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & shift_end_time == 1742 ~ duration + 1,
            venue == "away" & shift_start_time == 1742 ~ duration - 1,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "away" & shift_start_time == 1742 ~ 1743,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2016020521) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & shift_end_time == 2108 ~ duration + 1,
            venue == "home" & shift_start_time == 2108 ~ duration - 1,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & shift_start_time == 2108 ~ 2109,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2016020511) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(35, 47, 54, 88, 37),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400, 1245, 1245, 1233, 1233),
          shift_end_clock = c(2400, 1245, 1245, 1233, 1233),
          duration = c(1200, 45, 45, 33, 33)
        )
      )
  } else if (g_id == 2016020508) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & shift_end_time == 60 ~ duration - 5,
            venue == "away" & shift_start_time == 60 ~ duration + 5,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "away" & shift_start_time == 60 ~ 55,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2016020502) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & shift_end_time == 2666 ~ duration - 1,
            venue == "home" & shift_start_time == 2666 ~ duration + 1,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & shift_start_time == 2666 ~ 2665,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2016020421) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(30, 3, 7, 81, 44, 39),
          game_period = c(3),
          shift_start_time = c(2666, 3588, 3588, 3588, 3588, 3588),
          shift_start_clock = c(2666, 3588, 3588, 3588, 3588, 3588),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(934, 12, 12, 12, 12, 12)
        )
      )
  } else if (g_id == 2016020419) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(37, 8, 44, 9, 13, 22),
          game_period = c(3),
          shift_start_time = c(2400, 3528, 3528, 3599, 3599, 3599),
          shift_start_clock = c(2400, 3528, 3528, 3599, 3599, 3599),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200, 72, 72, 1, 1, 1)
        )
      )
  } else if (g_id == 2016020367) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & shift_end_time == 1358 ~ duration - 1,
            venue == "away" & shift_start_time == 1358 ~ duration + 1,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "away" & shift_start_time == 1358 ~ 1357,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2016020179) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & shift_end_time == 3211 ~ duration - 2,
            venue == "home" & shift_start_time == 3211 ~ duration + 2,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & shift_start_time == 3211 ~ 3209,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2016020163) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(25, 60, 5, 21, 44, 27),
          game_period = c(3),
          shift_start_time = c(3292, 3567, 3582, 3582, 3582, 3592),
          shift_start_clock = c(3292, 3567, 3582, 3582, 3582, 3592),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(308, 33, 18, 18, 18, 8)
        )
      )
  } else if (g_id == 2016020139) {
    s |>
      dplyr::filter(
        !(venue == "away" & game_period == 1 & shift_start_time == 1200)
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(1),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = c(1200),
          shift_end_time = c(2400),
          shift_end_clock = c(2400),
          duration = c(1200)
        )
      )
  } else if (g_id == 2016020099) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(17, 32, 28, 93, 53, 10),
          game_period = c(3),
          shift_start_time = c(3488, 3494, 3537, 3537, 3537, 3537),
          shift_start_clock = c(3488, 3494, 3537, 3537, 3537, 3537),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(112, 106, 63, 63, 63, 63)
        )
      )
  } else if (g_id == 2015021224) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 40 & game_period == 1 ~ 1200,
            venue == "home" & sweater_number == 40 & game_period == 2 ~ 606,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & sweater_number == 40 & game_period == 2 ~ 1200,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2015021151) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & shift_end_time == 241 ~ duration - 1,
            venue == "home" & shift_start_time == 241 ~ duration + 1,
            venue == "home" & shift_end_time == 1533 ~ duration - 3,
            venue == "home" & shift_start_time == 1533 ~ duration + 3,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & shift_start_time == 241 ~ 240,
            venue == "home" & shift_start_time == 1533 ~ 1530,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2015021049) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(33, 82, 28, 2, 93, 26),
          game_period = c(3),
          shift_start_time = c(3244, 3521, 3582, 3582, 3582, 3582),
          shift_start_clock = c(3244, 3521, 3582, 3582, 3582, 3582),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(356, 79, 18, 18, 18, 18)
        )
      )
  } else if (g_id == 2015021003) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(13, 37, 11, 27),
          game_period = c(4),
          shift_start_time = c(3600, 3600, 3670, 3670),
          shift_start_clock = c(3600, 3600, 3670, 3670),
          shift_end_time = c(3672),
          shift_end_clock = c(3672),
          duration = c(72, 72, 2, 2)
        )
      )
  } else if (g_id == 2015020969) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(40),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200)
        )
      )
  } else if (g_id == 2015020918) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(14, 24, 30, 34, 47, 54),
          game_period = c(3),
          shift_start_time = c(3586),
          shift_start_clock = c(3586),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(14)
        )
      )
  } else if (g_id == 2015020900) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 33 & game_period == 2 ~ 1200,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & sweater_number == 33 & game_period == 2 ~ 1200,
            T ~ shift_start_time
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(33, 2, 4, 16, 19, 29),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(3386, 2443, 2443, 2443, 2443, 2443),
          shift_end_clock = c(3386, 2443, 2443, 2443, 2443, 2443),
          duration = c(986, 43, 43, 43, 43, 43)
        )
      )
  } else if (g_id == 2015020866) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 33 & game_period == 2 ~ 1200,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & sweater_number == 33 & game_period == 2 ~ 1200,
            T ~ shift_start_time
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(33, 2, 19),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(3491, 2428, 2428),
          shift_end_clock = c(3491, 2428, 2428),
          duration = c(1091, 28, 28)
        )
      )
  } else if (g_id == 2015020849) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(16, 26, 12, 57, 33, 18),
          game_period = c(3),
          shift_start_time = c(3555, 3555, 3555, 3555, 3573, 3573),
          shift_start_clock = c(3555, 3555, 3555, 3555, 3573, 3573),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(45, 45, 45, 45, 27, 27)
        )
      )
  } else if (g_id == 2015020825) {
    s |>
      dplyr::filter(
        !(shift_start_time == 3600 & game_period == 3),
        !(venue == "away" & shift_start_time == 3302),
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 35 & shift_end_time == 3300 ~ 1200,
            venue == "away" & sweater_number == 39 & shift_end_time == 3300 ~ duration + 7,
            venue == "away" & sweater_number == 8 & shift_end_time == 3300 ~ duration + 10,
            venue == "away" & sweater_number == 16 & shift_end_time == 3300 ~ duration + 10,
            venue == "away" & sweater_number == 48 & shift_end_time == 3300 ~ duration + 10,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(12, 35, 6),
          game_period = c(3, 4, 4),
          shift_start_time = c(3300, 3600, 3600),
          shift_start_clock = c(3300, 3600, 3600),
          shift_end_time = c(3310, 3778, 3634),
          shift_end_clock = c(3310, 3778, 3634),
          duration = c(8, 178, 34)
        )
      )
  } else if (g_id == 2015020560) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(33, 19, 7, 88, 72),
          game_period = c(4),
          shift_start_time = c(3600, 3742, 3763, 3763, 3763),
          shift_start_clock = c(3600, 3742, 3763, 3763, 3763),
          shift_end_time = c(3809),
          shift_end_clock = c(3809),
          duration = c(209, 67, 46, 46, 46)
        )
      )
  } else if (g_id == 2015020504) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(31, 3, 33, 43, 83, 90),
          game_period = c(3),
          shift_start_time = c(2400, 3573, 3586, 3586, 3586, 3586),
          shift_start_clock = c(2400, 3573, 3586, 3586, 3586, 3586),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(1200, 27, 14, 14, 14, 14)
        )
      )
  } else if (g_id == 2015020306) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(1, 2, 15, 17, 37, 53),
          game_period = c(3),
          shift_start_time = c(3591),
          shift_start_clock = c(3591),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(9)
        )
      )
  } else if (g_id == 2015020260) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(93),
          game_period = c(3),
          shift_start_time = c(3550),
          shift_start_clock = c(3550),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(50)
        )
      )
  } else if (g_id == 2015020229) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & shift_end_time == 2901 ~ duration - 2,
            venue == "home" & shift_start_time == 2901 ~ duration + 2,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & shift_start_time == 2901 ~ 2899,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2015020212) {
    s |>
      dplyr::filter(
        !(venue == "away" & sweater_number == 30 & shift_end_time == 1407),
        !(venue == "home" & sweater_number == 1 & shift_end_time == 1426),
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            venue == "away" & sweater_number == 30 & shift_end_time == 2400 ~ 1200,
            venue == "home" & sweater_number == 1 & shift_end_time == 2400 ~ 1200,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 30 & shift_end_time == 2400 ~ 1200,
            venue == "home" & sweater_number == 1 & shift_end_time == 2400 ~ 1200,
            venue == "away" & shift_end_time == 1407 ~ duration + 26,
            venue == "home" & shift_end_time == 1426 ~ duration + 7,
            T ~ duration
          )
      )
  } else if (g_id == 2015020153) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & shift_end_time == 3460 ~ duration - 1,
            venue == "home" & shift_start_time == 3460 ~ duration + 1,
            T ~ duration
          ),
        shift_start_time =
          dplyr::case_when(
            venue == "home" & shift_start_time == 3460 ~ 3459,
            T ~ shift_start_time
          )
      )
  } else if (g_id == 2015020008) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(19, 10, 28, 33, 51, 77),
          game_period = c(3),
          shift_start_time = c(3521, 3582, 3582, 3582, 3582, 3582),
          shift_start_clock = c(3521, 3582, 3582, 3582, 3582, 3582),
          shift_end_time = c(3600),
          shift_end_clock = c(3600),
          duration = c(79, 18, 18, 18, 18, 18)
        )
      )
  } else if (g_id == 2014021210) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(20, 2, 5),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = c(2400),
          shift_end_time = c(3600, 2431, 2431),
          shift_end_clock = c(3600, 2431, 2431),
          duration = c(1200, 31, 31)
        )
      )
  } else if (g_id == 2014021197) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(1, 17, 90, 2),
          game_period = c(3),
          shift_start_time = c(2400, 3485, 3542, 3593),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200, 115, 58, 7)
        )
      )
  } else if (g_id == 2014021079) {
    s |>
      dplyr::filter(
        !(venue == "away" & sweater_number == 34 & shift_end_time == 3300)
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            venue == "away" & sweater_number == 34 & shift_end_time == 3600 ~ 2400,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 34 & shift_end_time == 3600 ~ 1200,
            venue == "away" & shift_end_time == 3300 ~ duration + 2,
            T ~ duration
          )
      )
  } else if (g_id == 2014021057) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(9, 11, 20, 22, 26, 44),
          game_period = c(3),
          shift_start_time = c(3508, 3508, 3579, 3598, 3598, 3598),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(92, 92, 21, 2, 2, 2)
        )
      )
  } else if (g_id == 2014021036) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(4, 9, 15),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2428),
          shift_end_clock = shift_end_time,
          duration = c(28)
        )
      )
  } else if (g_id == 2014021011) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & shift_end_time == 3562 ~ duration - 1,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(50, 2, 4, 16, 19),
          game_period = c(3),
          shift_start_time = c(2400, 3561, 3561, 3561, 3561),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200, 39, 39, 39, 39)
        )
      )
  } else if (g_id == 2014021001) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(36, 4, 7, 33),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600, 2429, 2429, 2429),
          shift_end_clock = shift_end_time,
          duration = c(1200, 29, 29, 29)
        )
      )
  } else if (g_id == 2014020942) {
    s |>
      dplyr::filter(
        !(venue == "away" & sweater_number == 6 & shift_start_time == 3540),
        !(venue == "away" & sweater_number == 9 & shift_start_time == 3540),
        !(venue == "away" & sweater_number == 33 & shift_start_time == 3540),
        !(venue == "away" & sweater_number == 59 & shift_start_time == 3540)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 6 & shift_end_time == 3539 ~ duration + 61,
            venue == "away" & sweater_number == 9 & shift_end_time == 3539 ~ duration + 49,
            venue == "away" & sweater_number == 18 & shift_end_time == 3539 ~ duration + 1,
            venue == "away" & sweater_number == 33 & shift_end_time == 3539 ~ duration + 61,
            venue == "away" & sweater_number == 59 & shift_end_time == 3539 ~ duration + 61,
            venue == "away" & sweater_number == 63 & shift_end_time == 3539 ~ duration + 1,
            T ~ duration
          )
      )
  } else if (g_id == 2014020833) {
    s |>
      dplyr::filter(
        !(venue == "home" & sweater_number == 2 & shift_start_time == 3782),
        !(venue == "home" & sweater_number == 20 & shift_start_time == 3782),
        !(venue == "home" & sweater_number == 32 & shift_start_time == 3782),
        !(venue == "home" & sweater_number == 50 & shift_start_time == 3782),
        !(venue == "away" & sweater_number == 4 & shift_start_time == 3796),
        !(venue == "away" & sweater_number == 29 & shift_start_time == 3796),
        !(venue == "away" & sweater_number == 47 & shift_start_time == 3796),
        !(venue == "away" & sweater_number == 72 & shift_start_time == 3796)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 2 & shift_end_time == 3780 ~ duration + 31,
            venue == "home" & sweater_number == 20 & shift_end_time == 3780 ~ duration + 7,
            venue == "home" & sweater_number == 32 & shift_end_time == 3780 ~ duration + 17,
            venue == "home" & sweater_number == 50 & shift_end_time == 3780 ~ duration + 120,
            venue == "home" & sweater_number == 65 & shift_end_time == 3780 ~ duration + 2,
            venue == "away" & sweater_number == 4 & shift_end_time == 3780 ~ duration + 45,
            venue == "away" & sweater_number == 29 & shift_end_time == 3780 ~ duration + 120,
            venue == "away" & sweater_number == 47 & shift_end_time == 3780 ~ duration + 45,
            venue == "away" & sweater_number == 72 & shift_end_time == 3780 ~ duration + 21,
            venue == "away" & sweater_number == 39 & shift_end_time == 3780 ~ duration + 16,
            T ~ duration
          )
      )
  } else if (g_id == 2014020780) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(40),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200)
        )
      )
  } else if (g_id == 2014020681) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(32, 11, 14, 27, 44),
          game_period = c(4),
          shift_start_time = c(3600, 3758, 3758, 3805, 3805),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3848),
          shift_end_clock = shift_end_time,
          duration = c(248, 90, 90, 43, 43)
        )
      )
  } else if (g_id == 2014020608) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(1),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400),
          shift_end_clock = shift_end_time,
          duration = c(1200)
        )
      )
  } else if (g_id == 2014020588) {
    s |>
      dplyr::filter(
        !(game_period == 1 & shift_start_time == 1200)
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(35, 20, 25),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400, 2426, 2426),
          shift_end_clock = shift_end_time,
          duration = c(1200, 26, 26)
        )
      )
  } else if (g_id == 2014020555) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(4, 30, 2, 5, 93),
          game_period = c(4),
          shift_start_time = c(3600, 3600, 3661, 3666, 3666),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3668),
          shift_end_clock = shift_end_time,
          duration = c(68, 68, 7, 2, 2)
        )
      )
  } else if (g_id == 2014020552) {
    s |>
      dplyr::filter(
        !(game_period == 2 & shift_start_time == 2400)
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(31, 12, 39),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600, 2519, 2519),
          shift_end_clock = shift_end_time,
          duration = c(1200, 119, 119)
        )
      )
  } else if (g_id == 2014020528) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(31, 4, 6, 12, 15),
          game_period = c(4),
          shift_start_time = c(3600, 3729, 3729, 3729, 3729),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3762),
          shift_end_clock = shift_end_time,
          duration = c(162, 33, 33, 33, 33)
        )
      )
  } else if (g_id == 2014020521) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(91, 34),
          game_period = c(3),
          shift_start_time = c(3568, 3586),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(32, 14)
        )
      )
  } else if (g_id == 2014020520) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(33, 22, 29, 6, 44),
          game_period = c(4),
          shift_start_time = c(3600, 3727, 3742, 3746, 3746),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3767),
          shift_end_clock = shift_end_time,
          duration = c(167, 40, 25, 21, 21)
        )
      )
  } else if (g_id == 2014020477) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(34, 55, 67, 85, 33, 63),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600, 2456, 2456, 2456, 2440, 2440),
          shift_end_clock = shift_end_time,
          duration = c(1200, 56, 56, 56, 40, 40)
        )
      )
  } else if (g_id == 2014020437) {
    s |>
      dplyr::filter(
        !(game_period == 3 & shift_start_time == 3600)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 35 & shift_start_time == 2400 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(35),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3629),
          shift_end_clock = shift_end_time,
          duration = c(29)
        )
      )
  } else if (g_id == 2014020414) {
    s |>
      dplyr::filter(
        !(game_period == 1 & shift_start_time == 1200)
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "away", "away", "away"),
          sweater_number = c(30, 70, 43, 44),
          game_period = c(1, 2, 2, 2),
          shift_start_time = c(0, 1200, 1200, 1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(1200, 2400, 2447, 2447),
          shift_end_clock = shift_end_time,
          duration = c(1200, 1200, 47, 47)
        )
      )
  } else if (g_id == 2014020217) {
    s |>
      dplyr::filter(
        !(venue == "home" & sweater_number == 15 & shift_start_time == 3599),
        !(venue == "home" & sweater_number == 31 & shift_start_time == 3599)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 15 & shift_end_time == 3598 ~ duration + 2,
            venue == "home" & sweater_number == 31 & shift_end_time == 3598 ~ duration + 2,
            venue == "home" & shift_end_time == 3598 ~ duration + 1,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "away", "home", "away", "away"),
          sweater_number = c(31, 31, 33, 22, 33),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3900, 3900, 3657, 3655, 3660),
          shift_end_clock = shift_end_time,
          duration = c(300, 300, 57, 55, 60)
        )
      )
  } else if (g_id == 2014020165) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(30, 20),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600, 2477),
          shift_end_clock = shift_end_time,
          duration = c(1200, 77)
        )
      )
  } else if (g_id == 2014020120) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(31, 3, 7, 67, 23),
          game_period = c(3),
          shift_start_time = c(2400, 3414, 3509, 3509, 3580),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200, 186, 91, 91, 20)
        )
      )
  } else if (g_id == 2014020101) {
    s |>
      dplyr::filter(
        !(game_period == 2 & shift_start_time == 2400)
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(24),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2433),
          shift_end_clock = shift_end_time,
          duration = c(33)
        )
      )
  } else if (g_id == 2014020023) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(31),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400),
          shift_end_clock = shift_end_time,
          duration = c(1200)
        )
      )
  } else if (g_id == 2014020017) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(41),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200)
        )
      )
  } else if (g_id == 2014020003) {
    s |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            venue == "home" & sweater_number == 1 & shift_end_time == 3515 ~ 2400,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 1 & shift_start_time == 1200 ~ 1200,
            venue == "home" & sweater_number == 1 & shift_start_time == 2400 ~ 1115,
            T ~ duration
          )
      )
  } else if (g_id == 2013021142) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(19, 36, 51, 57, 4, 93),
          game_period = c(3),
          shift_start_time = c(3519, 3519, 3519, 3519, 3557, 3598),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(81, 81, 81, 81, 43, 2)
        )
      )
  } else if (g_id == 2013021093) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(1, 4, 23, 28),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600, 2442, 2442, 2442),
          shift_end_clock = shift_end_time,
          duration = c(1200, 42, 42, 42)
        )
      )
  } else if (g_id == 2013021058) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(7, 3, 65, 93, 16, 19),
          game_period = c(3),
          shift_start_time = c(3519, 3533, 3533, 3533, 3561, 3561),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(81, 67, 67, 67, 39, 39)
        )
      )
  } else if (g_id == 2013021049) {
    s |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            venue == "home" & sweater_number == 44 & shift_start_time == 3737 ~ 3773,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 44 & shift_start_time == 3773 ~ 2,
            T ~ duration
          )
      )
  } else if (g_id == 2013020891) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 40 & shift_start_time == 0 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(40, 9, 14, 19),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(1504, 1238, 1238, 1238),
          shift_end_clock = shift_end_time,
          duration = c(304, 38, 38, 38)
        )
      )
  } else if (g_id == 2013020814) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(1, 8, 24, 40, 61, 92),
          game_period = c(3),
          shift_start_time = c(2400, 3571, 3571, 3571, 3571, 3571),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200, 29, 29, 29, 29, 29)
        )
      )
  } else if (g_id == 2013020806) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(20, 21, 26, 91),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400, 1236, 1236, 1236),
          shift_end_clock = shift_end_time,
          duration = c(1200, 36, 36, 36)
        )
      )
  } else if (g_id == 2013020664) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 39 & shift_start_time == 1200 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(39, 15, 6, 59),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3514, 2450, 2459, 2459),
          shift_end_clock = shift_end_time,
          duration = c(1114, 50, 59, 59)
        )
      )
  } else if (g_id == 2013020630) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(16, 41, 61, 65, 93),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3658),
          shift_end_clock = shift_end_time,
          duration = c(58)
        )
      )
  } else if (g_id == 2013020607) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 31 & shift_start_time == 1200 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(31, 7, 61),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2474, 2437, 2437),
          shift_end_clock = shift_end_time,
          duration = c(74, 37, 37)
        )
      )
  } else if (g_id == 2013020515) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(83),
          game_period = c(3),
          shift_start_time = c(2417),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2454),
          shift_end_clock = shift_end_time,
          duration = c(37)
        )
      )
  } else if (g_id == 2013020399) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(31, 8, 32, 28, 52),
          game_period = c(3),
          shift_start_time = c(2400, 3554, 3554, 3598, 3598),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200, 46, 46, 2, 2)
        )
      )
  } else if (g_id == 2013020271) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 34 & shift_start_time == 1200 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(34, 51, 77),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2949, 2437, 2437),
          shift_end_clock = shift_end_time,
          duration = c(549, 37, 37)
        )
      )
  } else if (g_id == 2013020257) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(30, 5, 27),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400, 1244, 1244),
          shift_end_clock = shift_end_time,
          duration = c(1200, 44, 44)
        )
      )
  } else if (g_id == 2013020077) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(40),
          game_period = c(2),
          shift_start_time = c(1314, 1459),
          shift_start_clock = shift_start_time,
          shift_end_time = c(1320, 1506),
          shift_end_clock = shift_end_time,
          duration = c(6, 47)
        )
      )
  } else if (g_id == 2012020534) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(18, 12, 71),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(1242, 1254, 1254),
          shift_end_clock = shift_end_time,
          duration = c(42, 54, 54)
        )
      )
  } else if (g_id == 2012020526) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(1, 22, 33, 75, 57),
          game_period = c(3),
          shift_start_time = c(2400, 3549, 3549, 3549, 3596),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200, 51, 51, 51, 4)
        )
      )
  } else if (g_id == 2012020388) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & game_period == 2 & sweater_number == 31 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(31),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2947),
          shift_end_clock = shift_end_time,
          duration = c(547)
        )
      )
  } else if (g_id == 2012020384) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(32),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400),
          shift_end_clock = shift_end_time,
          duration = c(1200)
        )
      )
  } else if (g_id == 2012020281) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(9, 20),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2417),
          shift_end_clock = shift_end_time,
          duration = c(17)
        )
      )
  } else if (g_id == 2012020261) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(35, 44, 12, 17, 26, 47),
          game_period = c(3),
          shift_start_time = c(2400, 3568, 3591, 3591, 3591, 3591),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200, 32, 9, 9, 9, 9)
        )
      )
  } else if (g_id == 2012020179) {
    s |>
      dplyr::filter(
        !(venue == "home" & sweater_number == 40 & shift_start_time == 3805),
        !(venue == "home" & sweater_number == 17 & shift_start_time == 3805),
        !(venue == "home" & sweater_number == 49 & shift_start_time == 3805),
        !(venue == "away" & sweater_number == 4 & shift_start_time == 3796),
        !(venue == "away" & sweater_number == 19 & shift_start_time == 3796),
        !(venue == "away" & sweater_number == 62 & shift_start_time == 3796),
        !(venue == "away" & sweater_number == 30 & shift_start_time == 3796)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 40 & shift_start_time == 3600 ~ 300,
            venue == "home" & sweater_number == 17 & shift_start_time == 3755 ~ duration + 25 + 18,
            venue == "home" & sweater_number == 33 & shift_start_time == 3755 ~ duration + 25,
            venue == "home" & sweater_number == 49 & shift_start_time == 3755 ~ duration + 25 + 12,
            venue == "home" & sweater_number == 55 & shift_start_time == 3755 ~ duration + 25,
            venue == "away" & sweater_number == 30 & shift_start_time == 3600 ~ 300,
            venue == "away" & sweater_number == 4 & shift_start_time == 3729 ~ duration + 16 + 27,
            venue == "away" & sweater_number == 19 & shift_start_time == 3755 ~ duration + 16 + 19,
            venue == "away" & sweater_number == 27 & shift_start_time == 3747 ~ duration + 16,
            venue == "away" & sweater_number == 62 & shift_start_time == 3755 ~ duration + 16 + 9,
            T ~ duration
          )
      )
  } else if (g_id == 2012020171) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(35, 22, 12, 6, 18, 74),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400, 1257, 1256, 1245, 1245, 1245),
          shift_end_clock = shift_end_time,
          duration = c(1200, 57, 56, 45, 45, 45)
        )
      )
  } else if (g_id == 2012020163) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(1),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3900),
          shift_end_clock = shift_end_time,
          duration = c(300)
        )
      )
  } else if (g_id == 2012020102) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(31, 27, 6, 11, 19, 21),
          game_period = c(3),
          shift_start_time = c(2400, 3499, 3503, 3521, 3595, 3595),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200, 101, 97, 79, 5, 5)
        )
      )
  } else if (g_id == 2012020030) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(19, 28, 93),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2446),
          shift_end_clock = shift_end_time,
          duration = c(46)
        )
      )
  } else if (g_id == 2011021077) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 30 & shift_start_time == 1200 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(30, 6, 8, 23, 55),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2516, 2431, 2431, 2431, 2428),
          shift_end_clock = shift_end_time,
          duration = c(116, 31, 31, 31, 28)
        )
      )
  } else if (g_id == 2011020980) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(31, 15, 21, 25, 6),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400, 1256, 1256, 1256, 1233),
          shift_end_clock = shift_end_time,
          duration = c(1200, 56, 56, 56, 33)
        )
      )
  } else if (g_id == 2011020799) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(30, 4, 36, 44, 12),
          game_period = c(4),
          shift_start_time = c(3600, 3687, 3687, 3687, 3732),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3734),
          shift_end_clock = shift_end_time,
          duration = c(134, 47, 47, 47, 2)
        )
      )
  } else if (g_id == 2011020768) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(28, 19, 44, 68, 17),
          game_period = c(3),
          shift_start_time = c(3534, 3568, 3568, 3568, 3577),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(66, 32, 32, 32, 23)
        )
      )
  } else if (g_id == 2011020606) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(1, 27, 28),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600, 2432, 2432),
          shift_end_clock = shift_end_time,
          duration = c(1200, 32, 32)
        )
      )
  } else if (g_id == 2011020499) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 31 & shift_start_time == 0 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(31),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2277),
          shift_end_clock = shift_end_time,
          duration = c(1077)
        )
      )
  } else if (g_id == 2011020409) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(71, 11, 18, 48, 9, 14),
          game_period = c(3),
          shift_start_time = c(3501, 3506, 3506, 3578, 3596, 3596),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(99, 94, 94, 22, 4, 4)
        )
      )
  } else if (g_id == 2011020264) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(75),
          game_period = c(2),
          shift_start_time = c(1484),
          shift_start_clock = shift_start_time,
          shift_end_time = c(1492),
          shift_end_clock = shift_end_time,
          duration = c(8)
        )
      )
  } else if (g_id == 2011020175) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 1 & shift_start_time == 1200 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(1),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3497),
          shift_end_clock = shift_end_time,
          duration = c(1097)
        )
      )
  } else if (g_id == 2011020103) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(50, 15, 19, 22, 33, 39),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400, 1232, 1232, 1232, 1232, 1232),
          shift_end_clock = shift_end_time,
          duration = c(1200, 32, 32, 32, 32, 32)
        )
      )
  } else if (g_id == 2011020094) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(32),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200)
        )
      )
  } else if (g_id == 2010021175) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(5, 18),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2445),
          shift_end_clock = shift_end_time,
          duration = c(45)
        )
      )
  } else if (g_id == 2010021160) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(18, 17, 19, 44, 48, 5),
          game_period = c(3),
          shift_start_time = c(3477, 3519, 3544, 3544, 3544, 3599),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(123, 81, 56, 56, 56, 1)
        )
      )
  } else if (g_id == 2010021122) {
    s |>
      dplyr::filter(
        !(venue == "home" & sweater_number == 35 & shift_start_time == 1357),
        !(venue == "home" & sweater_number == 33 & shift_start_time == 1357),
        !(venue == "home" & sweater_number == 43 & shift_start_time == 1357),
        !(venue == "home" & sweater_number == 52 & shift_start_time == 1357),
        !(venue == "home" & sweater_number == 55 & shift_start_time == 1357),


        !(venue == "away" & sweater_number == 34 & shift_start_time == 1358),
        !(venue == "away" & sweater_number == 2 & shift_start_time == 1358),
        !(venue == "away" & sweater_number == 19 & shift_start_time == 1358),
        !(venue == "away" & sweater_number == 81 & shift_start_time == 1358)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 35 & shift_start_time == 1200 ~ 1079,
            venue == "home" & sweater_number == 17 & shift_start_time == 1314 ~ duration + 2,
            venue == "home" & sweater_number == 33 & shift_start_time == 1314 ~ duration + 2 + 18,
            venue == "home" & sweater_number == 43 & shift_start_time == 1314 ~ duration + 2 + 26,
            venue == "home" & sweater_number == 52 & shift_start_time == 1314 ~ duration + 2 + 18,
            venue == "home" & sweater_number == 55 & shift_start_time == 1314 ~ duration + 2 + 33,

            venue == "away" & sweater_number == 34 & shift_start_time == 1200 ~ 1079,
            venue == "away" & sweater_number == 23 & shift_start_time == 1294 ~ duration + 3,
            venue == "away" & sweater_number == 42 & shift_start_time == 1294 ~ duration + 3,
            venue == "away" & sweater_number == 2 & shift_start_time == 1332 ~ duration + 3 + 11,
            venue == "away" & sweater_number == 19 & shift_start_time == 1306 ~ duration + 3 + 30,
            venue == "away" & sweater_number == 81 & shift_start_time == 1318 ~ duration + 3 + 30,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "away"),
          sweater_number = c(35, 34),
          game_period = c(2),
          shift_start_time = c(2279),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400),
          shift_end_clock = shift_end_time,
          duration = c(121)
        )
      )
  } else if (g_id == 2010021085) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(35, 4, 12, 28, 40),
          game_period = c(4),
          shift_start_time = c(3600, 3754, 3754, 3754, 3754),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3758),
          shift_end_clock = shift_end_time,
          duration = c(158, 4, 4, 4, 4)
        )
      )
  } else if (g_id == 2010021065) {
    s |>
      dplyr::filter(
        !(venue == "away" & sweater_number == 24 & shift_start_time == 3580)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & sweater_number == 24 &  shift_start_time == 3530) ~ 70,
            T ~ duration
          )
      )
  } else if (g_id == 2010021006) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(32, 14, 29),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400, 1228, 1228),
          shift_end_clock = shift_end_time,
          duration = c(1200, 28, 28)
        )
      )
  } else if (g_id == 2010020996) {
    s |>
      dplyr::filter(
        !(venue == "home" & sweater_number == 31 & shift_start_time == 1397),
        !(venue == "home" & sweater_number == 13 & shift_start_time == 1397),
        !(venue == "home" & sweater_number == 14 & shift_start_time == 1397),
        !(venue == "home" & sweater_number == 20 & shift_start_time == 1397),
        !(venue == "home" & sweater_number == 32 & shift_start_time == 1397),
        !(venue == "home" & sweater_number == 44 & shift_start_time == 1397),

        !(venue == "away" & sweater_number == 40 & shift_start_time == 1381),
        !(venue == "away" & sweater_number == 23 & shift_start_time == 1381),
        !(venue == "away" & sweater_number == 43 & shift_start_time == 1381),
        !(venue == "away" & sweater_number == 44 & shift_start_time == 1381),
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 31 & shift_start_time == 1200 ~ 1200,
            venue == "home" & sweater_number == 13 & shift_start_time == 1355 ~ duration + 12,
            venue == "home" & sweater_number == 14 & shift_start_time == 1375 ~ duration + 12,
            venue == "home" & sweater_number == 20 & shift_start_time == 1369 ~ duration + 12 + 19,
            venue == "home" & sweater_number == 32 & shift_start_time == 1369 ~ duration + 12 + 19,
            venue == "home" & sweater_number == 44 & shift_start_time == 1369 ~ duration + 12 + 29,

            venue == "away" & sweater_number == 40 & shift_start_time == 1200 ~ 1200,
            venue == "away" & sweater_number == 23 & shift_start_time == 1353 ~ duration + 1 + 27,
            venue == "away" & sweater_number == 28 & shift_start_time == 1316 ~ duration + 1,
            venue == "away" & sweater_number == 43 & shift_start_time == 1353 ~ duration + 1 + 45,
            venue == "away" & sweater_number == 44 & shift_start_time == 1335 ~ duration + 1 + 45,
            venue == "away" & sweater_number == 73 & shift_start_time == 1316 ~ duration + 1,
            T ~ duration
          )
      )
  } else if (g_id == 2010020924) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(6, 20, 11, 74, 10, 12),
          game_period = c(3),
          shift_start_time = c(3497, 3497, 3592, 3592, 3599, 3599),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(103, 103, 8, 8, 1, 1)
        )
      )
  } else if (g_id == 2010020870) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 30 & shift_start_time == 0 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(30, 30, 5, 18, 17, 24, 42, 27, 38),
          game_period = c(2, 3, 2, 2, 3, 3, 3, 3, 3),
          shift_start_time = c(1200, 2400, 1200, 1200, 2400, 2400, 2400, 2400, 2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400, 3518, 1256, 1256, 2421, 2421, 2421, 2492, 2492),
          shift_end_clock = shift_end_time,
          duration = c(1200, 1118, 56, 56, 21, 21, 21, 92, 92)
        )
      )
  } else if (g_id == 2010020662) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(19),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2426),
          shift_end_clock = shift_end_time,
          duration = c(26)
        )
      )
  } else if (g_id == 2010020523) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(33, 4, 91, 6, 26),
          game_period = c(4),
          shift_start_time = c(3600, 3636, 3636, 3655, 3655),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3673),
          shift_end_clock = shift_end_time,
          duration = c(73, 37, 37, 18, 18)
        )
      )
  } else if (g_id == 2010020284) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(19, 29, 44, 26, 9, 28),
          game_period = c(3),
          shift_start_time = c(3545, 3545, 3554, 3562, 3576, 3597),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(55, 55, 46, 38, 24, 3)
        )
      )
  } else if (g_id == 2010020201) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(35),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200)
        )
      )
  } else if (g_id == 2010020122) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "home", "home", "home", "home", "home", "home", "home", "home", "away", "away", "away", "away", "away"),
          sweater_number = c(1, 17, 3, 41, 1, 3, 17, 21, 29, 31, 6, 10, 25, 26),
          game_period = c(2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
          shift_start_time = c(1200, 1200, 1200, 1200, 3600, 3625, 3625, 3625, 3625, 3600, 3625, 3625, 3625, 3625),
          shift_start_clock = shift_start_time,
          shift_end_time = c(1569, 1252, 1254, 1254, 3628, 3628, 3628, 3628, 3628, 3628, 3628, 3628, 3628, 3628),
          shift_end_clock = shift_end_time,
          duration = c(369, 52, 54, 54, 28, 3, 3, 3, 3, 28, 3, 3, 3, 3)
        )
      )
  } else if (g_id == 2010020068) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(30, 4, 21, 29, 39, 44),
          game_period = c(3),
          shift_start_time = c(2400, 3590, 3590, 3590, 3590, 3590),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200, 10, 10, 10, 10, 10)
        )
      )
  } else if (g_id == 2009021227) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(30, 2, 8, 11, 23, 28),
          game_period = c(3),
          shift_start_time = c(2400, 3597, 3597, 3597, 3597, 3597),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200, 3, 3, 3, 3, 3)
        )
      )
  } else if (g_id == 2009021170) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(30, 18, 10, 26),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600, 2432, 2418, 2418),
          shift_end_clock = shift_end_time,
          duration = c(1200, 32, 18, 18)
        )
      )
  } else if (g_id == 2009021141) {
    s |>
      dplyr::filter(
        !(venue == "home" & sweater_number == 3 & shift_start_time == 953),
        !(venue == "home" & sweater_number == 41 & shift_start_time == 980),
        !(venue == "home" & sweater_number == 14 & shift_start_time == 1040),
        !(venue == "home" & sweater_number == 21 & shift_start_time == 1080),
      ) |>
      dplyr::mutate(
        shift_start_time =
          dplyr::case_when(
            venue == "home" & sweater_number == 5 & shift_start_time == 977 ~ 980,
            T ~ shift_start_time
          ),
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 5 & shift_start_time == 980 ~ duration - 3,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "away", "away", "away", "away", "away", "away", "away", "away"),
          sweater_number = c(15, 30, 4, 11, 29, 55, 30, 11, 17),
          game_period = c(1, 2, 2, 2, 2, 2, 3, 3, 3),
          shift_start_time = c(1122, 1200, 1200, 1200, 1200, 1200, 2400, 2400, 2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(1040, 2400, 1281, 1281, 1281, 1281, 3600, 2452, 2437),
          shift_end_clock = shift_end_time,
          duration = c(18, 1200, 81, 81, 81, 81, 1200, 52, 37)
        )
      )
  } else if (g_id == 2009021132) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 32 & shift_start_time == 1200 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(32, 3, 26, 37),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3502, 2456, 2456, 2456),
          shift_end_clock = shift_end_time,
          duration = c(1102, 56, 56, 56)
        )
      )
  } else if (g_id == 2009021131) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 32 & shift_start_time == 1200 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(32, 3),
          game_period = c(3, 3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2807, 2438),
          shift_end_clock = shift_end_time,
          duration = c(407, 38)
        )
      )
  } else if (g_id == 2009021112) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 40 & shift_start_time == 1617 ~ 783,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(40, 25, 57),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2715, 2447, 2447),
          shift_end_clock = shift_end_time,
          duration = c(315, 47, 47)
        )
      )
  } else if (g_id == 2009021098) {
    s |>
      dplyr::filter(
        !(venue == "home" & shift_start_time == 1196)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 2 & shift_start_time == 1171 ~ duration + 5,
            venue == "home" & sweater_number == 12 & shift_start_time == 1130 ~ duration + 1,
            venue == "home" & sweater_number == 16 & shift_start_time == 1171 ~ duration + 5,
            venue == "home" & sweater_number == 20 & shift_start_time == 1171 ~ duration + 5,
            venue == "home" & sweater_number == 38 & shift_start_time == 1171 ~ duration + 5,
            venue == "home" & sweater_number == 43 & shift_start_time == 0 ~ duration + 5,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(51, 43, 2),
          game_period = c(1, 2, 2),
          shift_start_time = c(1196, 1200, 1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(1200, 2400, 1246),
          shift_end_clock = shift_end_time,
          duration = c(4, 1200, 46)
        )
      )
  } else if (g_id == 2009021067) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(32, 15, 17, 11, 7, 8),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600, 2451, 2451, 2443, 2434, 2434),
          shift_end_clock = shift_end_time,
          duration = c(1200, 51, 51, 43, 34, 34)
        )
      )
  } else if (g_id == 2009021038) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(1),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400),
          shift_end_clock = shift_end_time,
          duration = c(1200)
        )
      )
  } else if (g_id == 2009021030) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(44, 5, 29),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(1253, 1265, 1265),
          shift_end_clock = shift_end_time,
          duration = c(53, 65, 65)
        )
      )
  } else if (g_id == 2009020979) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(91, 9, 26, 4, 6, 16),
          game_period = c(3),
          shift_start_time = c(3512, 3533, 3533, 3552, 3556, 3588),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(88, 67, 67, 48, 44, 12)
        )
      )
  } else if (g_id == 2009020918) {
    s |>
      dplyr::filter(
        !(venue == "home" & sweater_number == 25 & shift_start_time == 2791),
        !(venue == "home" & sweater_number == 32 & shift_start_time == 2791),
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 3 & shift_start_time == 2755 ~ duration + 18,
            venue == "home" & sweater_number == 6 & shift_start_time == 2755 ~ duration + 18,
            venue == "home" & sweater_number == 11 & shift_start_time == 2724 ~ duration + 18,
            venue == "home" & sweater_number == 25 & shift_start_time == 2724 ~ duration + 18 + 29,
            venue == "home" & sweater_number == 32 & shift_start_time == 2400 ~ 1200,
            venue == "home" & sweater_number == 51 & shift_start_time == 2724 ~ duration + 18,
            T ~ duration
          )
      )
  } else if (g_id == 2009020898) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(30, 26, 27),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400, 1240, 1240),
          shift_end_clock = shift_end_time,
          duration = c(1200, 40, 40)
        )
      )
  } else if (g_id == 2009020871) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(20, 4, 28, 3, 8, 21),
          game_period = c(3),
          shift_start_time = c(2400, 3530, 3572, 3590, 3590, 3590),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200, 70, 28, 10, 10, 10)
        )
      )
  } else if (g_id == 2009020708) {
    s |>
      dplyr::filter(
        !(venue == "home" & sweater_number == 3 & shift_start_time == 3841),
        !(venue == "home" & sweater_number == 31 & shift_start_time == 3841),
        !(venue == "home" & sweater_number == 37 & shift_start_time == 3841),
        !(venue == "home" & sweater_number == 91 & shift_start_time == 3841),
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 3 & shift_start_time == 3808 ~ duration + 1 + 6,
            venue == "home" & sweater_number == 17 & shift_start_time == 3826 ~ duration + 1,
            venue == "home" & sweater_number == 31 & shift_start_time == 3600 ~ 300,
            venue == "home" & sweater_number == 37 & shift_start_time == 3796 ~ duration + 1 + 6,
            venue == "home" & sweater_number == 91 & shift_start_time == 3808 ~ duration + 1 + 28,
            T ~ duration
          )
      )
  } else if (g_id == 2009020665) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(49, 20, 14, 36, 44, 11),
          game_period = c(3),
          shift_start_time = c(2400, 3481, 3520, 3540, 3540, 3555),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200, 119, 80, 60, 60, 45)
        )
      )
  } else if (g_id == 2009020652) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(1, 5, 55),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600, 2457, 2452),
          shift_end_clock = shift_end_time,
          duration = c(1200, 57, 52)
        )
      )
  } else if (g_id == 2009020609) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 49 & shift_start_time == 2400 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(44, 49, 44, 20, 11),
          game_period = c(3, 4, 4, 4, 4),
          shift_start_time = c(3550, 3600, 3600, 3600, 3600),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600, 3717, 3683, 3683, 3645),
          shift_end_clock = shift_end_time,
          duration = c(50, 117, 83, 83, 45)
        )
      )
  } else if (g_id == 2009020541) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 30 & shift_start_time == 1200 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(30, 4, 55),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2940, 2485, 2462),
          shift_end_clock = shift_end_time,
          duration = c(540, 85, 62)
        )
      )
  } else if (g_id == 2009020494) {
    s |>
      dplyr::filter(
        !(venue == "home" & sweater_number == 6 & shift_start_time == 2387)
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 30 & shift_start_time == 1200 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(6),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2446),
          shift_end_clock = shift_end_time,
          duration = c(46)
        )
      )
  } else if (g_id == 2009020482) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 30 & shift_start_time == 1200 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(30),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2455),
          shift_end_clock = shift_end_time,
          duration = c(55)
        )
      )
  } else if (g_id == 2009020391) {
    s |>
      dplyr::filter(
        !(venue == "home" & sweater_number == 2 & shift_start_time == 3795),
        !(venue == "home" & sweater_number == 7 & shift_start_time == 3795),
        !(venue == "home" & sweater_number == 39 & shift_start_time == 3795),
        !(venue == "home" & sweater_number == 88 & shift_start_time == 3795),

        !(venue == "away" & sweater_number == 1 & shift_start_time == 3822),
        !(venue == "away" & sweater_number == 8 & shift_start_time == 3822),
        !(venue == "away" & sweater_number == 50 & shift_start_time == 3822),
        !(venue == "away" & sweater_number == 61 & shift_start_time == 3822),
      ) |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 39 & shift_start_time == 3600 ~ 300,
            venue == "home" & sweater_number == 2 & shift_start_time == 3729 ~ duration + 10 + 14,
            venue == "home" & sweater_number == 7 & shift_start_time == 3756 ~ duration + 10 + 48,
            venue == "home" & sweater_number == 19 & shift_start_time == 3756 ~ duration + 10,
            venue == "home" & sweater_number == 88 & shift_start_time == 3776 ~ duration + 10 + 30,

            venue == "away" & sweater_number == 1 & shift_start_time == 3600 ~ 300,
            venue == "away" & sweater_number == 6 & shift_start_time == 3775 ~ duration + 37,
            venue == "away" & sweater_number == 8 & shift_start_time == 3775 ~ duration + 37 + 48,
            venue == "away" & sweater_number == 50 & shift_start_time == 3775 ~ duration + 37 + 8,
            venue == "away" & sweater_number == 61 & shift_start_time == 3775 ~ duration + 37 + 17,
            T ~ duration
          )
      )
  } else if (g_id == 2009020380) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(30, 2, 3, 19, 20),
          game_period = c(4),
          shift_start_time = c(3600, 3638, 3638, 3638, 3638),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3678),
          shift_end_clock = shift_end_time,
          duration = c(78, 40, 40, 40, 40)
        )
      )
  } else if (g_id == 2009020275) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(32),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3900),
          shift_end_clock = shift_end_time,
          duration = c(300)
        )
      )
  } else if (g_id == 2009020157) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(50, 18),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400, 1251),
          shift_end_clock = shift_end_time,
          duration = c(1200, 51)
        )
      )
  } else if (g_id == 2009020079) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(1, 18),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400, 1276),
          shift_end_clock = shift_end_time,
          duration = c(1200, 76)
        )
      )
  } else if (g_id == 2009020002) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 31 & shift_start_time == 1200 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "away", "away", "away"),
          sweater_number = c(15, 31, 79, 6),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2433, 3298, 2455, 2436),
          shift_end_clock = shift_end_time,
          duration = c(33, 898, 55, 36)
        )
      )
  } else if (g_id == 2019030151) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 2 & shift_start_time == 304 ~ duration + 1,
            venue == "home" & sweater_number == 22 & shift_start_time == 304 ~ duration + 1,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(19, 26, 81, 88, 23),
          game_period = c(3),
          shift_start_time = c(2400, 2400, 2400, 2400, 3164),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2444, 2444, 2444, 2435, 3170),
          shift_end_clock = shift_end_time,
          duration = c(44, 44, 44, 35, 6)
        )
      )
  } else if (g_id == 2017030242) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 29 & shift_start_time == 3600 ~ 1200,
            venue == "away" & sweater_number == 31 & shift_start_time == 3600 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home", "away", "away"),
          sweater_number = c(29, 31, 61),
          game_period = c(5),
          shift_start_time = c(4800),
          shift_start_clock = shift_start_time,
          shift_end_time = c(5113, 5113, 4813),
          shift_end_clock = shift_end_time,
          duration = c(313, 313, 13)
        )
      )
  } else if (g_id == 2016030185) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(33, 29, 67, 13, 6, 77),
          game_period = c(4),
          shift_start_time = c(3600, 4624, 4682, 4691, 4694, 4694),
          shift_start_clock = shift_start_time,
          shift_end_time = c(4695),
          shift_end_clock = shift_end_time,
          duration = c(1095, 71, 13, 4, 1, 1)
        )
      )
  } else if (g_id == 2014030135) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(29, 7, 14, 71, 72, 87),
          game_period = c(2),
          shift_start_time = c(1200),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2400, 2496, 2469, 2469, 2469, 2469),
          shift_end_clock = shift_end_time,
          duration = c(1200, 96, 69, 69, 69, 69)
        )
      )
  } else if (g_id == 2013030236) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(50, 10, 12, 88, 8, 17),
          game_period = c(4),
          shift_start_time = c(3600, 4135, 4170, 4170, 4180, 4180),
          shift_start_clock = shift_start_time,
          shift_end_time = c(4182),
          shift_end_clock = shift_end_time,
          duration = c(582, 47, 12, 12, 2, 2)
        )
      )
  } else if (g_id == 2012030162) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 35 & shift_start_time == 2400 ~ 1200,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(35, 55),
          game_period = c(4),
          shift_start_time = c(3600),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3681, 3673),
          shift_end_clock = shift_end_time,
          duration = c(81, 73)
        )
      )
  } else if (g_id == 2012030153) {
    s |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("home"),
          sweater_number = c(37),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(3600),
          shift_end_clock = shift_end_time,
          duration = c(1200)
        )
      )
  } else if (g_id == 2010030181) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 35 & shift_start_time == 1270 ~ 1130,
            T ~ duration
          )
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          game_id = g_id,
          venue = c("away"),
          sweater_number = c(35, 6, 10, 11, 20, 29),
          game_period = c(3),
          shift_start_time = c(2400),
          shift_start_clock = shift_start_time,
          shift_end_time = c(2830, 2441, 2441, 2441, 2441, 2441),
          shift_end_clock = shift_end_time,
          duration = c(430, 41, 41, 41, 41, 41)
        )
      )
  } else {
    s |>
      print()
  }
}
