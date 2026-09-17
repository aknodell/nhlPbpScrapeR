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
  } else if (g_id == 2016021194) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            (venue == "away" & shift_end_time == 3895) ~ duration + 5,
            T ~ duration
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
  } else if (g_id == 2016020139) {
    s |>
      dplyr::filter(
        !(venue == "away" & game_period == 1 & shift_start_time == 1200)
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
  } else if (g_id == 2015021197) {
    s |>
      dplyr::mutate(
        sweater_number =
          dplyr::case_when(
            venue == "away" & sweater_number == 13 ~ 15,
            T ~ sweater_number
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
      )
  } else if (g_id == 2015020508) {
    s |>
      dplyr::filter(
        !(shift_start_time == 12 & sweater_number == 5 & venue == "away"),
        !(shift_start_time == 182 & sweater_number == 45 & venue == "away"),
        !(shift_start_time == 531 & sweater_number == 5 & venue == "away"),
        !(shift_start_time == 653 & sweater_number == 5 & venue == "away"),
        !(shift_start_time == 654 & sweater_number == 45 & venue == "away"),
      ) |>
      dplyr::mutate(
        sweater_number =
          dplyr::case_when(
            venue == "away" & sweater_number == 5 ~ 45,
            T ~ sweater_number
          ),
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 45 & shift_start_time == 0 ~ duration + 53,
            venue == "away" & sweater_number == 45 & shift_start_time == 148 ~ duration + 14,
            venue == "away" & sweater_number == 45 & shift_start_time == 506 ~ duration + 44,
            venue == "away" & sweater_number == 45 & shift_start_time == 647 ~ duration + 23,
            T ~ duration
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
  } else if (g_id == 2014021011) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & shift_end_time == 3562 ~ duration - 1,
            T ~ duration
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
  } else if (g_id == 2014020588) {
    s |>
      dplyr::filter(
        !(game_period == 1 & shift_start_time == 1200)
      )
  } else if (g_id == 2014020552) {
    s |>
      dplyr::filter(
        !(game_period == 2 & shift_start_time == 2400)
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
      )
  } else if (g_id == 2014020414) {
    s |>
      dplyr::filter(
        !(game_period == 1 & shift_start_time == 1200)
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
      )
  } else if (g_id == 2014020101) {
    s |>
      dplyr::filter(
        !(game_period == 2 & shift_start_time == 2400)
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
  } else if (g_id == 2013020934) {
    s |>
      dplyr::filter(
        !(venue == "away" & sweater_number == 71 & shift_start_time == 135)
      )
  } else if (g_id == 2013020891) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 40 & shift_start_time == 0 ~ 1200,
            T ~ duration
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
      )
  } else if (g_id == 2013020607) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 31 & shift_start_time == 1200 ~ 1200,
            T ~ duration
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
      )
  } else if (g_id == 2012020388) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & game_period == 2 & sweater_number == 31 ~ 1200,
            T ~ duration
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
  } else if (g_id == 2011021077) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 30 & shift_start_time == 1200 ~ 1200,
            T ~ duration
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
      )
  } else if (g_id == 2011020175) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 1 & shift_start_time == 1200 ~ 1200,
            T ~ duration
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
  } else if (g_id == 2010020870) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 30 & shift_start_time == 0 ~ 1200,
            T ~ duration
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
      )
  } else if (g_id == 2009021132) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 32 & shift_start_time == 1200 ~ 1200,
            T ~ duration
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
      )
  } else if (g_id == 2009021112) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 40 & shift_start_time == 1617 ~ 783,
            T ~ duration
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
  } else if (g_id == 2009020609) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 49 & shift_start_time == 2400 ~ 1200,
            T ~ duration
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
      )
  } else if (g_id == 2009020482) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "home" & sweater_number == 30 & shift_start_time == 1200 ~ 1200,
            T ~ duration
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
  } else if (g_id == 2009020079) {
    s |>
      dplyr::filter(
        !(venue == "away" & is.na(sweater_number))
      ) |>
      dplyr::mutate(
        sweater_number =
          dplyr::case_when(
            venue == "away" & sweater_number == 24 ~ 19,
            T ~ sweater_number
          ),
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 19 & shift_start_time == 509 ~ duration + 33,
            T ~ duration
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
      )
  } else if (g_id == 2012030162) {
    s |>
      dplyr::mutate(
        duration =
          dplyr::case_when(
            venue == "away" & sweater_number == 35 & shift_start_time == 2400 ~ 1200,
            T ~ duration
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
      )
  } else {
    s
  }
}
