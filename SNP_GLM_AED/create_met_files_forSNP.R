config
lake_directory



forecast_horizon <-  35
site <- "sunp"

start_datetime <- lubridate::as_datetime("2020-11-01")
use_ler_vars <- FALSE

past_dir <- arrow::open_dataset(arrow::s3_bucket(paste0("bio230121-bucket01/flare/drivers/met/gefs-v12/stage3/site_id=", site), endpoint_override = "https://amnh1.osn.mghpcc.org", anonymous = TRUE)) 

hist_met <- (past_dir) |>
  dplyr::select(datetime, parameter,variable,prediction) |>
  dplyr::collect() |>
  # dplyr::filter(datetime %in% lubridate::as_datetime(full_time_hist)) |>
  dplyr::distinct() |>
  tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
  dplyr::arrange(parameter, datetime)

hist_met <- hist_met |>
  dplyr::mutate(WindSpeed = sqrt(eastward_wind^2 + northward_wind^2))

hist_met <- hist_met |>
  # dplyr::mutate(use_ler_vars = use_ler_vars) |>
  dplyr::rename(AirTemp = air_temperature,
                ShortWave = surface_downwelling_shortwave_flux_in_air,
                LongWave = surface_downwelling_longwave_flux_in_air,
                RelHum = relative_humidity,
                Rain = precipitation_flux,
                ensemble = parameter,
                time = datetime) |>
  dplyr::mutate(AirTemp = AirTemp - 273.15,
                RelHum = RelHum * 100,
                RelHum = ifelse(RelHum > 100, 100, RelHum),
                Rain = ifelse(use_ler_vars, Rain * (60 * 60), Rain * (60 * 60 * 24)/1000),
                Snow = 0.0) |>
  dplyr::mutate_at(dplyr::vars(all_of(c("AirTemp", "ShortWave","LongWave","RelHum","WindSpeed"))),
                   list(~round(., 2))) |>
  dplyr::mutate(Rain = round(Rain, 5),
                time = format(time, format="%Y-%m-%d %H:%M", tz = "UTC")) |>
  dplyr::select(ensemble, time, AirTemp,ShortWave, LongWave, RelHum, WindSpeed,Rain, Snow) |>
  dplyr::group_by(ensemble) |>
  dplyr::slice(-dplyr::n()) |>
  dplyr::ungroup()

n_gaps <- hist_met |>
  dplyr::mutate(time = lubridate::ymd_hm(time)) |>
  tsibble::as_tsibble(index = time, key = ensemble) |>
  tsibble::count_gaps()

if(any(hist_met$RelHum <= 0.0)) {
  idx <- which(hist_met$RelHum <= 0.0)
  hist_met$RelHum[idx] <- NA
  hist_met$RelHum <- zoo::na.approx(hist_met$RelHum, rule = 2)
}


if (nrow(n_gaps) > 0) {
  n_gaps <- n_gaps |>
    dplyr::summarise(n_gaps = max(.n, na.rm = T)) |> pull()
  message('up to ', n_gaps, ' timesteps of missing data were interpolated per ensemble in stage 3 data')
}

# fill in any missed timesteps to ensure a continuous time series
hist_met <-  hist_met |>
  dplyr::mutate(time = lubridate::ymd_hm(time)) |>
  tsibble::as_tsibble(index = time, key = ensemble) |>
  tsibble::fill_gaps() |>
  dplyr::mutate(across(AirTemp:Snow,imputeTS::na_interpolation)) |>
  dplyr::as_tibble() |>
  dplyr::mutate(time = format(time, format="%Y-%m-%d %H:%M", tz = "UTC"))

hist_met_hourly <- hist_met %>% 
  group_by(time) %>% 
  summarise(across(AirTemp:Snow, mean))

write_csv(hist_met_hourly, "~/SUNP-GLM_AED/SNP_GLM_AED/sim/inputs/SunapeeMet_Stage3_2025-05-05.csv")
