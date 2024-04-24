.noaa_env <- new.env()
local(
  {
    dotenv::load_dot_env()
    .key <- Sys.getenv("NOAA_API_KEY")
    Sys.unsetenv("NOAA_API_KEY")
  },
  env = .noaa_env
)
