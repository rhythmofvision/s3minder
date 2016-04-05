
local config = {
   redis_host = "127.0.0.1",
   redis_port = 6379,
   keys_limit = 50,

   cookie_name = "hts"
}

config.redis_failed_connection = function (err)
   return "Failed to connect to redis"
end

return config
