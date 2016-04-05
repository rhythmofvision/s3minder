
local config  = require "src/config"
local sha1    = require 'src/sha1'
local cjson   = require "cjson"
local redis   = require "resty.redis"
local session = require "src/session"

local red = redis:new()
local ok, err = red:connect(config.redis_host, config.redis_port)
if not ok then
   ngx.say(config.redis_failed_connection(err))
   return
end

local email, err = session.check_session(red)

if not email then
   local res = { status = "fail",
                 msg = "Authentication required" }

   if err then
      ngx.log(ngx.ERR, err)
   end
   
   ngx.say(cjson.encode(res))
   return
end

local hashed_email = sha1(email)   
local user_key = "user://" .. hashed_email .. "/"

local bs, err = red:smembers(user_key)
if not bs then
   ngx.say("failed to get keys: ", err)
   return
end

local buckets_res = {}
local valueset={}
local n=0

for k,v in pairs(bs) do
  n=n+1
  valueset[n]=v
end

table.sort(valueset, function (a, b)
   return string.lower(a) < string.lower(b)
end)

for idx, v in ipairs(valueset) do
   local bucket_info = {
      ukey = v,
      name = string.sub(v, 6),
      cd = "",
      size = 0,
      meta = ""
   }
   
   table.insert(buckets_res, bucket_info)
end

local res = { buckets = buckets_res }

ngx.say(cjson.encode(res))
