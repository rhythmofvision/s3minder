
local config  = require "src/config"
local util    = require "src/util"
local cjson   = require "cjson"
local redis   = require "resty.redis"
local session = require "src/session"
local red     = redis:new()


local ok, err = red:connect(config.redis_host, config.redis_port)
if not ok then
   ngx.say(config.redis_failed_connection(err))
   return
end

function get_keys(prefix, start, limit)
   local keys = {}

   if not prefix or string.len(prefix) == 0 then
      return keys
   end
   
   if not start then start = 0 end
   
   -- local db_key = "s3://" .. bucket .. unescape(prefix)
   local bucket_keys_len, err = red:llen(prefix)
   
   if bucket_keys_len == 0 then
      return keys
   end
   
   local end_index = start + limit
   if end_index > bucket_keys_len then
      end_index = bucket_keys_len
   end
   
   local bucket_keys_full = {}
   local bucket_keys_ukey = {}
   local bucket_keys, err = red:lrange(prefix, start, end_index)
   
   for k, v in ipairs(bucket_keys) do
      table.insert(bucket_keys_ukey, prefix .. v)
      
      if string.ends(v, "/") then
         v = string.trimslash(v)
      end
      
      table.insert(bucket_keys_full, prefix .. v)
      
   end
   
   -- "s3://" .. bucket .. unescape(prefix) .. v
   
   local bucket_keys_data, err = red:mget(unpack(bucket_keys_full))
   
   for k,v in ipairs(bucket_keys_data) do
      local key_data = cjson.decode(v)
      key_data.ukey  = bucket_keys_ukey[k]
      key_data.kind = "image"
      key_data.meta = "&nbsp;"
      
      -- local key = {
      --    key = v,
      --    size = 100,
      --    kind = "image",
      --    sc = 0,
      --    owner = "mikahil",
      --    lm = "date",
      --    meta = "meta"
      -- }
      
      table.insert(keys, key_data)
   end
   
   return keys
end

-- The decoded URI can be found in ngx.var.uri. It does not contain
-- the query string, if you need it see ngx.var.query_string.

-- EDIT: if you cannot use this, here is a simple way to unescape a
-- URL in Lua.

local bucket = ngx.var.arg_bucket
local prefix = ngx.var.arg_prefix
local key    = ngx.var.arg_key
local start  = ngx.var.arg_start

-- local keys = get_keys(bucket, prefix, start, config.keys_limit)

local email, err = session.check_session(red)
if not email then
   local res = { status = "fail",
                 msg = "Authentication required1" }
                 

   if err then
      ngx.log(ngx.ERR, err)
   end
   
   ngx.say(cjson.encode(res))
   return
end

local unkey = unescape(key)
if not string.ends(unkey, "/") then
   unkey = unkey .. "/"
end

local owns, data  = session.user_owns_key(red, email, unkey)

if not owns then
   local res = { status = "fail",
                 msg = "Authentication required2" }

   if err then
      ngx.log(ngx.ERR, err)
   end
   
   ngx.say(cjson.encode(res))
   return
end

local keys  = get_keys(unkey, start, config.keys_limit)

if next(keys) == nil then
   ngx.say(cjson.encode({ status = "okay",
                          keys = cjson.null }))
   return
end

local res = { status = "okay",
              keys = keys }

local ok, err = red:set_keepalive(10000, 20)

ngx.say(cjson.encode(res))
