
-- Check sessions

local ck     = require "src/cookie"
local cjson  = require "cjson"
local config = require "src/config"
local redis  = require "resty.redis"
local sha1   = require 'src/sha1'

-- mymodule.lua
local M = {} -- public interface

-- private
-- local x = 1
-- local function baz() print 'test' end

-- function M.foo() print("foo", x) end

-- function M.bar()
--   M.foo()
--   baz()
--   print "bar"
-- end

function M.check_session(red)
   local cookie, err = ck:new()
   if not cookie then
      ngx.log(ngx.ERR, err)
      return
   end

   local field, err = cookie:get(config.cookie_name)
   if not field then
      ngx.log(ngx.ERR, err)
      return
   end
   
   local session_json, err = red:get(field)
   if not session_json then
      ngx.log(ngx.ERR, err)
      return
   end
   
   local session_data = cjson.decode(session_json)
   if session_data.sessionData.userEmail then
      return session_data.sessionData.userEmail, nil
   end
   
   return nil
end

function M.user_owns_key(red, user_email, key)
   if not user_email then
      return nil
   end
   
   local hashed_email = sha1(user_email)   
   local user_key = "user://" .. hashed_email .. "/"
   local bs, err = red:smembers(user_key)
   
   if not bs then
      ngx.say("failed to get keys: ", err)
      return
   end

   for k,v in pairs(bs) do
      local mstart, mend = string.find(key, v, 1, true)
      if mstart == 1 then
         return true
      end
   end
   
   return nil
end

return M
