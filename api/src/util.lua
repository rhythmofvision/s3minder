
-- Some utility functions

function string.ends(String,End)
   return End=='' or string.sub(String,-string.len(End))==End
end

function hex_to_char(x)
  return string.char(tonumber(x, 16))
end

function unescape(url)
  return url:gsub("%%(%x%x)", hex_to_char)
end

function string.trimslash(String)
   return (String:gsub("^(.-)/$", "%1"))
end

