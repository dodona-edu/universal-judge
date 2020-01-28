---
--- Filter MathJax double dollars $$ to single dollars for use in LaTeX and friends.
---

local f = io.open(filename, "r")
local contents = f:read("*all")
f:close()
local filtered = contents:gsub("%$%$", "$")
local name = filename:gsub("(.*/)(.*)", "%2")
local r = io.open(name .. ".filtered", "w")
r:write(filtered, "\n")
r:close()
return name .. ".filtered"
