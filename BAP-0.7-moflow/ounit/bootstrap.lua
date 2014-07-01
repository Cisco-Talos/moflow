
local os = os
local error = error
local lfs = require("lfs")
local package = package

module("bootstrap")

function init () 
  local ci_url = os.getenv("CI_URL")
  if not ci_url then
    ci_url = "http://mini:8080/job/continuous-integration/label=debian-squeeze64/lastSuccessfulBuild/artifact/dist/ci.zip"
  end
  local bootstrapdir = "build/bootstrap"
  if lfs.attributes(bootstrapdir) then
    for fn in lfs.dir(bootstrapdir) do
      if fn ~= "." and fn ~= ".." then
        local realfn = bootstrapdir .. "/" .. fn
        if not os.remove(realfn) then
          error("Cannot remove " .. realfn)
        end
      end
    end
    lfs.rmdir("build/bootstrap")
  end
  lfs.mkdir("build")
  lfs.mkdir("build/bootstrap")
  local topdir = lfs.currentdir()
  lfs.chdir("build/bootstrap")
  if os.execute("curl -o ci.zip " .. ci_url) ~=  0 then
    error "Cannot download ci.zip"
  end
  if os.execute("unzip ci.zip") ~= 0 then
    error "Cannot unzip ci.zip"
  end
  lfs.chdir(topdir)
  package.path = "./build/bootstrap/?.lua;" .. package.path
end
