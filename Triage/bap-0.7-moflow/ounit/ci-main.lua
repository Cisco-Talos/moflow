
bootstrap = require("bootstrap")

bootstrap.init()

oasis = require("oasis")
darcs = require("darcs")
ci = require("ci")
godi = require("godi")

ci.init()
godi.init()
oasis.init()
darcs.init()

godi.bootstrap("3.12")
godi.update()
godi.upgrade()
godi.build("godi-findlib")

ci.exec("ocaml", "setup.ml", "-configure", "--enable-backtrace")
ci.exec("ocaml", "setup.ml", "-build")
ci.exec("ocaml", "setup.ml", "-test")
darcs.create_tag(oasis.package_version())
