#
# Edit these variables if MinGW or OCaml are installed to other directories
#
# NOTE: "/c/" in the paths below is used instead of Windows "c:/" drive
# letter specification.
#

MINGW_INSTALL_PATH="/c/MinGW"

OCAML_INSTALL_PATH="/c/Program Files/Objective Caml"

FLEXDLL_INSTALL_PATH="/c/Program Files/flexdll"


#
# don't change the settings below -- they are necessary for the build process
#

export OCAMLLIB="$OCAML_INSTALL_PATH/lib"

# XXX: alternatively, we could do this:
# export FLEXLINKFLAGS="-nocygpath -L $MINGW_INSTALL_PATH/lib"
export OCAMLLDFLAGS="-I $MINGW_INSTALL_PATH/lib"

export PATH="$FLEXDLL_INSTALL_PATH:$OCAML_INSTALL_PATH/bin:$MINGW_INSTALL_PATH/bin:$PATH"

