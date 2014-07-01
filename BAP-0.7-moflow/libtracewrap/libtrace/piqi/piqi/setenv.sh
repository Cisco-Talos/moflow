# run this script if you want to run tests; otherwise, use ./configure


# base build directory
export PIQI_ROOT="`pwd`"

# temporary build directory
export PIQI_BUILD="$PIQI_ROOT/build"

# path to piqi and piqic executables
export PATH="$PIQI_ROOT/bin:$PATH"

# path to .piqi files
export PIQI_PATH="$PIQI_ROOT"


# this is for backward compatibility with older packaging scripts that were
# created before we added ./configure script
if [ ! -f Makefile.config ]
then
        ./configure
        if [ $? -ne 0 ]
        then
                echo "./configure command failed" 1>&2
                exit 1
        fi
fi

