# (linux) commands for compiling and running. Source this script to make the
# functions available in your local shell

compile() {
        ./blunt "$@" -write-elf-file -elf-file out.o &&
                cc -Wall -o out runtime/support.c out.o
}

docompile() {
        local dash=false
        for i in "$@" ; do
                if [ "$i" = -- ]; then dash=true; fi
                if ! "$dash"; then set -- "$@" "$i"; fi
                shift
        done
        compile "$@"
}

dorun() {
        while [ "$#" -gt 0 ] && [ "$1" != -- ]; do shift; done
        shift
        ./out "$@"
}

compileandrun() {
        docompile "$@" && dorun "$@"
}



compile_with_pprof() (
        # . Needs google-perftools
        #
        # Use -filetype switch to use output format other than text (dot, ps,
        # pdf...)
        set -e

        if [ "$1" = -filetype ]; then
                filetype=$2
                shift 2
        else
                filetype=text
        fi

        LD_PRELOAD=/usr/lib/libprofiler.so.0 CPUPROFILE=/tmp/test.prof \
                ./blunt "$@"

        outfile=blunt.pprof."$filetype"
        google-pprof --"$filetype" --files \
                ./blunt /tmp/test.prof > "$outfile"
        echo >&2 "Output written to $outfile"
)
