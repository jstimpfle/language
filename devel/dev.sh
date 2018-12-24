# (linux) commands for compiling and running. Source this script to make the
# functions available in your local shell

compile() {
	build/language "$@" -write-elf-object &&
		cc -Wall -o out runtime/support.c out.o;
}

docompile() {
	local dash=false;
	for i in "$@" ; do
		if [ "$i" = -- ]; then dash=true; fi;
		if ! "$dash"; then set -- "$@" "$i"; fi;
		shift;
	done;
	compile "$@";
}

dorun() {
	while [ "$#" -gt 0 ] && [ "$1" != -- ]; do shift; done;
	shift;
	./out "$@";
}

compileandrun() {
	docompile "$@" && dorun "$@";
}
