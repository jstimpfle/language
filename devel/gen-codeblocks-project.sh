# (linux) shell script to generate a codeblocks project. I've used codeblocks
# for automated variable renamings.

PROJECTNAME=language
CB_PROJECTDIR=codeblocks
CB_PROJECTFILE=$CB_PROJECTDIR/$PROJECTNAME.cbp


deleteifexists=false
if [ "$1" = -f ]; then
	deleteifexists=true
	shift
fi

if [ -d "$CB_PROJECTDIR" ]; then
	if ! "$deleteifexists"; then
		echo >&2 "$CB_PROJECTDIR directory exists and -f option was not given. Aborting"
		exit 1
	fi
	rm -rf "$CB_PROJECTDIR"
fi

mkdir "$CB_PROJECTDIR"

cat > "$CB_PROJECTFILE" <<EOF
<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>
<CodeBlocks_project_file>
	<FileVersion major="1" minor="6" />
	<Project>
		<Option title="$PROJECTNAME" />
		<Option pch_mode="2" />
		<Option compiler="gcc" />
		<Build>
			<Target title="Debug">
				<Option output="bin/Debug/$PROJECTNAME" prefix_auto="1" extension_auto="1" />
				<Option working_dir="../" />
				<Option object_output="obj/Debug/" />
				<Option type="1" />
				<Option compiler="gcc" />
				<Compiler>
					<Add option="-g" />
				</Compiler>
			</Target>
			<Target title="Release">
				<Option output="bin/Release/$PROJECTNAME" prefix_auto="1" extension_auto="1" />
				<Option working_dir="../" />
				<Option object_output="obj/Release/" />
				<Option type="1" />
				<Option compiler="gcc" />
				<Compiler>
					<Add option="-O2" />
				</Compiler>
				<Linker>
					<Add option="-s" />
				</Linker>
			</Target>
		</Build>
		<Compiler>
			<Add option="-std=c99" />
			<Add option="-Wall" />
			<Add option="-Wextra" />
			<Add directory="../include/" />
		</Compiler>
EOF

for i in src/*.c include/*.h; do
	cat >> "$CB_PROJECTFILE" <<EOF
		<Unit filename="../$i"><Option compilerVar="CC" /></Unit>
EOF
done

cat >> "$CB_PROJECTFILE" <<EOF
		<Extensions>
			<code_completion />
			<debugger />
		</Extensions>
	</Project>
</CodeBlocks_project_file>
EOF
