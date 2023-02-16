#!/bin/bash -

VERSION=0.6

USAGE=$(cat <<ENDSTR
Usage: ${0/\.\//} <command> [arguments]
Commands:
	install
		Install a package(s) by name
	remove
		Remove a package(s) by name
	search
		Search for package(s) by name
	update
		Update local catalog then Upgrade Packages ->
		Upgrades installed packages which have a newer version
	locate FILE
		Find the package which provides FILE
	info
		Print information about a package
	list
		List the files installed with a package
	version
		Gives the version number of a package(s) by name
		
-		
ENDSTR
)

sudo=$(which sudo 2> /dev/null|cut -d ':' -f 2)
if [ $(id -u) -eq 0 ]; then sudo=""; fi

command="$1"
shift
case "$command" in

	help|--help)
        	echo $VERSION
		echo "$USAGE"
		;;

#...........................................
	it)
		nix-env -iA nixos."$@"
		;;
	rm)
		nix-env -e "$@"
		;;
  hms)
		home-manager switch
		;;
	qr)
		nix-env -q
		;;
	hq)
    home-manager packages
		;;
#...........................................
	*)
		[ -z $command ] || echo Unknown command: $command
		echo "$USAGE"
		;;
esac

