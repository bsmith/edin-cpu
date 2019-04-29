#!/bin/bash

## require this file to be sourced
## see https://unix.stackexchange.com/questions/214079/differentiating-between-running-and-being-sourced-in-a-bash-shell-script 
if [ "$(basename "$BASH_SOURCE")" = "$(basename "${0#-}")" ]; then
	echo setup.sh is designed to be sourced
	echo eg. . ${BASE_SOURCE[0]}
	exit 1
fi

# see https://stackoverflow.com/questions/59895/get-the-source-directory-of-a-bash-script-from-within-the-script-itself
#edin_cpu_base="$(dirname "$(readlink -f "$0")")"
# Okay, so you need this because we're sourcing this script
edin_cpu_base="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

echo "edin_cpu_base=$edin_cpu_base"

eval $(perl -Mlocal::lib="$edin_cpu_base/locallib")

echo "PERL_LOCAL_LIB_ROOT=$PERL_LOCAL_LIB_ROOT"
