#!/bin/sh

set -ue

# (c) Copyright Fabrice Le Fessant INRIA/OCamlPro 2013
# (c) Copyright Louis Gesbert OCamlPro 2014

VERSION='1.1.1-RC2'

default_ocaml=4.01.0

usage() {
cat <<EOF

Usage:
    ./opam_install BINDIR [COMP]

    Download and installs the latest binary version of OPAM

    BINDIR is the directory where it should be installed, e.g. /usr/local/bin
    (it should be in your PATH).

    COMP is an optional argument, specifying the initial version of OCaml you
    want to use ($default_ocaml by default. You may use 'system' if you want to
    use an ocaml compiler already present on your system).
EOF
    exit 1
}

#
#       Report an error and exit
#
PROGNAME=$0
error() {
    echo -n "`basename $PROGNAME`: " >&2
    for s in "$@"; do echo $s; done
    exit 1
}


if which wget >/dev/null; then
    WGETOPTS="--passive-ftp -q -O"
else
    WGETOPTS="-OL -o"
    wget() {
	shift
	curl -OL $*
    }
fi

TMP=${TMPDIR:-/tmp}

getopam() {
    url="$1"
    opamfile="$2"

    wget $WGETOPTS $TMP/$opamfile "$url/$opamfile" ||
	error "Couldn't download $url/$opamfile." "There may not yet be a binary release for your architecture or OS, sorry."
}

if [ $# -lt 1 ] || [ $# -gt 2 ]; then
    echo "OPAM binary installer v. $VERSION"
    usage
fi

BINDIR=$1
COMP=${2:-$default_ocaml}

file="opam-$VERSION-$(uname -m || echo unknown)-$(uname -s || echo unknown)"

echo Downloading OPAM...
getopam "https://github.com/ocaml/opam/releases/download/$VERSION" $file

if [ ! -w "$BINDIR" ] && ! mkdir -p "$BINDIR" 2>/dev/null; then
    echo "You don't have write access to $BINDIR: sudo may ask for your password"
    if [ ! -d "$BINDIR" ]; then sudo mkdir -p "$BINDIR"; fi
    sudo install -g root -o root -m 755 $TMP/$file $BINDIR/opam
else
    install -m 755 $TMP/$file $BINDIR/opam
fi
rm -f $TMP/$file

OPAM=$(which opam || echo "$BINDIR/opam")
if [ "$OPAM" != "$BINDIR/opam" ]; then
    echo "WARNING: you have a different version of OPAM installed at $OPAM"
    echo "It is highly recommended that you remove it."
    read -p "[press enter to continue]" x
    OPAM="$BINDIR/opam"
fi

echo "Initializing with compiler $COMP"
"$OPAM" init --comp "$COMP"

echo "Installation done. If you need to uninstall, simply remove $BINDIR/opam"
echo "and $("$OPAM" config var root)"
