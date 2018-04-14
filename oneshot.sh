#!/bin/bash

# To build: ./oneshot.sh build
# To install: ./oneshot.sh install [root-directory]
# To link: ./oneshot.sh link [root-directory]
# To unlink: ./oneshot.sh unlink
# To uninstall: ./oneshot.sh uninstall


_versionVte="0.28.2"
_editionVte="14"
_destinationVte="packages/vte/${_versionVte}-${_editionVte}"

if (test "$1" = "build"); then
	#gcc $(pkg-config --cflags --libs gtk+-x11-2.0 gio-unix-2.0) -I. -Wno-deprecated-declarations -fPIC src/{caps,debug,iso2022,marshal,matcher,pty,reaper,ring,vte,vteconv,vtedraw,vteseq,vtetc,vtetypebuiltins,vteunistr}.c -shared -o libvte.so || exit 1
	gcc $(pkg-config --cflags --libs gtk+-x11-2.0 gio-unix-2.0) -I. -Wno-deprecated-declarations -fPIC src/{caps,debug,iso2022,marshal,matcher,pty,reaper,ring,vte,vteconv,vtedraw,vteseq,vtetc,vteunistr}.c -shared -o libvte.so || exit 1
elif (test "$1" = "install"); then
	$0 uninstall || exit 1
	
	mkdir -p $2/${_destinationVte}/{includes,assets/{pkgconfig,termcaps}} || exit 1
	cp -f libvte.so $2/${_destinationVte}
	#cp -f src/{pty,reaper,vte,vtepty,vtetypebuiltins,vteversion,vtedeprecated}.h $2/${_destinationVte}/includes
	cp -f src/{pty,reaper,vte,vtepty,vteversion,vtedeprecated}.h $2/${_destinationVte}/includes
	cp -f termcaps/xterm $2/${_destinationVte}/assets/termcaps
	cp -f vte-mod.pc $2/${_destinationVte}/assets/pkgconfig/vte.pc
elif (test "$1" = "link"); then
	$0 unlink || exit 1
	
	mkdir -p /etc/termcaps || exit 1
	#ln -sf $2/${_destinationVte}/{libvte.so,gnome-pty-helper} /lib
	ln -sf $2/${_destinationVte}/libvte.so /lib
	ln -sf $2/${_destinationVte}/includes /usr/include/vte
	ln -sf $2/${_destinationVte}/assets/pkgconfig/vte.pc /lib/pkgconfig
	ln -sf $2/${_destinationVte}/assets/termcaps/xterm /etc/termcaps
elif (test "$1" = "unlink"); then
	rm -f /lib/{libvte.so,gnome-pty-helper}
	rm -f /usr/include/vte
	rm -f /lib/pkgconfig/vte.pc
	rm -f /etc/termcaps/xterm
elif (test "$1" = "uninstall"); then
	rm -rf $2/${_destinationVte} || exit 1
fi
