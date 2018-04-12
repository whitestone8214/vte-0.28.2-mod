#!/bin/bash

# To build: ./oneshot.sh build
# To install: ./oneshot.sh install [root-directory]
# To link: ./oneshot.sh link [root-directory]
# To unlink: ./oneshot.sh unlink
# To uninstall: ./oneshot.sh uninstall


_versionVte="0.28.2"
_editionVte="4"
_destinationVte="packages/vte/${_versionVte}-${_editionVte}"

if (test "$1" = "build"); then
	#./configure --prefix=$2/${_destinationVte} --with-gtk=2.0 --disable-python || exit 1
	#make V=1 > ../build.log
	gcc $(pkg-config --cflags --libs gtk+-x11-2.0 gio-unix-2.0 ncurses) -I. -Wno-deprecated-declarations -fPIC src/{caps,debug,iso2022,keymap,marshal,matcher,pty,reaper,ring,table,trie,vte,vteaccess,vtebg,vteconv,vtedraw,vteregex,vterowdata,vteseq,vtestream,vtetc,vtetree,vtetypebuiltins,vteunistr}.c -shared -o libvte.so || exit 1
	#gcc -Ignome-pty-helper -I. -lutil gnome-pty-helper/gnome-{login-support,pty-helper,utmp}.c -o gnome-pty-helper.elf
elif (test "$1" = "install"); then
	$0 uninstall || exit 1
	
	#make V=1 install > ../install.log
	mkdir -p $2/${_destinationVte}/{includes,assets/{pkgconfig,termcaps}} || exit 1
	cp -f libvte.so $2/${_destinationVte}
	#cp -f gnome-pty-helper.elf $2/${_destinationVte}/gnome-pty-helper
	cp -f src/{pty,reaper,vte,vteaccess,vtepty,vtetypebuiltins,vteversion,vtedeprecated}.h $2/${_destinationVte}/includes
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
