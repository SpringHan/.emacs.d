# This is a makefile to initialize the emacs configuration

install:
	mkdir ~/.emacs.d/third-party
	mkdir ~/.emacs.d/var
	ln -sf ~/.emacs.d/bookmarks ~/.emacs.d/var/bookmarks
	mkdir ~/.emacs.d/var/netease-cloud-music/
	ln -sf ~/.emacs.d/playlist.txt ~/.emacs.d/var/netease-cloud-music/
	@echo "Make successful"
