# This is a makefile to initialize the emacs configuration

install:
	mkdir ~/.emacs.d/third-party
	git clone https://github.com/SpringHan/package-require.git ~/.emacs.d/third-party/package-require --depth=1
	@echo "Package-Require downloaded."
	mkdir ~/.emacs.d/var
	ln -sf ~/.emacs.d/bookmarks ~/.emacs.d/var/bookmarks
	mkdir ~/.emacs.d/var/netease-cloud-music/
	ln -sf ~/.emacs.d/playlist.txt ~/.emacs.d/var/netease-cloud-music/
	@echo "Make successful"
