# This is a makefile to initialize the emacs configuration

install:
	mkdir ~/.emacs.d/third-party
	git clone https://github.com/manateelazycat/awesome-tray.git ~/.emacs.d/third-party/awesome-tray --depth=1
	@echo "Awesome-Tray downloaded."
	git clone https://github.com/manateelazycat/awesome-tab.git ~/.emacs.d/third-party/awesome-tab --depth=1
	@echo "Awesome-Tab downloaded."
	mkdir ~/.emacs.d/var
	ln -sf ~/.emacs.d/bookmarks ~/.emacs.d/var/bookmarks
	mkdir ~/.emacs.d/var/netease-cloud-music/
	ln -sf ~/.emacs.d/playlist.txt ~/.emacs.d/var/netease-cloud-music/
	@echo "Make successful"
