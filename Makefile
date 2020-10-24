# This is a makefile to initialize the emacs configuration

install:
	mkdir ~/.emacs.d/third-party
	git clone https://github.com/loyalpartner/english-teacher.el.git ~/.emacs.d/third-party/english-teacher.el --depth=1
	@echo "English-teacher.el downloaded"
	git clone https://github.com/SpringHan/netease-cloud-music.el.git ~/.emacs.d/third-party/netease-cloud-music.el --depth=1
	@echo "Netease-Cloud-Music downloaded"
	git clone https://github.com/manateelazycat/awesome-tray.git ~/.emacs.d/third-party/awesome-tray --depth=1
	mkdir ~/.emacs.d/var
	ln -sf ~/.emacs.d/bookmarks ~/.emacs.d/var/bookmarks
	mkdir ~/.emacs.d/var/netease-cloud-music/
	ln -sf ~/.emacs.d/playlist.txt ~/.emacs.d/var/netease-cloud-music/
	@echo "Make successful"
