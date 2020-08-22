# This is a makefile to initialize the emacs configuration

install:
	mkdir ~/.emacs.d/third-party
	git clone https://github.com/loyalpartner/english-teacher.el ~/.emacs.d/third-party/english-teacher.el --depth=1
	@echo "English-teacher.el downloaded"
	mkdir ~/.emacs.d/var
	mv ~/.emacs.d/bookmarks ~/.emacs.d/var/bookmarks
	@echo "Make successful"
