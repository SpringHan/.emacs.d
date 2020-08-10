# This is a makefile to initialize the emacs configuration

install:
	mkdir ~/.emacs.d/third-party
	git clone https://gitee.com/springhan/emacs-application-framework.git ~/.emacs.d/third-party/emacs-application-framework --depth=1
	@echo "Emacs Application Framework downloaded"
	mkdir ~/.emacs.d/var
	mv ~/.emacs.d/bookmarks ~/.emacs.d/var/bookmarks
	@echo "Make successful"
