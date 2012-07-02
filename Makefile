.PHONY: all clean install

ARCHIVE_NAME:=php-extras
VERSION:=$(shell emacs --batch --eval "(with-temp-buffer \
		(require 'package)\
		(insert-file \"$(ARCHIVE_NAME).el\")\
		(princ (aref (package-buffer-info) 3)))")
PACKAGE_NAME:=$(ARCHIVE_NAME)-$(VERSION)

all: $(PACKAGE_NAME).tar

$(ARCHIVE_NAME).info: README.md
	pandoc -t texinfo $^ | makeinfo -o $@

README: README.md
	pandoc --atx-headers -t plain -o $@ $^

php-extras-eldoc-functions.el: php-extras-gen-eldoc.el
	emacs --batch -l php-extras.el -l php-extras-gen-eldoc.el -f php-extras-generate-eldoc-1

# requires package-build.el from https://github.com/milkypostman/melpa
# to be available in your emacs load-path
$(ARCHIVE_NAME)-pkg.el: $(ARCHIVE_NAME).el
	emacs --batch --user `whoami` --eval "(progn \
		(require 'package-build) \
		(pb/write-pkg-file \
			\"$(ARCHIVE_NAME)-pkg.el\" \
			(with-temp-buffer \
				(insert-file \"$(ARCHIVE_NAME).el\") \
				(package-buffer-info))))"

# create a tar ball in package.el format for uploading to http://marmalade-repo.org
$(PACKAGE_NAME).tar: README $(ARCHIVE_NAME).el $(ARCHIVE_NAME)-pkg.el $(ARCHIVE_NAME).info dir php-extras-eldoc-functions.el php-extras-gen-eldoc.el
	tar -c -s "@^@$(PACKAGE_NAME)/@" -f $(PACKAGE_NAME).tar $^

install: $(PACKAGE_NAME).tar
	emacs --batch --user `whoami` --eval "(progn \
		(package-initialize)\
		(package-install-file \"`pwd`/$(PACKAGE_NAME).tar\"))"

clean:
	$(RM) $(ARCHIVE_NAME)-*.tar $(ARCHIVE_NAME)-pkg.el README $(ARCHIVE_NAME).info php-extras-eldoc-functions.el
