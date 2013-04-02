.PHONY: all clean install

ARCHIVE_NAME:=php-extras
VERSION:=$(shell carton version)
PACKAGE_NAME:=$(ARCHIVE_NAME)-$(VERSION)

all: $(PACKAGE_NAME).tar

$(ARCHIVE_NAME).info: README.md
	pandoc -t texinfo $^ | makeinfo -o $@

README: README.md
	pandoc --atx-headers -t plain -o $@ $^

php-extras-eldoc-functions.el: php-extras-gen-eldoc.el
	emacs --batch -l php-extras.el -l php-extras-gen-eldoc.el -f php-extras-generate-eldoc-1

# requires carton from https://github.com/rejeep/carton to be
# available in your $PATH
$(ARCHIVE_NAME)-pkg.el: $(ARCHIVE_NAME).el
	carton package

# create a tar ball in package.el format for uploading to http://marmalade-repo.org
$(PACKAGE_NAME).tar: README $(ARCHIVE_NAME).el $(ARCHIVE_NAME)-pkg.el $(ARCHIVE_NAME).info dir php-extras-eldoc-functions.el php-extras-gen-eldoc.el
	bsdtar -c -s "@^@$(PACKAGE_NAME)/@" -f $(PACKAGE_NAME).tar $^

install: $(PACKAGE_NAME).tar
	emacs --batch --user `whoami` --eval "(progn \
		(package-initialize)\
		(package-install-file \"`pwd`/$(PACKAGE_NAME).tar\"))"

clean:
	$(RM) $(ARCHIVE_NAME)-*.tar $(ARCHIVE_NAME)-pkg.el README $(ARCHIVE_NAME).info php-extras-eldoc-functions.el
