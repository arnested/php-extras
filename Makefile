.PHONY: all clean install

CARTON?=carton
EMACS?=emacs
TAR?=bsdtar
PANDOC?=pandoc --atx-headers

VERSION?=$(shell $(CARTON) version)

ARCHIVE_NAME=php-extras
PACKAGE_NAME=$(ARCHIVE_NAME)-$(VERSION)

all: $(PACKAGE_NAME).tar

$(ARCHIVE_NAME).info: README.md
	$(PANDOC) -t texinfo $^ | makeinfo -o $@

README: README.md
	$(PANDOC) -t plain -o $@ $^

php-extras-eldoc-functions.el: php-extras-gen-eldoc.el
	$(CARTON) exec $(EMACS) --batch -l php-extras.el -l php-extras-gen-eldoc.el -f php-extras-generate-eldoc-1

$(ARCHIVE_NAME)-pkg.el: $(ARCHIVE_NAME).el
	$(CARTON) package

# create a tar ball in package.el format for uploading to http://marmalade-repo.org
$(PACKAGE_NAME).tar: README $(ARCHIVE_NAME).el $(ARCHIVE_NAME)-pkg.el $(ARCHIVE_NAME).info dir php-extras-eldoc-functions.el php-extras-gen-eldoc.el
	$(TAR) -c -s "@^@$(PACKAGE_NAME)/@" -f $(PACKAGE_NAME).tar $^

install: $(PACKAGE_NAME).tar
	$(EMACS) --batch -l package -f package-initialize --eval "(package-install-file \"$(PWD)/$(PACKAGE_NAME).tar\")"

clean:
	$(RM) $(ARCHIVE_NAME)-*.tar $(ARCHIVE_NAME)-pkg.el README $(ARCHIVE_NAME).info php-extras-eldoc-functions.el
	$(RM) -r elpa
