.PHONY: all clean install

CASK?=cask
EMACS?=emacs
TAR?=COPYFILE_DISABLE=1 bsdtar
PANDOC?=pandoc --atx-headers

VERSION?=$(shell $(CASK) version)

ARCHIVE_NAME=php-extras
PACKAGE_NAME=$(ARCHIVE_NAME)-$(VERSION)

all: $(PACKAGE_NAME).tar

$(ARCHIVE_NAME).info: README.md
	$(PANDOC) -t texinfo $^ | makeinfo -o $@

README: README.md
	$(PANDOC) -t plain -o $@ $^

php-extras-eldoc-functions.el: php-extras-gen-eldoc.el
	$(CASK) install
	$(CASK) exec $(EMACS) --batch -l php-extras.el -l php-extras-gen-eldoc.el -f php-extras-generate-eldoc-1

$(ARCHIVE_NAME)-pkg.el: $(ARCHIVE_NAME).el
	$(CASK) package

# create a tar ball in package.el format for uploading to http://marmalade-repo.org
$(PACKAGE_NAME).tar: README $(ARCHIVE_NAME).el $(ARCHIVE_NAME)-pkg.el $(ARCHIVE_NAME).info dir php-extras-eldoc-functions.el php-extras-gen-eldoc.el
	$(TAR) -c -s "@^@$(PACKAGE_NAME)/@" -f $(PACKAGE_NAME).tar $^

install: $(PACKAGE_NAME).tar
	$(EMACS) --batch -l package -f package-initialize --eval "(package-install-file \"$(PWD)/$(PACKAGE_NAME).tar\")"

clean:
	$(RM) $(ARCHIVE_NAME)-*.tar $(ARCHIVE_NAME)-pkg.el README $(ARCHIVE_NAME).info php-extras-eldoc-functions.el
	$(RM) -r .cask
