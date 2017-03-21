
FLAGS=--pedantic

all: init docs package

init:
	make deps

# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# deploy:
# prep and push

# Link everything into ~/bin/
install:
	stack install

hlint:
	hlint *.hs src specs

clean:
	stack clean

build:
	stack build --pedantic

watch:
	stack build --pedantic --file-watch

.PHONY: all init run clean distclean configure deps build rebuild hlint
