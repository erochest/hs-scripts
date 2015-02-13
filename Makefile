
FLAGS=--enable-tests

all: init docs package

init:
	cabal sandbox init
	make deps

run:
	cabal run

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
	cabal install
	for fn in .cabal-sandbox/bin/*; do ln -fs `pwd`/$${fn} ${HOME}/bin; done

hlint:
	hlint *.hs src specs

clean:
	cabal clean

distclean: clean
	cabal sandbox delete

configure: clean
	cabal configure ${FLAGS}

deps: clean
	cabal install --only-dependencies --allow-newer ${FLAGS}
	make configure

build:
	cabal build

restart: distclean init build

rebuild: clean configure build

.PHONY: all init run clean distclean configure deps build rebuild hlint
