INSTALLDIR ?= $(HOME)/.local/bin

.PHONY: build test install clean

build:
	cabal build

test:
	cabal test --test-show-details=direct

install:
	cabal install exe:claude-statusline --install-method=copy \
	  --installdir=$(INSTALLDIR) --overwrite-policy=always

clean:
	cabal clean
