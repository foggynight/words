.PHONY: all
all:
	ghc words.hs

.PHONY: install
install:
	cp words /usr/local/bin

.PHONY: uninstall
uninstall:
	rm /usr/local/bin/words
