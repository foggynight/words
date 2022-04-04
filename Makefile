.PHONY: all
all:
	ghc -O2 words.hs

.PHONY: install
install:
	cp words /usr/local/bin

.PHONY: uninstall
uninstall:
	rm /usr/local/bin/words
