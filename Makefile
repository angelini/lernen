.PHONY: setup build run repl

data/translations.zip:
	@echo "= Dataset requires accepting a terms of use"
	@echo "= Download archive.zip from https://data.europa.eu/euodp/en/data/dataset/elrc_41/resource/5eb2e568-51cc-4f7d-9183-f648de569e94"
	@echo "= Move it to data/translations.zip"
	@echo "=================================="
	test -d data/translations.zip

data/translations.xml: data/translations.zip
	unzip -o -d data data/translations.zip
	mv data/de-en_*_Website_final.tmx data/translations.xml
	sed -E -i 's/&#x1E;|&#x1D;|&#x1D;|&#x14;|&#x18;|&#x7;//g' data/translations.xml
	sed -i '/<prop type=/d' data/translations.xml

setup: data/translations.xml

build:
	stack build --fast

run: build
	stack run

repl: build
	stack ghci src/Lib.hs
