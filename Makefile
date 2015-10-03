


all: dist/setup-config static/css/dmi.css
	cabal build


clean:
	cabal clean
	rm -f static/css/dmi.css*

dist/setup-config: openetcs-dmi.cabal
	cabal configure --ghcjs


static/css/dmi.css: static/css/dmi.scss
	scss static/css/dmi.scss static/css/dmi.css

sass:
	scss --watch static/css/dmi.scss:static/css/dmi.css
