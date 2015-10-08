
js_target  = static/js/compiled
css_target = static/css/dmi

cabal_bin = cabal
scss_bin = scss
closure_bin = closure-compiler 
yui_bin = yuicompressor


sass_source = static/css/dmi.scss
js_source = dist/build/openetcs-dmi/openetcs-dmi.jsexe/all.js



main_js = $(js_target).js
main_min_js = $(js_target).min.js

main_css = $(css_target).css
main_min_css = $(css_target).min.css

cabal_conf = dist/setup-config


all: $(main_css) $(main_min_css) $(main_js) $(main_min_js)

clean:
	rm -rf $(main_css)* $(main_js) $(main_min_css) $(main_min_js)
	$(cabal_bin) clean



$(main_css): $(sass_source)
	$(scss_bin) $? $@

$(main_min_css): $(main_css)
	$(yui_bin) $? > $@

$(main_js): $(js_source)
	cp $? $@

$(main_min_js): $(js_source)
	$(closure_bin) -O ADVANCED $? > $@

$(js_source): $(cabal_conf)
	$(cabal_bin) build 

$(cabal_conf): openetcs-dmi.cabal
	$(cabal_bin) configure --enable-tests --ghcjs
sass:
	$(scss_bin) --watch $(sass_source):$(main_css)
