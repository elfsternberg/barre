test:
	cargo +nightly test

build:
	cargo +nightly build

clean:
	rm -f output*.dot output*.png

realclean: clean
	cargo clean

route: clean
	- cargo test --features "render_trees" -- --nocapture test_barre::just_a_cat
	for i in output-*.dot ; do dot -Tpng $$i -o $$(basename $$i .dot).png ; done	
