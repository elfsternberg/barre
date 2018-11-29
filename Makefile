test:
	cargo +nightly test

build:
	cargo +nightly build

clean:
	rm output*.dot

realclean: clean
	cargo clean
