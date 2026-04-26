.PHONY: all
all: gkt

.PHONY: gkt
gkt: .deps-installed
	dune build gkt

.PHONY: gkt_release
gkt_release: .deps-installed
	dune build --release gkt

.PHONY: watch
watch: dependencies
	dune build --watch @all

.PHONY: dependencies
dependencies: .deps-installed

.deps-installed: gk-tools.opam
	opam install . --deps-only
	touch .deps-installed

gk-tools.opam: dune-project
	dune build gk-tools.opam

.PHONY: test
test: gkt
	dune runtest --force

.PHONY: checks
checks: gkt test
	dune build @fmt
