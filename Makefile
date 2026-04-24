.PHONY: all
all: gk-tools

.PHONY: gk-tools
gk-tools: .deps-installed
	dune build gk-tools

.PHONY: gk-tools_release
gk-tools_release: .deps-installed
	dune build --release gk-tools

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
test: gk-tools
	dune runtest --force

.PHONY: checks
checks: gk-tools test
	dune build @fmt
