package = course
#the reason this fails on windows is literally just "stack.yaml" instead of stack.yaml ffs
stack_yaml = STACK_YAML="stack.yaml" 
stack = $(stack_yaml) stack

build:
	$(stack) build $(package)

build-dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

build-profile:
	$(stack) --work-dir .stack-work-profiling --profile build

run:
	$(stack) build --fast && $(stack) exec -- $(package)

install:
	$(stack) install

ghci:
	$(stack) ghci $(package):lib

test:
	$(stack) test $(package)

test-ghci:
	$(stack) ghci $(package):test:$(package)-tests

bench:
	$(stack) bench $(package)

ghcid:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind'"

dev-deps:
	stack install ghcid

.PHONY : build build-dirty build-profile run install ghci test test-ghci ghcid dev-deps
