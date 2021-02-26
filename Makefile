build:
	stack build

BIN=huha
run:
	@stack exec $(BIN)-exe

########################################
#               Cabal                  #
########################################
cabal-update:
	cabal update
	cabal build --only-dependencies --enable-tests --enable-benchmarks

cabal-build:
	cabal build --enable-tests --enable-benchmarks all

cabal-test:
	cabal test --test-show-details='always' all

########################################
#             GH Action                #
########################################
ACTION=build
run-action:
	act --job $(ACTION)
