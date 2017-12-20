# Racket site makefile

BIN = racket
APP = main.rkt

MAIN = $(BIN) $(APP)


run:
	$(MAIN)


debug:
	$(MAIN) -w

production:
	$(MAIN) -p -w

# end
