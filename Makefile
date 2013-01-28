BIN = bin
SRC = src

.PHONY: both all clean dependencies

all: $(BIN)/cgibildc $(BIN)/cgibildc

dependencies:
	./check_cmds.sh

$(BIN)/bildc: dependencies
	$(MAKE) -C $(SRC) $(MFLAGS)
	mkdir -p $(BIN)
	cp $(SRC)/bildc $(BIN)/bildc

$(BIN)/cgibildc: dependencies
	$(MAKE) -C $(SRC) $(MFLAGS)
	mkdir -p $(BIN)
	cp $(SRC)/cgibildc $(BIN)/cgibildc

clean:
	$(MAKE) -C $(SRC) clean
	-rm -f $(BIN)/bildc $(BIN)/cgibildc

.NOTPARALLEL:
