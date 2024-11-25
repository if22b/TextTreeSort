GHC = ghc
GHC_FLAGS = -package containers -outputdir bin

BIN_DIR = bin

SRC_FILES = src/main.hs
EXEC = $(BIN_DIR)/sort

.PHONY: all clean run

all: $(EXEC)

# compile
$(EXEC): $(SRC_FILES)
	@echo "Compiling Haskell program..."
	$(GHC) $(SRC_FILES) $(GHC_FLAGS) -o $(EXEC)

clean:
	@echo "Cleaning up..."
	rm -f $(BIN_DIR)/*.hi $(BIN_DIR)/*.o $(EXEC)