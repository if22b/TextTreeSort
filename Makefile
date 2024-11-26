GHC = ghc
GHC_FLAGS = -package containers -outputdir bin

BIN_DIR = bin

SRC_FILES = src/Main.hs src/RBTree.hs src/Helperfunctions.hs
TEST_FILES = scr/TestHelperfunctions.hs
TEST_HELPER_EXEC = $(BIN_DIR)/test
EXEC = $(BIN_DIR)/sort

.PHONY: all clean run

all: $(EXEC)

# compile
$(EXEC): $(SRC_FILES)
	@echo "Compiling Haskell program..."
	mkdir -p $(BIN_DIR)
	$(GHC) $(SRC_FILES) $(GHC_FLAGS) -o $(EXEC)

test: $(TEST_FILES)
	@echo "Compiling TestHelperfunctions..."
	mkdir -p $(BIN_DIR)
	$(GHC) $(TEST_FILES) $(SRC_FILES) $(GHC_FLAGS) -o $(TEST_HELPER_EXEC)
	@echo "Running TestHelperfunctions..."
	./$(TEST_HELPER_EXEC)

clean:
	@echo "Cleaning up..."
	rm -f $(BIN_DIR)/*.hi $(BIN_DIR)/*.o $(EXEC)