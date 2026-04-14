# Compiler and options
CC = gcc800
CFLAGS = -std=gnu99
OPTFLAGS = -O3 -D NDEBUG

# Directory paths
REFERENCE_DIR = reference
SRC_DIR = src
TEST_DIR = test

# Source files
TEST_SRC = $(TEST_DIR)/testheapmgr.c
HEAPMGR_GNU = $(REFERENCE_DIR)/heapmgrgnu.c
HEAPMGR_KR = $(REFERENCE_DIR)/heapmgrkr.c
HEAPMGR_BASE = $(REFERENCE_DIR)/heapmgrbase.c
CHUNK_BASE = $(REFERENCE_DIR)/chunkbase.c
HEAPMGR1 = $(SRC_DIR)/heapmgr1.c
HEAPMGR2 = $(SRC_DIR)/heapmgr2.c
CHUNK = $(SRC_DIR)/chunk.c

# Output binaries
BIN_GNU = $(TEST_DIR)/testheapmgrgnu
BIN_KR = $(TEST_DIR)/testheapmgrkr
BIN_BASE = $(TEST_DIR)/testheapmgrbase
BIN_1 = $(TEST_DIR)/testheapmgr1
BIN_2 = $(TEST_DIR)/testheapmgr2
BIN_1_DBG = $(TEST_DIR)/testheapmgr1-dbg
BIN_2_DBG = $(TEST_DIR)/testheapmgr2-dbg

# Testing server
TESTING_SERVER ?= http://sp02.snucse.org:8088
LAB := lab3

.PHONY: test1-local test2-local test1-remote test2-remote check1 check2 clean

# ============================================================
# Test (local): optimized build + full test suite
# ============================================================

test1-local: $(BIN_GNU) $(BIN_KR) $(BIN_BASE) $(BIN_1)
	@cd $(TEST_DIR) && ./testheap1

test2-local: $(BIN_GNU) $(BIN_KR) $(BIN_BASE) $(BIN_1) $(BIN_2)
	@cd $(TEST_DIR) && ./testheap2

# ============================================================
# Test (remote): submit to grading server
# ============================================================

test1-remote:
	@tar -czf /tmp/$(LAB)_submit.tar.gz $(SRC_DIR)/
	@curl -sN -X POST $(TESTING_SERVER)/submit \
		-F "lab=$(LAB)" -F "mode=1" \
		-F "file=@/tmp/$(LAB)_submit.tar.gz"
	@rm -f /tmp/$(LAB)_submit.tar.gz

test2-remote:
	@tar -czf /tmp/$(LAB)_submit.tar.gz $(SRC_DIR)/
	@curl -sN -X POST $(TESTING_SERVER)/submit \
		-F "lab=$(LAB)" -F "mode=2" \
		-F "file=@/tmp/$(LAB)_submit.tar.gz"
	@rm -f /tmp/$(LAB)_submit.tar.gz

# ============================================================
# Check: debug build (assertions enabled) + correctness test
# ============================================================

check1: $(BIN_1_DBG)
	@echo "=== check1: debug build correctness test ==="
	@$(BIN_1_DBG) LIFO_fixed 1000 100
	@$(BIN_1_DBG) FIFO_fixed 1000 100
	@$(BIN_1_DBG) LIFO_random 1000 100
	@$(BIN_1_DBG) FIFO_random 1000 100
	@$(BIN_1_DBG) random_fixed 1000 100
	@$(BIN_1_DBG) random_random 1000 100
	@$(BIN_1_DBG) worst 1000 100
	@echo "=== check1: all passed ==="

check2: $(BIN_2_DBG)
	@echo "=== check2: debug build correctness test ==="
	@$(BIN_2_DBG) LIFO_fixed 1000 100
	@$(BIN_2_DBG) FIFO_fixed 1000 100
	@$(BIN_2_DBG) LIFO_random 1000 100
	@$(BIN_2_DBG) FIFO_random 1000 100
	@$(BIN_2_DBG) random_fixed 1000 100
	@$(BIN_2_DBG) random_random 1000 100
	@$(BIN_2_DBG) worst 1000 100
	@echo "=== check2: all passed ==="

# ============================================================
# Build rules (optimized)
# ============================================================

$(BIN_GNU): $(TEST_SRC) $(HEAPMGR_GNU)
	$(CC) $(OPTFLAGS) $(CFLAGS) $^ -o $@

$(BIN_KR): $(TEST_SRC) $(HEAPMGR_KR)
	$(CC) $(OPTFLAGS) $(CFLAGS) $^ -o $@

$(BIN_BASE): $(TEST_SRC) $(HEAPMGR_BASE) $(CHUNK_BASE)
	$(CC) $(OPTFLAGS) $(CFLAGS) $^ -o $@

$(BIN_1): $(TEST_SRC) $(HEAPMGR1) $(CHUNK)
	$(CC) $(OPTFLAGS) $(CFLAGS) $^ -o $@

$(BIN_2): $(TEST_SRC) $(HEAPMGR2) $(CHUNK)
	$(CC) $(OPTFLAGS) $(CFLAGS) $^ -o $@

# ============================================================
# Build rules (debug, assertions enabled)
# ============================================================

$(BIN_1_DBG): $(TEST_SRC) $(HEAPMGR1) $(CHUNK)
	$(CC) $(CFLAGS) $^ -o $@

$(BIN_2_DBG): $(TEST_SRC) $(HEAPMGR2) $(CHUNK)
	$(CC) $(CFLAGS) $^ -o $@

# ============================================================
# Clean
# ============================================================

clean:
	rm -f $(BIN_GNU) $(BIN_KR) $(BIN_BASE) $(BIN_1) $(BIN_2) $(BIN_1_DBG) $(BIN_2_DBG)
