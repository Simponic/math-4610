SRC_DIR := src
OBJ_DIR := build
BIN_DIR := dist
LIB_DIR := lib

TEST_SRC_DIR := test
TEST_SRC := $(wildcard $(TEST_SRC_DIR)/*.c)
TEST_OBJ := $(TEST_SRC:$(TEST_SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

TEST_EXE := $(BIN_DIR)/lizfcm.test
EXE      := $(BIN_DIR)/lizfcm
LIBRARY  := $(LIB_DIR)/lizfcm.a
SRC      := $(wildcard $(SRC_DIR)/*.c)
OBJ      := $(SRC:$(SRC_DIR)/%.c=$(OBJ_DIR)/%.o)

CPPFLAGS := -Iinc -MMD -MP
CFLAGS   := -Wall
LDFLAGS  := -lm

.PHONY: all clean

all: $(TEST_EXE)

$(TEST_EXE): $(TEST_OBJ) $(LIBRARY) | $(BIN_DIR)
	$(CC) $(CPPFLAGS) $(CFLAGS) $^ -o $@ $(LDFLAGS)

$(LIBRARY): $(OBJ) | $(LIB_DIR)
	ar rcs $(LIBRARY) $(OBJ_DIR)/*.o
	ranlib $(LIBRARY)

$(OBJ_DIR)/%.o: $(TEST_SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

$(BIN_DIR) $(OBJ_DIR) $(LIB_DIR):
	mkdir -p $@

print-%  : ; @echo $* = $($*)

clean:
	@$(RM) -r $(BIN_DIR) $(OBJ_DIR) $(LIB_DIR)

-include $(OBJ:.o=.d)
-include $(TEST_OBJ:.o=.d)
