TEST_SRC := test/main.c
SRC_DIR := src
OBJ_DIR := build
BIN_DIR := dist
LIB_DIR := lib

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

$(TEST_EXE): $(BIN_DIR) | $(LIBRARY)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(TEST_SRC) $(LIBRARY) -o $@

$(LIBRARY): $(OBJ)
	ar rcs $(LIBRARY) $(OBJ_DIR)/*.o
	ranlib $(LIBRARY)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c | $(OBJ_DIR)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

$(BIN_DIR) $(OBJ_DIR) $(LIB_DIR):
	mkdir -p $@

clean:
	@$(RM) -r $(BIN_DIR) $(OBJ_DIR)

-include $(OBJ:.o=.d)
