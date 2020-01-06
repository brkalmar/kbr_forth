# Makefile for kbr_forth.  GNU make is recommended.
#
# Compiling all files:
#
# 	make all
#
# Running the kbr_forth interpreter on its own:
#
# 	make run
#
# It's possible to run custom FORTH scripts by setting the environment variable
# SCRIPT to the script's filename.  E.g. using `env`:
#
# 	env SCRIPT=custom_script.fs make run
#
# Everything is built in debug mode by default.  To build non-debug builds, set
# the environment variable DEBUG to 0.

CC = gcc

ASFLAGS += -pipe -m64
LDFLAGS += -nostdlib -static

CAT = cat

DEBUG ?= 1
ifeq ($(DEBUG), 1)
	ASFLAGS += -g
else
	CPPFLAGS += -DNDEBUG
endif


BIN       = kbr_forth
BIN_FORTH = $(BIN).fs

$(BIN) : $(BIN).S
	$(CC) $(ASFLAGS) $(CPPFLAGS) $(LDFLAGS) -o $@ $^

.PHONY : all
all : $(BIN)

.PHONY : run
run : $(BIN)
	$(CAT) $(BIN_FORTH) $(SCRIPT) - | ./$(BIN)

.PHONY : clean
clean :
	$(RM) $(BIN)
