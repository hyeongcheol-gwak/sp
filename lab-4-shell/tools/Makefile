#--------------------------------------------------------------------*/
# Shell Lab
# Spring 2026
# System Programming
#
# Makefile for regression testing
#
# GNU make documentation: https://www.gnu.org/software/make/manual/make.html
#
# Typically, the only thing you need to modify in this Makefile is the list of source files.
#

#--- variable declarations

# C compiler and compilation flags
CC=gcc
CFLAGS=-Wall -O2 -g

SOURCES=$(wildcard my*.c)
TARGETS=$(SOURCES:.c=)

#--- rules

all: tools

tools: $(TARGETS)

my%: my%.c
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -f $(TARGETS)


