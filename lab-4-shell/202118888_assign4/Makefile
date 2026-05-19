CC= gcc
OBJS = dynarray.o snush.o token.o execute.o job.o util.o lexsyn.o
TARGET = snush
CFLAGS = -g -D_GNU_SOURCE -O0 -Wall -DNDEBUG --static
SUBDIRS = tools

.SUFFIXES : .c .o

all : $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)
	@for d in $(SUBDIRS); do \
		if [ -d $$d ] && [ -f $$d/Makefile ]; then \
			$(MAKE) -C $$d; \
		fi; \
	done

clean :
	rm -f $(OBJS) $(TARGET)
	@for d in $(SUBDIRS); do \
		if [ -d $$d ] && [ -f $$d/Makefile ]; then \
			$(MAKE) -C $$d clean; \
		fi; \
	done
