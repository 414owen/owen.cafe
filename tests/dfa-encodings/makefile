CC = gcc
CFLAGS = -O2 -s

TARGET = test_is_number
SRCS = is_number.c direct.c switchy.c table.c table_equiv.c manual.c
OBJS = $(SRCS:.c=.o)

run: $(TARGET)
	./$(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f $(OBJS) $(TARGET)

.PHONY: run clean
