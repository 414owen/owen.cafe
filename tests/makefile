CC = gcc
CFLAGS = -Wall -Wextra -std=c11 -O2

TARGET = test_is_number
SRCS = is_number.c
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
