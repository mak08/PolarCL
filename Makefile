#
# Makefile for CFFI mbedTLS bindings
#

MKDIR_P = mkdir -p

PREFIX := /usr/local

# Building the executable as a prerequisite from the .cl file did not work,
all: ctypes ctypes.cl

mbed-ctypes.cl: ctypes
	./ctypes

%.o: %.c
	gcc -fPIC -g -c $(CFLAGS) -Wall $<

%.so: %.o
	gcc -shared -o lib$(*).so $<

ctypes: ctypes.c
	gcc $(CFLAGS) ctypes.c -o ctypes

ctypes.cl: ctypes
	./ctypes
