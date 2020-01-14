
OBJ = debug.o scope.o main.o lex.o parse.o runtime.o

#clang on free
pacc: $(OBJ) runtime.h pacc.h
	cc -Wl,-error-limit=0 $(OBJ) -o pacc

.c.o:
	cc $< -c -I. 

clean:
	rm *.o
