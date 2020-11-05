
OBJ = debug.o scope.o main.o lex.o parse.o runtime.o

#clang on free
pacc: $(OBJ) runtime.h pacc.h
	cc $(OBJ) -g -o pacc

.c.o:
	cc $< -g -c -I. 

clean:
	rm -f *.o pacc *~


