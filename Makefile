
# toc.o scope.o
# expression.o

OBJ =  main.o map.o lex.o parse.o runtime.o statement.o declarator.o expression.o union.o

#clang on free
pacc: $(OBJ) 
	cc $(OBJ) -g -o pacc

.c.o: runtime.h pacc.h
	cc $< -g -c -I. 

run: pacc
	./pacc

clean:
	rm -f *.o pacc *~


