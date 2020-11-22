
# parse.o toc.o scope.o

OBJ =  main.o map.o lex.o parse.o runtime.o statement.o expression.o

#clang on free
pacc: $(OBJ) 
	cc $(OBJ) -g -o pacc

.c.o: runtime.h pacc.h
	cc $< -g -c -I. 

run: pacc
	./pacc

clean:
	rm -f *.o pacc *~


