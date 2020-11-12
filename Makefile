
# parse.o toc.o scope.o

OBJ =  main.o lex.o  runtime.o

#clang on free
pacc: $(OBJ) 
	cc $(OBJ) -g -o pacc

.c.o: runtime.h pacc.h
	cc $< -g -c -I. 

clean:
	rm -f *.o pacc *~


