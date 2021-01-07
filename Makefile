
# toc.o scope.o
# expression.o

OBJ =  main.o map.o lex.o parse.o runtime.o statement.o declarator.o expression.o union.o syntax.o data.o
HEADERS = pacc.h runtime.h

#clang on free
pacc: $(OBJ) 
	cc $(OBJ) -g -o pacc

# elf platforms - mac seems to have fucked this
syntax.o: syntax
	objcopy --input binary --output-target elf64-x86-64 syntax $@

$(OBJ): $(HEADERS)
.c.o: 
	cc $< -g -c -I. 

run: pacc
	./pacc

clean:
	rm -f *.o pacc *~


