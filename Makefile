
# toc.o scope.o
# expression.o

OBJ =\
   data.o\
   declarator.o\
   expression.o\
   lex.o\
   main.o\
   map.o\
   parse.o\
   runtime.o\
   statement.o\
   syntax.o\
   toc.o\
   union.o

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


