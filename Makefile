
pacc: debug.o scope.o main.o lex.o parse.o 
	cc $< -o pacc

.c.o:
	cc $< -c -I. 
