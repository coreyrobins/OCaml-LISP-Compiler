# Corey Robins. Section 0201. crobins. 111185399
# I pledge on my honor that I have not given or receieved any unauthorized
# assistance on this assignment 
CC = ocamlc
CFLAGS = -c
PROGS = scanner parser interpreter

all: $(PROGS)

scanner: get_input.cmo scanner_module.cmo scanner.cmo
	$(CC) str.cma get_input.cmo scanner_module.cmo scanner.cmo -o scanner

parser: get_input.cmo scanner_module.cmo parser_module.cmo parser.cmo
	$(CC) str.cma get_input.cmo scanner_module.cmo parser_module.cmo parser.cmo -o parser

interpreter: get_input.cmo scanner_module.cmo parser_module.cmo interpreter_module.cmo interpreter.cmo
	$(CC) str.cma get_input.cmo scanner_module.cmo parser_module.cmo interpreter_module.cmo interpreter.cmo -o interpreter

scanner.cmo: scanner.ml scanner_module.cmi 
	$(CC) $(CFLAGS) scanner.ml

parser.cmo: parser.ml parser_module.cmi 
	$(CC) $(CFLAGS) parser.ml

interpreter.cmo: interpreter.ml  interpreter_module.cmi
	$(CC) $(CFLAGS) interpreter.ml

scanner_module.cmi: scanner_module.mli
	$(CC) $(CFLAGS) scanner_module.mli

scanner_module.cmo: scanner_module.cmi scanner_module.ml
	$(CC) $(CFLAGS) scanner_module.ml

parser_module.cmi: parser_module.mli
	$(CC) $(CFLAGS) parser_module.mli

parser_module.cmo: parser_module.cmi parser_module.ml
	$(CC) $(CFLAGS) parser_module.ml

interpreter_module.cmi: interpreter_module.mli
	$(CC) $(CFLAGS) interpreter_module.mli

interpreter_module.cmo: interpreter_module.cmi interpreter_module.ml
	$(CC) $(CFLAGS) interpreter_module.ml

clean:
	rm -f interpreter.cmo parser.cmo scanner.cmo interpreter_module.cmo parser_module.cmo scanner_module.cmo $(PROGS)
