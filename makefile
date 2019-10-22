CC=ocamlc
RM	= rm -f
DIR_SRC = ./src
DIR_BUILD=./build
EXEC = $(DIR_BUILD)/abr
SRC = $(wildcard ${DIR_SRC}/*.ml)


all : $(EXEC)

$(EXEC): $(SRC)
	$(CC) -o $@ $^


.PHONY: clean test
clean:
	$(RM) $(DIR_SRC)/*.cmo $(DIR_SRC)/*.cmi
	$(RM) *.cmo *.cmi
	$(RM) $(EXEC)

test:
	$(EXEC)
