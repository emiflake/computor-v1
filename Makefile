NAME=computor-v1-exe

all: $(NAME)
$(NAME):
	stack build --ghc-options=-O3 --copy-bins --local-bin-path ./

ghci:
	stack ghci

clean:
	rm -rf ./.stack-work

fclean: clean
	rm -rf $(NAME)
