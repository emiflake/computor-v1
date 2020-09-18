NAME=computor-v1

all: $(NAME)
$(NAME): $(wildcard **/*.hs)
	stack build --ghc-options=-O3 --copy-bins --local-bin-path ./
	mv ./computor-v1-exe $(NAME)

ghci:
	stack ghci

clean:
	rm -rf ./.stack-work

fclean: clean
	rm -rf $(NAME)

re:
	$(MAKE) fclean
	$(MAKE) $(NAME)
