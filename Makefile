##
## EPITECH PROJECT, 2019
## minishell1
## File description:
## Makefile
##

SRC				=		$(shell find ./ -name '*.hs')	\

OBJ             =		$(SRC:.c=.o)

NAME			=		wolfram

all:	$(NAME)

$(NAME):	$(OBJ)
		ghc -o $(NAME) $(OBJ)

error:
		ghc -o $(NAME) $(OBJ) $(SRCDIR)

clean:
		rm -f $(shell find $(SOURCEDIR) -name '*.o')
		rm -f $(shell find $(SOURCEDIR) -name '*.hi')
		rm -f $(shell find $(SOURCEDIR) -name '*~')
		rm -f $(shell find $(SOURCEDIR) -name '*#')
		rm -f $(shell find $(SOURCEDIR) -name '*vg*')
		rm -f $(shell find $(SOURCEDIR) -name '*.gc*')

fclean: clean
		rm -f $(NAME)

re:	clean fclean all

.PHONY:	all clean re
