objects = Tree.o TreeNode.o

%.o:	%.cpp
	g++ -c -g $<

main:	main.o $(objects)
	g++ -g main.o $(objects) -o tree 

clean:
	rm *.o
