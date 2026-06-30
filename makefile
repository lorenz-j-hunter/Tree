# Compiler
includes = -I./main -I./lowlevel

# Project Structure
target = main/tree 
sources = $(wildcard main/*.cpp) \
					$(wildcard lowlevel/*.cpp)
objects = $(sources:%=objects/%) # for each relpath in sources, prepend "objects" to it
objects := $(objects:.cpp=.o) # change/map ".cpp" to ".o".

# Link
$(target): $(objects)
	g++ -g $(includes) $(objects) -o $(target) 

# Compile
objects/%.o: %.cpp 
	g++ -g $(includes) -c $< -o $@

# Cleanup
clean:
	rm ./objects/*.o $(target) 
