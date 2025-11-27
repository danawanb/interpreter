package main

import (
	"fmt"
	"inter/repl"
	"os"
	"os/user"
)

func main() {

	fmt.Println("interpreter")
	fmt.Println("danawan")

	user, err := user.Current()
	if err != nil {
		panic(err)
	}

	fmt.Printf("Hello %s! This is the Monkey programming language!\n",
		user.Username)
	fmt.Printf("Feel free to type in commands\n")
	repl.Start(os.Stdin, os.Stdout)
}
