package main

import (
	"github.com/garious/icontrib/client"
	"log"
	"net/http"
)

func main() {
	client.Start()
	log.Fatal(http.ListenAndServe(":8080", nil))
}

