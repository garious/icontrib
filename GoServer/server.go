package main

import (
	"github.com/garious/icontrib/GoServer/jsappserver"
	"log"
	"net/http"
)

func main() {
	jsappserver.HandleDir("/Tag/", "../Tag")
	jsappserver.HandleDir("/Toys/", "../Toys")
	jsappserver.HandleDir("/", "../Client")

	log.Fatal(http.ListenAndServe(":8080", nil))
}
