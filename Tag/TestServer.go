package main

import (
	"github.com/garious/icontrib/GoServer/jsappserver"
	"log"
	"net/http"
)

func main() {
	jsappserver.HandleDir("/Tag/", ".")
	log.Fatal(http.ListenAndServe(":8080", nil))
}
