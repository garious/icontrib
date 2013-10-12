package main

import (
	"github.com/garious/yoink/jsappserver"
	"log"
	"net/http"
)

func main() {
	jsappserver.HandleDir("/Tag/", "../Tag")
	jsappserver.HandleDir("/", ".")

	log.Fatal(http.ListenAndServe(":8080", nil))
}
