package main

import (
	"github.com/garious/yoink/jsappserver"
	"github.com/garious/yoink/yoink"
	"log"
	"net/http"
)

func main() {
	jsappserver.HandleDir("/yoink/", yoink.Dir())
	jsappserver.HandleDir("/", ".")
	log.Fatal(http.ListenAndServe(":8080", nil))
}
