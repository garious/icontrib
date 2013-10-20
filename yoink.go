package main

import (
	"github.com/garious/yoink/jsappserver"
	"log"
	"net/http"
	"path"
	"runtime"
)

func main() {
	_, filename, _, _ := runtime.Caller(0)
	yoinkDir := path.Join(path.Dir(filename), "yoink")
	jsappserver.HandleDir("/yoink/", yoinkDir)
	jsappserver.HandleDir("/", ".")

	log.Fatal(http.ListenAndServe(":8080", nil))
}
