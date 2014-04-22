package client

import (
	"github.com/garious/yoink/jsok"
	"path/filepath"
	"testing"
	"log"
	"os"
)

func TestLint(t *testing.T) {
	err := filepath.Walk(".", handleJsLint)
	if err != nil {
		log.Fatal(err)
	}
}

func TestTests(t *testing.T) {
	err := filepath.Walk(".", handleJsExec)
	if err != nil {
		log.Fatal(err)
	}
}

func handleJsLint(path string, info os.FileInfo, err error) error {
	if filepath.Ext(path) == ".js" {
		log.Printf("Linting: %v", path)
		return jsok.JsLint(path)
	}
	return nil
}

func handleJsExec(path string, info os.FileInfo, err error) error {
	matched, err := filepath.Match("*_test.js", path)
	if err != nil {
		return err
	}
	if matched {
		log.Printf("Testing: %v", path)
		return jsok.JsExec(path)
	}
	return nil
}

