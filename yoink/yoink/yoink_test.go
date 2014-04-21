package yoink

import (
	"errors"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

func TestFile(t *testing.T) {
	err := filepath.Walk(".", onFile)
	if err != nil {
		log.Fatal(err)
	}
}

func TestTest(t *testing.T) {
	err := filepath.Walk(".", onTestFile)
	if err != nil {
		log.Fatal(err)
	}
}

func onFile(path string, info os.FileInfo, err error) error {
	if filepath.Ext(path) == ".js" {
		log.Printf("Linting: %v", path)
		return lint(path)
	}
	return nil
}

func onTestFile(path string, info os.FileInfo, err error) error {
	matched, err := filepath.Match("*_test.js", path)
	if err != nil {
		return err
	}
	if matched {
		log.Printf("Testing: %v", path)
		return test(path)
	}
	return nil
}

// Lint the source file.
func lint(p string) error {
	out, err := exec.Command("jsl", "-nologo", "-nofilelisting", "-nosummary", "-output-format", "__FILE__:__LINE__:__COL__: __ERROR__", "-process", p).Output()
	if err != nil {
		return errors.New(string(out))
	}
	return nil
}

func test(p string) error {
	out, err := exec.Command("node", "yoink-adapter.js", p).Output()
	if err != nil {
		return errors.New(string(out))
	}
	return nil
}
