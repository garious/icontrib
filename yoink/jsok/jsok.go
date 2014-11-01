package jsok

import (
	"encoding/json"
	"errors"
	"os/exec"
	"path"
	"path/filepath"
	"runtime"
)

// Lint the source file.
func JsLint(p string) error {
	out, err := exec.Command("jsl", "-nologo", "-nofilelisting", "-nosummary", "-output-format", "__FILE__:__LINE__:__COL__: __ERROR__", "-process", p).Output()
	if _, ok := err.(*exec.ExitError); ok {
		return errors.New(string(out))
	} else if err != nil {
		return err
	}
	return nil
}

func JsExec(p string) error {
	return JsExecWithModuleMap(p, nil)
}

func JsExecWithModuleMap(p string, modMap map[string]string) error {
	_, filename, _, _ := runtime.Caller(0)
	thisDir := path.Dir(filename)

	jsonModMap, err := json.Marshal(modMap)
	if err != nil {
		return err
	}

	out, err := exec.Command(
		"node",
		filepath.Join(thisDir, "yoink-adapter.js"),
		"--modspec",
		string(jsonModMap),
		p).CombinedOutput()
	if _, ok := err.(*exec.ExitError); ok {
		return errors.New(string(out))
	} else if err != nil {
		return errors.New(string(out))
	}
	return nil
}
