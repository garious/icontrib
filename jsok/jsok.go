package jsok

import (
	"errors"
	"os/exec"
        "path"
        "path/filepath"
        "runtime"
)


// Lint the source file.
func JsLint(p string) error {
	out, err := exec.Command("jsl", "-nologo", "-nofilelisting", "-nosummary", "-output-format", "__FILE__:__LINE__:__COL__: __ERROR__", "-process", p).Output()
	if err != nil {
		return errors.New(string(out))
	}
	return nil
}

func JsExec(p string) error {
        _, filename, _, _ := runtime.Caller(0)
        thisDir := path.Dir(filename)

	out, err := exec.Command("node", filepath.Join(thisDir, "yoink-adapter.js"), p).Output()
	if err != nil {
		return errors.New(string(out))
	}
	return nil
}
