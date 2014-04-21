package yoink

import (
        "path"
        "runtime"
)

func Dir() string {
        _, filename, _, _ := runtime.Caller(0)
        return path.Dir(filename)
}

