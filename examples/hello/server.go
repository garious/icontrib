package example

import "github.com/garious/yoink/jsappserver"

func init() {
	jsappserver.HandleDir("/", ".")
}
