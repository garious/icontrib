package jsappserver

import (
	"io/ioutil"
	"log"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestGetTag(t *testing.T) {
	ts := httptest.NewServer(NewJsAppServer("/Tag", "../../Tag"))
	defer ts.Close()

	res, err := http.Get(ts.URL + "/Tag/LayoutTest")
	if err != nil {
		log.Fatal(err)
	}
	_, err = ioutil.ReadAll(res.Body)
	res.Body.Close()
	if err != nil {
		log.Fatal(err)
	}
}
