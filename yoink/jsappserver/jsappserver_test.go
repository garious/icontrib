package jsappserver

import (
	"io/ioutil"
	"log"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestGetPages(t *testing.T) {
	getJsPage(t, "/stdlib", "../stdlib", "/stdlib/dom_test", 200)
	getJsPage(t, "/stdlib", "../stdlib", "/stdlib/", 200) // contains index.js
	getJsPage(t, "/stdlib", "../stdlib", "/stdlib", 200)  // contains index.js
	getJsPage(t, "/stdlib", "../stdlib", "/bogus", 404)

	// When the user requests "test" and both
        // "test" and "test.js" exist, return "test".
	s := getJsPage(t, "/x", "./testdata", "/x/test", 200)
	if s != "It worked\n" {
		print(s)
		t.Fail();
	}
}

func getJsPage(t *testing.T, patt, dir, url string, code int) string {
	ts := httptest.NewServer(NewJsAppServer(patt, dir))
	defer ts.Close()

	res, err := http.Get(ts.URL + url)
	if err != nil {
		log.Fatal(err)
	}
	if res.StatusCode != code {
		log.Fatal(res.Status)
	}
	by, err := ioutil.ReadAll(res.Body)
	res.Body.Close()
	if err != nil {
		log.Fatal(err)
	}
	return string(by)
}
