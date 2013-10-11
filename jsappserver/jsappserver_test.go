package jsappserver

import (
	"io/ioutil"
	"log"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestGetPages(t *testing.T) {
	//getJsPage(t, "/Tag", "tag", "/Tag/LayoutTest", 200)
	//getJsPage(t, "/Tag", "tag", "/Tag/", 200)  // contains Index.js
	//getJsPage(t, "/Tag", "tag", "/Tag", 200)   // contains Index.js
	getJsPage(t, "/Tag", "tag", "/bogus", 404)
}

func getJsPage(t *testing.T, patt, dir, url string, code int) {
	ts := httptest.NewServer(NewJsAppServer(patt, dir))
	defer ts.Close()

	res, err := http.Get(ts.URL + url)
	if err != nil {
		log.Fatal(err)
	}
	if res.StatusCode != code {
		log.Fatal(res.Status)
	}
	_, err = ioutil.ReadAll(res.Body)
	res.Body.Close()
	if err != nil {
		log.Fatal(err)
	}
}
