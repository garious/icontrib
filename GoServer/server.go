package main

import (
	"html/template"
	"log"
	"net/http"
	"net/url"
	"os"
	"strings"
)

func main() {
	http.HandleFunc("/Yoink.js", onYoink)
	http.Handle("/Tag/", newJsAppServer("/Tag", "../Tag"))
	http.Handle("/Toys/", newJsAppServer("/Toys", "../Toys"))
	http.Handle("/", newJsAppServer("", "../Client"))
	log.Fatal(http.ListenAndServe(":8080", nil))
}

type Page struct {
	Filename string
	Params   url.Values
}

type JsAppServer struct {
	Prefix string
	Root   string
}

func newJsAppServer(prefix string, root string) http.Handler {
	return &JsAppServer{Prefix: prefix, Root: root}
}

func (h *JsAppServer) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	path := h.Root + strings.TrimPrefix(r.URL.Path, h.Prefix)

	if exists(path) {
		http.ServeFile(w, r, path)
	} else if exists(path + ".js") {
		templ := template.New("bar")
		parsedTempl, _ := templ.Parse(jsAppHtml)

		r.ParseForm() // Parse the parameters in the request's URI
		page := &Page{Filename: r.URL.Path + ".js", Params: r.Form}

		w.Header().Set("Content-Type", "text/html")
		parsedTempl.Execute(w, page)
	} else {
		http.NotFound(w, r)
	}
}

func onYoink(w http.ResponseWriter, r *http.Request) {
	http.ServeFile(w, r, "../Yoink/Yoink.js")
}

func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

var jsAppHtml = `<!DOCTYPE html>
<html>
  <head></head>
  <body>
    <script src="/Yoink.js"></script>
    <script>
      YOINK.setDebugLevel(1);
      YOINK.resourceLoader('', {}, window.PRELOADED_MODULES).getResources([
          '/Tag/Interface.js',
          '/Tag/ToDom.js',
          '/Tag/Webpage.js',
          {path: '{{.Filename}}', params: {{.Params}}}
      ], function(Iface, Dom, Webpage, widget) {
          var page = Iface.getInterface(widget, Webpage.webpageId);
          if (page) {
              var title = page.getTitle(widget);
              if (title) {
                  document.title = title;
              }
          }
          var iface = Iface.getInterface(widget, Dom.toDomId);
          var nd = iface ? iface.toDom(widget) : widget;
          document.body.appendChild(nd);
      });
    </script>
  </body>
</html>`
