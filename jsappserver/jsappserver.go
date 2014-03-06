package jsappserver

import (
	"html/template"
	"log"
	"net/http"
	"net/url"
	"os"
	"strings"
)

type Page struct {
	Filename string
	Params   url.Values
	Yoink    template.JS
}

type JsAppServer struct {
	Prefix string
	Root   string
}

func HandleDir(patt string, fp string) {
	prefix := strings.TrimSuffix(patt, "/")
	http.Handle(patt, NewJsAppServer(prefix, fp))
}

func NewJsAppServer(prefix string, root string) http.Handler {
	return &JsAppServer{Prefix: prefix, Root: root}
}

func mkPage(w http.ResponseWriter, r *http.Request, url string) {
	templ := template.New("bar")
	parsedTempl, _ := templ.Parse(jsAppHtml)

	yoinkBytes, err := Asset("../loader/yoink.js")
	if err != nil {
		log.Fatal(err)
	}

	if err := r.ParseForm(); err != nil { // Parse the parameters in the request's URI then
		log.Fatal(err)
	}

	page := &Page{Filename: url, Params: r.Form, Yoink: template.JS(yoinkBytes)}

	w.Header().Set("Content-Type", "text/html")
	parsedTempl.Execute(w, page)
}

func (h *JsAppServer) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	p := h.Root + strings.TrimPrefix(r.URL.Path, h.Prefix)

	if exists(p + "index.js") {
		mkPage(w, r, r.URL.Path + "index.js")
	} else if exists(p + "Index.js") {
		mkPage(w, r, r.URL.Path + "Index.js")
	} else if exists(p) {
		http.ServeFile(w, r, p)
	} else if exists(p + ".js") {
		mkPage(w, r, r.URL.Path + ".js")
	} else {
		http.NotFound(w, r)
	}
}

func exists(p string) bool {

	_, err := os.Stat(p)
	return err == nil
}

var jsAppHtml = `<!DOCTYPE html>
<html>
  <head></head>
  <body style="margin: 0; padding: 0">
    <script>{{.Yoink}}</script>
    <script>
      YOINK.setDebugLevel(1);
      YOINK.resourceLoader('', {}, window.PRELOADED_MODULES).getResources([
          {path: '{{.Filename}}', params: {{.Params}}}
      ], function(widget) {
          if (widget.getTitle) {
              document.title = widget.getTitle();
          }
          var nd = widget;
          if (typeof widget === 'string') {
              nd = document.createTextNode(widget);
          } else if (typeof widget.toDom === 'function')  {
              nd = widget.toDom();
          }
          document.body.appendChild(nd);
      });
    </script>
  </body>
</html>`
