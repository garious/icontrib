package jsappserver

import (
	"html/template"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
	"path"
	"runtime"
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

func (h *JsAppServer) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	p := h.Root + strings.TrimPrefix(r.URL.Path, h.Prefix)

	if exists(p) {
		http.ServeFile(w, r, p)
	} else if exists(p + ".js") {
		templ := template.New("bar")
		parsedTempl, _ := templ.Parse(jsAppHtml)

		_, filename, _, _ := runtime.Caller(0)
		yoinkBytes, err := ioutil.ReadFile(path.Join(path.Dir(filename), "../../Yoink/Yoink.js"))
		if err != nil {
			log.Fatal(err)
		}

		r.ParseForm() // Parse the parameters in the request's URI
		page := &Page{Filename: r.URL.Path + ".js", Params: r.Form, Yoink: template.JS(yoinkBytes)}

		w.Header().Set("Content-Type", "text/html")
		parsedTempl.Execute(w, page)
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
  <body>
    <script>{{.Yoink}}</script>
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
