package jsappserver

import (
	"html/template"
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

func mkPage(w http.ResponseWriter) error {
	templ := template.New("bar")
	parsedTempl, _ := templ.Parse(jsAppHtml)

	yoinkBytes, err := Asset("../loader/yoink.js")
	if err != nil {
		return err
	}

	page := &Page{Yoink: template.JS(yoinkBytes)}

	w.Header().Set("Content-Type", "text/html")
	return parsedTempl.Execute(w, page)
}

func serveURL(p string, w http.ResponseWriter, r *http.Request) error {
	if exists(p) {
		http.ServeFile(w, r, p)
		return nil
	} else if exists(p + "index.js") || exists(p + ".js") {
		return mkPage(w)
	} else {
		http.NotFound(w, r)
		return nil
	}
}

func (h *JsAppServer) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	p := h.Root + strings.TrimPrefix(r.URL.Path, h.Prefix)
	err := serveURL(p, w, r)
	if err != nil {
		http.Error(w, err.Error(), 500)
	}
}

func exists(p string) bool {

	finfo, err := os.Stat(p)
	return err == nil && !finfo.IsDir()
}

var jsAppHtml = `<!DOCTYPE html>
<html>
  <head></head>
  <body style="margin: 0; padding: 0">
    <script>{{.Yoink}}</script>
    <script>
      (function() {
          var path = window.location.pathname;
          if (path === '/') {
              path = '/index';
          }
          path += '.js';
          YOINK.setDebugLevel(1);
          YOINK.resourceLoader('', {}, window.PRELOADED_MODULES).getResources([
              {path: path, params: YOINK.parseQueryString(window.location.search.substring(1))}
          ], function(widget) {
              if (widget.getTitle) {
                  document.title = widget.getTitle();
              }
              var nd = widget;
              if (typeof widget === 'string') {
                  nd = document.createTextNode(widget);
              } else if (typeof widget.render === 'function')  {
                  nd = widget.render();
              }
              document.body.appendChild(nd);
          });
      })();
    </script>
  </body>
</html>`
