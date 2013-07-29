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
          {path: '{{.Filename}}', params: {{.Params}}}
      ], function(widget) {
          function getInterface(obj, iid, funcNames) {
              var x = obj.constructor.interfaces;
              if (x) {
                  var iface;
                  if (typeof iid === 'string') {
                      iface = x[iid];
                      if (iface) {
                          return iface;
                      }
                  }
                  for (var nm in x) {
                      var o = x[nm];
                      var match = true;
                      for (var i = 0; i < funcNames.length; i++) {
                          var need = funcNames[i];
                          if (typeof o[need] !== 'function') {
                              match = false;
                              break;
                          }
                      }
                      if (match) {
                          return o;
                      }
                  }
              }
          }
          var title;
          var page = getInterface(widget, null, ["getTitle"]);
          if (page) {
              title = page.getTitle(widget);
          } else if (widget.getTitle) {
              title = widget.getTitle();
          }
          if (title) {
              document.title = title;
          }
          var nd;
          var iface = getInterface(widget, null, ["toDom"]);
          if (iface) {
              nd = iface.toDom(widget)
          } else if (widget.toDom) {
              nd = widget.toDom()
          }
          document.body.appendChild(nd);
      });
    </script>
  </body>
</html>`
