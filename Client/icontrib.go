package main

import (
	"encoding/json"
	"github.com/garious/yoink/jsappserver"
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	http.HandleFunc("/charity/popular.json", popular)
	http.HandleFunc("/stats/community.json", community)
	http.HandleFunc("/donor/checkUser.json", checkUser)
	http.Handle("/donor/", http.FileServer(http.Dir("../private/static")))
	http.Handle("/charity/", http.FileServer(http.Dir("../private/static")))
	jsappserver.HandleDir("/Tag/", "../Tag")
	jsappserver.HandleDir("/", ".")

	log.Fatal(http.ListenAndServe(":8080", nil))
}

func popular(w http.ResponseWriter, r *http.Request) {
	type PopularCharity struct {
		Cid      string `json:"cid"`
		Name     string `json:"name"`
		ImageUrl string `json:"imageUrl"`
	}
	group := []interface{}{
		PopularCharity{"lacc", "LACC", "yikes"},
	}
	b, err := json.Marshal(group)
	if err != nil {
		log.Fatal(err)
	}
	w.Write(b)
}

func community(w http.ResponseWriter, r *http.Request) {
	b, err := ioutil.ReadFile("../private/static/donor/greg.json")
	if err != nil {
		log.Fatal(err)
	}
	w.Write(b)
}

func checkUser(w http.ResponseWriter, r *http.Request) {
	type Auth struct {
		Right string
	}
	b, err := json.Marshal(Auth{"greg"})
	if err != nil {
		log.Fatal(err)
	}
	w.Write(b)
}
