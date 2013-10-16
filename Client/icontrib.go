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
	userBlob, err := ioutil.ReadFile("../private/static/donor/greg.json")
	if err != nil {
		log.Fatal(err)
	}
	var userInfo UserInfo

	err = json.Unmarshal(userBlob, &userInfo)
	if err != nil {
		log.Fatal(err)
	}
	b, err := json.Marshal(userInfo)
	if err != nil {
		log.Fatal(err)
	}
	w.Write(b)
}

type UserInfo struct {
	Owner          string         `json:"owner"`
	FirstName      string         `json:"firstName"`
	LastName       string         `json:"lastName"`
	Phone          string         `json:"phone"`
	Email          string         `json:"email"`
	ImageUrl       string         `json:"imageUrl"`
	CentsDonated   int            `json:"centsDonated"`
	AlignedDonated int            `json:"alignedDonated"`
	AlignedUsers   []string       `json:"alignedUsers"`
	Distribution   []Distribution `json:"distribution"`
	Funds          []Fund         `json:"funds"`
}

type Distribution struct {
	Name   string   `json:"name"`
	Cid    string   `json:"cid"`
	Shares float64  `json:"shares"`
	Labels []string `json:"labels"`
}

type Fund struct {
	Name  string `json:"name"`
	Label string `json:"label"`
}

func checkUser(w http.ResponseWriter, r *http.Request) {
	type Auth struct {
		Right UserInfo
	}
	userBlob, err := ioutil.ReadFile("../private/static/donor/greg.json")
	if err != nil {
		log.Fatal(err)
	}
	var userInfo UserInfo

	err = json.Unmarshal(userBlob, &userInfo)
	if err != nil {
		log.Fatal(err)
	}
	b, err := json.Marshal(Auth{userInfo})
	if err != nil {
		log.Fatal(err)
	}
	w.Write(b)
}
