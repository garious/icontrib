package client

import (
	"encoding/json"
	"github.com/garious/yoink/jsappserver"
	"github.com/garious/yoink/yoink"
	"io/ioutil"
	"log"
	"net/http"
	"path"
	"runtime"
)

func init() {
        _, filename, _, _ := runtime.Caller(0)
	dir := path.Dir(filename)

	http.HandleFunc("/charity/popular.json", popular)
	http.HandleFunc("/stats/community.json", community)
	http.HandleFunc("/donor/checkUser.json", checkUser)
	http.Handle("/static/", http.FileServer(http.Dir(path.Join(dir, "../data"))))
	jsappserver.HandleDir("/yoink/", yoink.Dir())

	jsappserver.HandleDir("/", dir)
}

func Start() {
}

func popular(w http.ResponseWriter, r *http.Request) {
	type PopularCharity struct {
		Cid      string `json:"cid"`
		Name     string `json:"name"`
		ImageUrl string `json:"imageUrl"`
	}
	group := []interface{}{
		PopularCharity{"lacc", "LACC", "/charity/lacc.gif"},
	}
	b, err := json.Marshal(group)
	if err != nil {
		log.Fatal(err)
	}
	w.Write(b)
}

func community(w http.ResponseWriter, r *http.Request) {
        _, filename, _, _ := runtime.Caller(0)
	userBlob, err := ioutil.ReadFile(path.Join(path.Dir(filename), "../data/static/donor/greg.json"))
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
	userBlob, err := ioutil.ReadFile("../data/static/donor/greg.json")
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
