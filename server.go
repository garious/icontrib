package client

import (
	"appengine"
	"appengine/datastore"
	"appengine/user"
	"encoding/json"
	"errors"
	"github.com/garious/yoink/jsappserver"
	"io/ioutil"
	"net/http"
	"sort"
)

func init() {
	http.HandleFunc("/charity/popular.json", popular)
	http.HandleFunc("/stats/community.json", community)
	http.HandleFunc("/donor/checkUser.json", checkUser)

	jsappserver.HandleDir("/skin/", "client/skin")
	jsappserver.HandleDir("/", "client/pages")
}

// Migrate user information from the file system and store it in the database
func storeUser(c appengine.Context, usrName string) error {
	c.Infof("Migrating user to DB: %v", usrName)

	userBlob, err := ioutil.ReadFile("static/donor/" + usrName + ".json")
	if err != nil {
		return err
	}

	var usr User
	err = json.Unmarshal(userBlob, &usr)
	if err != nil {
		return err
	}

	userInfo := UserInfo{
		Owner:     usr.Owner,
		FirstName: usr.FirstName,
		LastName:  usr.LastName,
		Phone:     usr.Phone,
		Email:     usr.Email,
		ImageUrl:  usr.ImageUrl,
	}

	if usr.Email == "" {
		return errors.New("email address required")
	}

	parent := datastore.NewKey(c, "UserInfo", usr.Email, 0, nil)
	_, err = datastore.Put(c, parent, &userInfo)
	if err != nil {
		return err
	}

	for _, dist := range usr.Distribution {
		distInfo := DistributionInfo{
			Name:   dist.Name,
			Cid:    dist.Cid,
			Shares: dist.Shares,
			Labels: dist.Labels,
		}
		key := datastore.NewIncompleteKey(c, "DistributionInfo", parent)
		_, err := datastore.Put(c, key, &distInfo)
		if err != nil {
			return err
		}
	}

	for _, fund := range usr.Funds {
		fundInfo := FundInfo{
			Name:  fund.Name,
			Label: fund.Label,
		}
		key := datastore.NewIncompleteKey(c, "FundInfo", parent)
		_, err := datastore.Put(c, key, &fundInfo)
		if err != nil {
			return err
		}
	}

	return nil
}

type ByName []Distribution

func (a ByName) Len() int           { return len(a) }
func (a ByName) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a ByName) Less(i, j int) bool { return a[i].Name < a[j].Name }

func loadUserByEmail(c appengine.Context, email string, usr *User) error {
	key := datastore.NewKey(c, "UserInfo", email, 0, nil)
	var userInfo UserInfo
	if err := datastore.Get(c, key, &userInfo); err != nil {
		return err
	}

	usr.Owner = userInfo.Owner
	usr.FirstName = userInfo.FirstName
	usr.LastName = userInfo.LastName
	usr.Phone = userInfo.Phone
	usr.Email = userInfo.Email
	usr.ImageUrl = userInfo.ImageUrl
	usr.Distribution = []Distribution{}
	usr.Funds = []Fund{}

	q := datastore.NewQuery("DistributionInfo").Ancestor(key)
	distributions := []DistributionInfo{}
	_, err := q.GetAll(c, &distributions)
	if err != nil {
		return err
	}
	for _, distInfo := range distributions {
		dist := Distribution{
			Name:   distInfo.Name,
			Cid:    distInfo.Cid,
			Shares: distInfo.Shares,
			Labels: distInfo.Labels,
		}
		usr.Distribution = append(usr.Distribution, dist)
	}

	// Sort distribution list by name
	sort.Sort(ByName(usr.Distribution))

	q = datastore.NewQuery("FundInfo").Ancestor(key)
	funds := []FundInfo{}
	_, err = q.GetAll(c, &funds)
	if err != nil {
		return err
	}
	for _, fundInfo := range funds {
		fund := Fund{
			Name:  fundInfo.Name,
			Label: fundInfo.Label,
		}
		usr.Funds = append(usr.Funds, fund)
	}

	return nil
}

func initdb(c appengine.Context) error {
	// Out with the old
	q := datastore.NewQuery("").KeysOnly()
	keys, _ := q.GetAll(c, nil)
	err := datastore.DeleteMulti(c, keys)
	if err != nil {
		return err
	}

	// In with the new
	users := []string{"greg", "elisa", "eric", "tom"}

	for _, u := range users {
		if err := storeUser(c, u); err != nil {
			return err
		}
	}

	return nil
}

func popular(w http.ResponseWriter, r *http.Request) {
	type PopularCharity struct {
		Cid      string `json:"cid"`
		Name     string `json:"name"`
		ImageUrl string `json:"imageUrl"`
	}
	group := []interface{}{
		PopularCharity{"lacc", "LACC", "/static/charity/lacc.gif"},
	}
	b, err := json.Marshal(group)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Write(b)
}

func community(w http.ResponseWriter, r *http.Request) {
	c := appengine.NewContext(r)
	if err := initdb(c); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
	userBlob, err := ioutil.ReadFile("static/donor/greg.json")
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	var usr User
	err = json.Unmarshal(userBlob, &usr)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	b, err := json.Marshal(usr)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Write(b)
}

type UserInfo struct {
	Owner     string
	FirstName string
	LastName  string
	Phone     string
	Email     string
	ImageUrl  string
}

type DistributionInfo struct {
	Name   string
	Cid    string
	Shares float64
	Labels []string
}

type FundInfo struct {
	Name  string
	Label string
}

type User struct {
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
	LogoutUrl      string         `json:"logoutUrl"`
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
	c := appengine.NewContext(r)
	if err := initdb(c); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
	u := user.Current(c)
	if u == nil {
		type Auth struct {
			Left struct {
				LoginUrl string `json:"loginUrl"`
			}
		}

		url, _ := user.LoginURL(c, "/")

		auth := Auth{}
		auth.Left.LoginUrl = url

		b, err := json.Marshal(auth)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		w.Write(b)
		return
	}

	type Auth struct {
		Right User
	}

	email := "garious@gmail.com"
	if u.Email != "test@example.com" {
		email = u.Email
	}

	var usr User
	err := loadUserByEmail(c, email, &usr)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	url, _ := user.LogoutURL(c, "/")
	usr.LogoutUrl = url
	b, err := json.Marshal(Auth{usr})
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Write(b)
}
