# analytics

Toy analytics engine written in Haskell using Warp.

Depends on Redis.

## Installation

You will need [`stack`](http://docs.haskellstack.org/en/stable/README.html)

```
$ stack build
```

# Running

The engine needs Redis to run, using default connection settings (port 6379).

Once built, you can run the project using

```
$ stack exec analytics
```

# Usage

The webservice contains two endpoints

* `GET /analytics?name=NAME&page=PAGE` tell the engine that a new connection
has been made to the site called `NAME`, to the page `PAGE`

* `GET /analytics/NAME` fetch all pages (and their connections) of the site
called `NAME`. Returns result as a JSON object (see example)

## Example

```
$ curl -X GET http://localhost:3000/analytics?name=mysite&page=mypage
$ curl -X GET http://localhost:3000/analytics?name=mysite&page=mypage
$ curl -X GET http://localhost:3000/analytics?name=mysite&page=index
```

```
$ curl -X GET http://localhost:3000/analytics/mysite | jq .
{
  "pages": [
    {
      "connections": [
        "2015-12-14T20:20:40.804Z",
        "2015-12-14T20:20:43.145Z"
      ],
      "page": "mypage"
    },
    {
      "connections": [
        "2015-12-14T20:23:14.447Z"
      ],
      "page": "index"
    }
  ]
}
```
