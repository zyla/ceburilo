Ceburilo API [![Build Status](https://travis-ci.org/zyla/ceburilo.svg?branch=master)](https://travis-ci.org/zyla/ceburilo)
========

Wyszukanie trasy
----------------

URL:

    /route GET

Parametry:

- `beg_lat` - wymagany
- `beg_lng` - wymagany
- `dest_lat` - wymagany
- `dest_lng` - wymagany

Odpowiedź:

```
{
  "beg_coord": [FLOAT, FLOAT],
  "dest_coord": [FLOAT, FLOAT],
  "path": {
    "points": {
      "coordinates": [[FLOAT, FLOAT]] // lista par współrzędnych węzłów
    },
    "time": INT, // przewidywany czas trwania podróży (ms)
    "distance": INT, // dystans (m)
  },
  "stations": [ {
    "location": [FLOAT, FLOAT], // współrzędne stacji
    "name": STRING, // nazwa stacji
    "number": INT // numer stacji
  } ]
}
```


Przykładowe zapytanie:

    /route?beg_lat=1.0&dest_lat=1.0&beg_lng=1.0&dest_lng=1.0
Przykładowa odpowiedź:

```
{
  "dest_coord": [ 52.235706, 20.996794 ],
  "path": {
    "points": {
      "coordinates": [
        [ 52.235706, 20.996794 ],
        [ 52.248848, 21.005457 ],
        [ 52.230217, 21.012592 ],
        [ 52.231426, 21.021326 ],
        [ 52.23935, 21.016813 ]
      ]
    },
    "time": 234000,
    "distance": 30
  },
  "beg_coord": [ 52.23935, 21.016813 ],
  "stations": [
    {
      "location": [ 52.133163, 21.07482 ],
      "name": "ul. Wąwozowa- ul.Rosoła",
      "number": 6446
    },
    {
      "location": [ 52.162342, 21.038431 ],
      "name": "SGGW I",
      "number": 6341
    }
  ]
}
```
