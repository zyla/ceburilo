Ceburilo
========

Wyszukanie trasy
----------------

URL:

    /route GET

Parametry:

- beg_lat - wymagany
- beg_lng - wymagany
- dest_lat - wymagany
- dest_lng - wymagany
- begining - nazwa lokacji początkowej (zwracana bez zmian w odpowiedzi)
- destination - nazwa lokacji końcowej (zwracana bez zmian w odpowiedzi)

Odpowiedź:

    {
        "begining": STRING // nazwa lokacji podana w argumencie
        "destination": STRING, // nazwa lokacji podana w argumencie
        "path":
            {
            "points":
                {
                "coordinates": [[FLOAT, FLOAT]] // lista par współrzędnych węzłów
                },
            "time": INT, // przewidywany czas trwania podróży (ms)
            "distance": INT, // dystans (m)
            "instructions": // instrukcje
                [
                    {
                        // tekst instrukcji
                        "text": STRING,
                        // indeksy wezłów na których trwa dana instrukcja (od 0)
                        "interval": [INT, INT]
                    }
                ]
            },
    }


Przykładowe zapytanie:

    /route?beg_lat=1.0&dest_lat=1.0&beg_lng=1.0&dest_lng=1.0
Przykładowa odpowiedź:

    {
        "destination": "asd",
        "path":
            {
                "points":
                    {
                        "coordinates":
                            [
                                [52.235706, 20.996794],
                                [52.248848, 21.005457],
                                [52.230217, 21.012592],
                                [52.231426, 21.021326],
                                [52.23935, 21.016813]
                            ]
                    },
                "time": 234000,
                "distance": 30,
                "instructions":
                    [
                        {
                            "text": "Skręć prosto",
                            "interval": [0,3]
                        },
                        {
                            "text": "Zawróć",
                            "interval": [3,4]
                        }
                    ]
            },
            "begining": "dsa"
    }
