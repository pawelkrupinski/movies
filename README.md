# scala-play-app

A REST API built with **Scala + Play Framework 2.9 + MongoDB**.

## Stack

| Layer       | Technology                        |
|-------------|-----------------------------------|
| Language    | Scala 2.13                        |
| Web         | Play Framework 2.9                |
| Database    | MongoDB (official Scala driver 5) |
| DI          | Guice                             |
| Build       | sbt 1.10                          |
| Testing     | ScalaTest + Mockito               |

---

## Prerequisites

- **JDK 17+** — `java -version`
- **sbt 1.10+** — https://www.scala-sbt.org/download
- **MongoDB 6+** running locally on port `27017`

Start MongoDB locally:
```bash
# macOS (Homebrew)
brew services start mongodb-community

# Docker (fastest)
docker run -d -p 27017:27017 --name mongo mongo:7
```

---

## Run

```bash
# Development mode (hot reload)
sbt run

# The app starts at http://localhost:9000
```

---

## REST API

### Health
```
GET  /health
```

### Items

| Method | Path             | Description        |
|--------|------------------|--------------------|
| GET    | /api/items       | List all items     |
| POST   | /api/items       | Create an item     |
| GET    | /api/items/:id   | Get item by ID     |
| PUT    | /api/items/:id   | Update item        |
| DELETE | /api/items/:id   | Delete item        |

### Example — Create an item
```bash
curl -X POST http://localhost:9000/api/items \
  -H "Content-Type: application/json" \
  -d '{
    "name": "Widget",
    "description": "A very useful widget",
    "price": 9.99,
    "tags": ["sale", "new"]
  }'
```

### Example — List all items
```bash
curl http://localhost:9000/api/items
```

### Example — Update an item
```bash
curl -X PUT http://localhost:9000/api/items/<id> \
  -H "Content-Type: application/json" \
  -d '{"price": 7.49}'
```

---

## Configuration

Key settings in `conf/application.conf`:

| Key                  | Default                    | Env var              |
|----------------------|----------------------------|----------------------|
| `mongodb.uri`        | `mongodb://localhost:27017` | `MONGODB_URI`        |
| `mongodb.database`   | `scala_play_db`            | `MONGODB_DATABASE`   |
| `play.http.secret.key` | *(change this!)*         | `APPLICATION_SECRET` |

For production, always set `APPLICATION_SECRET` to a long random string:
```bash
export APPLICATION_SECRET=$(openssl rand -base64 32)
```

---

## Test

```bash
sbt test
```

---

## Project Structure

```
.
├── app/
│   ├── controllers/
│   │   ├── HealthController.scala   # GET /health
│   │   └── ItemController.scala    # REST endpoints
│   ├── models/
│   │   └── Item.scala              # Domain model + request shapes
│   ├── modules/
│   │   └── MongoModule.scala       # Guice DI wiring for Mongo
│   ├── repositories/
│   │   └── ItemRepository.scala    # All MongoDB access
│   └── services/
│       └── ItemService.scala       # Business logic
├── conf/
│   ├── application.conf
│   └── routes
├── test/
│   └── controllers/
│       └── ItemControllerSpec.scala
├── build.sbt
└── project/
    ├── build.properties
    └── plugins.sbt
```

---

## Extending

To add a new resource (e.g. `User`):

1. Add `User.scala` in `models/`
2. Add `UserRepository.scala` in `repositories/`
3. Add `UserService.scala` in `services/`
4. Add `UserController.scala` in `controllers/`
5. Add routes in `conf/routes`
