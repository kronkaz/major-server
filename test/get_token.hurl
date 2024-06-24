GET http://localhost:8080/v1/token
HTTP 400

[Captures]
msg1: body

GET http://localhost:8080/v1/token
`add`
HTTP 400

[Captures]
msg2: body

GET http://localhost:8080/v1/token
`{}`
HTTP 400

[Captures]
msg3: body

GET http://localhost:8080/v1/token
`{ "voter_id": 0, "password": "1234"`
HTTP 400

[Captures]
msg4: body

GET http://localhost:8080/v1/token
`{ "voter_id": 0, "password": "1234"}`
HTTP 401

[Captures]
msg5: body

GET http://localhost:8080/v1/token
`{ "voter_id": 0, "password": "0000"}`
HTTP 200

[Captures]
access_token: jsonpath "$.access_token"
refresh_token: jsonpath "$.refresh_token"

GET http://localhost:8080/v1/token
`{ "voter_id": 0, "password": "0000"}`
HTTP 406

[Captures]
msg6: body

# NB: this requires voter 1 to have an active token that gets inactive at the beginning of the test
# TODO: clear tokens (cf https://github.com/kronkaz/major-server/issues/1)

# GET http://localhost:8080/v1/token
# `{ "voter_id": 1, "password": "1111"}`
# HTTP 200

# [Captures]
# access_token: jsonpath "$.access_token"
# refresh_token: jsonpath "$.refresh_token"
