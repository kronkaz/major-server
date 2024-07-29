.PHONY: stop build run test

stop:
	docker compose down

build:
	docker compose build

openapi: openapi/template.html openapi/openapi.yml
	sed -e '/<<YAML>>/{r openapi/openapi.yml' -e 'd' -e '}' openapi/template.html \
		> openapi/openapi.html

run: build
	docker compose up -d

test:
	dune exec test
