.PHONY: stop build run test

stop:
	docker compose down

reset:
	make stop
	docker image rm major-major
	make run

openapi: openapi/template.html openapi/openapi.yml
	sed -e '/<<YAML>>/{r openapi/openapi.yml' -e 'd' -e '}' openapi/template.html \
		> openapi/openapi.html

run: build
	MAJOR_PORT=8000 MAJOR_ADMIN_USER=admin MAJOR_ADMIN_PASSWORD=1234 docker compose up -d

runlocal: build
	PORT=8000 ADMIN_USER=admin ADMIN_PASSWORD=1234 dune exec major -- --test

test:
	dune exec test
