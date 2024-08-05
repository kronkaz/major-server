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
	ADMIN_USER=admin ADMIN_PASSWORD=1234 docker compose up -d

test:
	dune exec test
