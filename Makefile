.PHONY: \
  build test lint format clean docker-deps docker-build

build:
	stack build --pedantic --install-ghc --allow-different-user

test:
	stack test --pedantic --install-ghc --allow-different-user

lint:
	./scripts/lint-general.rb $(shell \
	  find . -type d \( \
	    -path ./.git -o \
	    -path ./.stack-work \
	  \) -prune -o \( \
	    -name '*.hs' -o \
	    -name '*.rb' -o \
	    -name '*.sh' -o \
	    -name '*.yml' -o \
	    -name 'Dockerfile' -o \
	    -name 'Makefile' \
	  \) -print \
	)
	for file in $(shell \
	  find . -type d \( \
	    -path ./.git -o \
	    -path ./.stack-work \
	  \) -prune -o \( \
	    -name '*.hs' \
	  \) -print \
	); do \
	  brittany "$$file" > "$$file.tmp"; \
	  (cmp "$$file.tmp" "$$file" && rm "$$file.tmp") || \
	    (rm "$$file.tmp" && false) || exit 1; \
	done

format:
	for file in $(shell \
	  find . -type d \( \
	    -path ./.git -o \
	    -path ./.stack-work \
	  \) -prune -o \( \
	    -name '*.hs' \
	  \) -print \
	); do \
	  brittany --write-mode=inplace "$$file"; \
	done

clean:
	rm -rf .stack-work

docker-deps:
	docker build -f scripts/Dockerfile -t stephanmisc/effects:deps .

docker-build:
	CONTAINER="$$( \
	  docker create --rm --user=root stephanmisc/effects:deps \
	    bash -c ' \
	      chown -R user:user repo && \
	      cd repo && \
	      su user -s /bin/bash -l -c " \
	        cd repo && make clean && make build test lint \
	      " \
	    ' \
	)" && \
	docker cp . "$$CONTAINER:/home/user/repo" && \
	docker start --attach "$$CONTAINER"
