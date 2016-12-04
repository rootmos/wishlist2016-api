IMAGE=wishlist2016-postgres
PGPORT=5432

.PHONY: postgres
postgres: 
	docker build -t $(IMAGE) postgres-image
	docker run --rm -it --env-file=.env -p $(PGPORT):5432 $(IMAGE)
