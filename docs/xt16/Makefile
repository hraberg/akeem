TALK_PREFIX = the-search-for-simplicity
TALK_ARCHIVE = $(TALK_PREFIX).tgz

run:
	google-chrome http://localhost:8000 &
	python -m SimpleHTTPServer

$(TALK_PREFIX).tgz: $(shell git ls-files)
	 git archive --format=tar.gz --prefix=$(TALK_PREFIX)/ HEAD > $(TALK_ARCHIVE)

archive: $(TALK_PREFIX).tgz

clean:
	rm -f $(TALK_PREFIX).tgz

.PHONY: run, archive, clean
