%.out: %.bf
	bfc $< && mv a.out $@

%.bf: %.bs
	bfs -O $< > $@

.PRECIOUS: %.bf
