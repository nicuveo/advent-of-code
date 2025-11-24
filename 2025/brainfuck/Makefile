%.out: %.bf
	bfc $< -s 10000000 -a 10000000 && mv a.out $@

%.bf: %.bs
	bfs -O $< > $@

.PRECIOUS: %.bf
