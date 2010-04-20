puts (1..999).inject(0) { |memo,k| ((k%3).zero? || (k%5).zero?) ? memo+k : memo }
