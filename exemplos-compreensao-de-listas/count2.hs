count2::(a->Bool)->[a]->Int
count2 f xs = length$filter f xs

