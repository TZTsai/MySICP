# format a word list into a scheme expression
def format(file):
    f = open(file,"r")
    fo = open("./formatted.txt","w")
    fo.write("(list \n")
    for line in f:
        pair = line.strip('\n').split(None, 1)
        fo.write("(make-entry '{} \"{}\")\n".
            format(pair[0].strip(','), pair[1]))
    fo.write(")")
    fo.close()

format("./wordlist.txt")