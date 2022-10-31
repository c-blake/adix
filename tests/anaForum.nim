# Condensed & generalized version of Nim Forum post.  It's
# here for reference but is 17x slower than anaPrime due
# to: sortedSig, Tab[*,seq[string]], hasKey, no presize...

when not declared(open): import std/syncio
import strutils, algorithm, os, tables, unidecode

proc signature(word: string): string =
  let ascii = unidecode(word).toLowerAscii
  let sorted_word = sorted(ascii, system.cmp)
  result = sorted_word.join()

proc main =
  if paramCount() < 2:       # Parse command line
    quit("Usage: anagram <dictionary> <word>")
  let lookup_word = paramStr(2)
  let lookup_signature = signature(lookup_word)
  echo "Looking up '", lookup_word, "'"

  var anagrams = initTable[string, seq[string]]()
  for word in open(paramStr(1)).lines():
    let signature = signature(word)
    if anagrams.hasKey(signature):
      anagrams[signature].add(word)
    else:
      anagrams[signature] = @[word]
  
  if anagrams[lookup_signature].len == 1:
    echo "'", lookup_word, "' has no anagrams"
  else:
    echo anagrams[lookup_signature]

main()
