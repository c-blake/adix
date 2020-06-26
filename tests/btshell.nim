import ppss, btree, strutils, strformat, os, times

var   verb   = false                    # global verbosity flag
var   found  = false                    # global last-seek found flag
var   f      = stdout                   # to allow switch to shared stderr
when defined(btTall):                   # Deep 234 trees hit all cases fast..
  const nodz = 32
else:                                   #..But default to 2*64B cache lines
  const nodz = 128

type                                    # Instantiate a tree type/suite
  Pair = tuple[key, val: int16]
  Ix = int16                            #XXX should not need signed type; Chk
  Ln = uint16

proc getKey(x: Pair): int16 = x.key

when defined(btHisto):
  proc size(ob: Pair): int = int(ob.val)
  defBTree(Pair, int16, int32, int16, nodz, Ix, Ln)
else:
  defBTree(Pair, int16, int32, void, nodz, Ix, Ln)
const m = btOrder()

proc print(path: Path) =                # This prints `path` on one line, but
  if not verb: return                   #..*ONLY* in verbose mode; else no-op.
  f.write &"PATH({path.len})"
  if path.len > 0:
    let k = path[^1].p[].ob[path[^1].i].key
    let v = path[^1].p[].ob[path[^1].i].val
    f.write &": {path}  K: {k} V: {v}"
  f.write '\n'

# For B-Trees, both objects on internal nodes bracketing the link are "inPath".
# This is at least better than highlighting the *entire* B-tree node.
const colorL = [ "30;7", "101", "102", "103", "104", "105", "106", "107" ]
const colorD = [ "40;7",  "41",  "42",  "43",  "44",  "45",  "46",  "47" ]
let color = if getEnv("LC_THEME", "L").toUpper[0] == 'L': colorL else: colorD
proc print(t: Ln, path: Path, lab="") =
  const indent = 8
  proc inPath(t: Ln, path: Path, ob: Pair): bool =  # path markup flag
    for i, e in path: # `apply` acts on obs=>colorize obs around each e.p,i link
      if (e.p == t and e.i < 2*m-1 and e.p[].ob[e.i] == ob) or
         (i < path.len-1 and e.i > 0 and e.p[].ob[e.i-1] == ob):
           return true
  if lab.len > 0: f.write lab
  for (t, ob, depth) in t.nodes:
    let c = if t.inPath(path,ob): color[0] else: color[(76543*int(t)) mod 7 + 1]
    f.write repeat(" ", depth * indent), "\e[", c, "m",
            &"{int(t)}: k{ob.key},v{ob.val} (w{t[].wt})", "\e[0m\n"
  path.print

proc treeCk(t: Ln): int =
  result = t.check
  if result == 0: result += int(t.refCk)

let help = """c            check many tree invariants
p            print colorized tree
P            print current `path`
h            height/occupancy statistics
m <s>        set path to s-most side; 0=min; 1=max
a <s>        set path to [01]-side neighbor
k <k>        set path to where ob with key <k> is/should be
i <k> <v>    insert k,v at current path
d            delete ob at current path
n[01] <n>    set path to 0-origin <n>-th element; optional [01] => print key,ob
r            compute 1-origin rank; show seekNth autocalc
+<s> <k> <v> seek k, then s=0|1 (pre|ap)pend k,v ; ins|ctr inc in btHisto mode
-<s> <k>     seek k, then s=0|1 (front|back)pop k; ctr dec|del in btHisto mode
A<s> <k> <x> bulk load/add k,0 from empty in s=0|1 rev|fwd order; spare <x>
D<s>         done with bulk adds
t            start stop-watch
T            stop stop-watch and print nanoseconds, #ops
z            print node size in bytes
X            no-op; maybe useful to time loop dispatch"""

proc btshell(verbose=false, quiet=false, check=false, errstd=false): int =
  ## This shell reads ``ppss`` output to test all BTree ops & post-mutate check
  ## tree invariants.  Interactive use (e.g. ppss|btshell) is good to see how it
  ## works/fails "in the small".  Small programs to generate series of inputs in
  ## ``check`` mode is good to exercise all usage modes/trap bugs reproducibly.
  ## Color-structure highlighted print outs helps show structural bugs/features.
  verb = verbose                        # Propagate CL -> convenience globals
  if errstd: f = stderr
  let t = newN()                        # Init necessary to start w/empty tree
  var path: Path
  var r = 0
  var cin: Command
  var t0: Time
  var nOp = 0
  var ob: Pair
  template maybeCk =
    if check and t.treeCk > 0: t.print(path, "ERR\n"); return 1
  while stdin.readObject(cin.addr, cin.sizeof) == 1:
    nOp.inc
    if verbose: f.write cin, '\n'
    let s = cin.side; let k = int16(cin.key); let v = int16(cin.val)
    ob.key = k; ob.val = v
    case cin.letter
    of 'c': (let nE = t.treeCk; if nE > 0: f.write nE, " ERRS\n") # check
    of 'p': t.print(path)                                         # print tree
    of 'P': (let tmp = verb; verb = true; path.print; verb = tmp) # print path
    of 'h':                                                       # height stats
      var nN,nO: int; let h = stats(t,nN,nO); let u=float(nO)/float(nN*(2*m-1))
      f.write &"nOb: {nO} nNode: {nN} height: {h} <util>: {u}\n"
    of 'm': path.setLen 0; seekMost(t, path, bool(k)); path.print # most `k`
    of 'a': seekAdj(path, bool(k)); path.print                    # adjacent `k`
    of 'k':                                                       # key search
      found = if cin.sided: t.seekKeys(path, s, k) else: t.seekKey(path, k)
      path.print
    of 'i':                                         # insert ob @path
      if path.len > 0: path.add(bool(k), ob, found)
      else: f.write "cannot insert @empty path\n"
      maybeCk
    of 'd': (if path.len > 0: path.del; maybeCk)    # Delete Ob @current path
    of 'n':                                         # Move path to Nth elt
      r = t.seekNth(path, k); path.print
      if cin.sided and path.len > 0:
        if s: f.write path[^1].p[].ob[path[^1].i], '\n'
        else: f.write path[^1].p[].ob[path[^1].i].key, '\n'
    of 'r': f.write &"rnk: {rank(path)} r: {r}\n"   # 1/0 origin rank of path
    of '+':
      when defined(btHisto):        # This eg. ignores `sided`, doing only ctr;
        found = t.seekKey(path, k)  #..With GC allocator could instead do seq
        if found:                   #..with real prepend/append with offset `r`.
          path[^1].p[].ob[path[^1].i].val.inc
          for j in 0 ..< path.len: path[j].p[].wt += 1
        else:
          ob.val = 1                    # Only reason `ob` needs to be `var`
          path.add true, ob
      else:                             # +0 k v prepend k,v; +1 k v append k,v
        if cin.sided:                   # Unconditional add
          found = t.seekKeys(path, s, k)
          path.add(s, ob, found)
        else:                           # Add if missing
          found = t.seekKey(path, k)
          if found: f.write k, " already present\n"
          else:
            path.add(s, ob, found)
            if check:                   # Maybe dbl ck path post-add
              var path2: Path; discard t.seekKey(path2, k)
              if path2 != path: f.write "post add path mismatch\n"
      maybeCk
    of '-':
      when defined(btHisto):            # Externally managed-rank integrated key
        found = t.seekKey(path, k)      #..counter example just decrements|pops.
        if found:
          if path[^1].p[].ob[path[^1].i].val == 1: path.del
          else:
            path[^1].p[].ob[path[^1].i].val.dec
            for j in 0 ..< path.len: path[j].p[].wt -= 1
        elif not quiet: f.write &"{k} not found\n"
      else:                             # -0 k front pops k; -1 k pops k
        found = if cin.sided: t.seekKeys(path, s, k) else: t.seekKey(path, k)
        if found: path.del; maybeCk
        elif not quiet: f.write &"{k} not found\n"
    of 'A': badd(path, s, ob, t, v)     # A1 k S bulk adds k,0 w/spare S
    of 'D': baddDone(t, s, k); maybeCk  # Finalize after bulk adds
    of 'X': discard
    of 'z': f.write "node size: ", Node.sizeof, " bytes\n"
    of 't': nOp = 0; t0 = getTime()
    of 'T': f.write nOp, " ops in ", (getTime() - t0).inNanoseconds, " ns\n"
    else: f.write &"unknown command '{cin.letter}'; choices are:\n{help}\n"
  return nOp mod 2                      # Ensure compiler cannot elide calc

when isMainModule:
  import cligen
  dispatch(btshell, help={ "verbose": "echo read ops & path post [maknp]",
                           "quiet"  : "\"not found\" del vs silent no-op",
                           "check"  : "auto check after all mutating ops",
                           "errstd" : "echos -> stderr" })
