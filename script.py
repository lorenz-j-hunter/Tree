import math
from functools import partial

print_no_newline = partial(print, end='')

def begin_statement():
  print_no_newline('\n\n\n-------------------------------------------------\n')

def end_statement():
  print_no_newline('\n-------------------------------------------------\n\n\n')

def separate():
  print_no_newline('\n\n-------------------------------------------------\n\n')

def clo(bf: int, h: int) -> int:
  if h < 0:
    raise RuntimeError('h < 0')
  ret: int = 0
  for ch in range(h):
    ret += pow(bf, ch)
  return ret

def cap(bf: int, h: int) -> int:
  if h < 0:
    raise RuntimeError('h < 0')
  ret: int = 0
  for ch in range(h+1):
    ret += pow(bf, ch)
  return ret

def height(bf: int, index: int) -> int:
  if index < 0:
    raise RuntimeError('index < 0')
  ret: int = 1
  if index == 0:
     return 0
  while not (clo(bf, ret) <= index and index < cap(bf, ret)):
    ret += 1
  
  return ret

def choose(d_abs_ind=5) -> bool:
  """Wrapper for DFS."""
  val: bool = DFS(d_abs_ind) 
  return val

def choose_no_print(d_abs_ind=5) -> bool:
  """Wrapper for DFS."""
  val: bool = DFS_no_print(d_abs_ind) 
  return val

def see(bf: int, blw: int, cur: int) -> bool:
  """See if `blw` is a valid subindex of `cur` of a tree with some `bf`."""
  h_blw = height(bf=bf, index=blw)
  h_cur = height(bf=bf, index=cur)
  clo_blw = clo(bf=bf, h=h_blw)
  clo_cur = clo(bf=bf, h=h_cur)
  # case 1
  if h_blw is not h_cur+1:
    print(f'\tsee(bf={bf}, blw={blw}, cur={cur}) returns False')
    print(f'\tFirst case failed')
    return False
  r_blw: float = (blw - clo_blw) / pow(bf, h_blw)
  r_cur: float = (cur - clo_cur) / pow(bf, h_cur)
  begin = math.floor(r_cur * pow(bf, h_cur)) * bf
  end = begin + bf # end of range
  multiple = bf / pow(bf, h_blw) # fit it to a predetermined range
  r_begin = math.floor(begin / multiple) * pow(bf, h_blw)
  r_end = r_begin + multiple 
  #r_begin = begin / pow(bf, h_blw)
  #r_end = end / pow(bf, h_blw)
  # case 2
  if r_end == 1:
    if not (r_blw <= r_end and r_blw >= r_begin):
      print(f'\tsee(bf={bf}, blw={blw}, cur={cur}) returns False')
      print(f'''
            Second case failed.\n\n
            r_blw={r_blw}\tr_cur={r_cur}\tbegin={begin}\n\n
            end={end}\tr_begin={r_begin}\tr_end={r_end}
            ''')
      return False
  elif r_begin == 0:
    if not (r_blw < r_end and r_blw >= r_begin):
      print(f'\tsee(bf={bf}, blw={blw}, cur={cur}) returns False')
      print(f'''Second case failed.\n\n
            r_blw={r_blw}\tr_cur={r_cur}\tbegin={begin}\n\n
            end={end}\tr_begin={r_begin}\tr_end={r_end}''')
      return False
  else:
    if (r_blw == 0 and r_begin > r_blw):
      return True
    if not (r_blw < r_end and r_blw >= r_begin):
      print(f'\tsee(bf={bf}, blw={blw}, cur={cur}) returns False')
      print(f'''Second case failed.\n\n
            r_blw={r_blw}\tr_cur={r_cur}\tbegin={begin}\n\n
            end={end}\tr_begin={r_begin}\tr_end={r_end}''')
      return False
  print(f'\tsee(bf={bf}, blw={blw}, cur={cur}) returns True')
  return True

def see_no_print(bf: int, blw: int, cur: int) -> bool:
  """See if `blw` is a valid subindex of `cur` of a tree with some `bf`."""
  h_blw = height(bf=bf, index=blw)
  h_cur = height(bf=bf, index=cur)
  clo_blw = clo(bf=bf, h=h_blw)
  clo_cur = clo(bf=bf, h=h_cur)
  # case 1
  if h_blw is not h_cur+1:
    #print(f'\tsee(bf={bf}, blw={blw}, cur={cur}) returns False')
    #print(f'\tFirst case failed')
    return False
  r_blw: float = (blw - clo_blw) / pow(bf, h_blw)
  r_cur: float = (cur - clo_cur) / pow(bf, h_cur)
  begin = math.floor(r_cur * pow(bf, h_cur)) * bf
  end = begin + bf # end of range
  multiple = bf / pow(bf, h_blw) # fit it to a predetermined range
  r_begin = math.floor(begin / multiple) * pow(bf, h_blw)
  r_end = r_begin + multiple 
#  r_begin = begin / pow(bf, h_blw)
#  r_end = end / pow(bf, h_blw)
  # case 2
  if r_end == 1:
    if not (r_blw <= r_end and r_blw >= r_begin):
      #print(f'\tsee(bf={bf}, blw={blw}, cur={cur}) returns False')
      #print(f'''
      #      Second case failed.\n\n
      #      r_blw={r_blw}\tr_cur={r_cur}\tbegin={begin}\n\n
      #      end={end}\tr_begin={r_begin}\tr_end={r_end}
      #      ''')
      return False
  elif r_begin == 0:
    if not (r_blw < r_end and r_blw >= r_begin):
      #print(f'\tsee(bf={bf}, blw={blw}, cur={cur}) returns False')
      #print(f'''Second case failed.\n\n
      #      r_blw={r_blw}\tr_cur={r_cur}\tbegin={begin}\n\n
      #      end={end}\tr_begin={r_begin}\tr_end={r_end}''')
      return False
  else:
    if (r_blw == 0 and r_begin > r_blw):
      return True
    if not (r_blw < r_end and r_blw >= r_begin):
      #print(f'\tsee(bf={bf}, blw={blw}, cur={cur}) returns False')
      #print(f'''Second case failed.\n\n
      #      r_blw={r_blw}\tr_cur={r_cur}\tbegin={begin}\n\n
      #      end={end}\tr_begin={r_begin}\tr_end={r_end}''')
      return False
  #print(f'\tsee(bf={bf}, blw={blw}, cur={cur}) returns True')
  return True

def DFS(d_abs_ind=5) -> bool:
  """Run all tests."""
  bf = 3
  dh = height(bf=bf, index=d_abs_ind) 
  cap_less_one = clo(bf=bf, h=dh)
  begin_statement()
  print_no_newline('d_abs_ind=', d_abs_ind, '\n')
  print_no_newline('d_abs_ind - clo(dh)=',d_abs_ind-cap_less_one)
  end_statement()
  future: int = -1
  offset: int = 0
  clo_: int = 0 
  blw: int = -1
  cap_: int = 0 
  cur: int = 0
  begin_statement()
  for ch in range(1, dh+1):
    if blw == -1:
      print(f'ch={ch}')
      clo_ = 1 
      cap_ = 1 + bf
      # base case: future == -1

      """Create valid traversal indices."""
      # create cur
      cur = math.floor( ((d_abs_ind - cap_less_one) / pow(bf, dh)) / (1 / bf) ) + 1
      r_cur: float = (cur - clo_ + 1) / pow(bf, ch)
      near_begin = math.floor(r_cur / (1 / pow(bf, ch))) * bf # begin of range
      near_end = near_begin + bf

      # traverse to cur here.
      blw = 0

      print(f'''
            cur={cur}\t
            ''')
      if ch == dh:
        break
      # create offst, future
      d: float = d_abs_ind - cap_less_one
      frame: int = pow(bf, dh-1)
      offset = math.floor(d / frame) * frame
      future = offset

      clo_ = 0
      cap_ = 1 
      separate()

    else:
      # ch is currently that for blw, not cur.
      # recursive: future > 0
      cap_ += pow(bf, ch-1) # same lvl as cur, currently.
      clo_ += pow(bf, ch-2) # same lvl as cur, currently.
      print(f'ch={ch}')

      """Find valid traversal index range."""
      # create valid traversal range.
      r_cur: float = (cur - clo_) / pow(bf, ch) # near
      near_begin = math.floor(r_cur * pow(bf, ch)) * bf # begin of range
      near_end = near_begin + bf # end of valid range


      """Hone in."""
      # find frame
      frame: int = pow(bf, (dh-ch+1)) # far
      end = offset + frame  # far

      # hone in with a ratio.
      r = (d_abs_ind - cap_less_one - offset) / (end - offset) # far
      addition: int = math.floor(r * bf) # near

      """Select a valid index"""
      # valid index is 'addition'.
      blw = cap_ + near_begin + addition # near

      # create blw, trav_index

      print(f'''
            blw={blw}\ttrav_index={(blw-1) % bf}\tcur={cur}\n\n
            clo_={clo_}\tr={r}\toffset={offset}\n\n
            end={end}\tnear_begin={near_begin}\tcap_={cap_}\n\n
            addition={addition}
            ''')
      val: bool = see(bf=bf, blw=blw, cur=cur)

      separate()

      # future 
      cur = blw
      future = addition * pow(bf, (dh-ch)) # far
      offset += future # far
  end_statement()
  return True

def DFS_no_print(d_abs_ind=5) -> bool:
  """Run all tests."""
  bf = 3
  dh = height(bf=bf, index=d_abs_ind) 
  cap_less_one = clo(bf=bf, h=dh)
  future: int = -1
  offset: int = 0
  clo_: int = 0 
  blw: int = -1
  cap_: int = 0 
  cur: int = 0
  for ch in range(1, dh+1):
    if blw == -1:
      clo_ = 1 
      cap_ = 1 + bf
      # base case: future == -1

      """Create valid traversal indices."""
      # create cur
      cur = math.floor( ((d_abs_ind - cap_less_one) / pow(bf, dh)) / (1 / bf) ) + 1
      r_cur: float = (cur - clo_ + 1) / pow(bf, ch)
      near_begin = math.floor(r_cur / (1 / pow(bf, ch))) * bf # begin of range
      near_end = near_begin + bf

      # traverse to cur here.
      blw = 0
      print_no_newline(f'blw={blw}\tcur={cur}\n')
      if ch == dh:
        break
      # create offst, future
      d: float = d_abs_ind - cap_less_one
      frame: int = pow(bf, dh-1)
      offset = math.floor(d / frame) * frame
      future = offset

      clo_ = 0
      cap_ = 1 

    else:
      # ch is currently that for blw, not cur.
      # recursive: future > 0
      cap_ += pow(bf, ch-1) # same lvl as cur, currently.
      clo_ += pow(bf, ch-2) # same lvl as cur, currently.

      """Find valid traversal index range."""
      # create valid traversal range.
      r_cur: float = (cur - clo_) / pow(bf, ch) # near
      near_begin = math.floor(r_cur * pow(bf, ch)) * bf # begin of range
      near_end = near_begin + bf # end of valid range


      """Hone in."""
      # find frame
      frame: int = pow(bf, (dh-ch+1)) # far
      end = offset + frame  # far

      # hone in with a ratio.
      r = (d_abs_ind - cap_less_one - offset) / (end - offset) # far
      addition: int = math.floor(r * bf) # near

      """Select a valid index"""
      # valid index is 'addition'.
      if ch == dh:
        blw = cap_ + offset + addition # near
      else:
        blw = cap_ + near_begin + addition # near

      # create blw, trav_index
      print_no_newline(f'blw={blw}\tcur={cur}\n')

      val: bool = see_no_print(bf=bf, blw=blw, cur=cur)

      if (ch == dh and val == False):
        if (blw == d_abs_ind):
          pass
        else:
          print(f'Error')

      # future 
      cur = blw
      future = addition * pow(bf, (dh-ch)) # far
      offset += future # far
  return True

def main():
  choice: bool = bool(
    1==1 if 'True' == input("Entered desired type (True for Single, False for Many)\t")
    else 1==0)
  if choice == True:
    d_abs_ind: int = int(input("Enter desired index:\t"))
    choose(d_abs_ind=d_abs_ind)
  elif choice == False:
    for i in range(10_000):
      choose_no_print(d_abs_ind=i)

if __name__=="__main__":
  main()