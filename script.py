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
  ret: int = 0
  for ch in range(h):
    ret += pow(bf, ch)
  return ret

def cap(bf: int, h: int) -> int:
  ret: int = 0
  for ch in range(h+1):
    ret += pow(bf, ch)
  return ret

def height(bf: int, index: int) -> int:
  ret: int = 1
  if index == 0:
     return 0
  
  while not (clo(bf, ret) <= index and index < cap(bf, ret)):
    ret += 1
  
  return ret

def choose(d_abs_ind=5):
  """Wrapper for DFS."""
  DFS(d_abs_ind) 

def DFS(d_abs_ind=5):
  """Run all tests."""
  bf = 3
  dh = height(bf=bf, index=d_abs_ind) 
  cap_less_one = clo(bf=bf, h=dh)
  begin_statement()
  print_no_newline('d_abs_ind=', d_abs_ind, '\n')
  print_no_newline('d_abs_ind - clo(dh)=',d_abs_ind-cap_less_one)
  end_statement()
  blw: int = -1
  future: int = -1
  offset: int = 0
  clo_: int = 0 
  cap_: int = 1 
  begin_statement()
  for ch in range(1, dh+1):
    cap_ += pow(bf, ch);
    if blw == -1:
      print(f'ch={ch}')
      clo_ += pow(bf, ch-1)
      # base case: future == -1

      # find frame 
      d: float = d_abs_ind - cap_less_one
      frame: int = pow(bf, dh-1)
      offst: int = math.floor(d / frame) * frame

      # determine addition
      r = (d_abs_ind - cap_less_one - offst) / frame
      addition: int = math.floor(r / (1 / bf))

      # create blw
      sst: int = pow((offset / bf), ch/dh) * frame
      blw: int = cap_ + sst + addition
      print(f'trav_index={(blw-1) % bf}')
      if ch == dh:
        break
      future = offst
      print(f'd={d}\tframe={frame}\tblw={blw}\n\nfuture={future}\tclo_={clo_}\toffst={offst}\n\naddition={addition}\tr={r}')
      separate()
    else:
      # recursive: future > 0
      cap_ += pow(bf, ch) # same lvl as blw, currently.
      clo_ += pow(bf, ch-1)
      print(f'ch={ch}')

      # find frame
      offset += future
      frame: int = pow(bf, (dh-ch))
      end = offset + pow(bf, (dh-ch+1))

      # create addition
      r: float = (d_abs_ind - cap_less_one - offset) / (end - offset) # bottom
      addition: int = math.floor(r / (1/bf))

      # create blw, trav_index
      sst: int = math.floor(pow(offset, ch/dh))
      blw: int = int(clo_ + sst + addition)
      print(f'trav_index={(blw-1) % bf}')
      print(f'offset={offset}\tframe={frame}\tr={r}\n\n1/bf={1/bf}\taddition={addition}\tsst={sst}\n\nblw={blw}\tclo_={clo_}')
      separate()
      future = addition * pow(bf, (dh-ch))
  end_statement()

def main():
  d_abs_ind: int = int(input("Enter d_abs_ind:\t"))
  choose(d_abs_ind)

if __name__=="__main__":
  main()