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
  

def main():
  """Run all tests."""
  d_abs_ind = 5 
  bf = 3
  dh = height(bf=bf, index=d_abs_ind) 
  cap_less_one = clo(bf=bf, h=dh)
  cap_: int = cap(bf=bf, h=dh)
  begin_statement()
  print_no_newline('d_abs_ind=', d_abs_ind, '\n')
  print_no_newline('d_abs_ind - clo(dh)=',d_abs_ind-cap_less_one)
  end_statement()
  blw: int = -1
  future: int = -1
  offset: int = 0
  begin_statement()
  for ch in range(1, dh+1):
    if blw == -1:
      # base case: future == -1
      d: float = d_abs_ind - cap_less_one
      div: float = pow(bf, dh-1)
      addition: int = math.floor(d / div)
      blw = cap_ + addition + 1
      print(f'trav_index={(blw-1) % bf}')
      future = int((blw - 1) * div)
      print(f'd={d}\tdiv={div}\tblw={blw}\n\nfuture={future}\tblw-cap={blw-cap_}')
      separate()
    else:
      # recursive: future > 0
      cap_ += pow(bf, ch)
      offset += future
      p_rge: int = pow(bf, (dh-ch+1))
      end: int = offset + p_rge
      r: float = (d_abs_ind - cap_less_one - offset) / (end - offset)
      div: float = 1 / bf
      addition: int = math.floor(r / div)
      blw = int(cap_ + (offset / p_rge * bf) + addition)
      print(f'trav_index={(blw-1) % bf}')
      print(f'offset={offset}\tp_rge={p_rge}\tend={end}\n\nr={r}\tdiv={div}\taddition={addition}\n\nblw={blw}\tblw-cap={blw-cap_}')
      separate()
      future = addition * pow(bf, (dh-ch))
  end_statement()

if __name__=="__main__":
  main()