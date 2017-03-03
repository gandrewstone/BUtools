import threading
import pdb

class LambdaThread(threading.Thread):
    def __init__(self,fn,*args,**kwargs):
        self.args = args
        self.kwargs = kwargs
        self.fn = fn
        self.ret = None
        self.exc = None
        threading.Thread.__init__(self)
    def run(self):
        print "running...", str(self.fn), " ", str(self.args)
        try:
          self.ret = self.fn(*self.args,**self.kwargs)
        except Exception, e:
          self.exc = e
        print "completed ", str(self.fn), " ", str(self.args)

          
def alltogether(*args):
    if len(args)==1 and type(args[0]) is type([]):  # user passed a list
        args = args[0]
    p = None

    if 0:  # use this thread
      if len(args) > 1:
        p = Parallelize(*args[1:])

      a = args[0]  # Execute the first job in this thread
      if type(a) is type(tuple()):
        ret = [a[0](*a[1:])]
      else:
        ret = [a()]
      
      if p: ret = ret + p.join(10.0)
    else:
      p = Parallelize(*args)
      print "waiting for join"
      ret = p.join(10.0)
      print "completed"
    return ret
                     
          
class Parallelize:
    def __init__(self,*args):
        self.fns = []
        for a in args:
            if type(a) is type(tuple()):
                self.fns.append(LambdaThread(a[0],*a[1:]))
            else:
                self.fns.append(LambdaThread(a))

        for fn in self.fns:
            fn.start()
    def join(self,timeout=None):
        for fn in self.fns:
            fn.join(timeout)
            # Todo fix to wait for only timeout time
        ret = [ x.ret for x in self.fns]
        return ret
